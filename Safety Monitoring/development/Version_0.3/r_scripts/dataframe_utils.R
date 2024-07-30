#-----------------------------------------------------------------------------
# Purpose:  Utility functions dealing with data frames
# Author:   Feiyang Niu
# Date:     October, 2016
#-----------------------------------------------------------------------------


# load packaegs and necessary R script files
use_package('dplyr')


# check whether a vector is numeric or numeric-convertable
is_numeric_like_vec <- function(vec) {
    if(!is.atomic(vec)) {
        stop(paste0('`vec` is of class ', class(vec), '.',
                    ' `is_numeric_like` only supports atomic vector'))
    }
    if(is.numeric(vec)) return(TRUE)
    if(is.logical(vec)) return(FALSE)
    if(is.factor(vec)) vec <- as.character(vec)
    vec <- vec[!is.na(vec)]
    if(length(vec) == 0) return(FALSE)
    if(all(!is.na(suppressWarnings(as.numeric(vec))))) return(TRUE)
    return(FALSE)
}


# convert numeric-like vector
convert_numeric_like_vec <- function(vec) {
    if(!is.atomic(vec)) {
        stop(paste0('`vec` is of class ', class(vec), '.',
                    ' `is_numeric_like` only supports atomic vector'))
    }
    if(is.numeric(vec) || !is_numeric_like_vec(vec)) return(vec)
    else return(as.numeric(as.character(vec)))
}


# convert all numeric-like columns in a dataframe to numeric ones
convert_numeric_like_df <- function(df) {
    if(!is.data.frame(df))
        stop('`df` must be a dataframe')
    df <- lapply(df, convert_numeric_like_vec)
    df <- data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
}


# aggregate rows of a dataframe by group variables in the following way
#   1. For numeric columns, calculate mean of each category
#   2. For character columns, extract the first non-NA element
aggregate_df <- function(df, by) {
    if(!is.data.frame(df))
        stop('`df` must be a dataframe')
    if(length(df) < 1 || !is.character(by))
        stop('`by` must be a character vector of positive length')
    df <- convert_numeric_like_df(df)
    filter_str <- paste(paste0('!is.na(', by, ')'), collapse = ' & ')
    group_by_dots <- lapply(by, as.symbol)
    
    avg_custom <- function(vec) {
        if(length(na.omit(vec)) == 0) return(vec[1])
        else if(is.numeric(vec)) return(mean(vec, na.rm = TRUE))
        else return(na.omit(vec)[1])
    }
    
    res <- df %>%
        filter_(filter_str) %>%
        group_by_(.dots = group_by_dots) %>%
        summarise_each(funs(avg_custom))
    return(res)
}


# merge duplicated columns when deploying `join` function in `dplyr` package
merge_join_duplicated_cols <- function(df, suffix = c('.x'='.x', '.y'='.y')) {
    
    endswith <- function(string, suffixes) {
        pattern <- paste0(suffixes, '$', collapse = '|')
        return(grepl(pattern, string))
    }
    
    to_keep <- c()
    to_merge <- c()
    all_cols <- names(df)
    duplicated_xcols <- sort(all_cols[endswith(all_cols, '.x')])
    duplicated_ycols <- sort(all_cols[endswith(all_cols, '.y')])
    duplicated_cols <- intersect(gsub('.x', '', duplicated_xcols),
                                 gsub('.y', '', duplicated_ycols))
    for(col in duplicated_cols) {
        xcol <- na.omit(trimws(as.character(df[[paste0(col, '.x')]])))
        ycol <- na.omit(trimws(as.character(df[[paste0(col, '.y')]])))
        if(length(xcol) == length(ycol)) {
            print(sum(xcol == ycol) / length(xcol))
            if(length(xcol) == 0 || identical(xcol, ycol))
                to_merge <- c(to_merge, col)
            else to_keep <- c(to_keep, col)
        } else to_keep <- c(to_keep, col)
    }
    
    if(length(to_merge) > 0) {
        to_merge_x <- paste0(to_merge, '.x')
        to_merge_y <- paste0(to_merge, '.y')
        df[to_merge] <- df[to_merge_x]
        df[c(to_merge_x, to_merge_y)] <- NULL
    }
    
    if(length(to_keep) > 0) {
        if(is.null(names(suffix))) {
            x_new_suffix <- suffix[[1]]
            y_new_suffix <- suffix[[2]]
        } else {
            new_suffix <- names(suffix)
            x_new_suffix <- new_suffix[[which(suffix == '.x')]]
            y_new_suffix <- new_suffix[[which(suffix == '.y')]]
        }
        to_keep_x_old <- paste0(to_keep, '.x')
        to_keep_x_new <- paste0(to_keep, x_new_suffix)
        to_keep_y_old <- paste0(to_keep, '.y')
        to_keep_y_new <- paste0(to_keep, y_new_suffix)
        df[to_keep_x_new] <- df[to_keep_x_old]
        df[to_keep_y_new] <- df[to_keep_y_old]
        df[to_keep_x_old] <- NULL
        df[to_keep_y_old] <- NULL
    }
    
    return(df)
}




























