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





























