#-----------------------------------------------------------------------------
# Purpose:  Utility functions to calculate some common statistics
# Author:   Feiyang Niu
# Date:     April 7, 2016
#-----------------------------------------------------------------------------


SAS_TYPE <- 2


# formats decimal places
specify_decimal <- function(x, k) {
    trimws(format(round(x, k), nsmall = k))
}


gceiling <- function(x, base = 1){ 
    base * ceiling(x / base) 
}


# calculate standard error for y_bar (sample average)
std_err <- function(vec, na.rm = TRUE) {
    len <- ifelse(na.rm, length(vec[!is.na(vec)]), length(vec))
    return(sd(vec, na.rm = na.rm) / sqrt(len))
}


# Calculate number of non-NA elements
n_nna <- function(vec) {
    return(sum(!is.na(vec)))
}


# Define sd function with na.rm = TRUE as default
sd_na <- function(vec) {
    return(stats::sd(vec, na.rm = TRUE))
}


# Define sd function with na.rm = TRUE as default and string output
sd_na_str <- function(vec, digits = 2) {
    result <- stats::sd(vec, na.rm = TRUE)
    if(is.na(result)) return('')
    return(specify_decimal(result, digits))
}


# Define mean function with na.rm = TRUE as default
mean_na <- function(vec) {
    if(is.null(vec)) return(NaN)
    return(base::mean(vec, na.rm = TRUE))
}


# Define mean function with na.rm = TRUE as default and string output
mean_na_str <- function(vec, digits = 2) {
    result <- base::mean(vec, na.rm = TRUE)
    if(is.na(result)) return('')
    return(specify_decimal(result, digits))
}


# Define mean of Mean of LN (natural-log scale)
mean_ln <- function(x, na.rm = TRUE) {
    if(any(x <= 0, na.rm = TRUE)) return(NaN)
    return(base::mean(log(x), na.rm = na.rm))
}


# Define mean of Mean of LN (natural-log scale) and string output
mean_ln_str <- function(x, na.rm = TRUE, digits = 2) {
    if(any(x <= 0, na.rm = TRUE)) return('')
    result <- base::mean(log(x), na.rm = na.rm)
    if(is.na(result)) return('')
    return(specify_decimal(result, digits))
}


# Define SD of LN (natural-log scale)
sd_ln <- function(x, na.rm = TRUE, digits = 2) {
    if(any(x <= 0, na.rm = TRUE)) return(NaN)
    return(stats::sd(log(x), na.rm = na.rm))
}


# Define SD of LN (natural-log scale) and string output
sd_ln_str <- function(x, na.rm = TRUE, digits = 2) {
    if(any(x <= 0, na.rm = TRUE)) return('')
    result <- stats::sd(log(x), na.rm = na.rm)
    if(is.na(result)) return('')
    return(specify_decimal(result, digits))
}


# Define geometric mean
geo_mean <- function(x, na.rm = TRUE) {
    if(any(is.na(x)) && (!na.rm)) return(NaN)
    if(any(x <= 0, na.rm = TRUE)) return(NaN)
    return(exp(base::mean(log(base::abs(x)))))
}


# Define geometric mean and string output
geo_mean_str <- function(x, na.rm = TRUE, digits = 2) {
    if(any(is.na(x)) && (!na.rm)) return('')
    if(any(x <= 0, na.rm = TRUE)) return('')
    result <- exp(base::mean(log(base::abs(x)), na.rm = na.rm))
    if(is.na(result)) return('')
    return(specify_decimal(result, digits))
}


# Define geometric standard deviation
geo_sd <- function(x, na.rm = TRUE) {
    if(any(is.na(x)) && (!na.rm)) return(NA)
    if(any(x <= 0, na.rm = TRUE)) return(NaN)
    return(exp(stats::sd(log(x))))
}


# Define geometric standard deviation with string output
geo_sd_str <- function(x, na.rm = TRUE, digits = 2) {
    if(any(is.na(x)) && (!na.rm)) return('')
    if(any(x <= 0, na.rm = TRUE)) return('')
    result <- exp(stats::sd(log(x), na.rm = na.rm))
    if(is.na(result)) return('')
    return(specify_decimal(result, digits))
}


# Define geometric cv
geo_cv <- function(x, na.rm = TRUE, percent = TRUE) {
    if(any(is.na(x)) && (!na.rm)) return(NA)
    if(any(x <= 0, na.rm = TRUE)) return(NaN)
    result <- base::sqrt(base::exp((stats::sd(log(x)))^2) - 1)
    return(if(percent) 100 * result else result)
}


# Define geometric cv and string output
geo_cv_str <- function(x, na.rm = TRUE, percent = TRUE, digits = 2) {
    if(any(is.na(x)) && (!na.rm)) return('')
    if(any(x <= 0, na.rm = TRUE)) return('')
    result <- geo_cv(x, na.rm = na.rm, percent = percent)
    if(is.na(result)) return('')
    return(specify_decimal(result, digits))
}


# Define geometric mean + CV
geo_mean_cv_str <- function(x, digits = 2) {
    mean_str <- geo_mean_str(x, digits = digits)
    cv_str <- geo_cv_str(x, digits = digits)
    result <- mean_str
    if(cv_str != '') result <- paste0(result, ' (', cv_str, ')')
    return(result)
}


# Define coefficient of variation
coeff_var <- function(vec, na.rm = TRUE, percent = TRUE) {
    ratio_ <- stats::sd(vec, na.rm = na.rm) / base::mean(vec, na.rm = na.rm)
    return(if(percent) 100 * ratio_ else ratio_)
}


# Define coefficient of variation
coeff_var_str <- function(vec, na.rm = TRUE, percent = TRUE, digits = 2) {
    ratio_ <- stats::sd(vec, na.rm = na.rm) / base::mean(vec, na.rm = na.rm)
    result <- if(percent) 100 * ratio_ else ratio_
    if(is.na(result)) return('')
    return(specify_decimal(result, digits))
}


# Define median function with na.rm = TRUE as default
median_na <- function(vec, na.rm = TRUE) {
    return(stats::median(vec, na.rm = na.rm))
}


# Define median function with na.rm = TRUE as default
median_str <- function(vec, na.rm = TRUE, digits = 2) {
    result <- stats::median(vec, na.rm = na.rm)
    if(is.na(result)) return('')
    return(specify_decimal(result, digits))
}


# Define median function with na.rm = TRUE as default
q1_na <- function(vec, na.rm = TRUE) {
    return(quantile(vec, prob = 0.25, na.rm = na.rm, type = SAS_TYPE))
}


# Define q1 function with na.rm = TRUE as default and string output
q1_na_str <- function(vec, na.rm = TRUE, digits = 2) {
    result <- quantile(vec, prob = 0.25, na.rm = na.rm, type = SAS_TYPE)
    if(is.na(result)) return('')
    return(specify_decimal(result, digits))
}


# Define median function with na.rm = TRUE as default
q3_na <- function(vec, na.rm = TRUE) {
    return(quantile(vec, prob = 0.75, na.rm = na.rm, type = SAS_TYPE))
}


# Define q3 function with na.rm = TRUE as default and string output
q3_na_str <- function(vec, na.rm = TRUE, digits = 2) {
    result <- quantile(vec, prob = 0.75, na.rm = na.rm, type = SAS_TYPE)
    if(is.na(result)) return('')
    return(specify_decimal(result, digits))
}


# Define Median + IQR function
median_iqr <- function(x) {
    data.frame(y = median_na(x), ymin = q1_na(x), ymax = q3_na(x))
}


# Define min function with na.rm = TRUE as default
min_na <- function(..., na.rm = TRUE) {
    if(length(na.omit(c(...))) == 0) return(NA)
    return(min(..., na.rm = na.rm))
}


# Define min function with na.rm = TRUE as default and string output
min_na_str <- function(..., na.rm = TRUE, digits = 2) {
    if(length(...) == 0 || length(na.omit(c(...))) == 0) return('')
    return(specify_decimal(min(..., na.rm = na.rm), digits))
}


# Define max function with na.rm = TRUE as default
max_na <- function(..., na.rm = TRUE) {
    if(length(na.omit(c(...))) == 0) return(NA)
    return(max(..., na.rm = na.rm))
}


# Define max function with na.rm = TRUE as default and string output
max_na_str <- function(..., na.rm = TRUE, digits = 2) {
    if(length(...) == 0 || length(na.omit(c(...))) == 0) return('')
    return(specify_decimal(max(..., na.rm = na.rm), digits))
}


# Define range function with na.rm = TRUE as default
range_na <- function(..., na.rm = TRUE) {
    if(length(na.omit(c(...))) == 0) return(NA)
    return(range(..., na.rm = na.rm))
}


# Define unique function with NA's removed
unique_na <- function(obj, ...) {
    if(length(na.omit(obj)) == 0) return(NA)
    obj_nna <- obj[!is.na(obj)]
    return(unique(obj_nna, ...))
}


# Define Mean + SD function
mean_sd <- function(x) {
    mean_ <- mean_na(x)
    sd_ <- sd(x, na.rm = TRUE)
    data.frame(y = mean_, ymin = mean_ - sd_, ymax = mean_ + sd_)
}


# mean (SD) format
mean_sd_str <- function(x, digits = 2) {
    mean_str <- mean_na_str(x, digits = digits)
    sd_str <- sd_na_str(x, digits = digits + 1)
    result <- mean_str
    if(sd_str != '') result <- paste0(result, ' (', sd_str, ')')
    return(result)
}


# Q1, Q3 format
q1_q3_str <- function(x, digits = 2) {
    q1_str <- q1_na_str(x, digits = digits)
    q3_str <- q3_na_str(x, digits = digits)
    if(q1_str == '' && q3_str == '') return('')
    result <- paste(q1_str, q3_str, sep = ', ')
    return(result)
}

# Min, Max format
min_max_str <- function(x, digits = 2) {
    min_str <- min_na_str(x, digits = digits)
    max_str <- max_na_str(x, digits = digits)
    if(min_str == '' && max_str == '') return('')
    result <- paste(min_str, max_str, sep = ', ')
    return(result)
}


# mean & sd in log scale
mean_sd_ln_str <- function(x, digits = 2) {
    mean_str <- mean_ln_str(x, digits = digits)
    sd_str <- sd_ln_str(x, digits = digits + 1)
    result <- mean_str
    if(sd_str != '') result <- paste0(result, ' (', sd_str, ')')
    return(result)
}


# take average for numeric input and unique otherwise
aggregate_func <- function(x, na.rm = TRUE, single_return = TRUE) {
    if(is.numeric(x)) return(mean(x, na.rm = na.rm))
    else {
        res <- unique_na(x)
        if(single_return) return(res[1])
        else return(res)
    }
}


# Calculation the intersection of two intervals
intersection <- function(vec_a, vec_b) {
    return(c(max_na(vec_a[1], vec_b[1]), min_na(vec_a[2], vec_b[2])))
}


# return x if expression if TRUE and y otherwise
ternary <- function(expr, obj_x, obj_y) {
    stopifnot(is.logical(expr))
    if(expr)
        return(obj_x)
    else
        return(obj_y)
}


# reproducible jitter function
rep_jitter <- function(..., seed = 0) {
    set.seed(seed = seed)
    jitter(...)
}


# test whether a variable is continuous or discrete
continuity_test <- function(vec, threshold = 5) {
    vec <- vec[!is.na(vec)]
    if(!(is.complex(vec) || is.numeric(vec)))
        return(FALSE)
    levls <- length(unique(vec))
    return(ifelse(levls > threshold, TRUE, FALSE))
}


# all.equal function variant with verbose argument
all_equal <- function(..., verbose = FALSE) {
    res <- all.equal(...)
    if(verbose)
        return(res)
    else {
        if(is.logical(res) && res)
            return(TRUE)
        else
            return(FALSE)
    }
}


# test whether an object contains nothing
is_blank <- function(obj, empty_str_triggers = TRUE, false_triggers = FALSE) {
    if(is.function(obj)) return(FALSE)
    return(
        is.null(obj) ||
            length(obj) == 0 ||
            all(is.na(obj)) ||
            (empty_str_triggers && is.character(obj) && all(obj == '')) ||
            (false_triggers && is.logical(obj) && all(!obj))
    )
}


# test if an object if logical value TRUE
is_true <- function(obj) {
    return(is.logical(obj) && obj == TRUE)
}


# test if an object is not null and equals to the value
is_equal <- function(obj, value, opposite = FALSE) {
    if(is.null(obj)) return(FALSE)
    return(!opposite && identical(obj, value))
}


# count occurances of a value in a vector
count_occurances <- function(vec, value) {
    vec <- suppressWarnings(na.omit(vec))
    if(length(vec) == 0) return(0)
    return(sum(vec == value))
}


# add an element to a list
append_alist <- function(element, alist) {
    alist[[length(alist) + 1]] <- element
    return(alist)
}


# update an existing data frame
update_df <- function(df, rows) {
    if(is.null(df)) return(rows)
    else return(rbind(df, setNames(rows, names(df))))
}


# applying multiple functions to data frame
multi_sapply <- function(...) {
    arglist <- match.call(expand.dots = FALSE)$...
    var.names <- sapply(arglist, deparse)
    has.name <- (names(arglist) != '')
    var.names[has.name] <- names(arglist)[has.name]
    arglist <- lapply(arglist, eval.parent, n = 2)
    x <- arglist[[1]]
    arglist[[1]] <- NULL
    result <- sapply(arglist, function (FUN, x) sapply(x, FUN), x)
    colnames(result) <- var.names[-1]
    return(result)
}


# apply a list of functions on a dataframe or a list
fapply <- function(func_list, obj, transpose = FALSE) {
    result <- sapply(func_list, function(FUN, x) sapply(x, FUN), obj)
    if(length(obj) == 1) {
        result <- t(as.data.frame(result))
        rownames(result) <- names(obj)
    }
    if(transpose) {
        result <- t(result)
        rownames(result) <- names(func_list)
    } else {
        colnames(result) <- names(func_list)
    }
    return(result)
}


# pryr::partial
partial <- function (`_f`, ..., .env = parent.frame(), .lazy = TRUE) {
    stopifnot(is.function(`_f`))
    if (.lazy) {
        fcall <- substitute(`_f`(...))
    }
    else {
        fcall <- make_call(substitute(`_f`), .args = list(...))
    }
    fcall[[length(fcall) + 1]] <- quote(...)
    args <- list(... = quote(expr = ))
    all_named <- function(x) {
        if (length(x) == 0) return(TRUE)
        !is.null(names(x)) && all(names(x) != "")
    }
    to_env <- function(x, quiet = FALSE) {
        if (is.environment(x)) {
            x
        } else if (is.list(x)) {
            list2env(x)
        } else if (is.function(x)) {
            environment(x)
        } else if (length(x) == 1 && is.character(x)) {
            if (!quiet) message("Using environment ", x)
            as.environment(x)
        } else if (length(x) == 1 && is.numeric(x) && x > 0) {
            if (!quiet) message("Using environment ", search()[x])
            as.environment(x)
        } else {
            stop("Input can not be coerced to an environment", call. = FALSE)
        }
    }
    make_function <- function (args, body, env = parent.frame()) {
        args <- as.pairlist(args)
        stopifnot(all_named(args), is.language(body))
        env <- to_env(env)
        eval(call("function", args, body), env)
    }
    make_function(args, fcall, .env)
}


# split a data frame according to two factors (row factor and column factor)
split_2d <- function(df, row_name, col_name = NULL, include_total = FALSE) {
    all_columns <- names(df)
    if(is.null(col_name)) {
        return(split(df[, -match(row_name, all_columns)], df[, row_name]))
    } else {
        other_cols <- base::setdiff(all_columns, c(row_name, col_name))
        temp <- split(df[, c(other_cols, row_name)], df[, col_name])
        data_list <- lapply(lapply(temp, `[`, other_cols), as.matrix)
        factors <- lapply(temp, `[`, row_name)
        result <- mapply(split, data_list, factors)
        if(include_total) {
            result <- cbind(result, array(
                split(df[, other_cols], df[row_name]),
                dim = c(length(unique(df[[row_name]])), 1),
                dimnames = list(dimnames(result)[[1]], 'Total')
            ))
        }
        return(result)
    }
}


# function that inserts value after positions
vector_insert <- function(vec, values, after) {
    
    if(!is.vector(vec) || is.list(vec))
        stop(paste("Argument 'vec' is not a vector:", class(vec)))
    len <- length(vec)
    if(any(after < 1 | after > len + 1))
        stop(paste("Argument 'after' contains indices out of range:",
                   paste(after, collapse = ", ")))
    if(any(duplicated(after)))
        stop(paste("Argument 'after' contains duplicated indices: ",
                   paste(after, collapse = ", ")))
    if(!is.vector(values))
        stop(paste("Argument 'values' is not a vector: ", class(values)))
    
    vec_split <- split(vec, cumsum(seq(vec) %in% (after + 1)))
    idx <- order(c(seq_along(vec_split), seq_along(values)))
    result <- unlist(c(vec_split, values)[idx], use.names = FALSE)
    return(result)
}























