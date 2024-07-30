#-----------------------------------------------------------------------------
# Purpose:  Utility functions to deal with string in R
# Author:   Feiyang Niu
# Date:     April 14, 2016
#-----------------------------------------------------------------------------


# R version of `endswith`; returns TRUE if the string ends with the suffix
endswith <- function(string, suffixes) {
    pattern <- paste0(suffixes, '$', collapse = '|')
    return(grepl(pattern, string))
}


# R version of `startswith`; returns TRUE if the string starts with the prefixes
startswith <- function(string, prefixes) {
    pattern <- paste0('^', prefixes, collapse = '|')
    return(grepl(pattern, string))
}


# add paranthesis to surround a string; return empty if the string is empty
add_parenthesis <- function(astring) {
    if(is_blank(astring)) return(character(0L))
    return(paste0('(', as.character(astring), ')'))
}


# obtain number of decimal places
decimalplaces <- function(x) {
    if((x %% 1) != 0) {
        parts <- strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)
        return(nchar(parts[[1]][[2]]))
    } else {
        return(0)
    }
}


# extracting the last n characters from a string
substr_tail <- function(x, n){
    substr(x, nchar(x) - n + 1, nchar(x))
}


# a function that computes the width of the given strings in lines
strlines <- function(a_string) {
    strwidth(a_string, units = 'inches') / 0.2
}


# a function that extracts last alphanumeric parts in a string
last_alnum <- function(vec) {
    pattern <- '[^0-9a-zA-Z]*([0-9a-zA-Z]+)\\z'
    return(stringr::str_match(vec, pattern)[, 2])
}


# a function that extracts last numeric parts in a string
last_num <- function(vec) {
    pattern <- '[^0-9]*([0-9]+)\\z'
    return(stringr::str_match(vec, pattern)[, 2])
}


# convert to character with option to replace <NA> to empty string
to_character <- function(vec, na_to_emptystr = TRUE) {
    na_to_emptystr <- isTRUE(na_to_emptystr)
    result <- as.character(vec)
    if(na_to_emptystr) {
        result[is.na(result)] <- ''
    }
    return(result)
}


# convert NA to empty string
na_to_blank <- function(vec, to_replace = '') {
    idx_na <- is.na(vec)
    if(sum(idx_na) > 0) vec[idx_na] <- to_replace
    return(vec)
}


# smart round
smart_round <- function(a_scalar) {
    if(!(is.character(a_scalar) | is.numeric(a_scalar) | is.complex(a_scalar)))
        return(a_scalar)
    else if(suppressWarnings(is.na(as.complex(a_scalar))))
        return(a_scalar)
    else {
        if(is.character(a_scalar)) {
            if(suppressWarnings(is.na(as.numeric(a_scalar))))
                a_scalar <- as.complex(a_scalar)
            else
                a_scalar <- as.numeric(a_scalar)
        }
        if(is.integer(a_scalar))
            return(a_scalar)
        else if(is.complex(a_scalar))
            return(smart_round(Re(a_scalar)) + 1i * smart_round(Im(a_scalar)))
        else if(abs(a_scalar) >= 10)
            return(round(a_scalar, 1))
        else if(abs(a_scalar) >= 1)
            return(round(a_scalar, 2))
        else
            return(signif(a_scalar, 2))
    }
}


# smart print
smart_print <- function(obj) {
    obj_char <- sapply(obj, as.character)
    obj_print <- sapply(obj_char, smart_round)
    return(obj_print)
}


# capitalize first letter
cap_first_letter <- function(string) {
    words <- unlist(strsplit(string, ' '))
    paste(toupper(substring(words, 1, 1)), substring(words, 2),
          sep = '', collapse = ' ')
}



























