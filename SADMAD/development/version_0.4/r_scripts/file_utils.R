#-----------------------------------------------------------------------------
# Purpose:  Utility functions to deal with files in R
# Author:   Feiyang Niu
# Date:     July 21, 2016
#-----------------------------------------------------------------------------


# load necessary scripts and packages
install_('XLConnect')


# returns the file paths without extensions (and the leading dot)
file_path_sans_ext <- function (x, compression = FALSE) {
    if (compression) 
        x <- sub("[.](gz|bz2|xz)$", "", x)
    sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x)
}


# returns the file (name) extensions
file_extension <- function (x) {
    pos <- regexpr("\\.([[:alnum:]]+)$", x)
    ifelse(pos > -1L, substring(x, pos + 1L), "")
}


# create a file at a given location
file_create <- function(file_name, file_dir, content, sheet = 'DATA',
                       format = NULL) {
    if(is.null(file_dir)) file_dir <- '.'
    file_path <- paste(file_dir, file_name, sep = '\\')
    if(file.exists(file_path)) file.remove(file_path)
    if(is.null(format)) format <- file_extension(file_name)
    format <- match.arg(format, c('xls', 'xlsx', 'csv', 'txt'))
    col_names <- names(content)
    has_name <- is.character(col_names) && any(sapply(col_names, nchar) > 0)
    if(format %in% c('xls', 'xlsx')) {
        file_path <- paste0(file_path_sans_ext(file_path), '.xlsx')
        openxlsx::write.xlsx(content, file_path, colNames = TRUE)
        # # Load workbook; create if not existing
        # wb <- XLConnect::loadWorkbook(file_path, create = TRUE)
        # # write to the workbook
        # XLConnect::createSheet(wb, name = sheet)
        # XLConnect::writeWorksheet(wb, content, sheet, header = has_name)
        # # Save workbook
        # XLConnect::saveWorkbook(wb)
    } else if(format == 'csv') {
        write.csv(content, file = file_path, row.names = FALSE, na = '')
    } else if(format == 'txt') {
        write.table(content, file = file_path, row.names = FALSE, na = '')
    }
}




























