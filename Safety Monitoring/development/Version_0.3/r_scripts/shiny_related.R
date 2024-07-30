#-----------------------------------------------------------------------------
# Purpose:  Utility functions to build customized Shiny UI widgets
# Author:   Feiyang Niu
# Date:     April 13, 2016
#-----------------------------------------------------------------------------


# load packaegs and necessary R script files
use_package('shiny')
install_('sas7bdat')
install_('XLConnect')
source('r_scripts/common_statistics.R')
source('r_scripts/strings.R')


fix_uploaded_files_names <- function(file_input) {
    #------------------------------------------------------------
    # renames all uploaded files to their original names
    #
    # @Arguments:
    #   file_input: data frame
    #       a data frame returned from a shiny::fileInput
    #
    # @Returns:
    #   file_input: data frame
    #       a data frame with the temporary file name corrected
    #       to its original
    #------------------------------------------------------------
    if (is.null(file_input)) {
        return()
    }
    old_name = file_input$datapath
    # file_name_time_stamp <- paste0(
    #     file_path_sans_ext(file_input$name),
    #     format(Sys.time(), format = '%Y%m%d%H%M%S'),
    #     file_extension(file_input$name)
    # )
    new_name = file.path(dirname(file_input$datapath), file_input$name)
    file.rename(from = old_name, to = new_name)
    file_input$datapath <- new_name
    return(file_input)
}


check_required_cols <- function(data, required_cols) {
    col_match <- required_cols %in% names(data)
    if(all(col_match)) return()
    error_message <- paste(
        'The following required columns are not found in the data:',
        paste(required_cols[!col_match], collapse = ',\t'), sep = '\n'
    )
    stop(error_message, call. = FALSE)
}


sas_num2date <- function(data){
    m <- lapply(attr(data, 'column.info'), function(x) x$format)
    ind1 <- as.numeric(lapply(m, function(x) x == 'DATE' ))
    ind2 <- as.numeric(lapply(m, function(x) x %in% c('DATETIME', 'TIME')))
    ind1 <- which(ind1 == 1)
    ind2 <- which(ind2 == 1)
    data[ind1] <- lapply(
        data[ind1], function(x) as.Date(x, origin = '1960-01-01')
    )
    data[ind2] <- lapply(
        data[ind2],
        function(x) as.Date(as.POSIXct(x, origin = '1960-01-01', tz = 'UTC'))
    )
    return(data)
} 


shiny_readin_file <- function(file_input, excel_sheet = 1, required_cols = NULL) {
    #------------------------------------------------------------
    # renames all uploaded files to their original names
    #
    # @Arguments:
    #   file_input: data frame
    #       a data frame returned from a shiny::fileInput which contains the
    #       following columns:
    #           a. name: The filename provided by the web browser
    #           b. size: The size of the uploaded data, in bytes.
    #           c. type: The MIME type reported by the browser (for example, 
    #              text/plain), or empty string if the browser didn't know.
    #           d. datapath: The path to a temp file that contains the data 
    #              that was uploaded.
    #   excel_sheet: The name or index of the worksheet to read from for 
    #                XLS/XLSX input data format
    #
    # @Returns:
    #   rawdata:    data frame
    #       the data uploaded by the user in data frame format
    #------------------------------------------------------------
    if (is.null(file_input)) {
        return()
    }
    file_input <- fix_uploaded_files_names(file_input)
    file_name <- tolower(file_input$name)
    file_types <- c('.csv', '.tsv', '.txt', '.rdata', '.sas7bdat', '.xls', '.xlsx')
    type_error_msg <- paste('File must be in one of the following formats:',
                            '1. CSV/TSV/TXT', '2. RData',
                            '3. sas7bdat', '4. XLS/XLSX')
    if(!endswith(file_name, file_types)) stop(type_error_msg)
    if(endswith(file_name, c('.csv', '.tsv', '.txt'))) {
        rawdata <- read.csv(file_input$datapath, header = TRUE,
                            stringsAsFactors = FALSE, na.strings = c('NA', ''),
                            check.names = FALSE)
        names(rawdata) <- trimws(names(rawdata))
        named_cols <- names(rawdata) != ''
        rawdata <- rawdata[, named_cols, drop = FALSE]
        if(!is_blank(required_cols)) check_required_cols(rawdata, required_cols)
        return(rawdata)
    } else if(endswith(file_name, '.rdata')) {
        data_env <- new.env()
        load(file_input$datapath, envir = data_env)
        object_names <- ls(data_env)
        if(length(object_names) > 1)
            stop('Uploaded RData file should only contain one data object')
        factor_idx <- sapply(data_env[[object_names]], is.factor)
        data_env[[object_names]][factor_idx] <- 
            lapply(data_env[[object_names]][factor_idx], as.character)
        data_env[[object_names]][data_env[[object_names]] == ''] <- NA
        if(!is_blank(required_cols))
            check_required_cols(data_env[[object_names]], required_cols)
        return(data_env[[object_names]])
    } else if(endswith(file_name, '.sas7bdat')) {
        # rawdata <- sas7bdat::read.sas7bdat(file_input$datapath)
        # rawdata <- sas_num2date(rawdata)
        rawdata <- haven::read_sas(file_input$datapath)
        factor_idx <- sapply(rawdata, is.factor)
        rawdata[factor_idx] <- lapply(rawdata[factor_idx], as.character)
        # rawdata[rawdata == ''] <- NA
        names(rawdata) <- trimws(names(rawdata))
        named_cols <- names(rawdata) != ''
        rawdata <- rawdata[, named_cols, drop = FALSE]
        if(!is_blank(required_cols)) check_required_cols(rawdata, required_cols)
        return(rawdata)
    } else if(endswith(file_name, c('.xls', '.xlsx'))) {
        wb <- XLConnect::loadWorkbook(file_input$datapath)
        XLConnect::setMissingValue(wb, value = c('NA', ''))
        if(is.character(excel_sheet)) {
            if(!XLConnect::existsSheet(wb, excel_sheet))
                stop(paste(file_input$name, 'does not have a sheet named',
                           excel_sheet))
        }
        rawdata <- XLConnect::readWorksheet(wb, sheet = excel_sheet,
                                            header = T, check.names = F)
        names(rawdata) <- trimws(names(rawdata))
        named_cols <- names(rawdata) != ''
        rawdata <- rawdata[, named_cols, drop = FALSE]
        if(!is_blank(required_cols)) check_required_cols(rawdata, required_cols)
        return(rawdata)
    }
}


# define an input area
textareaInput <- function(inputId, label, value = '', placeholder = '',
                          rows = 2){
    tagList(
        div(strong(label), style="margin-top: 5px;"),
        tags$style(type="text/css", "textarea {width:100%; margin-top: 5px;}"),
        tags$textarea(id = inputId, placeholder = placeholder, rows = rows, value))
}


# add tooltip to show point information when hovered/clicked
tooltip <- function(coord, data, xvar, yvar,
                    vars_print = c(xvar, yvar), vars_name = vars_print,
                    threshold = 5, maxpoints = 1) {
    the_point <- nearPoints(data, coord, xvar, yvar, threshold = threshold,
                            maxpoints = maxpoints)
    if(nrow(the_point) == 0)
        return(NULL)
    dmn <- coord$domain
    rng <- coord$range
    if(!is.null(coord$log$x))
        left_pct <- (log(coord$x, coord$log$x) - dmn$left) / (dmn$right - dmn$left)
    else
        left_pct <- (coord$x - dmn$left) / (dmn$right - dmn$left)
    if(!is.null(coord$log$y))
        top_pct <- (log(coord$y, coord$log$y) - dmn$top) / (dmn$bottom - dmn$top)
    else
        top_pct <- (coord$y - dmn$top) / (dmn$bottom - dmn$top)
    left_px <- rng$left + left_pct * (rng$right - rng$left)
    top_px <- rng$top + top_pct * (rng$bottom - rng$top)
    style <- paste0('position:absolute;', 'z-index:100;',
                    'left:', left_px + 2, 'px;',
                    'top:', top_px + 2, 'px;',
                    'background-color: rgba(40, 40, 40, 0.75);',
                    'color: rgb(255, 255, 255);',
                    'padding: 2.5px 10px;', 'border-radius: 5px;')
    msg <- paste(sapply(lapply(vars_name, tags$b), as.character),
                 smart_print(the_point[, vars_print]),
                 sep = ': ', collapse = '<br/>')
    return(tags$div(style = style, tags$span(HTML(msg))))
}


# add html page attributes
html_page <- function(content) {
    return(
        invisible(
            paste("<html>",
                  "<head>",
                  "<meta http-equiv=\"Content-type\" content=\"text/html; charset=UTF-8\">",
                  "</head>",
                  "<body>",
                  "<div style=\"margin: 0 auto; display: table; margin-top: 1em;\">",
                  content,
                  "</div>",
                  "</body>",
                  "</html>", sep = "\n")
        )
    )
}






































