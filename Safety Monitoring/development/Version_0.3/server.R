# server.R

shinyServer(function(input, output, session) {
    
    #-----------------------------------------------
    # Import data page
    #-----------------------------------------------
    
    # define some variables at the app launch
    data_ <- reactiveValues(
        adsl = NULL, adsl_naming = NULL, adsl_unchecked = NULL,
        adae = NULL, adae_naming = NULL, adae_subset = NULL, adae_unchecked = NULL,
        adslae_orig = NULL, adslae = NULL, adslae_subset = NULL
    )
    data_import_status <- reactiveValues(
        adsl = FALSE, adsl_naming = FALSE,
        adae = FALSE, adae_naming = FALSE,
        adslae = FALSE
    )
    data_import_msg <- reactiveValues(
        adsl = NULL, adsl_naming = NULL,
        adae = NULL, adae_naming = NULL
    )
    name_mapping <- reactiveValues(
        adsl = NULL, adae = NULL
    )

    
    # initialization:
    #   a. update data_
    #   b. update data importation status
    observe({
        if(!is.null(input$file_adsl_naming)) {
            result_ <- tryCatch(
                shiny_readin_file(input$file_adsl_naming,
                                  required_cols = adsl_naming_required_cols),
                error = c
            )
            if('message' %in% names(result_)) {
                data_import_status$adsl_naming <- FALSE
                data_import_msg$adsl_naming <- paste(
                    paste(file_import_fail_msg, input$file_adsl_naming$name),
                    result_$message, sep = '\n'
                )
            } else {
                data_import_status$adsl_naming <- TRUE
                data_import_msg$adsl_naming <- paste(
                    file_import_success_msg, input$file_adsl_naming$name
                )
                data_$adsl_naming <- result_
                name_col <- result_[[adsl_naming_required_cols[['name']]]]
                dscrp_col <-result_[[adsl_naming_required_cols[['description']]]]
                cond <- tolower(adsl_naming_dscrps)%in%trimws(tolower(dscrp_col))
                if(!all(cond)) {
                    stop(paste0(
                        'ADSL name file must contain all the following',
                        'description row:\n',
                        paste(adsl_naming_dscrps, collapse = '\n')
                    ))
                }
                match_idx <- match(
                    trimws(tolower(dscrp_col)), tolower(adsl_naming_dscrps)
                )
                name_mapping$adsl <- setNames(
                    name_col, adsl_naming_dscrps[match_idx]
                )
            }
        }
        # if(!is.null(input$file_adsl)){
        #     if(is.null(data_$adsl_unchecked)) {
        #         # file_read_into_memory_status$value <- 'Reading ADSL data...'
        #         output$file_read_into_memory_status <- renderUI({
        #             tags$html('Reading ADSL data...')
        #         })
        #         data_$adsl_unchecked <- tryCatch(
        #             shiny_readin_file(input$file_adsl, required_cols = NULL),
        #             error = c
        #         )
        #         output$file_read_into_memory_status <- renderUI({ tags$html('') })
        #         # file_read_into_memory_status$value <- ''
        #     } 
        #     if(isTRUE(data_import_status$adsl_naming)) {
        #         data_adsl_required_cols <- unname(name_mapping$adsl)
        #         result_ <- tryCatch(
        #             check_required_cols(data_$adsl_unchecked,
        #                                 data_adsl_required_cols),
        #             error = c
        #         )
        #         if('message' %in% names(result_)) {
        #             data_import_status$adsl <- FALSE
        #             data_import_msg$adsl <- paste(
        #                 paste(file_import_fail_msg, input$file_adsl$name),
        #                 result_$message, sep = '\n'
        #             )
        #         } else {
        #             data_import_status$adsl <- TRUE
        #             data_import_msg$adsl <- paste(
        #                 file_import_success_msg, input$file_adsl$name
        #             )
        #             data_$adsl <- data_$adsl_unchecked
        #         }
        #     }
        # }
        if(!is.null(input$file_adsl) && isTRUE(data_import_status$adsl_naming)){
            data_adsl_required_cols <- unname(name_mapping$adsl)
            result_ <- tryCatch(
                shiny_readin_file(input$file_adsl,
                                  required_cols = data_adsl_required_cols),
                error = c
            )
            if('message' %in% names(result_)) {
                data_import_status$adsl <- FALSE
                data_import_msg$adsl <- paste(
                    paste(file_import_fail_msg, input$file_adsl$name),
                    result_$message, sep = '\n'
                )
            } else {
                data_import_status$adsl <- TRUE
                data_import_msg$adsl <- paste(
                    file_import_success_msg, input$file_adsl$name
                )
                data_$adsl <- result_
            }
        }
        if(!is.null(input$file_adae_naming)) {
            result_ <- tryCatch(
                shiny_readin_file(input$file_adae_naming,
                                  required_cols = adae_naming_required_cols),
                error = c
            )
            if('message' %in% names(result_)) {
                data_import_status$adae_naming <- FALSE
                data_import_msg$adae_naming <- paste(
                    paste(file_import_fail_msg, input$file_adae_naming$name),
                    result_$message, sep = '\n'
                )
            } else {
                data_import_status$adae_naming <- TRUE
                data_import_msg$adae_naming <- paste(
                    file_import_success_msg, input$file_adae_naming$name
                )
                data_$adae_naming <- result_
                name_col <- result_[[adae_naming_required_cols[['name']]]]
                dscrp_col <-result_[[adae_naming_required_cols[['description']]]]
                cond <- tolower(adae_naming_dscrps)%in%trimws(tolower(dscrp_col))
                if(!all(cond)) {
                    stop(paste0(
                        'ADAE name file must contain all the following',
                        'description row:\n',
                        paste(adae_naming_dscrps, collapse = '\n')
                    ))
                }
                match_idx <- match(
                    trimws(tolower(dscrp_col)), tolower(adae_naming_dscrps)
                )
                name_mapping$adae <- setNames(
                    name_col, adae_naming_dscrps[match_idx]
                )
            }
        }
        if(!is.null(input$file_adae) && isTRUE(data_import_status$adae_naming)){
            data_adae_required_cols <- unname(name_mapping$adae)
            result_ <- tryCatch(
                shiny_readin_file(input$file_adae,
                                  required_cols = data_adae_required_cols),
                error = c
            )
            if('message' %in% names(result_)) {
                data_import_status$adae <- FALSE
                data_import_msg$adae <- paste(
                    paste(file_import_fail_msg, input$file_adae$name),
                    result_$message, sep = '\n'
                )
            } else {
                data_import_status$adae <- TRUE
                data_import_msg$adae <- paste(
                    file_import_success_msg, input$file_adae$name
                )
                data_$adae <- result_
            }
        }
    })
    
    observe({
        if(isTRUE(data_import_status$adae)) {
            to_filter <- rep(TRUE, nrow(data_$adae))
            if(isTRUE(input$file_subset_teae)){
                to_filter <- to_filter &
                    (!is.na(data_$adae[['TRTEMFL']])) &
                    (data_$adae[['TRTEMFL']] == 'Y')
            }
            # if(isTRUE(input$file_subset_remove)){
            #     to_filter <- to_filter &
            #         ((!is.na(data_$adslae[['DURATION']])) &
            #         (data_$adslae[['DURATION']] > 0))
            # }
            # to_filter <- to_filter &
            #     ((is.na(data_$adslae[['ASTDT']])) |
            #          (data_$adslae[['ASTDT']] <= input$file_subset_snapshot))
            data_$adae_subset <- data_$adae[to_filter, ]
            
        }
    })
    
    # merge ADSL and ADAE data
    output$file_adslae_merge <- renderUI({
        req(isTRUE(data_import_status$adsl), isTRUE(data_import_status$adae))
        actionButton('file_adslae_merge', 'Merge ADSL & ADAE')
    })
    observeEvent(input$file_adslae_merge, {
        adsl <- data_$adsl
        data_adsl_required_cols <- unname(name_mapping$adsl)
        adsl <- adsl[data_adsl_required_cols]
        # (In ADAE, you need to identify the first episode of the AE within
        # the same subject (by the earliest ASTDT) and remove the remaining
        # records.) This has now been changed, keep all AEs.
        USUBJID <- name_mapping$adae[['Subject ID']]
        AEBODSYS <- name_mapping$adae[['SOC']]
        AEDECOD <- name_mapping$adae[['PT']]
        ASTDT <- name_mapping$adae[['AE onset date']]
        adae <- data_$adae_subset %>%
            arrange_(USUBJID, AEBODSYS, AEDECOD, ASTDT)
        # %>%
        #     group_by_(USUBJID, AEBODSYS, AEDECOD)
        # %>%
        #     filter(row_number() == 1)
        adae[setdiff(data_adsl_required_cols, USUBJID)] <- NULL
        by_arg <- setNames(USUBJID, name_mapping$adsl[['Subject ID']])
        merged_data <- full_join(adsl, adae, by = by_arg)
        merged_data <- merge_join_duplicated_cols(
            merged_data, suffix = c('.ADSL' = '.x', '.ADAE' = '.y')
        )
        
        data_$adslae_orig <- merged_data
        data_$adslae <- merged_data
        data_import_status$adslae <- TRUE
    })
    observe({
        input$file_adsl
        input$file_adsl_naming
        input$file_adae
        input$file_adae_naming
        data_$adslae_orig <- NULL
        data_$adslae <- NULL
        data_$adslae_subset <- NULL
        data_import_status$adslae <- FALSE
    })
    
    # auto re-fresh merged data when adae gets updated
    observe({
        data_$adae_subset
        if(isTRUE(data_import_status$adslae)) {
            adsl <- data_$adsl
            data_adsl_required_cols <- unname(name_mapping$adsl)
            adsl <- adsl[data_adsl_required_cols]
            USUBJID <- name_mapping$adae[['Subject ID']]
            AEBODSYS <- name_mapping$adae[['SOC']]
            AEDECOD <- name_mapping$adae[['PT']]
            ASTDT <- name_mapping$adae[['AE onset date']]
            adae <- data_$adae_subset %>%
                arrange_(USUBJID, AEBODSYS, AEDECOD, ASTDT)
            adae[setdiff(data_adsl_required_cols, USUBJID)] <- NULL
            by_arg <- setNames(USUBJID, name_mapping$adsl[['Subject ID']])
            merged_data <- full_join(adsl, adae, by = by_arg)
            merged_data <- merge_join_duplicated_cols(
                merged_data, suffix = c('.ADSL' = '.x', '.ADAE' = '.y')
            )
            
            data_$adslae_orig <- merged_data
            data_$adslae <- merged_data
        }
    })
    
    # download merged ADSL and ADAE data
    output$file_adslae_download_button <- renderUI({
        req(data_import_status$adslae)
        downloadButton('file_adslae_download', 'Download merged data')
    })
    # download handler for merged ADSL and ADAE data
    output$file_adslae_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'ADSL&ADAE_', format(Sys.Date(), format = '%Y%m%d'), '.', 'xlsx'
            )
            return(file_name)
        },
        content = function(file) {
            # if(!exists('temp_dir') || is.null(temp_dir))
            #     temp_dir <<- tempdir()
            tlb <- data_$adslae_subset
            # file_create('output', temp_dir, tlb, format = 'xlsx')
            # file.copy(paste(temp_dir, 'output.xlsx', sep = '\\'), file)
            
            file_name <- paste(file, "xlsx", sep=".")
            # Load workbook; create if not existing
            wb <- XLConnect::loadWorkbook(file_name, create = TRUE)
            # write to the workbook
            sheet <- 'DATA'
            col_names <- names(tlb)
            has_name <- is.character(col_names) && any(sapply(col_names, nchar) > 0)
            XLConnect::createSheet(wb, name = sheet)
            XLConnect::writeWorksheet(wb, tlb, sheet, header = has_name)
            # Save workbook
            XLConnect::saveWorkbook(wb)
            # Rename the file.xlsx to file (since file cannot have extension)
            file.rename(file_name,file)
        }
    )
    
    # a tabset UI for upload data tables
    output$file_tabpanel <- renderUI({
        req(any(!is.null(input$file_adsl) || !is.null(input$file_adsl_naming),
                !is.null(input$file_adae) || !is.null(input$file_adae_naming)))
        file_tabs <- list(id = 'file_tabs')
        if(!is.null(input$file_adsl) || !is.null(input$file_adsl_naming)) {
            file_tabs <- append_alist(tabPanel(
                'ADSL data', uiOutput('file_adsl_import_msg'),
                uiOutput('file_adsl_naming_import_msg'),
                ternary(isTRUE(data_import_status$adsl),
                        dataTableOutput('file_adsl_table'), NULL)
            ), file_tabs)
        }
        if(!is.null(input$file_adae) || !is.null(input$file_adae_naming)) {
            file_tabs <- append_alist(tabPanel(
                'ADAE data', uiOutput('file_adae_import_msg'),
                uiOutput('file_adae_naming_import_msg'),
                ternary(isTRUE(data_import_status$adae),
                        dataTableOutput('file_adae_table'), NULL)
            ), file_tabs)
        }
        if(isTRUE(data_import_status$adslae)) {
            file_tabs <- append_alist(tabPanel(
                'ADSL & ADAE data', dataTableOutput('file_adslae_table')
            ), file_tabs)
        }
        do.call(tabsetPanel, file_tabs)
    })
    
    # print data importation status message
    output$file_adsl_import_msg <- renderUI({HTML(
        gsub('\n', tags$br(), data_import_msg$adsl)
    )})
    output$file_adsl_naming_import_msg <- renderUI({HTML(
        gsub('\n', tags$br(), data_import_msg$adsl_naming)
    )})
    output$file_adae_import_msg <- renderUI({HTML(
        gsub('\n', tags$br(), data_import_msg$adae)
    )})
    output$file_adae_naming_import_msg <- renderUI({HTML(
        gsub('\n', tags$br(), data_import_msg$adae_naming)
    )})
    
    # dynamically select the tab corresponding to the most recent file upload
    observe({
        input$file_adsl
        input$file_adsl_naming
        updateTabsetPanel(session, 'file_tabs', 'ADSL data')
    })
    observe({
        input$file_adae
        input$file_adae_naming
        updateTabsetPanel(session, 'file_tabs', 'ADAE data')
    })
    observe({
        data_$adslae_subset
        updateTabsetPanel(session, 'file_tabs', 'ADSL & ADAE data')
    })
    
    # data table output the four uploaded files
    output$file_adsl_table <- renderDataTable({data_$adsl})
    output$file_adae_table <- renderDataTable({data_$adae_subset})
    output$file_adslae_table <- renderDataTable({data_$adslae_subset})
    
    # perform merged ADSL & ADAE data subsetting
    observe({
        if(isTRUE(data_import_status$adslae)) {
            shinyBS::updateButton(
                session, 'file_subset_button', style = 'success', disabled = F
            )
        } else {
            shinyBS::updateButton(
                session, 'file_subset_button', style = 'default', disabled = T
            )
        }
    })
    
    
    # checkbox for data filtering
    # observe({
    #     if(isTRUE(data_import_status$adae)) {
    #         shinyjs::enable('file_subset_teae')
    #     } else {
    #         shinyjs::disable('file_subset_teae')
    #     }
    # })
    
    observe({
        if(isTRUE(data_import_status$adslae)) {
            shinyjs::enable('file_subset_teae')
            # shinyjs::enable('file_subset_remove')
            shinyjs::enable('file_subset_snapshot')
        } else {
            if(isTRUE(data_import_status$adae)){
                shinyjs::enable('file_subset_teae')
            } else {
                shinyjs::disable('file_subset_teae')
            }
            
            # shinyjs::disable('file_subset_remove')
            shinyjs::disable('file_subset_snapshot')
        }
    })
    
    file_subset_snapshot <- reactiveValues(value = NULL)
    observe({
        file_subset_snapshot$value <- input$file_subset_snapshot
    })
    
    # select input for choosing column on data subset modal window
    file_subset <- reactiveValues(number_rows = 1)
    #   -- Remark: These two lines take the value of file_subset$number_rows 
    #              and make it available in the client as
    #              output.file_subset_number_rows.
    output$file_subset_number_rows <- reactive(file_subset$number_rows)
    outputOptions(output, 'file_subset_number_rows', suspendWhenHidden = FALSE)
    output$file_subset_var <- renderUI({tagList(
        lapply(seq_len(file_subset_num_cond), function(i) {
            var_name <- paste0('file_subset_var_', i)
            conditionalPanel(
                condition = paste0('output.file_subset_number_rows', '>=', i),
                output[[var_name]] <- renderUI({
                    choices <- c('Choose'='', names(data_$adslae_orig))
                    selectInput(var_name, paste('Variable', i), choices)
                })
            )
        })
    )})
    
    # input widget for choosing values on data subset modal window
    output$file_subset_val <- renderUI({tagList(
        lapply(seq_len(file_subset_num_cond), function(i) {
            data <- data_$adslae_orig
            
            # data-filtering checkbox
            # if(isTRUE(input$file_subset_teae)) {
            #     data <- filter(data, TRTEMFL == 'Y')
            # }
            
            # if(isTRUE(input$file_subset_remove)){
            #     data <- filter(data, !is.na(DURATION) & DURATION > 0)
            # }
            
            # data-filtering based on snapshot date
            to_filter <- rep(TRUE, nrow(data))
            to_filter <- to_filter &
                ((is.na(data[[name_mapping$adae[['AE onset date']]]])) |
                     (data[[name_mapping$adae[['AE onset date']]]] <= input$file_subset_snapshot))
            data <- data[to_filter,]
            
            var_name <- input[[paste0('file_subset_var_', i)]]
            val_name <- paste0('file_subset_val_', i)
            show_condition <- paste0('input.file_subset_var_', i, ' && ',
                                     'output.file_subset_number_rows', '>=', i)
            if(!is.null(var_name) && is.numeric(data[[var_name]])) {
                val_min <- min_na(data[[var_name]])
                val_max <- max_na(data[[var_name]])
                stillSelected <- isolate(
                    ternary(length(input[[val_name]]) == 2, input[[val_name]],
                            ternary(length(input[[val_name]]) == 1,
                                    c(input[[val_name]], val_max),
                                    c(val_min, val_max)))
                )
                conditionalPanel(
                    condition = show_condition,
                    sliderInput(val_name, var_name, val_min, val_max,
                                stillSelected, sep = "")
                )
            } else if(!is.null(var_name) && is.date(data[[var_name]])) {
                start <- min_na(data[[var_name]])
                end <- max_na(data[[var_name]])
                conditionalPanel(
                    condition = show_condition,
                    dateRangeInput(val_name, var_name, start, end)
                )
            } else if(!is.null(var_name)) {
                if(is_blank(var_name))
                    choices <- character(0)
                else
                    choices <- c('Choose'='', unique(data[[var_name]]))
                stillSelected <- isolate(
                    input[[val_name]][input[[val_name]] %in% choices]
                )
                conditionalPanel(
                    condition = show_condition,
                    selectizeInput(
                        val_name, var_name, choices, stillSelected,
                        multiple = T,
                        options = list(plugins = list('drag_drop',
                                                      'remove_button'))
                    )
                )
            }
        })
    )})
    
    # input widget for choosing logical operations
    output$file_subset_logical <- renderUI({tagList(
        lapply(seq.int(from = 2, to = file_subset_num_cond), function(i) {
            var_name <- input[[paste0('file_subset_var_', i)]]
            val_name <- paste0('file_subset_val_', i)
            log_name <- paste0('file_subset_logical', i - 1)
            show_condition <- paste0('input.file_subset_var_', i, ' && ',
                                     'output.file_subset_number_rows', '>=', i)
            conditionalPanel(
                condition = show_condition,
                radioButtons(log_name, 'Logical operator', c('&', '|'),
                             inline = TRUE)
            )
        })
    )})
    
    # action button to add more data filtering rows
    output$file_subset_add <- renderUI({
        actionButton('file_subset_add', 'Add more', icon = icon('plus'))
    })
    observeEvent(input$file_subset_add, {
        file_subset$number_rows <- file_subset$number_rows + 1
    })
    
    # action button to clear the data filtering
    output$file_subset_clear <- renderUI({
        actionButton('file_subset_clear', 'Reset', icon = icon('undo'))
    })
    observeEvent(input$file_subset_clear, {
        for(i in seq_len(file_subset_num_cond)) {
            choices <- c('Choose'='', names(data_$adslae_orig))
            updateSelectInput(
                session, paste0('file_subset_var_', i), paste('Variable', i),
                choices = choices, selected = NULL
            )
        }
        file_subset$number_rows <- 1
        subset_expr$value <- ''
    })
    
    # action button to close the data subset modal window
    output$file_subset_done <- renderUI({
        actionButton('file_subset_done', 'Done', icon = icon('check'))
    })
    
    # update merged adsl and adae data based on data filtering conditions
    subset_expr <- reactiveValues(value = '')
    observeEvent(input$file_subset_done, {
        toggleModal(session, 'file_subset_bsmodal', toggle = 'close')
        
        data <- data_$adslae_orig
        to_filter <- rep(TRUE, nrow(data))
        subset_expr_str <- ''
        for(i in seq_len(file_subset_num_cond)) {
            var_name <- input[[paste0('file_subset_var_', i)]]
            val_name <- input[[paste0('file_subset_val_', i)]]
            if(!is_blank(var_name) && !is_blank(val_name)) {
                if(i >= 2) {
                    logop_name <- paste0('file_subset_logical', i - 1)
                    logop_fun <- ifelse(input[[logop_name]] == '&', `&`, `|`)
                    logop_str <- input[[logop_name]]
                } else {
                    logop_fun <- `&`
                    logop_str <- ''
                }
                if(is.numeric(data[[var_name]]) ||
                   is.date(data[[var_name]])) {
                    to_filter <- logop_fun(
                        to_filter,
                        between(data[[var_name]], val_name[1], val_name[2])
                    )
                    subset_expr_str <- paste0(
                        subset_expr_str, logop_str, ' (',
                        paste(paste(var_name, '>=', val_name[1]),
                              paste(var_name, '<=', val_name[2]),
                              sep = '&'), ')'
                    )
                } else {
                    to_filter <- logop_fun(
                        to_filter, data[[var_name]] %in% val_name
                    )
                    subset_expr_str <- paste0(
                        subset_expr_str, logop_str, ' (',
                        paste(var_name, '%in%', deparse(val_name)), ')'
                    )
                }
            }
        }
        data_$adslae <- data[to_filter, ]
        subset_expr$value <- trimws(subset_expr_str)
    })
    
    observe({
        if(isTRUE(data_import_status$adslae)) {
            to_filter <- rep(TRUE, nrow(data_$adslae))
            # if(isTRUE(input$file_subset_teae)){
            #     to_filter <- to_filter &
            #         (!is.na(data_$adslae[['TRTEMFL']])) &
            #         (data_$adslae[['TRTEMFL']] == 'Y')
            # }
            # if(isTRUE(input$file_subset_remove)){
            #     to_filter <- to_filter &
            #         ((!is.na(data_$adslae[['DURATION']])) &
            #         (data_$adslae[['DURATION']] > 0))
            # }
            to_filter <- to_filter &
                ((is.na(data_$adslae[[name_mapping$adae[['AE onset date']]]])) |
                (data_$adslae[[name_mapping$adae[['AE onset date']]]] <= input$file_subset_snapshot))
            data_$adslae_subset <- data_$adslae[to_filter, ]
            
        }
    })
    
    # # file upload status
    # # file_read_into_memory_status <- reactiveValues(value = '')
    # output$file_read_into_memory_status <- renderUI({
    #     tags$html('')
    # })
    
    
    #-----------------------------------------------
    # Safety analysis page
    #-----------------------------------------------
    
    # treatment mapping
    output$treatment_mapping_ui <- renderUI({
        req(data_import_status$adslae)
        tagList(
            tags$h4('Treatment mapping'),
            fluidRow(
                column(width = 4, tagList(HTML('Control'))),
                column(width = 8, uiOutput('control_mapping'))
            ),
            fluidRow(
                column(width = 4, tagList(HTML('Treatment'))),
                column(width = 8, uiOutput('treatment_mapping'))
            )
        )
    })
    output$control_mapping <- renderUI({
        req(data_import_status$adslae)
        data <- data_$adslae_subset
        treatment_col <- name_mapping$adsl[['Actual treatment received']]
        choices <- c('Choose' = '', unique(na.omit(data[[treatment_col]])))
        if(!is_blank(input$treatment_mapping))
            choices <- setdiff(choices, input$treatment_mapping)
        selected <- isolate(
            ternary(is_blank(input$control_mapping), '', input$control_mapping)
        )
        selectInput('control_mapping', NULL, choices, selected)
    })
    output$treatment_mapping <- renderUI({
        req(data_import_status$adslae)
        data <- data_$adslae_subset
        treatment_col <- name_mapping$adsl[['Actual treatment received']]
        choices <- c('Choose' = '', unique(na.omit(data[[treatment_col]])))
        if(!is_blank(input$control_mapping))
            choices <- setdiff(choices, input$control_mapping)
        selected <- isolate(
            ternary(is_blank(input$treatment_mapping), '',
                    input$treatment_mapping)
        )
        selectInput('treatment_mapping', NULL, choices, selected)
    })
    
    # specify AE structure
    output$ae_structure_ui <- renderUI({
        req(data_import_status$adslae)
        tagList(
            tags$h4('AE structure'),
            fluidRow(
                column(width = 4, tagList(HTML('High level'))),
                column(width = 8, uiOutput('ae_level_1'))
            ),
            fluidRow(
                column(width = 4, tagList(HTML('Low level'))),
                column(width = 8, uiOutput('ae_level_2'))
            )
        )
    })
    output$ae_level_1 <- renderUI({
        req(data_import_status$adslae)
        choices <- c('Choose' = '', 'SOC', 'HLGT', 'HLT')
        selectInput('ae_level_1', NULL, choices = choices)
    })
    output$ae_level_2 <- renderUI({
        req(data_import_status$adslae)
        if(is_blank(input$ae_level_1)) choices <- c()
        else if(input$ae_level_1 == 'SOC') choices <- c('HLGT', 'HLT', 'PT')
        else if(input$ae_level_1 == 'HLGT') choices <- c('HLT', 'PT')
        else if(input$ae_level_1 == 'HLT') choices <- 'PT'
        choices <- c('Choose' = '', choices)
        selected <- isolate(
            if(is.null(input$ae_level_2) || (!input$ae_level_2 %in% choices)) NULL
            else input$ae_level_2
        )
        selectInput('ae_level_2', NULL, choices = choices, selected = selected)
    })
    
    # last dose plus __ days
    output$lastdose_plus_ui <- renderUI({
        req(data_import_status$adslae)
        tagList(
            # fluidRow(
            #     column(width = 8, uiOutput('lastdose_plus_description')),
            #     column(width = 4, uiOutput('lastdose_plus'))
            # ),
            uiOutput('lastdose_plus_description_part1'),
            fluidRow(
                column(width = 6, uiOutput('lastdose_plus_description_part2')),
                column(width = 4, uiOutput('lastdose_plus')),
                column(width = 2, uiOutput('lastdose_plus_unit'))
            ),
            tags$style(
                type='text/css',
                "#lastdose_plus_description_part2 {margin-top: 5px;}",
                "#lastdose_plus_unit {margin-top: 5px;}"
            )
        )
    })
    output$lastdose_plus_description_part1 <- renderUI({
        tagList(HTML('Exposure for no AE occurrence is defined as: <br/>
                     First dose date to'))
    })
    output$lastdose_plus_description_part2 <- renderUI({
        tagList(HTML('Last dose date plus'))
    })
    output$lastdose_plus <- renderUI({
        textInput('lastdose_plus', NULL, 30)
    })
    output$lastdose_plus_unit <- renderUI({
        tagList(HTML('Days'))
    })
    
    # confidence level
    output$confidence_level_ui <- renderUI({
        req(data_import_status$adslae)
        tagList(
            fluidRow(
                column(width = 8, uiOutput('confidence_level_description')),
                column(width = 4, uiOutput('confidence_level'))
            ),
            tags$style(
                type='text/css',
                "#confidence_level_description {margin-top: 5px;}"
            )
        )
    })
    output$confidence_level_description <- renderUI({
        tagList(HTML('Confidence level'))
    })
    output$confidence_level <- renderUI({
        textInput('confidence_level', NULL, 0.95)
    })
    
    # landmark km time point
    output$landmark_km_t_ui <- renderUI({
        req(data_import_status$adslae)
        tagList(
            fluidRow(
                column(width = 8, uiOutput('landmark_km_t_description')),
                column(width = 4, uiOutput('landmark_km_t'))
            ),
            tags$style(
                type='text/css',
                "#landmark_km_t_description {margin-top: 5px;}"
            )
        )
    })
    output$landmark_km_t_description <- renderUI({
        tagList(HTML('Landmark KM time point (year)'))
    })
    output$landmark_km_t <- renderUI({
        default_value <- ''
        value <- isolate(ternary(is.null(input$landmark_km_t), default_value, input$landmark_km_t))
        textInput('landmark_km_t', NULL, value)
    })
    
    # DFDR p value threshold
    output$DFDR_p_ui <- renderUI({
        req(data_import_status$adslae)
        # req(data_import_status$adslae, !output_show$volplot,
        #     !output_show$combplot)
        tagList(
            fluidRow(
                column(width = 8, uiOutput('DFDR_p_text')),
                column(width = 4, uiOutput('DFDR_p'))
            ),
            tags$style(
                type='text/css',
                "#DFDR_p_text {margin-top: 5px;}"
            )
        )
    })
    output$DFDR_p_text <- renderUI({
        tagList(HTML('DFDR p value threshold'))
    })
    output$DFDR_p <- renderUI({
        value <- isolate(ternary(is.null(input$DFDR_p), '0.1', input$DFDR_p))
        textInput('DFDR_p', NULL, value)
    })
    
    # DFDR Rule of 4 checkbox
    output$DFDR_ruleof4_ui <- renderUI({
        req(data_import_status$adslae)
        selected <- isolate(ternary(is.null(input$DFDR_ruleof4), FALSE, input$DFDR_ruleof4))
        checkboxInput('DFDR_ruleof4', 'Apply Rule of 4 to DFDR', value = selected)
    })
    
    # BHMM Control Panel
    output$BHMM_control_ui <- renderUI({
        req(data_import_status$adslae)
        # req(data_import_status$adslae, !output_show$volplot,
        #     !output_show$combplot)
        tagList(
            tags$h4('BHMM models'),
            fluidRow(
                column(width = 8, uiOutput('BHMM_poisson_include_ui')),
                column(width = 4)
            ),
            fluidRow(
                column(width = 8, uiOutput('BHMM_logistic_include_ui')),
                column(width = 4)
            ),
            fluidRow(
                column(width = 8, uiOutput('BHMM_cutoff_text')),
                column(width = 4, uiOutput('BHMM_cutoff'))
            ),
            fluidRow(
                column(width = 8, uiOutput('BHMM_nDraw_text')),
                column(width = 4, uiOutput('BHMM_nDraw'))
            ),
            fluidRow(
                column(width = 8, uiOutput('BHMM_burnin_text')),
                column(width = 4, uiOutput('BHMM_burnin'))
            ),
            tags$style(
                type='text/css',
                "#BHMM_cutoff_text {margin-top: 5px;}"
            ),
            tags$style(
                type='text/css',
                "#BHMM_nDraw_text {margin-top: 5px;}"
            ),
            tags$style(
                type='text/css',
                "#BHMM_burnin_text {margin-top: 5px;}"
            )
        )
    })
    
    # Select or deselect BHMM models
    output$BHMM_poisson_include_ui <- renderUI({
        selected <- isolate(ternary(is.null(input$BHMM_poisson_include), FALSE, input$BHMM_poisson_include))
        checkboxInput('BHMM_poisson_include', 'Add BHMM Poisson model', value = selected)
    })
    
    output$BHMM_logistic_include_ui <- renderUI({
        selected <- isolate(ternary(is.null(input$BHMM_logistic_include), FALSE, input$BHMM_logistic_include))
        checkboxInput('BHMM_logistic_include', 'Add BHMM Logistic model', value = selected)
    })
    
    # BHMM posterior probability cutoff
    output$BHMM_cutoff_text <- renderUI({
        req(any(!is.null(input$BHMM_poisson_include) & isTRUE(input$BHMM_poisson_include),
                !is.null(input$BHMM_logistic_include) & isTRUE(input$BHMM_logistic_include))
            )
        tagList(HTML('BHMM log of relative risk cutoff (usually a positive number)'))
    })
    output$BHMM_cutoff <- renderUI({
        req(any(!is.null(input$BHMM_poisson_include) & isTRUE(input$BHMM_poisson_include),
                !is.null(input$BHMM_logistic_include) & isTRUE(input$BHMM_logistic_include))
        )
        value <- isolate(ternary(is.null(input$BHMM_cutoff), '0', input$BHMM_cutoff))
        textInput('BHMM_cutoff', NULL, value)
    })
    
    # BHMM nDraw
    output$BHMM_nDraw_text <- renderUI({
        req(any(!is.null(input$BHMM_poisson_include) & isTRUE(input$BHMM_poisson_include),
                !is.null(input$BHMM_logistic_include) & isTRUE(input$BHMM_logistic_include))
        )
        tagList(HTML('# of Draw'))
    })
    output$BHMM_nDraw <- renderUI({
        req(any(!is.null(input$BHMM_poisson_include) & isTRUE(input$BHMM_poisson_include),
                !is.null(input$BHMM_logistic_include) & isTRUE(input$BHMM_logistic_include))
        )
        value <- isolate(ternary(is.null(input$BHMM_nDraw), '10000', input$BHMM_nDraw))
        textInput('BHMM_nDraw', NULL, value)
    })
    
    # BHMM burn.in
    output$BHMM_burnin_text <- renderUI({
        req(any(!is.null(input$BHMM_poisson_include) & isTRUE(input$BHMM_poisson_include),
                !is.null(input$BHMM_logistic_include) & isTRUE(input$BHMM_logistic_include))
        )
        tagList(HTML('# of Burn in'))
    })
    output$BHMM_burnin <- renderUI({
        req(any(!is.null(input$BHMM_poisson_include) & isTRUE(input$BHMM_poisson_include),
                !is.null(input$BHMM_logistic_include) & isTRUE(input$BHMM_logistic_include))
        )
        value <- isolate(ternary(is.null(input$BHMM_burnin), '5000', input$BHMM_burnin))
        textInput('BHMM_burnin', NULL, value)
    })
    
    # Compute button (the code block below pairs with the conditionalPanel in ui.R)
    output$adslae_import_status <- reactive(isTRUE(data_import_status$adslae))
    output$show_table <- reactive(isTRUE(output_show$table))
    outputOptions(output, 'adslae_import_status', suspendWhenHidden = FALSE)
    outputOptions(output, 'show_table', suspendWhenHidden = FALSE)
    
    # output$compute_ui <- renderUI({
    #     # req(data_import_status$adslae, !output_show$volplot,
    #     #     !output_show$combplot)
    #     req(data_import_status$adslae)
    #     tagList(
    #         tags$br(),
    #         actionButton('compute', 'Compute output')
    #     )
    # })
    # 
    
    # select x variable for volcano plot
    output$volcano_x <- renderUI({
        req(output_show$volplot)
        choices <- stat_choices
        selected <- isolate(
            ternary(is.null(input$volcano_x), NULL, input$volcano_x)
        )
        selectInput('volcano_x', 'X-axis: measurements', choices, selected)
    })
    
    # select y variable for volcano plot
    output$volcano_y <- renderUI({
        req(output_show$volplot)
        choices <- p_value_choices
        selected <- isolate(
            ternary(is.null(input$volcano_y), NULL, input$volcano_y)
        )
        selectInput(
            'volcano_y', 'Y-axis: p value option', choices, selected
        )
    })
    
    # select x variable for dotplot
    output$forestplot_x <- renderUI({
        req(output_show$combplot)
        choices <- stat_choices
        selected <- isolate(
            ternary(is.null(input$forestplot_x), NULL, input$forestplot_x)
        )
        selectInput('forestplot_x', 'X-axis: measurements', choices, selected)
    })
    
    # safety monitoring output
    output$safety_output <- renderUI({
        req(data_import_status$adslae,
            input$control_mapping, input$treatment_mapping
            # ,
            # input$ae_level_1, input$ae_level_2
            # ,
            # input$confidence_level
            )
        req(input$control_mapping != input$treatment_mapping)
        tabsetPanel(
            tabPanel(
                'Output table',
                dataTableOutput('output_table')
            ),
            tabPanel(
                'Volcano plot',
                plotOutput(
                    'volplot', hover = hoverOpts('volplot_hover',
                                                 delay = 100,
                                                 delayType = 'debounce')
                ),
                uiOutput('volplot_hover'),
                uiOutput('volplot_fn')
            ),
            tabPanel(
                'Dotplot',
                uiOutput('combplot_title'),
                fluidRow(
                    column(
                        width = 6,
                        plotOutput('dotplot', width = '100%',
                                   click = 'dotplot_click')
                    ),
                    column(
                        width = 6,
                        plotOutput('forestplot', width = '100%',
                                   click = 'forestplot_click')
                    )
                ),
                plotOutput('combplot_legend', height = '50px'),
                uiOutput('combplot_fn')
            ),
            id = 'output_tabs'
        )
    })
    
    # footnote for output table
    output$output_table_footnote <- renderUI({
        req(output_table_subset_reactive())
        tagList(HTML(output_table_footnote), tags$br())
    })
    
    # footnote for abbreviations
    output$output_abbrev_footnote <- renderUI({
        req(data_import_status$adslae,
            input$control_mapping, input$treatment_mapping
        )
        req(input$control_mapping != input$treatment_mapping)
        HTML(abbreviation_footnote)
    })
    
    output_show <- reactiveValues(
        table = FALSE, volplot = FALSE, combplot = FALSE
    )
    observe({
        if(all(!is.null(input$output_tabs))) {
            if(input$output_tabs == 'Output table') {
                output_show$table <- TRUE
                output_show$volplot <- FALSE
                output_show$combplot <- FALSE
            } else if(input$output_tabs == 'Volcano plot') {
                output_show$table <- FALSE
                output_show$volplot <- TRUE
                output_show$combplot <- FALSE
            } else if(input$output_tabs == 'Dotplot') {
                output_show$table <- FALSE
                output_show$volplot <- FALSE
                output_show$combplot <- TRUE
            }
        } else {
            output_show$table <- FALSE
            output_show$volplot <- FALSE
            output_show$combplot <- FALSE
        }
    })
    
    #------------------------------------------------------
    # output table
    #------------------------------------------------------
    output_table <- eventReactive(input$compute, {
        req(data_import_status$adslae,
            input$control_mapping, input$treatment_mapping,
            input$ae_level_1, input$ae_level_2,
            input$confidence_level, input$DFDR_p)
        req(input$control_mapping != input$treatment_mapping)
        
        # Create a Progress object
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        
        progress$set(message = 'Computing output', value = 0)
        # Number of times we'll increment the progress bar
        # n_inc <- ternary(!is.null(input$BHMM_include) & isTRUE(input$BHMM_include), 6, 4)
        n_inc <- 4
        if(!is.null(input$BHMM_poisson_include) & isTRUE(input$BHMM_poisson_include)){
            n_inc <- n_inc + 1
        }
        if(!is.null(input$BHMM_logistic_include) & isTRUE(input$BHMM_logistic_include)){
            n_inc <- n_inc + 1
        }
        progress$inc(1/n_inc, detail = "Doing part 1: computing No. of AEs, No. of subjects, and exposure time")
        
        #--------------------------
        # input check
        #--------------------------
        validate(
            need(
                between(input$confidence_level, 0, 1,
                        left_inclusive = FALSE,
                        right_inclusive = FALSE, convert = TRUE),
                paste('Confidence level must be a number between (0, 1)')
            ),
            need(
                between(input$DFDR_p, 0, 1,
                        left_inclusive = FALSE,
                        right_inclusive = FALSE, convert = TRUE),
                paste('DFDR p value threshold must be a number between (0, 1)')
            )
        )
        
        
        name_to_descrp <- c(
            'SOC' = 'SOC', 'PT' = 'PT',
            'HLGT' = 'High level group term',
            'HLT' = 'High level term'
        )
        colname_to_name <- c(
            'AEBODSYS' = 'SOC', 'AEDECOD' = 'PT',
            'AEHLGT' = 'HLGT', 'AEHLT' = 'HLT'
        )
        trt_str <- input$treatment_mapping
        plb_str <- input$control_mapping
        data <- data_$adslae_subset
        treatment_col <- name_mapping$adsl[['Actual treatment received']]
        subj_col <- name_mapping$adsl[['Subject ID']]
        ae_onset <- name_mapping$adae[['AE onset date']]
        first_dose <- name_mapping$adsl[['First dose date of study drug']]
        last_dose <- name_mapping$adsl[['Last dose date of study drug']]
        ae_level1 <- name_mapping$adae[[name_to_descrp[input$ae_level_1]]]
        ae_level2 <- name_mapping$adae[[name_to_descrp[input$ae_level_2]]]

        # Compute duration between (AE onset - First dosing) for filtering out negative ones
        data$DURATION <- data[[ae_onset]] - data[[first_dose]] + 1
        
        # For patients without AE, keep first row
        # For patients with at least one AE after first dose, keep the first AE after first dose
        # For patients with all AEs prior to first dose, keep the last row
        data <- data %>%
            arrange_(subj_col, ae_level1, ae_level2, ae_onset) %>%
            group_by_(subj_col, ae_level1, ae_level2) %>%
            filter((all(is.na(DURATION)) & (row_number() == 1)) | 
                       (any(DURATION > 0) & (row_number() == which(DURATION > 0)[1])) | 
                       (all(DURATION[!is.na(DURATION)] <= 0) & (row_number() == n()))) %>%
            select(-DURATION) %>%
            as.data.frame
        lastdose_plus <- as.numeric(input$lastdose_plus)
        snapshot_date <- as.Date(file_subset_snapshot$value)
        data_trt <- data[data[[treatment_col]] == trt_str, , drop = FALSE]
        data_plb <- data[data[[treatment_col]] == plb_str, , drop = FALSE]
        npts_trt <- length(unique(na.omit(data_trt[[subj_col]])))
        npts_ctr <- length(unique(na.omit(data_plb[[subj_col]])))
        duration_data <- data %>%
            mutate(duration = pmin(
                (as.integer(data[[last_dose]] + lastdose_plus - data[[first_dose]] + 1) / 365.25),
                (as.integer(snapshot_date - data[[first_dose]] + 1) / 365.25),
                na.rm = T)) %>%
            group_by_(subj_col) %>%
            filter(row_number() == 1) %>%
            select_(subj_col, 'duration')
        duration_map <- setNames(c(duration_data$duration),
                                 duration_data[[subj_col]])
        all_subjs_plb <- unique(data_plb[[subj_col]])
        all_subjs_trt <- unique(data_trt[[subj_col]])
        filter_str <- paste(
            '!is.na(', c(ae_level1, ae_level2), ')', collapse = '&'
        )
        sum_str <- lazyeval::interp(
            ~length(unique(na.omit(var))), var = as.name(subj_col)
        )
        exposure_str_plb <- lazyeval::interp(
            ~sum(exposure, na.rm = TRUE) + sum(duration_map[
                base::setdiff(all_subjs_plb, unique(var))
                ], na.rm = TRUE), var = as.name(subj_col)
        )
        exposure_str_trt <- lazyeval::interp(
            ~sum(exposure, na.rm = TRUE) + sum(duration_map[
                base::setdiff(all_subjs_trt, unique(var))
                ], na.rm = TRUE), var = as.name(subj_col)
        )
        
        data_plb[['exposure']] <- as.integer(
            data_plb[[ae_onset]] - data_plb[[first_dose]] + 1
        ) / 365.25
        data_plb[['exposure']][is.na(data_plb[[ae_onset]])] <- pmin(
            (as.integer(data_plb[[last_dose]] + lastdose_plus - data_plb[[first_dose]] + 1
                        ) / 365.25)[is.na(data_plb[[ae_onset]])],
            (as.integer(snapshot_date - data_plb[[first_dose]] + 1
            ) / 365.25)[is.na(data_plb[[ae_onset]])],
            na.rm = T)
        data_plb[['exposure']][!is.finite(data_plb[['exposure']])] <- NA
        # Do not compute exposure for AE prior to first dose, leave as NA
        data_plb[['exposure']][data_plb[['exposure']] <= 0] <- NA
        
        data_trt[['exposure']] <- as.integer(
            data_trt[[ae_onset]] - data_trt[[first_dose]] + 1
        ) / 365.25
        data_trt[['exposure']][is.na(data_trt[[ae_onset]])] <- pmin(
            (as.integer(data_trt[[last_dose]] + lastdose_plus - data_trt[[first_dose]] + 1
                        ) / 365.25)[is.na(data_trt[[ae_onset]])],
            (as.integer(snapshot_date - data_trt[[first_dose]] + 1
            ) / 365.25)[is.na(data_trt[[ae_onset]])],
            na.rm = T)
        data_trt[['exposure']][!is.finite(data_trt[['exposure']])] <- NA
        # Do not compute exposure for AE prior to first dose, leave as NA
        data_trt[['exposure']][data_trt[['exposure']] <= 0] <- NA
        
        
        data_plb <- data_plb %>% filter_(filter_str)
        data_trt <- data_trt %>% filter_(filter_str)
        
        tbl_plb <- data_plb %>%
            group_by_(ae_level1, ae_level2) %>%
            summarise_('nevent_ctr' = sum_str, 'exposure_ctr' = exposure_str_plb, 
                       'NA_indicator_ctr' = ~as.numeric(any(is.na(exposure)))) %>%
            mutate(exposure_ctr = ifelse(NA_indicator_ctr == 1, NA, exposure_ctr)) %>%
            select(-NA_indicator_ctr)
            
        tbl_trt <- data_trt %>%
            group_by_(ae_level1, ae_level2) %>%
            summarise_('nevent_trt' = sum_str, 'exposure_trt' = exposure_str_trt, 
                       'NA_indicator_trt' = ~as.numeric(any(is.na(exposure)))) %>%
            mutate(exposure_trt = ifelse(NA_indicator_trt == 1, NA, exposure_trt)) %>%
            select(-NA_indicator_trt)
        
        tbl_comb <- full_join(tbl_trt, tbl_plb, by = c(ae_level1, ae_level2))
        tbl_comb[['nsubj_trt']] <- npts_trt
        tbl_comb[['nsubj_ctr']] <- npts_ctr
        rename_str <- setNames(
            paste0('`', c(ae_level1, ae_level2), '`'),
            c(colname_to_name[ae_level1], colname_to_name[ae_level2])
        )
        tbl_comb <- tbl_comb %>%
            arrange_(ae_level1, ae_level2) %>%
            rename_(.dots = rename_str)
        tbl_evt <- tbl_comb[c('nevent_ctr', 'nevent_trt')]
        tbl_exp <- tbl_comb[c('exposure_ctr', 'exposure_trt')]
        tbl_comb[c('nevent_ctr', 'nevent_trt')][is.na(tbl_evt)] <- 0
        # This part has been changed to check both nevent and exposure
        # If nevent == 0 & exposure == NA, then sum duration for all subjects; If nevent > 0 & exposure == NA, leave as NA
        tbl_comb[['exposure_ctr']][(tbl_comb[['nevent_ctr']] == 0) & is.na(tbl_comb[['exposure_ctr']])] <-
            sum(duration_map[all_subjs_plb], na.rm = TRUE)
        tbl_comb[['exposure_trt']][(tbl_comb[['nevent_trt']] == 0) & is.na(tbl_comb[['exposure_trt']])] <-
            sum(duration_map[all_subjs_trt], na.rm = TRUE)
        
        all_level1 <- unique(tbl_comb[[colname_to_name[ae_level1]]])
        level1_num_code <- as.integer(factor(
            tbl_comb[[colname_to_name[ae_level1]]], levels = all_level1
        ))
        
        n_level2 <- nrow(tbl_comb)
        level2_per_level1 <- data.frame(level1_num_code) %>%
            count(level1_num_code)
        level2_num_code <- unlist(sapply(level2_per_level1$n, function(x){seq(1,x)}))
        
        conf_level <- as.numeric(input$confidence_level)
        DFDR_p <- as.numeric(input$DFDR_p)
        
        #----------------------------------------------------------------
        #   Calculate the following
        #
        #       1. ir_trt: incidence rate for trt (nevent_trt/nsubj_trt)
        #       2. ir_ctr: incidence rate for ctr (nevent_ctr/nsubj_ctr)
        #       3. adjir_trt: adjusted incidence rate for trt (nevent_trt/exposure_trt)
        #       4. adjir_ctr: adjusted incidence rate for ctr (nevent_ctr/exposure_ctr)
        #----------------------------------------------------------------
        tbl_comb <- data.frame(tbl_comb)
        tbl_comb$ir_trt <- tbl_comb[['nevent_trt']]/tbl_comb[['nsubj_trt']]
        tbl_comb$ir_ctr <- tbl_comb[['nevent_ctr']]/tbl_comb[['nsubj_ctr']]
        tbl_comb$EAIR_trt <- rep(NA, n_level2)
        tbl_comb$EAIR_ci_lower_trt <- rep(NA, n_level2)
        tbl_comb$EAIR_ci_upper_trt <- rep(NA, n_level2)
        tbl_comb$EAIR_ctr <- rep(NA, n_level2)
        tbl_comb$EAIR_ci_lower_ctr <- rep(NA, n_level2)
        tbl_comb$EAIR_ci_upper_ctr <- rep(NA, n_level2)
        
        exposure_na_trt <- is.na(tbl_comb[['exposure_trt']])
        exposure_na_ctr <- is.na(tbl_comb[['exposure_ctr']])
        nevent_0_trt <- tbl_comb[['nevent_trt']] == 0
        nevent_0_ctr <- tbl_comb[['nevent_ctr']] == 0
        tbl_comb[['EAIR_trt']][!exposure_na_trt] <- tbl_comb[['nevent_trt']][!exposure_na_trt]/tbl_comb[['exposure_trt']][!exposure_na_trt]
        tbl_comb[['EAIR_ctr']][!exposure_na_ctr] <- tbl_comb[['nevent_ctr']][!exposure_na_ctr]/tbl_comb[['exposure_ctr']][!exposure_na_ctr]
        
        tbl_comb[['EAIR_ci_lower_trt']][(!exposure_na_trt)&(!nevent_0_trt)] <- qchisq((1-conf_level)/2, 2*tbl_comb[['nevent_trt']][(!exposure_na_trt)&(!nevent_0_trt)], 0)/2/tbl_comb[['exposure_trt']][(!exposure_na_trt)&(!nevent_0_trt)]
        tbl_comb[['EAIR_ci_lower_trt']][(!exposure_na_trt)&(nevent_0_trt)] <- 0
        tbl_comb[['EAIR_ci_upper_trt']][!exposure_na_trt] <- qchisq(1-(1-conf_level)/2, 2*(tbl_comb[['nevent_trt']][!exposure_na_trt] + 1))/2/tbl_comb[['exposure_trt']][!exposure_na_trt]
        
        tbl_comb[['EAIR_ci_lower_ctr']][(!exposure_na_ctr)&(!nevent_0_ctr)] <- qchisq((1-conf_level)/2, 2*tbl_comb[['nevent_ctr']][(!exposure_na_ctr)&(!nevent_0_ctr)], 0)/2/tbl_comb[['exposure_ctr']][(!exposure_na_ctr)&(!nevent_0_ctr)]
        tbl_comb[['EAIR_ci_lower_ctr']][(!exposure_na_ctr)&(nevent_0_ctr)] <- 0
        tbl_comb[['EAIR_ci_upper_ctr']][!exposure_na_ctr] <- qchisq(1-(1-conf_level)/2, 2*(tbl_comb[['nevent_ctr']][!exposure_na_ctr] + 1))/2/tbl_comb[['exposure_ctr']][!exposure_na_ctr]
        
        #----------------------------------------------------------------
        #   Calculate the following
        #
        #       1. surv_trt: censored survival for trt (time to AE or to 
        #                    the last follow-up (i.e. patient's level
        #                    exposure time))
        #       2. surv_ctr: censored survival for ctr (time to AE or to 
        #                    the last follow-up (i.e. patient's level
        #                    exposure time))
        #       3. status_trt: AE indicator for trt
        #       4. status_ctr: AE indicator for ctr
        #       5. landmark_km_t: Lankmark KM time point
        #----------------------------------------------------------------
        surv_trt <- NULL
        surv_ctr <- NULL
        landmark_km_t <- NULL
        status_trt <- NULL
        status_ctr <- NULL
        for(pt in tbl_comb[[colname_to_name[ae_level2]]]) {
            time_plb_iter <- rep(NA, length(all_subjs_plb))
            time_trt_iter <- rep(NA, length(all_subjs_trt))
            subj_has_ae_plb <- data_plb[data_plb[[ae_level2]] == pt, subj_col]
            subj_has_ae_trt <- data_trt[data_trt[[ae_level2]] == pt, subj_col]
            time_plb_iter[all_subjs_plb %in% subj_has_ae_plb] <-
                data_plb[data_plb[[ae_level2]] == pt, 'exposure']
            time_plb_iter[!(all_subjs_plb %in% subj_has_ae_plb)] <- duration_map[
                base::setdiff(all_subjs_plb, subj_has_ae_plb)
                ]
            time_trt_iter[all_subjs_trt %in% subj_has_ae_trt] <-
                data_trt[data_trt[[ae_level2]] == pt, 'exposure']
            time_trt_iter[!(all_subjs_trt %in% subj_has_ae_trt)] <- duration_map[
                base::setdiff(all_subjs_trt, subj_has_ae_trt)
                ]
            
            landmark_km_t_iter <- min(max(time_plb_iter), max(time_trt_iter))
            
            status_plb_iter <- rep(0, length(all_subjs_plb))
            status_trt_iter <- rep(0, length(all_subjs_trt))
            status_plb_iter[all_subjs_plb %in% subj_has_ae_plb] <- 1
            status_trt_iter[all_subjs_trt %in% subj_has_ae_trt] <- 1
            
            surv_trt <- cbind(surv_trt, time_trt_iter)
            surv_ctr <- cbind(surv_ctr, time_plb_iter)
            landmark_km_t <- rbind(landmark_km_t, landmark_km_t_iter)
            status_trt <- cbind(status_trt, status_trt_iter)
            status_ctr <- cbind(status_ctr, status_plb_iter)
        }
        
        tbl_comb$landmark_km_t <- ternary(!is.null(input$landmark_km_t)&(input$landmark_km_t != ''), 
                                         rep(as.numeric(input$landmark_km_t), nrow(tbl_comb)), 
                                         as.vector(landmark_km_t))
        
        progress$inc(1/n_inc, detail = "Doing part 2: computing Odds ratio, Relative risk, Risk difference and Hazard ratio")
        
        #----------------------------------------------------------------
        #   Calculate the four following statistics
        #
        #       1. odds ratio (OR)
        #       2. relative risk (RR)
        #       3. risk difference (RD)
        #       4. hazard ratio (HR)
        #----------------------------------------------------------------
        
        # odds ratio (OR)
        or_res <- odds_ratio(
            tbl_comb[['nevent_trt']],
            tbl_comb[['nsubj_trt']] - tbl_comb[['nevent_trt']],
            tbl_comb[['nevent_ctr']],
            tbl_comb[['nsubj_ctr']] - tbl_comb[['nevent_ctr']],
            conf_level = conf_level, ci = TRUE, p_value = FALSE
        )
        or_est <- or_res$estimate
        or_ci <- `colnames<-`(or_res$ci, c('or_lower', 'or_upper'))
        
        # relative risk (RR)
        rr_res <- relative_rick(
            tbl_comb[['nevent_trt']],
            tbl_comb[['nsubj_trt']] - tbl_comb[['nevent_trt']],
            tbl_comb[['nevent_ctr']],
            tbl_comb[['nsubj_ctr']] - tbl_comb[['nevent_ctr']],
            conf_level = conf_level, ci = TRUE
        )
        rr_est <- rr_res$estimate
        rr_ci <- `colnames<-`(rr_res$ci, c('rr_lower', 'rr_upper'))
        
        # risk difference (RD)
        rd_res <- risk_difference(
            tbl_comb[['nevent_trt']],
            tbl_comb[['nsubj_trt']] - tbl_comb[['nevent_trt']],
            tbl_comb[['nevent_ctr']],
            tbl_comb[['nsubj_ctr']] - tbl_comb[['nevent_ctr']],
            conf_level = conf_level, ci = TRUE, p_value = FALSE
        )
        rd_est <- rd_res$estimate
        rd_ci <- `colnames<-`(rd_res$ci, c('rd_lower', 'rd_upper'))
        
        # hazard ratio (HR)
        hr_res <- hazard_ratio(
            n_pts_trt = tbl_comb[['nsubj_trt']],
            n_pts_ctr = tbl_comb[['nsubj_ctr']],
            total_pt = nrow(tbl_comb),
            surv_trt = surv_trt, surv_ctr = surv_ctr,
            status_trt = status_trt, status_ctr = status_ctr,
            conf_level = conf_level, ci = TRUE
        )
        hr_est <- hr_res$estimate
        hr_ci <- `colnames<-`(hr_res$ci, c('hr_lower', 'hr_upper'))
        
        # Suppress the results on such HR estimate and its confidence interval
        # and mask them with ""-""
        hyphen_idx <- tbl_comb[['nevent_trt']] == 0 | tbl_comb[['nevent_ctr']] == 0
        NA_idx <- is.na(hr_est)
        
        progress$inc(1/n_inc, detail = "Doing part 3: computing p value for Fisher exact, EAIR, Logrank and Landmark KM tests")
        
        #----------------------------------------------------------------
        #   Calculate p value for the following four tests
        #
        #       1. Fisher's exact test
        #       2. Exposure adjusted method (EAIR-exact)
        #       3. Logrank test
        #       4. Landmark Kaplan-Meier method (landmark KM)
        #----------------------------------------------------------------
        
        # Fisher's exact test p value
        fisher_p <- fisher_pvalue(
            n_ae_trt = tbl_comb[['nevent_trt']],
            n_ae_ctr = tbl_comb[['nevent_ctr']],
            n_pts_trt = tbl_comb[['nsubj_trt']],
            n_pts_ctr = tbl_comb[['nsubj_ctr']]
        )
        
        # EAIR test p value
        EAIR_p <- EAIR_pvalue(
            n_ae_trt = tbl_comb[['nevent_trt']],
            n_ae_ctr = tbl_comb[['nevent_ctr']],
            exp_trt = tbl_comb[['exposure_trt']],
            exp_ctr = tbl_comb[['exposure_ctr']]
        )
        
        # logrank test p value
        logrank_p <- logrank_pvalue(
            n_pts_trt = tbl_comb[['nsubj_trt']],
            n_pts_ctr = tbl_comb[['nsubj_ctr']],
            total_pt = nrow(tbl_comb),
            surv_trt = surv_trt, surv_ctr = surv_ctr,
            status_trt = status_trt, status_ctr = status_ctr
        )
        
        # landmark KM test p value
        landmark_km_result <- landmark_km_pvalue(
            n_pts_trt = tbl_comb[['nsubj_trt']],
            n_pts_ctr = tbl_comb[['nsubj_ctr']],
            total_pt = nrow(tbl_comb),
            surv_trt = surv_trt, surv_ctr = surv_ctr,
            status_trt = status_trt, status_ctr = status_ctr,
            t.lm = tbl_comb[['landmark_km_t']]
        )
        landmark_km_trt <- landmark_km_result$km_trt
        landmark_km_ctr <- landmark_km_result$km_ctr
        landmark_km_p <- landmark_km_result$km_p
        
        # landmark_km_p <- landmark_km_pvalue(
        #     n_pts_trt = tbl_comb[['nsubj_trt']],
        #     n_pts_ctr = tbl_comb[['nsubj_ctr']],
        #     total_pt = nrow(tbl_comb),
        #     surv_trt = surv_trt, surv_ctr = surv_ctr,
        #     status_trt = status_trt, status_ctr = status_ctr,
        #     t.lm = tbl_comb[['landmark_km_t']]
        # )
        
        if(!is.null(input$BHMM_poisson_include) & isTRUE(input$BHMM_poisson_include)){
            progress$inc(1/n_inc, detail = "Doing part 4: computing posterior probability for BHMM Poisson model")
            
            #----------------------------------------------------------------
            #   Calculate BHMM posterior probability of risk for the following model:
            #
            #       1. BHMM Poisson model
            #
            #       Output is:  the posterior probability of theta > 0, 
            #                   where theta is the log of relative risk of trt over ctr after adjusting for exposure time, 
            #                   the higher the more likely to be a signal, 
            #                   the cutoff value can be 0.9 or 0.95
            #----------------------------------------------------------------
            
            # BHMM Poisson model
            # Need to compute only for NAs removed subset
            index_narm <- !is.na(tbl_comb[['exposure_trt']]) & !is.na(tbl_comb[['exposure_ctr']])
            tbl_comb_narm <- tbl_comb[index_narm,]
            n_level2_narm <- nrow(tbl_comb_narm)
            all_level1_narm <- unique(tbl_comb_narm[[colname_to_name[ae_level1]]])
            level1_num_code_narm <- as.integer(factor(tbl_comb_narm[[colname_to_name[ae_level1]]], levels = all_level1_narm))
            level2_per_level1_narm <- data.frame(level1_num_code_narm) %>%
                count(level1_num_code_narm)
            level2_num_code_narm <- unlist(sapply(level2_per_level1_narm$n, function(x){seq(1,x)}))
            
            bhmm_poisson_narm <- (function(Nae, N.SOC, SOC, PT, Y, X, exposure_t, exposure_c, model_type, model_file, cutoff = 0, nDraw = 10000, burn.in = 5000) {
                data <- list(Nae=Nae, N.SOC=N.SOC, SOC=SOC, PT=PT, Y=Y, X=X, exposure_t=exposure_t, exposure_c=exposure_c)
                
                index_narm <- !is.na(exposure_t) & !is.na(exposure_c) 
                
                init1=list(.RNG.name="base::Mersenne-Twister", .RNG.seed=1)
                init2=list(.RNG.name="base::Wichmann-Hill", .RNG.seed=2)
                init3=list(.RNG.name="base::Marsaglia-Multicarry", .RNG.seed=3)
                inits = list(init1,init2,init3)
                
                n.chains = 3
                
                model.l = jags.model(file=model_file, data, n.chains=n.chains, inits=inits, n.adapt=100)
                
                out_col = c("theta")   # output variables
                # nDraw = 10000   # sample size to be simulated
                output = coda.samples(model.l, out_col, nDraw)
                out.mat = as.matrix(output)
                
                # burn.in = nDraw/2
                Samp = NULL
                for (i in 1:n.chains)
                    Samp = rbind(Samp,out.mat[(i*nDraw+1-burn.in):(i*nDraw),])
                
                probg0 = colMeans(Samp>cutoff)
                
                Prob.G0 = numeric(Nae) ## the posterior probability of theta > 0, the higher the more likely to be a signal, the cutoff value can be 0.9 or 0.95
                for (i in 1:Nae)
                    Prob.G0[i] = probg0[paste("theta[",SOC[i],",",PT[i],"]",sep="")]

                return(bhmm_poisson_narm = Prob.G0)
                
            })(
                Nae = n_level2_narm, # total number of PT
                N.SOC = length(all_level1_narm), # number of SOC
                SOC = level1_num_code_narm, # vector of SOC ID
                PT = level2_num_code_narm, # vector of PT id within each SOC
                Y = tbl_comb_narm[['nevent_trt']], # number of AEs in trt
                X = tbl_comb_narm[['nevent_ctr']], # number of AEs in ctr
                exposure_t = tbl_comb_narm[['exposure_trt']], # total exposure time for each PT in trt
                exposure_c = tbl_comb_narm[['exposure_ctr']], # total exposure time for each PT in ctr
                model_type = 'Poisson',
                model_file = 'r_scripts/BHMM_poisson_model1.bug',
                # model_file = poisson_model_file,
                cutoff = as.numeric(input$BHMM_cutoff),
                nDraw = as.numeric(input$BHMM_nDraw),
                burn.in = as.numeric(input$BHMM_burnin)
            )
            bhmm_poisson <- rep(NA, n_level2)
            bhmm_poisson[index_narm] <- bhmm_poisson_narm
            
        }
        
        if(!is.null(input$BHMM_logistic_include) & isTRUE(input$BHMM_logistic_include)){
            progress$inc(1/n_inc, detail = paste("Doing part ", n_inc - 1, ": computing posterior probability for BHMM Logistic model", sep = ''))
            
            #----------------------------------------------------------------
            #   Calculate BHMM posterior probability of risk for the following model:
            #
            #       2. BHMM Logistic model
            #
            #       Output is:  the posterior probability of theta > 0, 
            #                   where theta is the log of relative risk of trt over ctr after adjusting for exposure time, 
            #                   the higher the more likely to be a signal, 
            #                   the cutoff value can be 0.9 or 0.95
            #----------------------------------------------------------------
            
            # BHMM Logistic model
            bhmm_logistic <- (function(Nae, Nc, Nt, N.SOC, SOC, PT, Y, X, model_type, model_file, cutoff = 0, nDraw = 10000, burn.in = 5000) {
                data <- list(Nae=Nae, Nc = Nc, Nt = Nt, N.SOC=N.SOC, SOC=SOC, PT=PT, Y=Y, X=X)
                
                init1=list(.RNG.name="base::Mersenne-Twister", .RNG.seed=1)
                init2=list(.RNG.name="base::Wichmann-Hill", .RNG.seed=2)
                init3=list(.RNG.name="base::Marsaglia-Multicarry", .RNG.seed=3)
                inits = list(init1,init2,init3)
                
                n.chains = 3
                
                model.l = jags.model(file=model_file, data, n.chains=n.chains, inits=inits, n.adapt=100)
                
                out_col = c("theta")   # output variables
                # nDraw = 10000   # sample size to be simulated
                output = coda.samples(model.l, out_col, nDraw)
                out.mat = as.matrix(output)
                
                # burn.in = nDraw/2
                Samp = NULL
                for (i in 1:n.chains)
                    Samp = rbind(Samp,out.mat[(i*nDraw+1-burn.in):(i*nDraw),])
                
                probg0 = colMeans(Samp>cutoff)
                
                Prob.G0 = numeric(Nae) ## the posterior probability of theta > 0, the higher the more likely to be a signal, the cutoff value can be 0.9 or 0.95
                for (i in 1:Nae)
                    Prob.G0[i] = probg0[paste("theta[",SOC[i],",",PT[i],"]",sep="")]
                return(bhmm_logistic = Prob.G0)
                
            })(
                Nae = n_level2, # total number of PT
                Nc = npts_ctr, # total number of patients in ctr
                Nt = npts_trt, # total number of patients in trt
                N.SOC = length(all_level1), # number of SOC
                SOC = level1_num_code, # vector of SOC ID
                PT = level2_num_code, # vector of PT id within each SOC
                Y = tbl_comb[['nevent_trt']], # number of AEs in trt
                X = tbl_comb[['nevent_ctr']], # number of AEs in ctr
                model_type = 'Logistic',
                model_file = 'r_scripts/BHMM_logis_model1.bug',
                # model_file = poisson_model_file,
                cutoff = as.numeric(input$BHMM_cutoff),
                nDraw = as.numeric(input$BHMM_nDraw),
                burn.in = as.numeric(input$BHMM_burnin)
            )
        }
        
        
        progress$inc(1/n_inc, detail = "Finish constructing the output table")
        
        # add the statistics to the output table
        # tbl_comb <- data.frame(tbl_comb)
        tbl_comb$or <- or_est
        tbl_comb <- cbind(tbl_comb, data.frame(or_ci))
        tbl_comb$rr <- rr_est
        tbl_comb <- cbind(tbl_comb, data.frame(rr_ci))
        tbl_comb$rd <- rd_est
        tbl_comb <- cbind(tbl_comb, data.frame(rd_ci))
        tbl_comb$hr <- hr_est
        tbl_comb <- cbind(tbl_comb, data.frame(hr_ci))
        
        # add the tests' p values to the output table
        tbl_comb$fisher_p <- fisher_p
        tbl_comb$EAIR_p <- EAIR_p
        tbl_comb$logrank_p <- logrank_p
        tbl_comb$landmark_km_trt <- landmark_km_trt
        tbl_comb$landmark_km_ctr <- landmark_km_ctr
        tbl_comb$landmark_km_p <- landmark_km_p
        
        # DFDR p value correction
        ruleof4_index <- ternary(isTRUE(input$DFDR_ruleof4),
                                 (tbl_comb[['nevent_trt']] < 4) & (tbl_comb[['nevent_ctr']] < 4),
                                 rep(FALSE, n_level2))
        
        fisher_p_na_index <- is.na(fisher_p)
        fisher_p_compute_index <- !ruleof4_index & !fisher_p_na_index
        fisher_p_dfdr <- rep(NA, n_level2)
        fisher_p_dfdr[fisher_p_compute_index] <- DFDR(fisher_p[fisher_p_compute_index], DFDR_p, level1_num_code[fisher_p_compute_index])
        fisher_p_dfdr[ruleof4_index] <- 1
        tbl_comb$fisher_p_dfdr <- fisher_p_dfdr
        
        EAIR_p_na_index <- is.na(EAIR_p)
        EAIR_p_compute_index <- !ruleof4_index & !EAIR_p_na_index
        EAIR_p_dfdr <- rep(NA, n_level2)
        EAIR_p_dfdr[EAIR_p_compute_index] <- DFDR(EAIR_p[EAIR_p_compute_index], DFDR_p, level1_num_code[EAIR_p_compute_index])
        EAIR_p_dfdr[ruleof4_index] <- 1
        tbl_comb$EAIR_p_dfdr <- EAIR_p_dfdr
        
        logrank_p_na_index <- is.na(logrank_p)
        logrank_p_compute_index <- !ruleof4_index & !logrank_p_na_index
        logrank_p_dfdr <- rep(NA, n_level2)
        logrank_p_dfdr[logrank_p_compute_index] <- DFDR(logrank_p[logrank_p_compute_index], DFDR_p, level1_num_code[logrank_p_compute_index])
        logrank_p_dfdr[ruleof4_index] <- 1
        tbl_comb$logrank_p_dfdr <- logrank_p_dfdr
        
        landmark_km_p_na_index <- is.na(landmark_km_p)
        landmark_km_p_compute_index <- !ruleof4_index & !landmark_km_p_na_index
        landmark_km_p_dfdr <- rep(NA, n_level2)
        landmark_km_p_dfdr[landmark_km_p_compute_index] <- DFDR(landmark_km_p[landmark_km_p_compute_index], DFDR_p, level1_num_code[landmark_km_p_compute_index])
        landmark_km_p_dfdr[ruleof4_index] <- 1
        tbl_comb$landmark_km_p_dfdr <- landmark_km_p_dfdr

        
        cols <- c(colname_to_name[ae_level1], colname_to_name[ae_level2],
                  'nevent_trt', 'nevent_ctr',
                  'nsubj_trt', 'nsubj_ctr', 'exposure_trt', 'exposure_ctr',
                  'ir_trt', 'ir_ctr', 
                  'or', 'or_lower', 'or_upper', 'rr', 'rr_lower', 'rr_upper',
                  'rd', 'rd_lower', 'rd_upper', 
                  'fisher_p', 
                  'EAIR_trt', 'EAIR_ci_lower_trt', 'EAIR_ci_upper_trt',
                  'EAIR_ctr', 'EAIR_ci_lower_ctr', 'EAIR_ci_upper_ctr',
                  'EAIR_p', 
                  'hr', 'hr_lower', 'hr_upper',
                  'logrank_p', 
                  'landmark_km_t', 'landmark_km_trt', 'landmark_km_ctr', 'landmark_km_p',
                  'fisher_p_dfdr', 'EAIR_p_dfdr', 'logrank_p_dfdr',
                  'landmark_km_p_dfdr')
        tbl <- rename(tbl_comb[cols],
                      'No. of Subjects w Events (Treatment)' = nevent_trt,
                      'No. of Subjects w Events (Control)' = nevent_ctr,
                      'Total No. of Subjects (Treatment)' = nsubj_trt,
                      'Total No. of Subjects (Control)' = nsubj_ctr,
                      'Drug Exposure at Risk (Treatment)' = exposure_trt,
                      'Drug Exposure at Risk (Control)' = exposure_ctr,
                      'Incidence Rate (Treatment)' = ir_trt,
                      'Incidence Rate (Control)' = ir_ctr,
                      'EAIR (Treatment)' = EAIR_trt,
                      'EAIR (Treatment) lower' = EAIR_ci_lower_trt,
                      'EAIR (Treatment) upper' = EAIR_ci_upper_trt,
                      'EAIR (Control)' = EAIR_ctr,
                      'EAIR (Control) lower' = EAIR_ci_lower_ctr,
                      'EAIR (Control) upper' = EAIR_ci_upper_ctr,
                      'Odds ratio' = or,
                      'Odds ratio lower' = or_lower,
                      'Odds ratio upper' = or_upper,
                      'Relative risk' = rr,
                      'Relative risk lower' = rr_lower,
                      'Relative risk upper' = rr_upper,
                      'Risk difference' = rd,
                      'Risk difference lower' = rd_lower,
                      'Risk difference upper' = rd_upper,
                      'Hazard ratio' = hr,
                      'Hazard ratio lower' = hr_lower,
                      'Hazard ratio upper' = hr_upper,
                      'Fisher exact test p value' = fisher_p,
                      'EAIR test p value' = EAIR_p,
                      'Logrank test p value' = logrank_p,
                      'Landmark KM timepoint' = landmark_km_t,
                      'Landmark KM estimate (Treatment)' = landmark_km_trt,
                      'Landmark KM estimate (Control)' = landmark_km_ctr,
                      'Landmark KM test p value' = landmark_km_p,
                      'DFDR Fisher exact test p value' = fisher_p_dfdr,
                      'DFDR EAIR test p value' = EAIR_p_dfdr,
                      'DFDR Logrank test p value' = logrank_p_dfdr,
                      'DFDR Landmark KM test p value' = landmark_km_p_dfdr)
        
        if(!is.null(input$BHMM_poisson_include) & isTRUE(input$BHMM_poisson_include)){
            tbl$bhmm_poisson <- bhmm_poisson
            tbl <- rename(tbl, 'BHMM Poisson posterior probability of risk' = bhmm_poisson)
        }
        if(!is.null(input$BHMM_logistic_include) & isTRUE(input$BHMM_logistic_include)){
            tbl$bhmm_logistic <- bhmm_logistic
            tbl <- rename(tbl, 'BHMM Logistic posterior probability of risk' = bhmm_logistic)
        }

        tbl[,7:dim(tbl)[2]] <- round(tbl[,7:dim(tbl)[2]], 4)
        
        #------------------------------------------------
        #   Correct Hazard ratio
        #       - 'NA' - Based on the merged dataset, at least one subject had all
        #         occurrences of the corresponding AE prior to his/her first
        #         dosing date
        #       - '-' - at least one treatment group had no event occurred
        #------------------------------------------------
        tbl[hyphen_idx, 'Hazard ratio'] <- '-'
        tbl[hyphen_idx, 'Hazard ratio lower'] <- '-'
        tbl[hyphen_idx, 'Hazard ratio upper'] <- '-'
        tbl[NA_idx, 'Hazard ratio'] <- 'NA'
        tbl[NA_idx, 'Hazard ratio lower'] <- 'NA'
        tbl[NA_idx, 'Hazard ratio'] <- 'NA'
        tbl[NA_idx, 'Hazard ratio upper'] <- 'NA'
        tbl[NA_idx, 'Logrank test p value'] <- 'NA'
        tbl[NA_idx, 'DFDR Logrank test p value'] <- 'NA'
        tbl
    })
    output_table_renamed <- reactive({
        req(output_table())
        tbl <- output_table()
        tbl
    })
    
    output_table_subset_reactive <- reactive({
        tbl <- ternary(
            isTRUE(output_table_subset_status$value), output_table_subset$value,
            output_table_renamed()
        )
        cols <- c(colnames(tbl)[1:2])
        if(length(input$output_sumstat_group) > 0) {
            cols <- c(cols, input$output_sumstat_group)
        }
        if(length(input$output_landmark_group) > 0) {
            cols <- c(cols, input$output_landmark_group)
        }
        if(length(input$output_eair_group) > 0) {
            cols <- c(cols, input$output_eair_group)
        }
        if(length(input$output_rr_group) > 0) {
            cols <- c(cols, input$output_rr_group)
        }
        if(length(input$output_rd_group) > 0) {
            cols <- c(cols, input$output_rd_group)
        }
        if(length(input$output_or_group) > 0) {
            cols <- c(cols, input$output_or_group)
        }
        if(length(input$output_hr_group) > 0) {
            cols <- c(cols, input$output_hr_group)
        }
        if(length(input$output_dfdr_group) > 0) {
            cols <- c(cols, input$output_dfdr_group)
        }
        if(length(input$output_bhmm_group) > 0) {
            cols <- c(cols, input$output_bhmm_group)
        }
        return(tbl[, cols])
    })
    output$output_table <- renderDataTable({
        # req(output_table_renamed())
        req(output_table_subset_reactive())
        tbl <- output_table_subset_reactive()
        DT::datatable(tbl,
                      extensions = c('FixedColumns'),
                      options = list(
                          # dom = 'RMDCT<"clear">lfrtip',
                          dom = 'lfrtip',
                          # searchHighlight = TRUE,
                          autoWidth = TRUE,
                          columnDefs = list(list(width = '150px', targets = list(0,1))),
                          scrollX = TRUE,
                          scrollY = '400px',
                          fixedColumns = list(leftColumns = 2)
                      ),
                      rownames = FALSE)
        
        # ternary(isTRUE(output_table_subset_status$value), output_table_subset$value, output_table_renamed())
        # output_table_renamed()
        # req(output_table_subset$value)
        # output_table_subset$value
    })
    
    #----------------------------------------------
    #   Select columns of output table
    #----------------------------------------------
    
    # select BHMM columns
    output$output_bhmm_ui <- renderUI({
        bhmm_poi <- !is.null(input$BHMM_poisson_include) && isTRUE(input$BHMM_poisson_include)
        bhmm_log <- !is.null(input$BHMM_logistic_include) && isTRUE(input$BHMM_logistic_include)
        req(bhmm_poi || bhmm_log)
        fluidRow(
            column(width = 7, tags$strong('BHMM')),
            column(width = 1, checkboxInput('output_bhmm', NULL, TRUE)),
            column(4)
        )
    })
    output$output_bhmm_group_ui <- renderUI({
        bhmm_poi <- !is.null(input$BHMM_poisson_include) && isTRUE(input$BHMM_poisson_include)
        bhmm_log <- !is.null(input$BHMM_logistic_include) && isTRUE(input$BHMM_logistic_include)
        req(bhmm_poi || bhmm_log)
        output_bhmm_group <- c()
        if(isTRUE(bhmm_poi)) {
            output_bhmm_group <- c(
                output_bhmm_group,
                'BHMM Poisson posterior probability of risk'
            )
        }
        if(isTRUE(bhmm_log)) {
            output_bhmm_group <- c(
                output_bhmm_group,
                'BHMM Logistic posterior probability of risk'
            )
        }
        fluidRow(
            column(
                width = 12,
                checkboxGroupInput(
                    'output_bhmm_group',
                    label = NULL, choices = output_bhmm_group
                )
            )
        )
    })
    
    # select columns by group
    observe({
        input$output_sumstat
        selected <- ternary(
            isTRUE(input$output_sumstat), output_sumstat_group, FALSE
        )
        updateCheckboxGroupInput(
            session, 'output_sumstat_group',
            selected = selected
        )
    })
    observe({
        input$output_landmark
        selected <- ternary(
            isTRUE(input$output_landmark), output_landmark_group, FALSE
        )
        updateCheckboxGroupInput(
            session, 'output_landmark_group',
            selected = selected
        )
    })
    observe({
        input$output_eair
        selected <- ternary(
            isTRUE(input$output_eair), output_eair_group, FALSE
        )
        updateCheckboxGroupInput(
            session, 'output_eair_group',
            selected = selected
        )
    })
    observe({
        input$output_rr
        selected <- ternary(
            isTRUE(input$output_rr), output_rr_group, FALSE
        )
        updateCheckboxGroupInput(
            session, 'output_rr_group',
            selected = selected
        )
    })
    observe({
        input$output_rd
        selected <- ternary(
            isTRUE(input$output_rd), output_rd_group, FALSE
        )
        updateCheckboxGroupInput(
            session, 'output_rd_group',
            selected = selected
        )
    })
    observe({
        input$output_or
        selected <- ternary(
            isTRUE(input$output_or), output_or_group, FALSE
        )
        updateCheckboxGroupInput(
            session, 'output_or_group',
            selected = selected
        )
    })
    observe({
        input$output_hr
        selected <- ternary(
            isTRUE(input$output_hr), output_hr_group, FALSE
        )
        updateCheckboxGroupInput(
            session, 'output_hr_group',
            selected = selected
        )
    })
    observe({
        input$output_dfdr
        selected <- ternary(
            isTRUE(input$output_dfdr), output_dfdr_group, FALSE
        )
        updateCheckboxGroupInput(
            session, 'output_dfdr_group',
            selected = selected
        )
    })
    observe({
        input$output_bhmm
        selected <- ternary(
            isTRUE(input$output_bhmm), output_bhmm_group, FALSE
        )
        updateCheckboxGroupInput(
            session, 'output_bhmm_group',
            selected = selected
        )
    })
    
    output$output_table_download_button <- renderUI({
        req(output_show$table)
        downloadButton('output_table_download', 'Download output')
    })
    
    # download handler for output table
    output$output_table_download <- downloadHandler(
        filename = function() {
            paste0(
                'Output_', format(Sys.Date(), format = '%Y%m%d'), '.', 'xlsx'
            )
        },
        content = function(file) {
            # if(!exists('temp_dir') || is.null(temp_dir))
            #     temp_dir <<- tempdir()
            # tlb <- output_table_subset_reactive()
            # file_create('output', temp_dir, tlb, format = 'xlsx')
            # file.copy(paste(temp_dir, 'output.xlsx', sep = '\\'), file)
            tlb <- output_table_subset_reactive()
            file_name <- paste(file, "xlsx", sep=".")
            # Load workbook; create if not existing
            wb <- XLConnect::loadWorkbook(file_name, create = TRUE)
            # Write to the workbook
            sheet <- 'DATA'
            col_names <- names(tlb)
            has_name <- is.character(col_names) && any(sapply(col_names, nchar) > 0)
            XLConnect::createSheet(wb, name = sheet)
            XLConnect::writeWorksheet(wb, tlb, sheet, header = has_name)
            # Save workbook
            XLConnect::saveWorkbook(wb)
            # Rename the file.xlsx to file (since file cannot have extension)
            file.rename(file_name,file)

            
        }
    )
    
    # output table subset button
    output$output_subset_button <- renderUI({
        req(output_show$table)
        shinyBS::bsButton(
            'output_subset_button', 'Filter output table',
            style = "success", disabled = F
        )
    })
    
    # select input for choosing column on output subset modal window
    output_subset <- reactiveValues(number_rows = 1)
    #   -- Remark: These two lines take the value of output_subset$number_rows 
    #              and make it available in the client as
    #              output.output_subset_number_rows.
    output$output_subset_number_rows <- reactive(output_subset$number_rows)
    outputOptions(output, 'output_subset_number_rows', suspendWhenHidden = FALSE)
    output$output_subset_var <- renderUI({tagList(
        lapply(seq_len(file_subset_num_cond), function(i) {
            var_name <- paste0('output_subset_var_', i)
            conditionalPanel(
                condition = paste0('output.output_subset_number_rows', '>=', i),
                output[[var_name]] <- renderUI({
                    choices <- c('Choose'='', names(output_table_renamed()))
                    selectInput(var_name, paste('Variable', i), choices)
                })
            )
        })
    )})
    
    # input widget for choosing values on output subset modal window
    output$output_subset_val <- renderUI({tagList(
        lapply(seq_len(file_subset_num_cond), function(i) {
            data <- output_table_renamed()
            
            var_name <- input[[paste0('output_subset_var_', i)]]
            val_name <- paste0('output_subset_val_', i)
            show_condition <- paste0('input.output_subset_var_', i, ' && ',
                                     'output.output_subset_number_rows', '>=', i)
            if(!is.null(var_name) && is.numeric(data[[var_name]])) {
                val_min <- min_na(data[[var_name]])
                val_max <- max_na(data[[var_name]])
                stillSelected <- isolate(
                    ternary(length(input[[val_name]]) == 2, input[[val_name]],
                            ternary(length(input[[val_name]]) == 1,
                                    c(input[[val_name]], val_max),
                                    c(val_min, val_max)))
                )
                conditionalPanel(
                    condition = show_condition,
                    sliderInput(val_name, var_name, val_min, val_max,
                                stillSelected, sep = "")
                )
            } else if(!is.null(var_name) && is.date(data[[var_name]])) {
                start <- min_na(data[[var_name]])
                end <- max_na(data[[var_name]])
                conditionalPanel(
                    condition = show_condition,
                    dateRangeInput(val_name, var_name, start, end)
                )
            } else if(!is.null(var_name)) {
                if(is_blank(var_name))
                    choices <- character(0)
                else
                    choices <- c('Choose'='', unique(data[[var_name]]))
                stillSelected <- isolate(
                    input[[val_name]][input[[val_name]] %in% choices]
                )
                conditionalPanel(
                    condition = show_condition,
                    selectizeInput(
                        val_name, var_name, choices, stillSelected,
                        multiple = T,
                        options = list(plugins = list('drag_drop',
                                                      'remove_button'))
                    )
                )
            }
        })
    )})
    
    # input widget for choosing logical operations
    output$output_subset_logical <- renderUI({tagList(
        lapply(seq.int(from = 2, to = file_subset_num_cond), function(i) {
            var_name <- input[[paste0('output_subset_var_', i)]]
            val_name <- paste0('output_subset_val_', i)
            log_name <- paste0('output_subset_logical', i - 1)
            show_condition <- paste0('input.output_subset_var_', i, ' && ',
                                     'output.output_subset_number_rows', '>=', i)
            conditionalPanel(
                condition = show_condition,
                radioButtons(log_name, 'Logical operator', c('&', '|'),
                             inline = TRUE)
            )
        })
    )})
    
    # action button to add more data filtering rows
    output$output_subset_add <- renderUI({
        actionButton('output_subset_add', 'Add more', icon = icon('plus'))
    })
    observeEvent(input$output_subset_add, {
        output_subset$number_rows <- output_subset$number_rows + 1
    })
    
    # action button to clear the data filtering
    output$output_subset_clear <- renderUI({
        actionButton('output_subset_clear', 'Reset', icon = icon('undo'))
    })
    observeEvent(input$output_subset_clear, {
        for(i in seq_len(file_subset_num_cond)) {
            choices <- c('Choose'='', names(output_table_renamed()))
            updateSelectInput(
                session, paste0('output_subset_var_', i), paste('Variable', i),
                choices = choices, selected = NULL
            )
        }
        output_subset$number_rows <- 1
        output_subset_expr$value <- ''
        output_table_subset_status$value <- FALSE
    })
    
    # action button to close the data subset modal window
    output$output_subset_done <- renderUI({
        actionButton('output_subset_done', 'Done', icon = icon('check'))
    })
    
    # update output table based on data filtering conditions
    output_table_subset <- reactiveValues(value = NULL)
    output_subset_expr <- reactiveValues(value = '')
    output_table_subset_status <- reactiveValues(value = FALSE)
    observeEvent(input$output_subset_done, {
        toggleModal(session, 'output_subset_bsmodal', toggle = 'close')
        
        data <- output_table_renamed()
        to_filter <- rep(TRUE, nrow(data))
        subset_expr_str <- ''
        for(i in seq_len(file_subset_num_cond)) {
            var_name <- input[[paste0('output_subset_var_', i)]]
            val_name <- input[[paste0('output_subset_val_', i)]]
            if(!is_blank(var_name) && !is_blank(val_name)) {
                if(i >= 2) {
                    logop_name <- paste0('output_subset_logical', i - 1)
                    logop_fun <- ifelse(input[[logop_name]] == '&', `&`, `|`)
                    logop_str <- input[[logop_name]]
                } else {
                    logop_fun <- `&`
                    logop_str <- ''
                }
                if(is.numeric(data[[var_name]]) ||
                   is.date(data[[var_name]])) {
                    to_filter <- logop_fun(
                        to_filter,
                        between(data[[var_name]], val_name[1], val_name[2])
                    )
                    subset_expr_str <- paste0(
                        subset_expr_str, logop_str, ' (',
                        paste(paste(var_name, '>=', val_name[1]),
                              paste(var_name, '<=', val_name[2]),
                              sep = '&'), ')'
                    )
                } else {
                    to_filter <- logop_fun(
                        to_filter, data[[var_name]] %in% val_name
                    )
                    subset_expr_str <- paste0(
                        subset_expr_str, logop_str, ' (',
                        paste(var_name, '%in%', deparse(val_name)), ')'
                    )
                }
            }
        }
        output_table_subset$value <- data[to_filter, ]
        output_subset_expr$value <- trimws(subset_expr_str)
        output_table_subset_status$value <- TRUE
    })
    
    
    #------------------------------------------------------
    # Volcano plot
    #------------------------------------------------------
    output$volplot_downloadpage_button <- renderUI({
        req(output_show$volplot)
        actionButton('volplot_downloadpage_button', 'Download plot')
    })
    
    output$volplot_title <- renderUI({
        req(output_show$volplot, volplot_xlab$value, volplot_ylab$value)
        value <- paste(volplot_ylab$value, 'vs', tolower(volplot_xlab$value))
        textAreaInput('volplot_title', 'Plot title', value = value)
    })
    volplot_title <- reactiveValues(value = NULL)
    observe({
        req(output_show$volplot, volplot_xlab$value, volplot_ylab$value)
        value <- paste(volplot_ylab$value, 'vs',tolower( volplot_xlab$value))
        volplot_title$value <- value
    })
    observe({
        volplot_title$value <- input$volplot_title
    })
    
    output$volplot_xlab <- renderUI({
        req(output_show$volplot)
        value <- input$volcano_x
        textInput('volplot_xlab', 'X-axis label', value = value)
    })
    volplot_xlab <- reactiveValues(value = NULL)
    observe({
        req(output_show$volplot)
        value <- input$volcano_x
        volplot_xlab$value <- value
    })
    observe({
        volplot_xlab$value <- input$volplot_xlab
    })
    
    output$volplot_ylab <- renderUI({
        req(output_show$volplot)
        value <- input$volcano_y
        textInput('volplot_ylab', 'Y-axis label', value = value)
    })
    volplot_ylab <- reactiveValues(value = NULL)
    observe({
        req(output_show$volplot)
        value <- input$volcano_y
        volplot_ylab$value <- value
    })
    observe({
        volplot_ylab$value <- input$volplot_ylab
    })
    
    output$volplot_hrefline <- renderUI({
        req(output_show$volplot)
        value <- 0.05
        numericInput('volplot_hrefline', 'Horizontal line', value = value)
    })
    
    output$volplot_vrefline <- renderUI({
        req(output_show$volplot)
        value <- 1
        numericInput('volplot_vrefline', 'Vertical line', value = value)
    })
    
    volplot_refline <- reactiveValues(horizontal = 0.05, vertical = 1)
    observe({
        if(is.null(volplot_xlab$value)) {
            volplot_refline$vertical <- 1
        } else if(volplot_xlab$value == 'Risk difference') {
            volplot_refline$vertical <- 0
        } else {
            volplot_refline$vertical <- 1
        }
    })
    observe({
        volplot_refline$horizontal <- input$volplot_hrefline
        volplot_refline$vertical <- input$volplot_vrefline
    })
    
    output$volplot_label_size <- renderUI({
        req(output_show$volplot)
        sliderInput(
            'volplot_label_size', 'Label size', 1, 6, value = 3, step = 0.1, sep = ""
        )
    })
    volplot_label_size <- reactiveValues(value = 3)
    observe({
        volplot_label_size$value <- input$volplot_label_size
    })
    
    output$volplot_legend_font_size <- renderUI({
        req(output_show$volplot)
        sliderInput(
            'volplot_legend_font_size', 'Legend font size', 1, 14, value = 8, step = 1, sep = ""
        )
    })
    volplot_legend_font_size <- reactiveValues(value = 8)
    observe({
        volplot_legend_font_size$value <- input$volplot_legend_font_size
    })
    
    output$volplot_fn_in <- renderUI({
        req(output_show$volplot)
        value <- ''
        textAreaInput('volplot_fn_in', 'Plot footnote',
                      value = default_footnote_lines)
    })
    
    volplot <- reactive({
        req(output_table(), !is.null(volplot_title$value),
            !is.null(volplot_xlab$value), !is.null(volplot_ylab$value),
            !is.null(volplot_label_size$value))
        data <- output_table()
        group_var <- input$ae_level_1
        x_axis_text <- c(sprintf('\u2190 Favors Treatment'),
                         sprintf('Favors Control \u2192'))
        y_axis_text <- c(sprintf('\u2190 Non-significant'),
                         sprintf('Significant \u2192'))
        if(!is.null(input$volplot_hrefline))
            label_ylim <- c(-Inf, input$volplot_hrefline)
        else label_ylim <- c(-Inf, Inf)
        if(!is.null(input$volplot_vrefline))
            label_xlim <- c(input$volplot_vrefline, Inf)
        else label_xlim <- c(-Inf, Inf)
        
        # Need to use renamed output table column names
        y_var <- volplot_ylab$value
        x_var <- volplot_xlab$value
        # y_var <- p_value_map[volplot_ylab$value]
        # x_var <- stat_map[volplot_xlab$value]
        
        # Need to remove NAs before sending data to plot
        data <- data[!is.na(data[[x_var]])&!is.na(data[[y_var]]),]
        
        x_log <- ifelse(unname(x_var) == 'rd', FALSE, TRUE)
        volcano_plot <- gg_volcano(
            data = data, x_var = x_var, y_var = y_var,
            color_var = group_var, shape_var = group_var,
            add_label = TRUE, label_var = input$ae_level_2, repel_label = TRUE,
            label_size = volplot_label_size$value,
            label_xlim = label_xlim, label_ylim = label_ylim,
            x_log = x_log, y_log = TRUE,
            x_lab = volplot_xlab$value, y_lab = volplot_ylab$value,
            title = volplot_title$value, y_reverse = TRUE,
            add_x_axis_text = FALSE, x_axis_text = x_axis_text,
            add_y_axis_text = FALSE, y_axis_text = y_axis_text,
            legend_pos = 'bottom', legend_font_size = volplot_legend_font_size$value,
            reference_hline = volplot_refline$horizontal,
            reference_vline = volplot_refline$vertical
        )
        return(volcano_plot)
    })
    
    output$volplot <- renderPlot({
        req(output_show$volplot)
        volplot()
    })
    
    # hover tooltip
    output$volplot_hover <- renderUI({
        req(output_show$volplot)
        data <- output_table()
        data[['y_inv']] <- -log(data[['Fisher exact test p value']], base = 10)
        y_var <- volplot_ylab$value
        x_var <- volplot_xlab$value
        # y_var <- p_value_map[volplot_ylab$value]
        # x_var <- stat_map[volplot_xlab$value]
        tooltip(
            input$volplot_hover, data, x_var, 'y_inv',
            vars_print = c(input$ae_level_1, input$ae_level_2, x_var, y_var),
            vars_name = c(input$ae_level_1, input$ae_level_2,
                          volplot_xlab$value, volplot_ylab$value)
        )
    })
    
    # volcano plot footnote
    output$volplot_fn <- renderUI({
        req(output_show$volplot, !is.null(input$volplot_fn_in))
        value <- trimws(input$volplot_fn_in)
        HTML(paste(unlist(strsplit(value, '\n')), collapse = '<br/>'))
    })
    
    # download page format select input widget
    output$volplot_download_format <- renderUI({
        selectInput(
            'volplot_download_format', 'Graph format',
            choices = c('Choose' = '', c('pdf', 'png', 'jpg', 'ps'))
        )
    })
    
    # download page height numeric input widget
    output$volplot_download_height <- renderUI({
        numericInput('volplot_download_height', 'Height (inches)', value = 6)
    })
    
    # download page width numeric input widget
    output$volplot_download_width <- renderUI({
        numericInput('volplot_download_width', 'Width (inches)', value = 10)
    })
    
    # download page with resolution widget
    output$volplot_download_resolution <- renderUI({
        sliderInput(
            'volplot_download_resolution', 'Resolution', 100, 800,
            value = 600, step = 100, sep = ""
        )
    })
    
    # download button
    output$volplot_download_button <- renderUI({
        shinyjs::disabled(downloadButton('volplot_download', 'Download'))
    })
    observe({
        if(all(!is_blank(input$volplot_download_format),
               !is_blank(input$volplot_download_height),
               !is_blank(input$volplot_download_width),
               !is_blank(input$volplot_download_resolution))) {
            shinyjs::enable('volplot_download')
        }
    })
    
    # download volcano plot
    output$volplot_download <- downloadHandler(
        filename = function() {
            paste('volcano_plot_', format(Sys.Date(), format = '%Y%m%d'),
                  input$volplot_download_format, sep = '.')
        },
        content = function(file) {
            height <- input$volplot_download_height
            width <- input$volplot_download_width
            dpi <- input$volplot_download_resolution
            format <- input$volplot_download_format
            footnote <- trimws(input$volplot_fn_in)
            ggsave(filename = file, plot = add_footnote(volplot(), footnote), width = width,
                   height = height, dpi = dpi)
            
            # plot_ <- volplot()
            # plot_ <- add_footnote(plot_, footnote)
            
            # dev <- plot_dev(format, file, dpi = dpi)
            # dev(file = file, width = width, height = height)
            # png(file, width = width, height = height, res = dpi, units = "in")
            # grid.draw(plot_)
            # grDevices::dev.off()
        }
    )
    
    
    #------------------------------------------------------
    # Combined plot: dotplot + forest plot
    #------------------------------------------------------
    
    # x-axis label
    output$forestplot_xlab <- renderUI({
        req(output_show$combplot)
        value <- input$forestplot_x
        textInput('forestplot_xlab', 'X-axis label', value = value)
    })
    forestplot_xlab <- reactiveValues(value = NULL)
    observe({
        req(output_show$combplot)
        value <- input$forestplot_x
        forestplot_xlab$value <- value
    })
    observe({
        forestplot_xlab$value <- input$forestplot_xlab
    })
    
    output$combplot_downloadpage_button <- renderUI({
        req(output_show$combplot)
        actionButton('combplot_downloadpage_button', 'Download plot')
    })

    output$combplot_title_in <- renderUI({
        req(output_show$combplot, forestplot_xlab$value)
        value <- paste('Adverse Events Sorted by',
                       cap_first_letter(forestplot_xlab$value))
        textAreaInput('combplot_title_in', 'Plot title', value = value)
    })

    output$combplot_title <- renderUI({
        req(output_show$combplot, !is.null(input$combplot_title_in))
        value <- input$combplot_title_in
        h4(value, align = 'center')
    })
    
    output$combplot_legend_in <- renderUI({
        req(output_show$combplot)
        value <- paste('')
        textAreaInput('combplot_legend_in', 'Plot legend', value = value)
    })
    
    # show top N
    output$combplot_top_n_ui <- renderUI({
        req(output_show$combplot)
        tagList(
            fluidRow(
                column(width = 6, uiOutput('combplot_top_n_txt')),
                column(width = 6, uiOutput('combplot_top_n'))
            ),
            tags$style(
                type='text/css',
                "#combplot_top_n_txt {margin-top: 5px;}"
            )
        )
    })
    output$combplot_top_n_txt <- renderUI({
        HTML('Show top')
    })
    output$combplot_top_n <- renderUI({
        value <- isolate(
            ternary(is.null(input$combplot_top_n), '20', input$combplot_top_n)
        )
        textInput('combplot_top_n', NULL, value)
    })
    
    # semi log scale of confidence interval
    output$combplot_ci_log <- renderUI({
        req(output_show$combplot, forestplot_xlab$value)
        value <- ifelse(unname(forestplot_xlab$value) %in% c('Risk difference', 'Hazard ratio'), F, T)
        checkboxInput('combplot_ci_log', 'CI log scale', value = value)
    })
    combplot_ci_log <- reactiveValues(value = NULL)
    observe({
        req(forestplot_xlab$value)
        value <- ifelse(unname(forestplot_xlab$value) %in% c('Risk difference', 'Hazard ratio'), F, T)
        combplot_ci_log$value <- value
    })
    observe({
        combplot_ci_log$value <- input$combplot_ci_log
    })

    # forest plot x-axis range slider bar control
    output$combplot_xrange <- renderUI({
        req(output_show$combplot,
            combplot_data())
        data <- combplot_data()
        x_var <- stat_map[forestplot_xlab$value]
        lower_var = paste(x_var, 'lower', sep = '_')
        upper_var = paste(x_var, 'upper', sep = '_')
        min <- ternary(all(!is.finite(data[[lower_var]])), min(data[[x_var]]-abs(data[[x_var]])*0.05), min(data[[lower_var]][is.finite(data[[lower_var]])]))
        max <- ternary(all(!is.finite(data[[upper_var]])), max(2*data[[x_var]]-min), max(data[[upper_var]][is.finite(data[[upper_var]])]))
        
        sliderInput(
            'combplot_xrange', 'X-axis range', min, max, value = c(min, max), step = 0.0001, sep = ""
        )
    })
    combplot_xrange <- reactiveValues(value = NULL)
    observe({
        data <- combplot_data()
        x_var <- stat_map[forestplot_xlab$value]
        lower_var = paste(x_var, 'lower', sep = '_')
        upper_var = paste(x_var, 'upper', sep = '_')
        min <- ternary(all(!is.finite(data[[lower_var]])), min(data[[x_var]]-abs(data[[x_var]])*0.05), min(data[[lower_var]][is.finite(data[[lower_var]])]))
        max <- ternary(all(!is.finite(data[[upper_var]])), max(2*data[[x_var]]-min), max(data[[upper_var]][is.finite(data[[upper_var]])]))
        combplot_xrange$value <- c(min, max)
    })
    observe({
        combplot_xrange$value <- input$combplot_xrange
    })
    
    output$combplot_fn_in <- renderUI({
        req(output_show$combplot)
        textAreaInput('combplot_fn_in', 'Plot footnote',
                      value = default_footnote_lines)
    })
    
    combplot_data <- reactive({
        req(output_table(), forestplot_xlab$value, input$combplot_top_n)
        
        # input check
        validate(
            need(
                is_positive_integer(input$combplot_top_n, convert = TRUE),
                'Show top N, where N must be a positive integer'
            )
        )
        
        data <- output_table()
        colnames(data)[colnames(data) == 'Odds ratio'] <- 'or'
        colnames(data)[colnames(data) == 'Odds ratio lower'] <- 'or_lower'
        colnames(data)[colnames(data) == 'Odds ratio upper'] <- 'or_upper'
        colnames(data)[colnames(data) == 'Relative risk'] <- 'rr'
        colnames(data)[colnames(data) == 'Relative risk lower'] <- 'rr_lower'
        colnames(data)[colnames(data) == 'Relative risk upper'] <- 'rr_upper'
        colnames(data)[colnames(data) == 'Risk difference'] <- 'rd'
        colnames(data)[colnames(data) == 'Risk difference lower'] <- 'rd_lower'
        colnames(data)[colnames(data) == 'Risk difference upper'] <- 'rd_upper'
        colnames(data)[colnames(data) == 'Hazard ratio'] <- 'hr'
        colnames(data)[colnames(data) == 'Hazard ratio lower'] <- 'hr_lower'
        colnames(data)[colnames(data) == 'Hazard ratio upper'] <- 'hr_upper'
        
        colnames(data)[colnames(data) == 'No. of Subjects w Events (Treatment)'] <- 'nevent_trt'
        colnames(data)[colnames(data) == 'No. of Subjects w Events (Control)'] <- 'nevent_ctr'
        colnames(data)[colnames(data) == 'Total No. of Subjects (Treatment)'] <- 'nsubj_trt'
        colnames(data)[colnames(data) == 'Total No. of Subjects (Control)'] <- 'nsubj_ctr'
        # data <- rename(data, 
        #                'nevent_trt' = No. of Subjects w Events (Treatment),
        #                'nevent_ctr' = No. of Subjects w Events (Control),
        #                'nsubj_trt' = Total No. of Subjects (Treatment),
        #                'nsubj_ctr' = Total No. of Subjects (Control),
        #                'or' = Odds ratio,
        #                'or_lower' = Odds ratio lower,
        #                'or_upper' = Odds ratio upper,
        #                'rr' = Relative risk,
        #                'rr'_lower = Relative risk lower,
        #                'rr'_upper = Relative risk upper,
        #                'rd' = Risk difference,
        #                'rd_lower' = Relative risk lower,
        #                'rd_upper' = Risk difference upper,
        #                'hr' = Hazard ratio,
        #                'hr_lower' = Hazard ratio lower,
        #                'hr_upper' = Hazard ratio upper)

        # 'Drug Exposure at Risk (Treatment)' = exposure_trt
        # 'Drug Exposure at Risk (Control)' = exposure_ctr
        # 
        # 'Fisher exact test p value' = fisher_p
        # 'EAIR test p value' = EAIR_p
        # 'Logrank test p value' = logrank_p
        # 'Landmark KM test p value' = landmark_km_p
        # 'DFDR Fisher exact test p value' = fisher_p_dfdr
        # 'DFDR EAIR test p value' = EAIR_p_dfdr
        # 'DFDR Logrank test p value' = logrank_p_dfdr
        # 'DFDR Landmark KM test p value' = landmark_km_p_dfdr
        
        var <- stat_map[forestplot_xlab$value]
        
        # Need to remove NAs before sending data to plot
        data <- data[!is.na(data[[var]]),]
        
        top_n <- as.numeric(input$combplot_top_n)
        tbl <- data %>%
            filter_(paste0('is.finite(', var, ')')) %>%
            mutate(prop_trt = nevent_trt / nsubj_trt,
                   prop_ctr = nevent_ctr / nsubj_ctr) %>%
            arrange_(paste0('desc(', var, ')')) %>%
            filter(row_number() <= top_n)
        tbl[[input$ae_level_1]] <- factor(
            tbl[[input$ae_level_1]], levels = rev(unique(tbl[[input$ae_level_1]]))
        )
        tbl[[input$ae_level_2]] <- factor(
            tbl[[input$ae_level_2]], levels = rev(unique(tbl[[input$ae_level_2]]))
        )
        return(tbl)
    })
    
    dotplot <- reactive({
        req(combplot_data())
        trt_str <- input$treatment_mapping
        ctr_str <- input$control_mapping
        prop_trt_str <- 'prop_trt'
        prop_ctr_str <- 'prop_ctr'
        rename_str <- setNames(
            paste0('`', c(prop_trt_str, prop_ctr_str), '`'),
            c(trt_str, ctr_str)
        )
        data <- combplot_data() %>%
            select_(input$ae_level_1, input$ae_level_2, prop_trt_str, prop_ctr_str) %>%
            rename_(.dots = rename_str)
        data <- reshape2::melt(
            data, id.vars = c(input$ae_level_1, input$ae_level_2)
        )
        dotplot <- gg_dotplot(
            data = data, x_var = 'value', y_var = input$ae_level_2,
            color_var = 'variable', shape_var = NULL,
            draw_x_axis = TRUE, draw_y_axis = TRUE,
            x_lab = '', y_lab = '', title = NULL,
            add_legend = TRUE, legend_pos = 'bottom',
            summarize_method = 'none',
            strip_title = 'Proportion', strip_color = '#ffe5cc',
            label_align = 1, grids = 'y'
        )
        dotplot <- dotplot + scale_colour_discrete(name = '')
        return(dotplot)
    })
    
    forestplot <- reactive({
        req(combplot_data())
        data <- combplot_data()
        x_var <- stat_map[forestplot_xlab$value]
        strip_title <- paste0(
            forestplot_xlab$value, ' with ',
            100 * as.numeric(input$confidence_level), '%', ' CI'
        )
        forestplot <- gg_forestplot(
            data = data, x_var = x_var, y_var = input$ae_level_2,
            lower_var = paste(x_var, 'lower', sep = '_'),
            upper_var = paste(x_var, 'upper', sep = '_'),
            draw_x_axis = TRUE, draw_y_axis = TRUE,
            x_lab = '', y_lab = '', title = NULL, 
            x_limit = combplot_xrange$value,
            x_log = combplot_ci_log$value,
            add_legend = FALSE, legend_pos = 'bottom',
            summarize_method = NULL, ci_method = NULL,
            strip_title = strip_title, strip_color = '#ffe5cc',
            label_align = 1, grids = 'y'
        )
        return(forestplot)
    })
    
    output$dotplot <- renderPlot({
        req(output_show$combplot)
        dotplot <- dotplot()
        dotplot <- gg_remove(dotplot, c('title', 'legend')) +
            theme(plot.margin = unit(c(0, 0.2, 0, 0.5), 'lines'))
        return(dotplot)
    })
    
    output$forestplot <- renderPlot({
        req(output_show$combplot)
        forestplot <- forestplot()
        forestplot <- gg_remove(forestplot, c('title', 'yaxis')) +
            theme(plot.margin = unit(c(0, 1, 0, 0.2), 'lines'))
        return(forestplot)
    })
    
    output$combplot_legend <- renderPlot({
        req(output_show$combplot)
        dotplot <- dotplot()
        legend_dotplot <- cowplot::plot_grid(cowplot::get_legend(
            dotplot + theme(legend.position = 'bottom')
        ))
        return(legend_dotplot)
    })
    
    output$combplot_fn <- renderUI({
        req(output_show$combplot, !is.null(input$combplot_fn_in))
        value <- trimws(input$combplot_fn_in)
        HTML(paste(unlist(strsplit(value, '\n')), collapse = '<br/>'))
    })
    
    # download page format select input widget
    output$combplot_download_format <- renderUI({
        selectInput(
            'combplot_download_format', 'Graph format',
            choices = c('Choose' = '', c('pdf', 'png', 'jpg', 'ps'))
        )
    })
    
    # download page height numeric input widget
    output$combplot_download_height <- renderUI({
        numericInput('combplot_download_height', 'Height (inches)', value = 6)
    })
    
    # download page width numeric input widget
    output$combplot_download_width <- renderUI({
        numericInput('combplot_download_width', 'Width (inches)', value = 10)
    })
    
    # download page with resolution widget
    output$combplot_download_resolution <- renderUI({
        sliderInput(
            'combplot_download_resolution', 'Resolution', 100, 800,
            value = 600, step = 100, sep = ""
        )
    })
    
    # download button
    output$combplot_download_button <- renderUI({
        shinyjs::disabled(downloadButton('combplot_download', 'Download'))
    })
    observe({
        if(all(!is_blank(input$combplot_download_format),
               !is_blank(input$combplot_download_height),
               !is_blank(input$combplot_download_width),
               !is_blank(input$combplot_download_resolution))) {
            shinyjs::enable('combplot_download')
        }
    })
    
    # download combplot
    output$combplot_download <- downloadHandler(
        filename = function() {
            paste('dotplot_', format(Sys.Date(), format = '%Y%m%d'),
                  input$combplot_download_format, sep = '.')
        },
        content = function(file) {
            height <- input$combplot_download_height
            width <- input$combplot_download_width
            resltn <- input$combplot_download_resolution
            format <- input$combplot_download_format

            dotplot <- dotplot()
            legend_dotplot <- cowplot::plot_grid(cowplot::get_legend(
                dotplot + theme(legend.position = 'bottom')
            ))
            dotplot <- gg_remove(dotplot, c('title', 'legend')) +
                theme(plot.margin = unit(c(0, 0.2, 0, 0.5), 'lines'))
            forestplot <- forestplot()
            forestplot <- gg_remove(forestplot, c('title', 'yaxis')) +
                theme(plot.margin = unit(c(0, 1, 0, 0.2), 'lines'))

            footnote <- grid::textGrob(
                trimws(input$combplot_fn_in), x = 0,  hjust = -0.1,
                vjust = 0.1, gp = gpar(fontface = 'italic', fontsize = 8)
            )
            
            combplot <- cowplot::plot_grid(
                dotplot, forestplot, align = 'h', nrow = 1,
                rel_widths = c(0.8, 0.5)
            )
            combplot <- cowplot::plot_grid(
                combplot, legend_dotplot, ncol = 1, rel_heights = c(1, 0.1)
            )
            combplot <- gridExtra::arrangeGrob(
                combplot, bottom = footnote, top = input$combplot_title_in
            )
            dev <- plot_dev(format, file, dpi = resltn)
            dev(file = file, width = width, height = height)
            grid.draw(combplot)
            grDevices::dev.off()
        }
    )
    
})































