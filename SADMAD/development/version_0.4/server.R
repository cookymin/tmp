# server.R

shinyServer(function(input, output, session) {
    
    #-----------------------------------------------
    # Import data page
    #-----------------------------------------------
    
    # define some variables at the app launch
    data_ <- reactiveValues(
        subj_pk_param = NULL, subj_pd = NULL, subj_pkpd = NULL,
        sample_pk_param = NULL, sample_pk_con = NULL, sample_pd = NULL,
        sample_pkpd = NULL, lab = NULL, ae = NULL, enr = NULL
    )
    data_name <- reactiveValues(
        subj_pkpd = NULL, sample_pkpd = NULL, ae_enr = NULL
    )
    data_import_status <- reactiveValues(
        subj_pk_param = NULL, subj_pd = NULL, subj_pkpd = NULL,
        sample_pk_param = NULL, sample_pk_con = NULL, sample_pd = NULL,
        sample_pkpd = NULL, lab = NULL, ae = NULL, enr = NULL, ae_enr = NULL
    )
    data_import_msg <- reactiveValues(
        subj_pk_param = NULL, subj_pd = NULL,
        sample_pk_param = NULL, sample_pk_con = NULL, sample_pd = NULL,
        lab = NULL, ae = NULL, enr = NULL
    )
    tnf_rows <- reactiveValues(
        subj_pk = NULL, subj_pd = NULL, subj_pkpd = NULL,
        sample_pk = NULL, sample_pd = NULL, sample_pkpd = NULL,
        lab = NULL, ae = NULL, enr = NULL
    )
    n_tfl <- reactiveValues(
        subj_pk_param_sumtable = NULL, subj_pk_param_boxplot = NULL,
        subj_pk_param_lineplot = NULL, subj_pk_param_doseprop = NULL,
        subj_pd_dose_table = NULL, subj_pd_dose_boxplot = NULL,
        subj_pd_dose_lineplot = NULL, subj_pd_corr_scatter = NULL,
        subj_pd_corr_forest = NULL, subj_pd_corr_2dforest = NULL,
        subj_pkpd_scatter = NULL, subj_pkpd_forest = NULL,
        subj_pkpd_2dforest = NULL, subj_pkpd_quartile = NULL,
        
        sample_pk_con_sumtable = NULL, sample_pk_con_sumline = NULL,
        sample_pk_con_indline = NULL, sample_pk_param_sumtable = NULL,
        sample_pk_param_sumline = NULL, sample_pk_param_indline = NULL,
        sample_pd_time_sumtable = NULL, sample_pd_time_sumline = NULL,
        sample_pd_time_indline = NULL, sample_pd_corr_sumline = NULL,
        sample_pd_corr_scatter = NULL, sample_pkpd_sumline = NULL,
        sample_pkpd_scatter = NULL, sample_pkpd_quartile = NULL,
        
        lab_table = NULL, lab_abnormality_table = NULL,
        lab_summary_lineplot = NULL, lab_individual_lineplot = NULL,
        
        figure = NULL, table = NULL
    )
    default_footnote <- reactiveValues(
        subj_pk_param = NULL, subj_pd = NULL, subj_pkpd = NULL,
        sample_pk_param = NULL, sample_pk_con = NULL, sample_pd = NULL,
        sample_pkpd = NULL, lab = NULL, ae = NULL, enr = NULL
    )
    
    # fileInput for sample-level PK concentration
    output$file_sample_pk_con_ui <- renderUI({
        req(input$file_sample_pk_type == 'PK concentration')
        fileInput(
            'file_sample_pk_con',
            tags$p('PK data', tags$a('(Need template?)', target = '_blank',
                                     href = file_pk_con_template))
        )
    })
    
    # fileInput for sample-level PK parameter
    output$file_sample_pk_param_ui <- renderUI({
        req(input$file_sample_pk_type == 'PK parameter')
        fileInput(
            'file_sample_pk_param',
            tags$p('PK data', tags$a('(Need template?)', target = '_blank',
                                     href = file_pk_param_template))
        )
    })
    
    # set either sample PK concentration or parameter data to NULL when
    # radioButtons changes
    observe({
        input$file_sample_pk_type
        if(identical(input$file_sample_pk_type, 'PK concentration')) {
            data_$sample_pk_param <- NULL
            data_import_status$sample_pk_param <- NULL
            data_import_msg$sample_pk_param <- NULL
        } else if(identical(input$file_sample_pk_type, 'PK parameter')) {
            data_$sample_pk_con <- NULL
            data_import_status$sample_pk_con <- NULL
            data_import_msg$sample_pk_con <- NULL
        }
    })
    
    # initialization:
    #   a. update data_
    #   b. update data importation status
    observe({
        if(!is.null(input$file_subj_pk_param)) {
            result_ <- tryCatch(
                shiny_readin_file(input$file_subj_pk_param, 'DATA',
                                  subj_pk_required_cols),
                error = c
            )
            if('message' %in% names(result_)) {
                data_import_status$subj_pk_param <- FALSE
                data_import_msg$subj_pk_param <- paste(
                    paste(file_import_fail_msg, input$file_subj_pk_param$name),
                    result_$message, sep = '\n'
                )
            } else {
                data_import_status$subj_pk_param <- TRUE
                data_import_msg$subj_pk_param <- paste(
                    file_import_success_msg, input$file_subj_pk_param$name
                )
                data_$subj_pk_param <- result_
                default_footnote$subj_pk_param <- default_footnote_lines()
            }
        }
        if(!is.null(input$file_subj_pd)) {
            result_ <- tryCatch(
                shiny_readin_file(input$file_subj_pd, 'DATA',
                                  subj_pd_required_cols),
                error = c
            )
            if('message' %in% names(result_)) {
                data_import_status$subj_pd <- FALSE
                data_import_msg$subj_pd <- paste(
                    paste(file_import_fail_msg, input$file_subj_pd$name),
                    result_$message, sep = '\n'
                )
            } else {
                data_import_status$subj_pd <- TRUE
                data_import_msg$subj_pd <- paste(
                    file_import_success_msg, input$file_subj_pd$name
                )
                data_$subj_pd <- result_
                default_footnote$subj_pd <- default_footnote_lines()
            }
        }
        if(!is.null(input$file_sample_pk_con)) {
            result_ <- tryCatch(
                shiny_readin_file(input$file_sample_pk_con, 'DATA',
                                  sample_pk_con_required_cols),
                error = c
            )
            if('message' %in% names(result_)) {
                data_import_status$sample_pk_con <- FALSE
                data_import_msg$sample_pk_con <- paste(
                    paste(file_import_fail_msg, input$file_sample_pk_con$name),
                    result_$message, sep = '\n'
                )
            } else {
                data_import_status$sample_pk_con <- TRUE
                data_import_msg$sample_pk_con <- paste(
                    file_import_success_msg, input$file_sample_pk_con$name
                )
                # replace BLQ in concentration column
                if('BLQ' %in% result_[[sample_pk_con_con_col]]) {
                    blq_idx <- result_[[sample_pk_con_con_col]] == 'BLQ'
                    nna_idx <- !is.na(result_[[sample_pk_con_con_col]])
                    blq_idx <- blq_idx & nna_idx
                    if(sum(blq_idx) > 0) {
                        if('LLOQ' %in% names(result_)) {
                            result_[blq_idx, sample_pk_con_con_col] <- as.numeric(
                                result_[blq_idx, 'LLOQ'] / 2
                            )
                        } else {
                            result_[blq_idx, sample_pk_con_con_col] <- NA
                        }
                        visit_char <- trimws(as.character(
                            result_[[sample_pk_con_visit_col]]
                        ))
                        timepoint_char <- trimws(as.character(
                            result_[[sample_pk_con_time_col]]
                        ))
                        if(!(sample_pk_con_visit_col %in% names(result_)) ||
                           !('1' %in% visit_char)) {
                            bs_idx <- timepoint_char == '0'
                            result_[blq_idx & bs_idx, sample_pk_con_con_col] <- 0
                        } else {
                            bs_idx <- visit_char == '1' & timepoint_char == '0'
                            result_[blq_idx & bs_idx, sample_pk_con_con_col] <- 0
                        }
                    }
                }
                data_$sample_pk_con <- result_
                default_footnote$sample_pk_con <- default_footnote_lines()
            }
        }
        if(!is.null(input$file_sample_pk_param)) {
            result_ <- tryCatch(
                shiny_readin_file(input$file_sample_pk_param, 'DATA',
                                  sample_pk_param_required_cols),
                error = c
            )
            if('message' %in% names(result_)) {
                data_import_status$sample_pk_param <- FALSE
                data_import_msg$sample_pk_param <- paste(
                    paste(file_import_fail_msg, input$file_sample_pk_param$name),
                    result_$message, sep = '\n'
                )
            } else {
                data_import_status$sample_pk_param <- TRUE
                data_import_msg$sample_pk_param <- paste(
                    file_import_success_msg, input$file_sample_pk_param$name
                )
                data_$sample_pk_param <- result_
                default_footnote$sample_pk_param <- default_footnote_lines()
            }
        }
        if(!is.null(input$file_sample_pd)) {
            result_ <- tryCatch(
                shiny_readin_file(input$file_sample_pd, 'DATA',
                                  sample_pd_required_cols),
                error = c
            )
            if('message' %in% names(result_)) {
                data_import_status$sample_pd <- FALSE
                data_import_msg$sample_pd <- paste(
                    paste(file_import_fail_msg, input$file_sample_pd$name),
                    result_$message, sep = '\n'
                )
            } else {
                data_import_status$sample_pd <- TRUE
                data_import_msg$sample_pd <- paste(
                    file_import_success_msg, input$file_sample_pd$name
                )
                data_$sample_pd <- result_
                default_footnote$sample_pd <- default_footnote_lines()
            }
        }
        if(!is.null(input$file_lab)) {
            result_ <- tryCatch(
                shiny_readin_file(input$file_lab, 'DATA', lab_required_cols),
                error = c
            )
            if('message' %in% names(result_)) {
                data_import_status$lab <- FALSE
                data_import_msg$lab <- paste(
                    paste(file_import_fail_msg, input$file_lab$name),
                    result_$message, sep = '\n'
                )
            } else {
                data_import_status$lab <- TRUE
                data_import_msg$lab <- paste(
                    file_import_success_msg, input$file_lab$name
                )
                data_$lab <- result_
                default_footnote$lab <- default_footnote_lines()
            }
        }
        if(!is.null(input$file_ae)) {
            result_ <- tryCatch(
                shiny_readin_file(input$file_ae, 'DATA', ae_required_cols),
                error = c
            )
            if('message' %in% names(result_)) {
                data_import_status$ae <- FALSE
                data_import_msg$ae <- paste(
                    paste(file_import_fail_msg, input$file_ae$name),
                    result_$message, sep = '\n'
                )
            } else {
                data_import_status$ae <- TRUE
                data_import_msg$ae <- paste(
                    file_import_success_msg, input$file_ae$name
                )
                data_$ae <- result_
                default_footnote$ae <- default_footnote_lines()
            }
        }
        if(!is.null(input$file_enr)) {
            result_ <- tryCatch(
                shiny_readin_file(input$file_enr, 'DATA', enr_required_cols),
                error = c
            )
            if('message' %in% names(result_)) {
                data_import_status$enr <- FALSE
                data_import_msg$enr <- paste(
                    paste(file_import_fail_msg, input$file_enr$name),
                    result_$message, sep = '\n'
                )
            } else {
                data_import_status$enr <- TRUE
                data_import_msg$enr <- paste(
                    file_import_success_msg, input$file_enr$name
                )
                data_$enr <- result_
                default_footnote$enr <- default_footnote_lines()
            }
        }
    }, priority = 1)
    
    # print data importation status message
    output$file_subj_pk_import_msg <- renderUI({HTML(
        gsub('\n', tags$br(), data_import_msg$subj_pk_param)
    )})
    output$file_subj_pd_import_msg <- renderUI({HTML(
        gsub('\n', tags$br(), data_import_msg$subj_pd)
    )})
    output$file_sample_pk_con_import_msg <- renderUI({HTML(
        gsub('\n', tags$br(), data_import_msg$sample_pk_con)
    )})
    output$file_sample_pk_param_import_msg <- renderUI({HTML(
        gsub('\n', tags$br(), data_import_msg$sample_pk_param)
    )})
    output$file_sample_pd_import_msg <- renderUI({HTML(
        gsub('\n', tags$br(), data_import_msg$sample_pd)
    )})
    output$file_lab_import_msg <- renderUI({HTML(
        gsub('\n', tags$br(), data_import_msg$lab)
    )})
    output$file_ae_import_msg <- renderUI({HTML(
        gsub('\n', tags$br(), data_import_msg$ae)
    )})
    output$file_enr_import_msg <- renderUI({HTML(
        gsub('\n', tags$br(), data_import_msg$enr)
    )})

    # merge subject-level PK and PD data
    output$file_subj_pkpd_merge <- renderUI({
        req(isTRUE(data_import_status$subj_pk_param),
            isTRUE(data_import_status$subj_pd))
        actionButton('file_subj_pkpd_merge', 'Merge PK PD')
    })
    observeEvent(input$file_subj_pkpd_merge, {
        data_pk <- data_$subj_pk_param
        data_pd <- data_$subj_pd
        
        # prepare for PK and PD data merge
        #   1. Add a standardized ID column for the two data to be merged on
        #   2. Add a comb_param column to have a combination of PK-PD params
        data_pk$std_id <- last_num(as.character(data_pk[[subj_pk_subj_col]]))
        data_pd$std_id <- last_num(as.character(data_pd[[subj_pd_subj_col]]))
        min_len_pk <- min(nchar(data_pk$std_id))
        min_len_pd <- min(nchar(data_pd$std_id))
        if(min(min_len_pk, min_len_pd) >= 5) {
            data_pk$std_id <- substr_tail(data_pk$std_id, n = 5)
            data_pd$std_id <- substr_tail(data_pd$std_id, n = 5)
        } else if(min(min_len_pk, min_len_pd) == 4) {
            data_pk$std_id <- substr_tail(data_pk$std_id, n = 4)
            data_pd$std_id <- substr_tail(data_pd$std_id, n = 4)
        } else {
            if(min_len_pk < 4 && min_len_pd < 4) {
                data_pk$std_id <- str_pad(data_pk$std_id, 4, pad = '0')
                data_pd$std_id <- str_pad(data_pd$std_id, 4, pad = '0')
            } else {
                err_msg <- paste('Cannot match PK and PD subject ID,',
                                 'at least 4 digits of subject ID required')
                stop(err_msg)
            }
        }
        
        data_pk <- arrange_(data_pk, 'std_id', subj_pk_param_col)
        data_pd <- arrange_(data_pd, 'std_id', subj_pd_param_col)
        nrows_pk <- nrow(data_pk)
        nrows_pd <- nrow(data_pd)
        params_pk <- unique(data_pk[[subj_pk_param_col]])
        params_pd <- unique(data_pd[[subj_pd_param_col]])
        nparams_pk <- length(params_pk)
        nparams_pd <- length(params_pd)
        
        # populate rows of PK data by iterating over PD parameters
        # vice versa for PD data
        data_pk <- data_pk[rep(seq_len(nrows_pk), each = nparams_pd), ]
        data_pd <- data_pd[rep(seq_len(nrows_pd), each = nparams_pk), ]
        data_pk$comb_param <- paste(
            data_pk[[subj_pk_param_col]], rep(params_pd, nrows_pk), sep = '-'
        )
        data_pd$comb_param <- paste(
            rep(params_pk, nrows_pd), data_pd[[subj_pd_param_col]], sep = '-'
        )
        data_pk <- arrange_(data_pk, 'std_id', 'comb_param')
        data_pd <- arrange_(data_pd, 'std_id', 'comb_param')
        data_pk$idx <- sequence(dplyr::count(data_pk, std_id, comb_param)$n)
        data_pd$idx <- sequence(dplyr::count(data_pd, std_id, comb_param)$n)
        merged_data <- full_join(
            data_pk, data_pd, by = c('std_id', 'comb_param', 'idx')
        )
        merged_data <- dplyr::select(merged_data, -std_id, -comb_param, -idx)
        data_$subj_pkpd <- merged_data
        default_footnote$subj_pkpd <- default_footnote_lines()
        
        file_fmt <- 'csv'
        data_name$subj_pkpd <- paste0(
            'subject_pkpd_', format(Sys.Date(), format = '%Y%m%d'), '.', file_fmt
        )
        if(!exists('temp_dir') || is.null(temp_dir))
            temp_dir <<- tempdir()
        file_create(data_name$subj_pkpd, temp_dir, data_$subj_pkpd,
                    format = file_fmt)
        data_import_status$subj_pkpd <- TRUE
    })
    
    # download button for download merged subject-level PK-PD data
    output$file_subj_pkpd_download_button <- renderUI({
        req(isTRUE(data_import_status$subj_pkpd))
        downloadButton('file_subj_pkpd_download', 'Download merged PK-PD data')
    })

    # download handler for merged subject-level PK-PD data
    output$file_subj_pkpd_download <- downloadHandler(
        filename = function() {data_name$subj_pkpd},
        content = function(file) {
            file.copy(paste(temp_dir, data_name$subj_pkpd, sep = '\\'), file)
        }
    )
    
    # specify whether merge sample-level PK-PD by visit or by timepoint
    output$file_sample_pkpd_merge_by <- renderUI({
        req(((input$file_sample_pk_type == 'PK parameter' &&
                  isTRUE(data_import_status$sample_pk_param)) ||
                 (input$file_sample_pk_type == 'PK concentration' &&
                      isTRUE(data_import_status$sample_pk_con))),
            isTRUE(data_import_status$sample_pd))
        if(input$file_sample_pk_type == 'PK parameter') {
            choices <- c('Choose' = '', 'Visit only')
            selected <- 'Visit only'
        } else {
            choices <- c('Choose' = '', 'Visit only', 'Visit and timepoint')
            selected <- NULL
        }
        selectInput(
            'file_sample_pkpd_merge_by',
            tags$html(
                'Merge by',
                tags$style(type = 'text/css',
                           '#qfile_sample_pkpd_merge_by {vertical-align: top;}'),
                shinyBS::bsButton('qfile_sample_pkpd_merge_by', label = '',
                                  icon = icon('question'), style = 'info',
                                  size = 'extra-small')
            ),
            choices = choices,
            selected = selected
        )
    })
    observe({
        input$qfile_sample_pkpd_merge_by
        shinyBS::addTooltip(
            session, 'qfile_sample_pkpd_merge_by',
            file_sample_pkpd_merge_by_htext,
            placement = 'right', trigger = 'hover',
            options = list(container = 'body')
        )
    })

    # merge sample-level PK and PD data
    output$file_sample_pkpd_merge <- renderUI({
        req(((input$file_sample_pk_type == 'PK parameter' &&
                  isTRUE(data_import_status$sample_pk_param)) ||
                 (input$file_sample_pk_type == 'PK concentration' &&
                      isTRUE(data_import_status$sample_pk_con))),
            isTRUE(data_import_status$sample_pd),
            input$file_sample_pkpd_merge_by)
        actionButton(
            'file_sample_pkpd_merge',
            tags$html(
                'Merge PK PD',
                tags$style(type = 'text/css',
                           '#qfile_sample_pkpd_merge {vertical-align: top;}'),
                shinyBS::bsButton('qfile_sample_pkpd_merge', label = '',
                                  icon = icon('question'),
                                  style = 'info',
                                  size = 'extra-small')
            )
        )
    })
    observe({
        input$qfile_sample_pkpd_merge
        shinyBS::addTooltip(
            session, 'qfile_sample_pkpd_merge', file_sample_pkpd_merge_htext,
            placement = 'right', trigger = 'hover',
            options = list(container = 'body')
        )
    })
    observeEvent(input$file_sample_pkpd_merge, {
        if(input$file_sample_pk_type == 'PK parameter' &&
           isTRUE(data_import_status$sample_pk_param)) {
            data_pk <- data_$sample_pk_param
            pk_subj_col <- sample_pk_param_subj_col
        } else if(input$file_sample_pk_type == 'PK concentration' &&
                  isTRUE(data_import_status$sample_pk_con)) {
            data_pk <- data_$sample_pk_con
            pk_subj_col <- sample_pk_con_subj_col
        }
        data_pd <- data_$sample_pd
        
        # prepare for PK and PD data merge
        #   1. Add a standardized ID column for the two data to be merged on
        #   2. Add a comb_param column to have a combination of PK-PD params
        data_pk$std_id <- last_num(as.character(data_pk[[pk_subj_col]]))
        data_pd$std_id <- last_num(as.character(data_pd[[sample_pd_subj_col]]))
        min_len_pk <- min(nchar(data_pk$std_id))
        min_len_pd <- min(nchar(data_pd$std_id))
        if(min(min_len_pk, min_len_pd) >= 5) {
            data_pk$std_id <- substr_tail(data_pk$std_id, n = 5)
            data_pd$std_id <- substr_tail(data_pd$std_id, n = 5)
        } else if(min(min_len_pk, min_len_pd) == 4) {
            data_pk$std_id <- substr_tail(data_pk$std_id, n = 4)
            data_pd$std_id <- substr_tail(data_pd$std_id, n = 4)
        } else {
            if(min_len_pk < 4 && min_len_pd < 4) {
                data_pk$std_id <- str_pad(data_pk$std_id, 4, pad = '0')
                data_pd$std_id <- str_pad(data_pd$std_id, 4, pad = '0')
            } else {
                err_msg <- paste('Cannot match PK and PD subject ID,',
                                 'at least 4 digits of subject ID required')
                stop(err_msg)
            }
        }
        
        if(input$file_sample_pk_type == 'PK parameter' &&
           isTRUE(data_import_status$sample_pk_param)) {
            by_pk <- c(sample_pk_param_subj_col,
                       sample_pk_param_visit_col,
                       sample_pk_param_param_col)
            by_pd <- c(sample_pd_subj_col,
                       sample_pd_avisitn_col,
                       sample_pd_param_col)
            data_pk <- aggregate_df(data_pk, by = by_pk)
            data_pd <- aggregate_df(data_pd, by = by_pd)
            data_pk[[sample_pk_param_visit_col]] <- as.numeric(
                data_pk[[sample_pk_param_visit_col]]
            )
            data_pd[[sample_pd_avisitn_col]] <- as.numeric(
                data_pd[[sample_pd_avisitn_col]]
            )
            by_vec <- c('std_id', sample_pd_avisitn_col)
            names(by_vec) <- c('std_id', sample_pk_param_visit_col)
            merged_data <- full_join(data_pk, data_pd, by = by_vec)
            merged_data <- dplyr::select(merged_data, -std_id)
            data_$sample_pkpd <- merged_data
            default_footnote$sample_pkpd <- default_footnote_lines()
        } else if(input$file_sample_pk_type == 'PK concentration' &&
                  isTRUE(data_import_status$sample_pk_con)) {
            by_visit <- input$file_sample_pkpd_merge_by == 'Visit only'
            by_pk <- c(sample_pk_con_subj_col, sample_pk_con_visit_col)
            by_pd <- c(sample_pd_subj_col, sample_pd_avisitn_col)
            if(!by_visit) {
                by_pk <- c(by_pk, sample_pk_con_time_col)
                by_pd <- c(by_pd, sample_pd_atptn_col)
            }
            by_pd <- c(by_pd, sample_pd_param_col)
            data_pk <- aggregate_df(data_pk, by = by_pk)
            data_pd <- aggregate_df(data_pd, by = by_pd)
            data_pk[[sample_pk_con_visit_col]] <- as.numeric(
                data_pk[[sample_pk_con_visit_col]]
            )
            data_pd[[sample_pd_avisitn_col]] <- as.numeric(
                data_pd[[sample_pd_avisitn_col]]
            )
            by_vec <- c('std_id', sample_pd_avisitn_col)
            name_vec <- c('std_id', sample_pk_con_visit_col)
            if(!by_visit) {
                data_pk[[sample_pk_con_time_col]] <- as.numeric(
                    data_pk[[sample_pk_con_time_col]]
                )
                data_pd[[sample_pd_atptn_col]] <- as.numeric(
                    data_pd[[sample_pd_atptn_col]]
                )
                by_vec <- c(by_vec, sample_pd_atptn_col)
                name_vec <- c(name_vec, sample_pk_con_time_col)
            }
            names(by_vec) <- name_vec
            # write.csv(data_pk, file = 'C:\\Users\\fniu\\Desktop\\data_pk.csv',
            #           row.names = FALSE, )
            merged_data <- full_join(data_pk, data_pd, by = by_vec)
            merged_data <- dplyr::select(merged_data, -std_id)
            data_$sample_pkpd <- merged_data
            default_footnote$sample_pkpd <- default_footnote_lines()
        }
        
        file_fmt <- 'csv'
        data_name$sample_pkpd <- paste0(
            'sample_pkpd_', format(Sys.Date(), format = '%Y%m%d'), '.', file_fmt
        )
        if(!exists('temp_dir') || is.null(temp_dir))
            temp_dir <<- tempdir()
        file_create(data_name$sample_pkpd, temp_dir, data_$sample_pkpd,
                    format = file_fmt)
    })
    
    # decide the status of the merged sample-level PK-PD data
    observe({
        if(((input$file_sample_pk_type == 'PK parameter' &&
             isTRUE(data_import_status$sample_pk_param)) ||
            (input$file_sample_pk_type == 'PK concentration' &&
             isTRUE(data_import_status$sample_pk_con))) &&
           isTRUE(data_import_status$sample_pd) &&
           !is.null(input$file_sample_pkpd_merge) &&
           input$file_sample_pkpd_merge > 0) {
            data_import_status$sample_pkpd <- TRUE
        } else {
            data_$sample_pkpd <- NULL
            data_import_status$sample_pkpd <- FALSE
        }
    })
    
    # download button for download merged sample-level PK-PD data
    output$file_sample_pkpd_download_button <- renderUI({
        req(((input$file_sample_pk_type == 'PK parameter' &&
                  isTRUE(data_import_status$sample_pk_param)) ||
                 (input$file_sample_pk_type == 'PK concentration' &&
                      isTRUE(data_import_status$sample_pk_con))),
            isTRUE(data_import_status$sample_pkpd))
        downloadButton('file_sample_pkpd_download','Download merged PK-PD')
    })
    
    # download handler for merged sample-level PK-PD data
    output$file_sample_pkpd_download <- downloadHandler(
        filename = function() {data_name$sample_pkpd},
        content = function(file) {
            file.copy(paste(temp_dir, data_name$sample_pkpd, sep = '\\'), file)
        }
    )
    
    # merge AE and enrollment data
    # output$file_ae_enr_merge <- renderUI({
    #     req(isTRUE(data_import_status$ae),
    #         isTRUE(data_import_status$enr))
    #     actionButton( 'file_ae_enr_merge', 'Merge AE & Enrollment')
    # })
    # observeEvent(input$file_ae_enr_merge, {
    #     by_vec <- setNames(c(ae_subj_col), c(enr_subj_col))
    #     ae_enr <- dplyr::full_join(data_$ae, data_$enr, by = by_vec)
    #     data_$ae_enr <- ae_enr
    #     default_footnote$ae_enr <- default_footnote_lines()
    #     
    #     file_fmt <- 'csv'
    #     data_name$ae_enr <- paste0(
    #         'ae_enrollment_', format(Sys.Date(), format = '%Y%m%d'),
    #         '.', file_fmt
    #     )
    #     if(!exists('temp_dir') || is.null(temp_dir))
    #         temp_dir <<- tempdir()
    #     file_create(data_name$ae_enr, temp_dir, data_$ae_enr, format = file_fmt)
    # })
    # observe({
    #     if(isTRUE(data_import_status$ae) && isTRUE(data_import_status$enr) &&
    #        !is.null(input$file_ae_enr_merge) && input$file_ae_enr_merge > 0) {
    #         data_import_status$ae_enr <- TRUE
    #     } else {
    #         data_$ae_enr <- NULL
    #         data_import_status$ae_enr <- FALSE
    #     }
    # })
    observe({
        req(isTRUE(data_import_status$ae), isTRUE(data_import_status$enr))
        by_vec <- setNames(c(ae_subj_col), c(enr_subj_col))
        ae_enr <- dplyr::full_join(data_$ae, data_$enr, by = by_vec)
        data_$ae_enr <- ae_enr
        default_footnote$ae_enr <- default_footnote_lines()
        
        file_fmt <- 'csv'
        data_name$ae_enr <- paste0(
            'ae_enrollment_', format(Sys.Date(), format = '%Y%m%d'),
            '.', file_fmt
        )
        if(!exists('temp_dir') || is.null(temp_dir))
            temp_dir <<- tempdir()
        file_create(data_name$ae_enr, temp_dir, data_$ae_enr, format = file_fmt)
    })
    observe({
        if(isTRUE(data_import_status$ae) && isTRUE(data_import_status$enr)) {
            data_import_status$ae_enr <- TRUE
        } else {
            data_$ae_enr <- NULL
            data_import_status$ae_enr <- FALSE
        }
    })
    
    # download button for download merged AE & Enrollment data
    output$file_ae_enr_download_button <- renderUI({
        req(isTRUE(data_import_status$ae_enr))
        downloadButton('file_ae_enr_download','Download merged AE & Enrollment')
    })
    
    # download handler for merged AE & Enrollment data
    output$file_ae_enr_download <- downloadHandler(
        filename = function() {data_name$ae_enr},
        content = function(file) {
            file.copy(paste(temp_dir, data_name$ae_enr, sep = '\\'), file)
        }
    )
    
    # a tabset UI for upload data tables
    output$file_tabpanel <- renderUI({
        req(input$file_panel)
        if(input$file_panel == 'Subject-level PKPD analysis data') {
            subj_tabs <- list(id = 'file_subj_tabs')
            if(!is.null(input$file_subj_pk_param)) {
                subj_tabs <- append_alist(tabPanel(
                    'PK parameter data', uiOutput('file_subj_pk_import_msg'),
                    ternary(isTRUE(data_import_status$subj_pk_param),
                            dataTableOutput('file_subj_pk_param_table'), NULL)
                ), subj_tabs)
            }
            if(!is.null(input$file_subj_pd)) {
                subj_tabs <- append_alist(tabPanel(
                    'PD univariate data', uiOutput('file_subj_pd_import_msg'),
                    ternary(isTRUE(data_import_status$subj_pd),
                            dataTableOutput('file_pd_uni_table'), NULL)
                ), subj_tabs)
            }
            if(isTRUE(data_import_status$subj_pkpd)) {
                subj_tabs <- append_alist(tabPanel(
                    'Merged PK-PD data', dataTableOutput('file_subj_pkpd_table')
                ), subj_tabs)
            }
            do.call(tabsetPanel, subj_tabs)
        } else if(input$file_panel == 'Sample-level PKPD analysis data') {
            sample_tabs <- list(id = 'file_sample_tabs')
            if(!is.null(input$file_sample_pk_con) &&
               req(input$file_sample_pk_type) == 'PK concentration') {
                sample_tabs <- append_alist(tabPanel(
                    'PK concentration data', uiOutput('file_sample_pk_con_import_msg'),
                    ternary(isTRUE(data_import_status$sample_pk_con),
                            dataTableOutput('file_pk_con_table'), NULL)
                ), sample_tabs)
            }
            if(!is.null(input$file_sample_pk_param) &&
               req(input$file_sample_pk_type) == 'PK parameter') {
                sample_tabs <- append_alist(tabPanel(
                    'PK parameter data', uiOutput('file_sample_pk_param_import_msg'),
                    ternary(isTRUE(data_import_status$sample_pk_param),
                            dataTableOutput('file_sample_pk_param_table'), NULL)
                ), sample_tabs)
            }
            if(!is.null(input$file_sample_pd)) {
                sample_tabs <- append_alist(tabPanel(
                    'PD multivariate data', uiOutput('file_sample_pd_import_msg'),
                    ternary(isTRUE(data_import_status$sample_pd),
                            dataTableOutput('file_pd_mul_table'), NULL)
                ), sample_tabs)
            }
            if(isTRUE(data_import_status$sample_pkpd)) {
                sample_tabs <- append_alist(tabPanel(
                    'Merged PK-PD data', dataTableOutput('file_sample_pkpd_table')
                ), sample_tabs)
            }
            do.call(tabsetPanel, sample_tabs)
        } else if(input$file_panel == 'Lab and AE safety analysis data') {
            lab_ae_tabs <- list(id = 'file_lab_ae_tabs')
            if(!is.null(input$file_lab)) {
                lab_ae_tabs <- append_alist(tabPanel(
                    'Lab data', uiOutput('file_lab_import_msg'),
                    ternary(isTRUE(data_import_status$lab),
                            dataTableOutput('file_lab_table'), NULL)
                ), lab_ae_tabs)
            }
            if(!is.null(input$file_ae)) {
                lab_ae_tabs <- append_alist(tabPanel(
                    'AE safety data', uiOutput('file_ae_import_msg'),
                    ternary(isTRUE(data_import_status$ae),
                            dataTableOutput('file_ae_table'), NULL)
                ), lab_ae_tabs)
            }
            if(!is.null(input$file_enr)) {
                lab_ae_tabs <- append_alist(tabPanel(
                    'Enrollment data', uiOutput('file_enr_import_msg'),
                    ternary(isTRUE(data_import_status$enr),
                            dataTableOutput('file_enr_table'), NULL)
                ), lab_ae_tabs)
            }
            # if(isTRUE(data_import_status$ae_enr)) {
            #     lab_ae_tabs <- append_alist(tabPanel(
            #         'Merged AE & Enrollment data',
            #         dataTableOutput('file_ae_enr_table')
            #     ), lab_ae_tabs)
            # }
            do.call(tabsetPanel, lab_ae_tabs)
        }
    })
    
    # dynamically select the tab corresponding to the most recent file upload
    observe({
        input$file_subj_pk_param
        updateTabsetPanel(session, 'file_subj_tabs', 'PK parameter data')
    })
    observe({
        input$file_subj_pd
        updateTabsetPanel(session, 'file_subj_tabs', 'PD univariate data')
    })
    observeEvent(input$file_subj_pkpd_merge, {
        updateTabsetPanel(session, 'file_subj_tabs', 'Merged PK-PD data')
    })
    observe({
        input$file_sample_pk_con
        updateTabsetPanel(session, 'file_sample_tabs', 'PK concentration data')
    })
    observe({
        input$file_sample_pk_param
        updateTabsetPanel(session, 'file_sample_tabs', 'PK parameter data')
    })
    observe({
        input$file_sample_pd
        updateTabsetPanel(session, 'file_sample_tabs', 'PD multivariate data')
    })
    observeEvent(input$file_sample_pkpd_merge, {
        updateTabsetPanel(session, 'file_sample_tabs', 'Merged PK-PD data')
    })
    observe({
        input$file_lab
        updateTabsetPanel(session, 'file_lab_ae_tabs', 'Lab data')
    })
    observe({
        input$file_ae
        updateTabsetPanel(session, 'file_lab_ae_tabs', 'AE safety data')
    })
    observe({
        input$file_enr
        updateTabsetPanel(session, 'file_lab_ae_tabs', 'Enrollment data')
    })
    # observe({
    #     req(data_import_status$ae_enr)
    #     updateTabsetPanel(session, 'file_lab_ae_tabs',
    #                       'Merged AE & Enrollment data')
    # })
    # observeEvent(input$file_ae_enr_merge, {
    #     updateTabsetPanel(session, 'file_lab_ae_tabs',
    #                       'Merged AE & Enrollment data')
    # })
    
    # data table output the four uploaded files
    output$file_subj_pk_param_table <- renderDataTable({data_$subj_pk_param})
    output$file_pd_uni_table <- renderDataTable({data_$subj_pd})
    output$file_subj_pkpd_table <- renderDataTable({data_$subj_pkpd})
    output$file_pk_con_table <- renderDataTable({data_$sample_pk_con})
    output$file_sample_pk_param_table <- renderDataTable({data_$sample_pk_param})
    output$file_pd_mul_table <- renderDataTable({data_$sample_pd})
    output$file_sample_pkpd_table <- renderDataTable({data_$sample_pkpd})
    output$file_lab_table <- renderDataTable({data_$lab})
    output$file_ae_table <- renderDataTable({data_$ae})
    output$file_enr_table <- renderDataTable({data_$enr})
    # output$file_ae_enr_table <- renderDataTable({data_$ae_enr})
    
    
    #-----------------------------------------------
    # 1.	Subject-level PKPD analysis
    #-----------------------------------------------
    
    # Dynamically select which tab to open based on file uploads
    observe({
        if(isTRUE(data_import_status$subj_pk_param))
            updateCollapse(session, 'subj_panel', 'PK analysis')
        else if(isTRUE(data_import_status$subj_pd))
            updateCollapse(session, 'subj_panel', 'PD analysis')
    })
    
    #-----------------------------------------------
    # UI widgets for PK analysis
    
    # selectInput for choosing PK parameter
    output$subj_pk_param <- renderUI({
        req(data_import_status$subj_pk_param)
        data <- data_$subj_pk_param
        choices <- c('Choose' = '', sort(unique(data[[subj_pk_param_col]])))
        selectInput('subj_pk_param', 'PK parameter', choices)
    })
    
    # selectInput for choosing x variable
    output$subj_pk_x <- renderUI({
        req(data_import_status$subj_pk_param,
            req(input$subj_pk_tabs) %in% c('Summary table', 'Box plot',
                                           'Line plot'))
        data <- data_$subj_pk_param
        to_exclude <- c(subj_pk_param_col, subj_pk_estm_col)
        choices <- c('', sort(setdiff(names(data), to_exclude)))
        if(input$subj_pk_tabs %in% c('Box plot', 'Line plot'))
            label <- 'X variable'
        else if(input$subj_pk_tabs == 'Summary table')
            label <- 'Group'
        selectInput('subj_pk_x', label, choices, selected = subj_pk_dose_col)
    })
    
    # radioButtons for deciding whether X is continuous or categorical
    output$subj_pk_x_type <- renderUI({
        req(data_import_status$subj_pk_param, input$subj_pk_x,
            req(input$subj_pk_tabs) %in% c('Box plot', 'Line plot'))
        data <- data_$subj_pk_param
        type <- ifelse(is.numeric(data[[input$subj_pk_x]]),
                       'Continuous', 'Categorical')
        radioButtons('subj_pk_x_type', NULL, c('Continuous', 'Categorical'),
                     selected = type, inline = TRUE)
    })
    subj_pk_x_type <- reactiveValues(value = NULL)
    observe({
        req(data_import_status$subj_pk_param, input$subj_pk_x,
            req(input$subj_pk_tabs) %in% c('Box plot', 'Line plot'))
        data <- data_$subj_pk_param
        type <- ifelse(is.numeric(data[[input$subj_pk_x]]),
                       'Continuous', 'Categorical')
        subj_pk_x_type$value <- type
    })
    observe({
        input$subj_pk_x_type
        subj_pk_x_type$value <- input$subj_pk_x_type
    })
    
    # a selectizeInput for choosing dose level
    output$subj_pk_dose_level <- renderUI({
        req(data_import_status$subj_pk_param,
            req(input$subj_pk_tabs) %in% c('Dose proportionality'))
        data <- data_$subj_pk_param
        choices <- unique(data[[subj_pk_dose_col]])
        selected <- isolate(ternary(
            is.null(input$subj_pk_dose_level), choices,
            input$subj_pk_dose_level[input$subj_pk_dose_level %in% choices]
        ))
        selectizeInput(
            'subj_pk_dose_level', 'Dose level',
            c('Choose'='', choices), selected = selected, multiple = TRUE,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    
    # selectInput for choosing group variable
    output$subj_pk_group <- renderUI({
        req(data_import_status$subj_pk_param,
            req(input$subj_pk_tabs) %in% c('Box plot', 'Line plot',
                                           'Dose proportionality'))
        data <- data_$subj_pk_param
        choices <- names(data)[!unlist(lapply(
            data, continuity_test, threshold = group_number_levels
        ))]
        to_exclude <- c(subj_pk_param_col, subj_pk_estm_col)
        if(input$subj_pk_tabs == 'Dose proportionality')
            to_exclude <- c(to_exclude, subj_pk_dose_col)
        choices <- sort(setdiff(choices, to_exclude))
        stillSelected <- isolate(
            input$subj_pk_group[input$subj_pk_group %in% choices]
        )
        selectInput('subj_pk_group', 'Group', c('Choose' = '', choices),
                    selected = stillSelected)
    })
    
    # selectInput for choosing summary method
    output$subj_pk_summary <- renderUI({
        req(data_import_status$subj_pk_param,
            req(input$subj_pk_tabs) == 'Line plot')
        stillSelected <- isolate(
            input$subj_pk_summary[input$subj_pk_summary %in% subj_pk_summary_opts]
        )
        selectInput('subj_pk_summary', 'Summary method',
                    subj_pk_summary_opts, selected = stillSelected)
    })
    
    # check box for log scale of y
    output$subj_pk_log_y <- renderUI({
        req(data_import_status$subj_pk_param,
            req(input$subj_pk_tabs) %in% c('Box plot', 'Line plot'))
        stillSelected <- isolate(
            !is_blank(input$subj_pk_log_y, false_triggers = TRUE)
        )
        checkboxInput('subj_pk_log_y', 'Log of Y', value = stillSelected)
    })
    
    # check box for including points in the graph
    output$subj_pk_points <- renderUI({
        req(data_import_status$subj_pk_param,
            req(input$subj_pk_tabs) %in% c('Box plot', 'Line plot'))
        stillSelected <- isolate(
            !is_blank(input$subj_pk_points, false_triggers = TRUE)
        )
        checkboxInput('subj_pk_points', 'Add points', value = stillSelected)
    })
    
    # a numericInput for specifying decimal place
    output$subj_pk_decimal <- renderUI({
        req(data_import_status$subj_pk_param,
            req(input$subj_pk_tabs) == 'Summary table')
        stillSelected <- isolate(
            ifelse(is.null(input$subj_pk_decimal), 2, input$subj_pk_decimal)
        )
        numericInput('subj_pk_decimal', 'Decimal places', value = stillSelected)
    })
    
    # a textInput for specifying projected dose
    output$subj_pk_projected_dose <- renderUI({
        req(subj_pk_show$doseprop)
        textInput('subj_pk_projected_dose', 'Projected dose', value = '')
    })
    
    # a textOutput for expected PK parameter value title
    output$subj_pk_expected_pk_text <- renderUI({
        req(subj_pk_show$doseprop)
        tags$strong('Expected PK parameter value')
    })
    
    # a textOutput for expected PK parameter value
    output$subj_pk_expected_pk_value <- renderText({
        lm_model <- subj_pk_doseprop_model()
        data <- data_$subj_pk_param
        req(data, subj_pk_show$doseprop, input$subj_pk_projected_dose, lm_model)
        proj_dose <- as.numeric(input$subj_pk_projected_dose)
        if(is.na(proj_dose)) return('Invalid projected dose input')
        new_dose <- data.frame(log_dose = log(proj_dose))
        expected_pk <- predict(lm_model, new_dose)
        message <- as.character(round(exp(expected_pk), 3))
        unit <- na.omit(unique(
            data[data[[subj_pk_param_col]] %in% input$subj_pk_param,
                 subj_pk_unit_col, drop = TRUE]
        ))[1]
        if(!is_blank(unit)) {
            message <- paste(message, unit)
        }
        return(message)
    })
    
    #-----------------------------------------------
    # UI widgets for PD analysis - '1 PD analysis'
    
    # radioButtons for choosing analysis type
    output$subj_pd_analysis_type <- renderUI({
        req(data_import_status$subj_pd)
        radioButtons('subj_pd_analysis_type', 'Analysis type',
                     subj_pd_analysis_types, selected = character(0),
                     inline = TRUE)
    })
    
    # selectInput for choosing PD parameter
    output$subj_pd_dose_param <- renderUI({
        req(data_import_status$subj_pd,
            req(input$subj_pd_analysis_type == '1 PD analysis'))
        data <- data_$subj_pd
        choices <- c('Choose' = '', sort(unique(data[[subj_pd_param_col]])))
        selectInput('subj_pd_dose_param', 'PD parameter', choices)
    })
    
    # selectInput for choosing y for PD parameter
    output$subj_pd_dose_y <- renderUI({
        req(data_import_status$subj_pd,
            req(input$subj_pd_analysis_type == '1 PD analysis'))
        selectInput('subj_pd_dose_y', 'PD value', subj_pd_dose_y_types)
    })
    
    # selectInput for choosing x variable
    output$subj_pd_dose_x <- renderUI({
        req(data_import_status$subj_pd,
            req(input$subj_pd_analysis_type == '1 PD analysis'))
        data <- data_$subj_pd
        to_exclude <- c(subj_pd_param_col, subj_pd_aval_col)
        choices <- c('', sort(setdiff(names(data), to_exclude)))
        selectInput('subj_pd_dose_x', 'X variable', choices,
                    selected = subj_pd_dose_col)
    })
    
    # radioButtons for deciding whether X is continuous or categorical
    output$subj_pd_dose_x_type <- renderUI({
        req(data_import_status$subj_pd, input$subj_pd_dose_x,
            req(input$subj_pd_analysis_type == '1 PD analysis'))
        data <- data_$subj_pd
        type <- ifelse(is.numeric(data[[input$subj_pd_dose_x]]),
                       'Continuous', 'Categorical')
        radioButtons('subj_pd_dose_x_type', NULL, c('Continuous', 'Categorical'),
                     selected = type, inline = TRUE)
    })
    subj_pd_dose_x_type <- reactiveValues(value = NULL)
    observe({
        req(data_import_status$subj_pd, input$subj_pd_dose_x,
            req(input$subj_pd_analysis_type == '1 PD analysis'))
        data <- data_$subj_pd
        type <- ifelse(is.numeric(data[[input$subj_pd_dose_x]]),
                       'Continuous', 'Categorical')
        subj_pd_dose_x_type$value <- type
    })
    observe({
        input$subj_pd_dose_x_type
        subj_pd_dose_x_type$value <- input$subj_pd_dose_x_type
    })
    
    # selectInput for choosing group variable
    output$subj_pd_dose_group <- renderUI({
        req(data_import_status$subj_pd,
            req(input$subj_pd_analysis_type == '1 PD analysis'),
            req(input$subj_pd_dose_tabs) %in% c('Box plot', 'Line plot'))
        data <- data_$subj_pd
        choices <- names(data)[!unlist(lapply(
            data, continuity_test, threshold = group_number_levels
        ))]
        to_exclude <- c(subj_pd_param_col, subj_pd_aval_col)
        choices <- sort(setdiff(choices, to_exclude))
        stillSelected <- isolate(
            input$subj_pd_dose_group[input$subj_pd_dose_group %in% choices]
        )
        selectInput('subj_pd_dose_group', 'Group', c('Choose' = '', choices),
                    selected = stillSelected)
    })
    
    # selectInput for choosing summary method
    output$subj_pd_dose_summary <- renderUI({
        req(data_import_status$subj_pd, req(input$subj_pd_dose_tabs) == 'Line plot',
            req(input$subj_pd_analysis_type == '1 PD analysis'))
        stillSelected <- isolate(
            input$subj_pd_dose_summary[input$subj_pd_dose_summary %in%
                                           subj_pd_dose_summary_opts]
        )
        selectInput('subj_pd_dose_summary', 'Summary method',
                    subj_pd_dose_summary_opts, selected = stillSelected)
    })
    
    # check box for log scale of y
    output$subj_pd_dose_log_y <- renderUI({
        req(data_import_status$subj_pd,
            req(input$subj_pd_analysis_type == '1 PD analysis'),
            req(input$subj_pd_dose_tabs) %in% c('Box plot', 'Line plot'))
        stillSelected <- isolate(
            !is_blank(input$subj_pd_dose_log_y, false_triggers = TRUE)
        )
        checkboxInput('subj_pd_dose_log_y', 'Log of Y', value = stillSelected)
    })
    
    # check box for including points in the graph
    output$subj_pd_dose_points <- renderUI({
        req(data_import_status$subj_pd,
            req(input$subj_pd_analysis_type == '1 PD analysis'),
            req(input$subj_pd_dose_tabs) %in% c('Box plot', 'Line plot'))
        stillSelected <- isolate(
            !is_blank(input$subj_pd_dose_points, false_triggers = TRUE)
        )
        checkboxInput('subj_pd_dose_points', 'Add points', value = stillSelected)
    })
    
    # a numericInput for specifying decimal place
    output$subj_pd_dose_decimal <- renderUI({
        req(data_import_status$subj_pd,
            req(input$subj_pd_analysis_type == '1 PD analysis'),
            req(input$subj_pd_dose_tabs) == 'Summary table')
        stillSelected <- isolate(
            ifelse(is.null(input$subj_pd_dose_decimal), 2, input$subj_pd_dose_decimal)
        )
        numericInput('subj_pd_dose_decimal', 'Decimal places', value = stillSelected)
    })
    
    
    #-----------------------------------------------
    # UI widgets for PD analysis - '2 PD analysis'
    
    # selectInput for choosing PD parameter 1
    output$subj_pd_corr_param1 <- renderUI({
        req(data_import_status$subj_pd,
            req(input$subj_pd_analysis_type == '2 PD analysis'))
        data <- data_$subj_pd
        choices <- c('Choose' = '', sort(unique(data[[subj_pd_param_col]])))
        selectInput('subj_pd_corr_param1', 'PD parameter 1', choices)
    })
    # selectInput for choosing y for PD parameter 1
    output$subj_pd_corr_y1 <- renderUI({
        req(data_import_status$subj_pd,
            req(input$subj_pd_analysis_type == '2 PD analysis'))
        selectInput('subj_pd_corr_y1', 'PD value 1', subj_pd_dose_y_types)
    })
    
    # selectInput for choosing PD parameter 2
    output$subj_pd_corr_param2 <- renderUI({
        req(data_import_status$subj_pd,
            req(input$subj_pd_analysis_type == '2 PD analysis'))
        data <- data_$subj_pd
        choices <- c('Choose' = '', sort(unique(data[[subj_pd_param_col]])))
        selectInput('subj_pd_corr_param2', 'PD parameter 2', choices)
    })
    # selectInput for choosing y for PD parameter 2
    output$subj_pd_corr_y2 <- renderUI({
        req(data_import_status$subj_pd,
            req(input$subj_pd_analysis_type == '2 PD analysis'))
        selectInput('subj_pd_corr_y2', 'PD value 2', subj_pd_dose_y_types)
    })
    
    # selectInput for choosing x variable
    output$subj_pd_corr_x <- renderUI({
        req(data_import_status$subj_pd,
            req(input$subj_pd_analysis_type == '2 PD analysis'),
            req(input$subj_pd_corr_tabs) == 'Forest plot')
        data <- data_$subj_pd
        to_exclude <- c(subj_pd_param_col, subj_pd_aval_col)
        choices <- c('', sort(setdiff(names(data), to_exclude)))
        selectInput('subj_pd_corr_x', 'X variable', choices,
                    selected = subj_pd_dose_col)
    })
    
    # radioButtons for deciding whether X is continuous or categorical
    output$subj_pd_corr_x_type <- renderUI({
        req(data_import_status$subj_pd, input$subj_pd_corr_x,
            req(input$subj_pd_analysis_type == '2 PD analysis'),
            req(input$subj_pd_corr_tabs) == 'Forest plot')
        data <- data_$subj_pd
        type <- ifelse(is.numeric(data[[input$subj_pd_corr_x]]),
                       'Continuous', 'Categorical')
        radioButtons('subj_pd_corr_x_type', NULL, c('Continuous', 'Categorical'),
                     selected = type, inline = TRUE)
    })
    subj_pd_corr_x_type <- reactiveValues(value = NULL)
    observe({
        req(data_import_status$subj_pd, input$subj_pd_corr_x,
            req(input$subj_pd_analysis_type == '2 PD analysis'),
            req(input$subj_pd_corr_tabs) == 'Forest plot')
        data <- data_$subj_pd
        type <- ifelse(is.numeric(data[[input$subj_pd_corr_x]]),
                       'Continuous', 'Categorical')
        subj_pd_corr_x_type$value <- type
    })
    observe({
        input$subj_pd_corr_x_type
        subj_pd_corr_x_type$value <- input$subj_pd_corr_x_type
    })
    
    # selectInput for choosing group variable
    output$subj_pd_corr_group <- renderUI({
        req(data_import_status$subj_pd,
            req(input$subj_pd_analysis_type == '2 PD analysis'),
            req(input$subj_pd_corr_tabs) %in% c('Scatter plot', '2D Forest plot'))
        data <- data_$subj_pd
        choices <- names(data)
        to_exclude <- c(subj_pd_param_col, subj_pd_aval_col)
        choices <- c('Choose' = '', sort(setdiff(choices, to_exclude)))
        stillSelected <- isolate(ifelse(
            is.null(input$subj_pd_corr_group), subj_pd_dose_col,
            input$subj_pd_corr_group[input$subj_pd_corr_group %in% choices]
        ))
        selectInput('subj_pd_corr_group', 'Group', choices, stillSelected)
    })
    
    # selectInput for choosing summary method
    output$subj_pd_corr_summary <- renderUI({
        req(data_import_status$subj_pd,
            req(input$subj_pd_analysis_type == '2 PD analysis'),
            req(input$subj_pd_corr_tabs) %in% c('Forest plot', '2D Forest plot'))
        stillSelected <- isolate(
            input$subj_pd_corr_summary[input$subj_pd_corr_summary %in%
                                           subj_pd_corr_summary_opts]
        )
        selectInput('subj_pd_corr_summary', 'Summary method',
                    subj_pd_corr_summary_opts, selected = stillSelected)
    })
    
    # a radioButtons for adding reference line to the scatter plot
    output$subj_pd_corr_refline <- renderUI({
        req(data_import_status$subj_pd,
            req(input$subj_pd_analysis_type == '2 PD analysis'),
            req(input$subj_pd_corr_tabs) == 'Scatter plot')
        choices <- c('None', 'Loess', 'Linear regression')
        radioButtons('subj_pd_corr_refline', 'Add reference line',
                     choices = choices, inline = TRUE)
    })

    # a checkboxInput for log scale of PD parameter 1
    output$subj_pd_corr_log_1 <- renderUI({
        req(data_import_status$subj_pd,
            req(input$subj_pd_analysis_type == '2 PD analysis'),
            req(input$subj_pd_corr_tabs) == 'Scatter plot')
        checkboxInput('subj_pd_corr_log_1', 'Log of PD parameter 1', value = F)
    })

    # a checkboxInput for log scale of PD parameter 2
    output$subj_pd_corr_log_2 <- renderUI({
        req(data_import_status$subj_pd,
            req(input$subj_pd_analysis_type == '2 PD analysis'),
            req(input$subj_pd_corr_tabs) == 'Scatter plot')
        checkboxInput('subj_pd_corr_log_2', 'Log of PD parameter 2', value = F)
    })
    
    
    
    #-----------------------------------------------
    # UI widgets for PK - PD analysis
    
    # a selectInput for choosing PK parameter
    output$subj_pkpd_pk <- renderUI({
        req(data_$subj_pkpd)
        choices <- c(
            'Choose'='', sort(unique(data_$subj_pk_param[[subj_pk_param_col]]))
        )
        selected <- isolate(
            ifelse(is.null(input$subj_pkpd_pk), '', input$subj_pkpd_pk)
        )
        selectInput('subj_pkpd_pk', 'PK parameter', choices = choices)
    })
    observe({
        req(data_$subj_pkpd)
        data <- data_$subj_pkpd
        if(!is_blank(input$subj_pkpd_pd)) {
            cond_pk_est <- !is.na(data[[subj_pk_estm_col]])
            cond_pd_param <- data[[subj_pd_param_col]] %in% input$subj_pkpd_pd
            if(!is_blank(input$subj_pkpd_pd_y))
                cond_pd_y <- !is.na(data[[input$subj_pkpd_pd_y]])
            else cond_pd_y <- !is.na(data[[subj_pd_aval_col]])
            data <- data[cond_pk_est & cond_pd_param & cond_pd_y, ]
        }
        choices <- c('Choose' = '', sort(unique(data[[subj_pk_param_col]])))
        if(!is_blank(input$subj_pkpd_pk) && input$subj_pkpd_pk %in% choices)
            selected <- input$subj_pkpd_pk
        else selected <- ''
        updateSelectInput(
            session, 'subj_pkpd_pk', 'PK parameter', choices, selected
        )
    })
    
    # a selectInput for choosing PD parameter
    output$subj_pkpd_pd <- renderUI({
        req(data_$subj_pkpd)
        choices <- c(
            'Choose' = '', sort(unique(data_$subj_pd[[subj_pd_param_col]]))
        )
        selectInput('subj_pkpd_pd', 'PD parameter', choices = choices)
    })
    observe({
        req(data_$subj_pkpd)
        data <- data_$subj_pkpd
        if(!is_blank(input$subj_pkpd_pk)) {
            cond_pk_est <- !is.na(data[[subj_pk_estm_col]])
            cond_pk_param <- data[[subj_pk_param_col]] %in% input$subj_pkpd_pk
            if(!is_blank(input$subj_pkpd_pd_y))
                cond_pd_y <- !is.na(data[[input$subj_pkpd_pd_y]])
            else cond_pd_y <- !is.na(data[[subj_pd_aval_col]])
            data <- data[cond_pk_est & cond_pk_param & cond_pd_y, ]
        }
        choices <- c('Choose' = '', sort(unique(data[[subj_pd_param_col]])))
        if(!is_blank(input$subj_pkpd_pd) && input$subj_pkpd_pd %in% choices)
            selected <- input$subj_pkpd_pd
        else selected <- ''
        updateSelectInput(
            session, 'subj_pkpd_pd', 'PD parameter', choices, selected
        )
    })
    
    # a selectInput for choosing y for PD parameter
    output$subj_pkpd_pd_y <- renderUI({
        req(data_$subj_pkpd)
        selectInput('subj_pkpd_pd_y', 'PD value', subj_pd_dose_y_types)
    })
    # a selectInput for choosing X
    output$subj_pkpd_x <- renderUI({
        req(data_$subj_pkpd, req(input$subj_pkpd_tabs) == 'Forest plot')
        data <- data_$subj_pkpd
        choices <- names(data)
        to_exclude <- c(subj_pk_param_col, subj_pk_estm_col, subj_pd_subj_col,
                        subj_pd_param_col, subj_pd_aval_col, subj_pd_dose_col)
        choices <- c('Choose' = '', sort(setdiff(choices, to_exclude)))
        selectInput('subj_pkpd_x', 'X', choices, selected = subj_pk_dose_col)
    })
    # radioButtons for deciding whether X is continuous or categorical
    output$subj_pkpd_x_type <- renderUI({
        req(data_$subj_pkpd, input$subj_pkpd_x,
            req(input$subj_pkpd_tabs) == 'Forest plot')
        data <- data_$subj_pkpd
        type <- ifelse(is.numeric(data[[input$subj_pkpd_x]]),
                       'Continuous', 'Categorical')
        radioButtons(
            'subj_pkpd_x_type', NULL, c('Continuous', 'Categorical'),
            selected = type, inline = TRUE
        )
    })
    subj_pkpd_x_type <- reactiveValues(value = NULL)
    observe({
        req(data_$subj_pkpd, input$subj_pkpd_x,
            req(input$subj_pkpd_tabs) == 'Forest plot')
        data <- data_$subj_pkpd
        type <- ifelse(is.numeric(data[[input$subj_pkpd_x]]),
                       'Continuous', 'Categorical')
        subj_pkpd_x_type$value <- type
    })
    observe({
        input$subj_pkpd_x_type
        subj_pkpd_x_type$value <- input$subj_pkpd_x_type
    })
    # a selectInput for choosing a group variable
    output$subj_pkpd_group <- renderUI({
        req(data_$subj_pkpd,
            req(input$subj_pkpd_tabs) %in% c('Scatter plot', '2D Forest plot',
                                             'Quartile plot'))
        data <- data_$subj_pkpd
        choices <- names(data)
        to_exclude <- c(subj_pk_param_col, subj_pk_estm_col, subj_pd_subj_col,
                        subj_pd_param_col, subj_pd_aval_col, subj_pd_dose_col)
        choices <- c('Choose' = '', sort(setdiff(choices, to_exclude)))
        # stillSelected <- isolate(
        #     ifelse(
        #         is.null(input$subj_pkpd_group),
        #         ifelse(input$subj_pkpd_tabs == 'Quartile plot',
        #                c('Choose' = ''), subj_pk_dose_col),
        #         input$subj_pkpd_group[input$subj_pkpd_group %in% choices]
        #     )
        # )
        selected <- ifelse(input$subj_pkpd_tabs == 'Quartile plot',
                           c('Choose' = ''), subj_pk_dose_col)
        selectInput('subj_pkpd_group', 'Group', choices, selected)
    })
    # selectInput for choosing summary method
    output$subj_pkpd_summary <- renderUI({
        req(data_$subj_pkpd,
            req(input$subj_pkpd_tabs) %in% c('Forest plot', '2D Forest plot'))
        stillSelected <- isolate(
            input$subj_pkpd_summary[input$subj_pkpd_summary %in%
                                        subj_pkpd_summary_opts]
        )
        selectInput('subj_pkpd_summary', 'Summary method',
                    subj_pkpd_summary_opts, selected = stillSelected)
    })
    # a group radioButtons for adding reference line to the scatter plot
    output$subj_pkpd_refline <- renderUI({
        req(data_$subj_pkpd, req(input$subj_pkpd_tabs) == 'Scatter plot')
        choices <- c('None', 'Loess', 'Linear regression')
        radioButtons('subj_pkpd_refline', 'Add reference line',
                     choices = choices, inline = TRUE)
    })
    # a checkbox for log scale of PK parameter
    output$subj_pkpd_log_pk <- renderUI({
        req(data_$subj_pkpd, req(input$subj_pkpd_tabs) == 'Scatter plot')
        stillSelected <- isolate(
            !is_blank(input$subj_pkpd_log_pk, false_triggers = TRUE)
        )
        checkboxInput('subj_pkpd_log_pk', 'Log of PK', value = stillSelected)
    })
    # a checkbox for log scale of PD parameter
    output$subj_pkpd_log_pd <- renderUI({
        req(data_$subj_pkpd,
            req(input$subj_pkpd_tabs) %in% c('Scatter plot', 'Quartile plot'))
        stillSelected <- isolate(
            !is_blank(input$subj_pkpd_log_pd, false_triggers = TRUE)
        )
        checkboxInput('subj_pkpd_log_pd', 'Log of PD', value = stillSelected)
    })
    # a checkbox for toggling points
    output$subj_pkpd_points <- renderUI({
        req(data_$subj_pkpd, req(input$subj_pkpd_tabs) %in% c('Quartile plot'))
        stillSelected <- isolate(
            !is_blank(input$subj_pkpd_points, false_triggers = TRUE)
        )
        checkboxInput('subj_pkpd_points', 'Add points', value = stillSelected)
    })
    # a checkbox for toggling line
    output$subj_pkpd_line <- renderUI({
        req(data_$subj_pkpd, req(input$subj_pkpd_tabs) %in% c('Quartile plot'))
        stillSelected <- isolate(
            !is_blank(input$subj_pkpd_line, false_triggers = TRUE)
        )
        checkboxInput(
            'subj_pkpd_line',
            tags$html(
                'Add line',
                tags$style(type = 'text/css',
                           '#qsubj_pkpd_line {vertical-align: top;}'),
                shinyBS::bsButton('qsubj_pkpd_line', label = '',
                                  icon = icon('question'),
                                  style = 'info', size = 'extra-small')
            ),
            value = stillSelected
        )
    })
    observe({
        input$qsubj_pkpd_line
        shinyBS::addTooltip(
            session, 'qsubj_pkpd_line', subj_pkpd_line_htext,
            placement = 'right', trigger = 'hover',
            options = list(container = 'body')
        )
    })
    
    
    # a tabset UI for Subject-level PKPD analysis output
    output$subj_tabpanel <- renderUI({
        req(input$subj_panel)
        if(input$subj_panel == 'PK analysis') {
            req(input$subj_pk_param)
            tabsetPanel(
                tabPanel('Analysis data', dataTableOutput('subj_pk_datatable')),
                tabPanel('Summary table', uiOutput('subj_pk_table')),
                tabPanel('Box plot', uiOutput('subj_pk_boxplot_ui')),
                tabPanel('Line plot', uiOutput('subj_pk_lineplot_ui')),
                tabPanel('Dose proportionality',
                         uiOutput('subj_pk_doseprop_plot_ui')),
                id = 'subj_pk_tabs',
                selected = isolate(
                    input$subj_pk_tabs[input$subj_pk_tabs %in% subj_pk_tabnames]
                )
            )
        } else if(input$subj_panel == 'PD analysis') {
            req(input$subj_pd_analysis_type)
            if(input$subj_pd_analysis_type == '1 PD analysis') {
                req(input$subj_pd_dose_param, input$subj_pd_dose_y)
                tabsetPanel(
                    tabPanel('Analysis data', dataTableOutput('subj_pd_dose_datatable')),
                    tabPanel('Summary table', uiOutput('subj_pd_dose_table')),
                    tabPanel('Box plot', uiOutput('subj_pd_dose_boxplot_ui')),
                    tabPanel('Line plot', uiOutput('subj_pd_dose_lineplot_ui')),
                    id = 'subj_pd_dose_tabs',
                    selected = isolate(
                        input$subj_pd_dose_tabs[input$subj_pd_dose_tabs %in%
                                                    subj_pd_dose_tabnames]
                    )
                )
            } else if(input$subj_pd_analysis_type == '2 PD analysis') {
                req(input$subj_pd_corr_param1, input$subj_pd_corr_param2,
                    input$subj_pd_corr_y1, input$subj_pd_corr_y2)
                tabsetPanel(
                    tabPanel('Scatter plot', uiOutput('subj_pd_corr_scatter_ui')),
                    tabPanel('Forest plot', uiOutput('subj_pd_corr_forest_ui')),
                    tabPanel('2D Forest plot', uiOutput('subj_pd_corr_2dforest_ui')),
                    id = 'subj_pd_corr_tabs',
                    selected = isolate(
                        input$subj_pd_corr_tabs[input$subj_pd_corr_tabs %in%
                                                    subj_pd_corr_tabnames]
                    )
                )
            }
        } else if(input$subj_panel == 'PK - PD analysis') {
            req(input$subj_pkpd_pk, input$subj_pkpd_pd, input$subj_pkpd_pd_y)
            tabsetPanel(
                tabPanel('Scatter plot', uiOutput('subj_pkpd_scatter_ui')),
                tabPanel('Forest plot', uiOutput('subj_pkpd_forest_ui')),
                tabPanel('2D Forest plot', uiOutput('subj_pkpd_2dforest_ui')),
                tabPanel('Quartile plot', uiOutput('subj_pkpd_quartile_ui')),
                id = 'subj_pkpd_tabs',
                selected = isolate(
                    input$subj_pkpd_tabs[input$subj_pkpd_tabs %in%
                                             subj_pkpd_tabnames]
                )
            )
        }
    })
    
    # determine whether plot or table is shown
    subj_pk_show <- reactiveValues(
        anatable = FALSE, sumtable = FALSE, boxplot = FALSE, lineplot = FALSE,
        doseprop = FALSE
    )
    observe({
        if(!is.null(input$subj_panel) &&
           input$subj_panel == 'PK analysis' &&
           isTRUE(data_import_status$subj_pk_param) &&
           !is_blank(input$subj_pk_param) &&
           !is.null(input$subj_pk_tabs)) {
            if(input$subj_pk_tabs == 'Analysis data') {
                subj_pk_show$anatable <- TRUE
                subj_pk_show$sumtable <- FALSE
                subj_pk_show$boxplot <- FALSE
                subj_pk_show$lineplot <- FALSE
                subj_pk_show$doseprop <- FALSE
            } else {
                if(!is.null(input$subj_pk_decimal) &&
                   input$subj_pk_tabs == 'Summary table') {
                    subj_pk_show$anatable <- FALSE
                    subj_pk_show$sumtable <- TRUE
                    subj_pk_show$boxplot <- FALSE
                    subj_pk_show$lineplot <- FALSE
                    subj_pk_show$doseprop <- FALSE
                } else if(!is_blank(input$subj_pk_x) &&
                          !is.null(input$subj_pk_group) &&
                          !is.null(input$subj_pk_log_y) &&
                          !is.null(input$subj_pk_points) &&
                          input$subj_pk_tabs == 'Box plot') {
                    subj_pk_show$anatable <- FALSE
                    subj_pk_show$sumtable <- FALSE
                    subj_pk_show$boxplot <- TRUE
                    subj_pk_show$lineplot <- FALSE
                    subj_pk_show$doseprop <- FALSE
                } else if(!is_blank(input$subj_pk_x) &&
                          !is.null(input$subj_pk_summary) &&
                          !is.null(input$subj_pk_group) &&
                          !is.null(input$subj_pk_log_y) &&
                          !is.null(input$subj_pk_points)&&
                          input$subj_pk_tabs == 'Line plot') {
                    subj_pk_show$anatable <- FALSE
                    subj_pk_show$sumtable <- FALSE
                    subj_pk_show$boxplot <- FALSE
                    subj_pk_show$lineplot <- TRUE
                    subj_pk_show$doseprop <- FALSE
                } else if(!is_blank(input$subj_pk_dose_level) &&
                          !is.null(input$subj_pk_group)) {
                    subj_pk_show$anatable <- FALSE
                    subj_pk_show$sumtable <- FALSE
                    subj_pk_show$boxplot <- FALSE
                    subj_pk_show$lineplot <- FALSE
                    subj_pk_show$doseprop <- TRUE
                }
            }
        } else {
            subj_pk_show$anatable <- FALSE
            subj_pk_show$sumtable <- FALSE
            subj_pk_show$boxplot <- FALSE
            subj_pk_show$lineplot <- FALSE
            subj_pk_show$doseprop <- FALSE
        }
    })
    subj_pd_dose_show <- reactiveValues(
        anatable = FALSE, sumtable = FALSE, boxplot = FALSE, lineplot = FALSE
    )
    observe({
        if(!is.null(input$subj_panel) &&
           input$subj_panel == 'PD analysis' &&
           !is.null(input$subj_pd_analysis_type) &&
           input$subj_pd_analysis_type == '1 PD analysis' &&
           isTRUE(data_import_status$subj_pd) &&
           !is_blank(input$subj_pd_dose_param) &&
           !is_blank(input$subj_pd_dose_y) &&
           !is.null(input$subj_pd_dose_tabs)) {
            if(input$subj_pd_dose_tabs == 'Analysis data') {
                subj_pd_dose_show$anatable <- TRUE
                subj_pd_dose_show$sumtable <- FALSE
                subj_pd_dose_show$boxplot <- FALSE
                subj_pd_dose_show$lineplot <- FALSE
            } else {
                if(!is.null(input$subj_pd_dose_decimal) &&
                   input$subj_pd_dose_tabs == 'Summary table') {
                    subj_pd_dose_show$anatable <- FALSE
                    subj_pd_dose_show$sumtable <- TRUE
                    subj_pd_dose_show$boxplot <- FALSE
                    subj_pd_dose_show$lineplot <- FALSE
                } else if(!is_blank(input$subj_pd_dose_x) &&
                          !is_blank(subj_pd_dose_x_type$value) &&
                          !is.null(input$subj_pd_dose_group) &&
                          !is.null(input$subj_pd_dose_log_y) &&
                          !is.null(input$subj_pd_dose_points) &&
                          input$subj_pd_dose_tabs == 'Box plot') {
                    subj_pd_dose_show$anatable <- FALSE
                    subj_pd_dose_show$sumtable <- FALSE
                    subj_pd_dose_show$boxplot <- TRUE
                    subj_pd_dose_show$lineplot <- FALSE
                } else if(!is_blank(input$subj_pd_dose_x) &&
                          !is_blank(subj_pd_dose_x_type$value) &&
                          !is.null(input$subj_pd_dose_summary) &&
                          !is.null(input$subj_pd_dose_group) &&
                          !is.null(input$subj_pd_dose_log_y) &&
                          !is.null(input$subj_pd_dose_points)&&
                          input$subj_pd_dose_tabs == 'Line plot') {
                    subj_pd_dose_show$anatable <- FALSE
                    subj_pd_dose_show$sumtable <- FALSE
                    subj_pd_dose_show$boxplot <- FALSE
                    subj_pd_dose_show$lineplot <- TRUE
                }
            }
        } else {
            subj_pd_dose_show$anatable <- FALSE
            subj_pd_dose_show$sumtable <- FALSE
            subj_pd_dose_show$boxplot <- FALSE
            subj_pd_dose_show$lineplot <- FALSE
        }
    })
    subj_pd_corr_show <- reactiveValues(
        scatterplot = FALSE, forestplot = FALSE, forestplot2d = FALSE
    )
    observe({
        if(!is.null(input$subj_panel) &&
           input$subj_panel == 'PD analysis' &&
           !is.null(input$subj_pd_analysis_type) &&
           input$subj_pd_analysis_type == '2 PD analysis' &&
           isTRUE(data_import_status$subj_pd) &&
           !is_blank(input$subj_pd_corr_param1) &&
           !is_blank(input$subj_pd_corr_param2) &&
           !is_blank(input$subj_pd_corr_y1) &&
           !is_blank(input$subj_pd_corr_y2) &&
           !is.null(input$subj_pd_corr_tabs)) {
            if(!is.null(input$subj_pd_corr_group) &&
               !is.null(input$subj_pd_corr_refline) &&
               !is.null(input$subj_pd_corr_log_1) &&
               !is.null(input$subj_pd_corr_log_2) &&
               input$subj_pd_corr_tabs == 'Scatter plot') {
                subj_pd_corr_show$scatterplot <- TRUE
                subj_pd_corr_show$forestplot <- FALSE
                subj_pd_corr_show$forestplot2d <- FALSE
            } else if(!is_blank(input$subj_pd_corr_x) &&
                      !is_blank(subj_pd_corr_x_type$value) &&
                      !is_blank(input$subj_pd_corr_summary) &&
                      input$subj_pd_corr_tabs == 'Forest plot') {
                subj_pd_corr_show$scatterplot <- FALSE
                subj_pd_corr_show$forestplot <- TRUE
                subj_pd_corr_show$forestplot2d <- FALSE
            } else if(!is_blank(input$subj_pd_corr_summary) &&
                      !is.null(input$subj_pd_corr_group) &&
                      input$subj_pd_corr_tabs == '2D Forest plot') {
                subj_pd_corr_show$scatterplot <- FALSE
                subj_pd_corr_show$forestplot <- FALSE
                subj_pd_corr_show$forestplot2d <- TRUE
            }
        } else {
            subj_pd_corr_show$scatterplot <- FALSE
            subj_pd_corr_show$forestplot <- FALSE
            subj_pd_corr_show$forestplot2d <- FALSE
        }
    })
    subj_pkpd_show <- reactiveValues(
        scatterplot = FALSE, forestplot = FALSE, forestplot2d = FALSE,
        quartileplot = FALSE
    )
    observe({
        if(!is.null(input$subj_panel) &&
           input$subj_panel == 'PK - PD analysis' &&
           !is_blank(input$subj_pkpd_pk) &&
           !is_blank(input$subj_pkpd_pd) &&
           !is_blank(input$subj_pkpd_pd_y) &&
           !is.null(input$subj_pkpd_tabs)) {
            if(!is.null(input$subj_pkpd_group) &&
               !is.null(input$subj_pkpd_log_pk) &&
               !is.null(input$subj_pkpd_log_pd) &&
               input$subj_pkpd_tabs == 'Scatter plot') {
                subj_pkpd_show$scatterplot <- TRUE
                subj_pkpd_show$forestplot <- FALSE
                subj_pkpd_show$forestplot2d <- FALSE
                subj_pkpd_show$quartileplot <- FALSE
            } else if(!is_blank(input$subj_pkpd_x) &&
                      !is_blank(subj_pkpd_x_type$value) &&
                      !is_blank(input$subj_pkpd_summary) &&
                      input$subj_pkpd_tabs == 'Forest plot') {
                subj_pkpd_show$scatterplot <- FALSE
                subj_pkpd_show$forestplot <- TRUE
                subj_pkpd_show$forestplot2d <- FALSE
                subj_pkpd_show$quartileplot <- FALSE
            } else if(!is_blank(input$subj_pkpd_summary) &&
                      !is.null(input$subj_pkpd_group) &&
                      input$subj_pkpd_tabs == '2D Forest plot') {
                subj_pkpd_show$scatterplot <- FALSE
                subj_pkpd_show$forestplot <- FALSE
                subj_pkpd_show$forestplot2d <- TRUE
                subj_pkpd_show$quartileplot <- FALSE
            } else if(!is.null(input$subj_pkpd_points) &&
                      !is.null(input$subj_pkpd_log_pd) &&
                      !is.null(input$subj_pkpd_group) &&
                      input$subj_pkpd_tabs == 'Quartile plot') {
                subj_pkpd_show$scatterplot <- FALSE
                subj_pkpd_show$forestplot <- FALSE
                subj_pkpd_show$forestplot2d <- FALSE
                subj_pkpd_show$quartileplot <- TRUE
            }
        } else {
            subj_pkpd_show$scatterplot <- FALSE
            subj_pkpd_show$forestplot <- FALSE
            subj_pkpd_show$forestplot2d <- FALSE
            subj_pkpd_show$quartileplot <- FALSE
        }
    })
    subj_show <- reactiveValues(plot = FALSE, table = FALSE)
    observe({
        if(any(isTRUE(subj_pk_show$boxplot),
               isTRUE(subj_pk_show$lineplot),
               isTRUE(subj_pd_dose_show$boxplot),
               isTRUE(subj_pd_dose_show$lineplot),
               isTRUE(subj_pd_corr_show$scatterplot),
               isTRUE(subj_pd_corr_show$forestplot),
               isTRUE(subj_pd_corr_show$forestplot2d),
               isTRUE(subj_pkpd_show$scatterplot),
               isTRUE(subj_pkpd_show$forestplot),
               isTRUE(subj_pkpd_show$forestplot2d))) {
            subj_show$plot <- TRUE
        } else {
            subj_show$plot <- FALSE
        }
    })
    observe({
        if(any(isTRUE(subj_pk_show$anatable),
               isTRUE(subj_pk_show$sumtable),
               isTRUE(subj_pd_dose_show$anatable),
               isTRUE(subj_pd_dose_show$sumtable))) {
            subj_show$table <- TRUE
        } else {
            subj_show$table <- FALSE
        }
    })
    
    # # action button for "add to download list"
    # download_list <- reactiveValues(
    #     
    # )
    # output$subj_add_to_download <- renderUI({
    #     req(isTrue(subj_show$plot) || isTRUE(subj_show$table))
    #     actionButton('subj_add_to_download', 'Add to download list',
    #                  icon = icon('cloud-upload'))
    # })
    # observeEvent(input$subj_add_to_download, {
    #     owd <- setwd(temp_dir)
    #     on.exit(setwd(owd))
    #     if(isTRUE(subj_show$plot)) {
    #         req(input$subj_plot_height, input$subj_plot_width)
    #         formats <- input$subj_plot_format
    #         if(isTRUE(subj_show$boxplot)) {
    #             output_names <- paste(paste(
    #                 'g-boxplot', length(download_list$boxplot) + 1,
    #                 time_stamp, sep = '-'
    #             ), formats, sep = '.')
    #             download_list$boxplot <- c(
    #                 download_list$boxplot, output_names
    #             )
    #             if(input$subj_panel == 'PK analysis')
    #                 plot_ <- subj_pk_boxplot()
    #             else if(input$subj_panel == 'PD analysis')
    #                 plot_ <- subj_pd_dose_boxplot()
    #         }
    #         else if(isTRUE(subj_show$lineplot)) {
    #             output_names <- paste(paste(
    #                 'g-lineplot', length(download_list$lineplot) + 1,
    #                 time_stamp, sep = '-'
    #             ), formats, sep = '.')
    #             download_list$lineplot <- c(
    #                 download_list$lineplot, output_names
    #             )
    #             if(input$subj_panel == 'PK analysis')
    #                 plot_ <- subj_pk_lineplot()
    #         }
    #         for(name_ in output_names) {
    #             ggsave(
    #                 name_, plot_, units = 'in', height = input$subj_plot_height,
    #                 width = input$subj_plot_width, dpi = dpi_default
    #             )
    #         }
    #     } else if(isTRUE(subj_show$table)){
    #         output_names <- paste(paste(
    #             't', length(download_list$table) + 1, time_stamp, sep = '-'
    #         ), 'pdf', sep = '.')
    #         file_html <- tempfile('table', fileext = '.html')
    #         htmltbl <- subj_pk_table()
    #         htmlpage <- html_page(htmltbl)
    #         cat(htmlpage, file = file_html)
    #         convert_command <- paste0(
    #             'wkhtmltopdf -q ', 
    #             file_html, ' ', output_names
    #         )
    #         system(convert_command)
    #         download_list$table <- c(download_list$table, output_names)
    #     }
    # })
    # 
    # # download saved TFL's button
    # output$subj_download_button <- renderUI({
    #     req(any(sapply(c(download_list$boxplot, download_list$lineplot,
    #                      download_list$table), length) > 0))
    #     downloadButton('subj_download', "Download saved TFL's")
    # })
    # 
    # # download plot handler
    # output$subj_download <- downloadHandler(
    #     filename = function() {paste('output', 'zip', sep = '.')},
    #     content = function(file) {
    #         owd <- setwd(temp_dir)
    #         on.exit(setwd(owd))
    #         system2(
    #             'zip', args = shQuote(
    #                 c('-r9X', file, download_list$boxplot,
    #                   download_list$lineplot, download_list$table)
    #             ), stdout = FALSE
    #         )
    #     }
    # )
    
    # group checkbox input for choosing download plot format
    output$subj_plot_format <- renderUI({
        req(subj_show$plot)
        checkboxGroupInput('subj_plot_format', 'Plot format',
                           c('pdf', 'png', 'jpeg'), 'pdf', inline = TRUE)
    })
    
    # numeric input for choosing download plot height
    output$subj_pk_boxplot_height <- renderUI({
        req(subj_pk_show$boxplot)
        numericInput('subj_pk_boxplot_height', 'Plot height (inches)',
                     value = 4)
    })
    output$subj_pk_lineplot_height <- renderUI({
        req(subj_pk_show$lineplot)
        numericInput('subj_pk_lineplot_height', 'Plot height (inches)',
                     value = 4)
    })
    output$subj_pd_dose_boxplot_height <- renderUI({
        req(subj_pd_dose_show$boxplot)
        numericInput('subj_pd_dose_boxplot_height', 'Plot height (inches)',
                     value = 4)
    })
    output$subj_pd_dose_lineplot_height <- renderUI({
        req(subj_pd_dose_show$lineplot)
        numericInput('subj_pd_dose_lineplot_height', 'Plot height (inches)',
                     value = 4)
    })
    output$subj_pd_corr_scatterplot_height <- renderUI({
        req(subj_pd_corr_show$scatterplot)
        numericInput('subj_pd_corr_scatterplot_height', 'Plot height (inches)',
                     value = 4)
    })
    output$subj_pd_corr_forestplot_height <- renderUI({
        req(subj_pd_corr_show$forestplot)
        numericInput('subj_pd_corr_forestplot_height', 'Plot height (inches)',
                     value = 4)
    })
    output$subj_pd_corr_forestplot2d_height <- renderUI({
        req(subj_pd_corr_show$forestplot2d)
        numericInput('subj_pd_corr_forestplot2d_height', 'Plot height (inches)',
                     value = 4)
    })
    output$subj_pkpd_scatterplot_height <- renderUI({
        req(subj_pkpd_show$scatterplot)
        numericInput('subj_pkpd_scatterplot_height', 'Plot height (inches)',
                     value = 4)
    })
    output$subj_pkpd_forestplot_height <- renderUI({
        req(subj_pkpd_show$forestplot)
        numericInput('subj_pkpd_forestplot_height', 'Plot height (inches)',
                     value = 4)
    })
    output$subj_pkpd_forestplot2d_height <- renderUI({
        req(subj_pkpd_show$forestplot2d)
        numericInput('subj_pkpd_forestplot2d_height', 'Plot height (inches)',
                     value = 4)
    })

    # numeric input for choosing download plot width
    output$subj_pk_boxplot_width <- renderUI({
        req(subj_pk_show$boxplot)
        numericInput('subj_pk_boxplot_width', 'Plot width (inches)', value = 4)
    })
    output$subj_pk_lineplot_width <- renderUI({
        req(subj_pk_show$lineplot)
        numericInput('subj_pk_lineplot_width', 'Plot width (inches)', value = 4)
    })
    output$subj_pd_dose_boxplot_width <- renderUI({
        req(subj_pd_dose_show$boxplot)
        numericInput('subj_pd_dose_boxplot_width', 'Plot width (inches)', value = 4)
    })
    output$subj_pd_dose_lineplot_width <- renderUI({
        req(subj_pd_dose_show$lineplot)
        numericInput('subj_pd_dose_lineplot_width', 'Plot width (inches)', value = 4)
    })
    output$subj_pd_corr_scatterplot_height <- renderUI({
        req(subj_pd_corr_show$scatterplot)
        numericInput('subj_pd_corr_scatterplot_width', 'Plot width (inches)', value = 4)
    })
    output$subj_pd_corr_forestplot_width <- renderUI({
        req(subj_pd_corr_show$forestplot)
        numericInput('subj_pd_corr_forestplot_width', 'Plot width (inches)', value = 4)
    })
    output$subj_pd_corr_forestplot2d_width <- renderUI({
        req(subj_pd_corr_show$forestplot2d)
        numericInput('subj_pd_corr_forestplot2d_width', 'Plot width (inches)', value = 4)
    })
    output$subj_pkpd_scatterplot_width <- renderUI({
        req(subj_pkpd_show$scatterplot)
        numericInput('subj_pkpd_scatterplot_width', 'Plot width (inches)', value = 4)
    })
    output$subj_pkpd_forestplot_width <- renderUI({
        req(subj_pkpd_show$forestplot)
        numericInput('subj_pkpd_forestplot_width', 'Plot width (inches)', value = 4)
    })
    output$subj_pkpd_forestplot2d_width <- renderUI({
        req(subj_pkpd_show$forestplot2d)
        numericInput('subj_pkpd_forestplot2d_width', 'Plot width (inches)', value = 4)
    })
    
    # text area input for specifying x-axis label
    output$subj_pk_boxplot_xlab <- renderUI({
        req(subj_pk_show$boxplot)
        value <- input$subj_pk_x
        textareaInput('subj_pk_boxplot_xlab', 'X-axis label', value = value)
    })
    subj_pk_boxplot_xlab <- reactiveValues(value = NULL)
    observe({
        req(subj_pk_show$boxplot)
        subj_pk_boxplot_xlab$value <- input$subj_pk_x
    })
    observe({
        input$subj_pk_boxplot_xlab
        subj_pk_boxplot_xlab$value <- input$subj_pk_boxplot_xlab
    })
    
    output$subj_pk_lineplot_xlab <- renderUI({
        req(subj_pk_show$lineplot)
        value <- input$subj_pk_x
        textareaInput('subj_pk_lineplot_xlab', 'X-axis label', value = value)
    })
    subj_pk_lineplot_xlab <- reactiveValues(value = NULL)
    observe({
        req(subj_pk_show$lineplot)
        subj_pk_lineplot_xlab$value <- input$subj_pk_x
    })
    observe({
        input$subj_pk_lineplot_xlab
        subj_pk_lineplot_xlab$value <- input$subj_pk_lineplot_xlab
    })
    
    output$subj_pk_doseprop_xlab <- renderUI({
        req(subj_pk_show$doseprop)
        value <- 'log(Dose)'
        textareaInput('subj_pk_doseprop_xlab', 'X-axis label', value = value)
    })
    subj_pk_doseprop_xlab <- reactiveValues(value = 'log(Dose)')
    observe({
        input$subj_pk_doseprop_xlab
        subj_pk_doseprop_xlab$value <- input$subj_pk_doseprop_xlab
    })
    
    output$subj_pd_dose_boxplot_xlab <- renderUI({
        req(subj_pd_dose_show$boxplot)
        value <- input$subj_pd_dose_x
        textareaInput('subj_pd_dose_boxplot_xlab', 'X-axis label', value = value)
    })
    subj_pd_dose_boxplot_xlab <- reactiveValues(value = NULL)
    observe({
        req(subj_pd_dose_show$boxplot)
        subj_pd_dose_boxplot_xlab$value <- input$subj_pd_dose_x
    })
    observe({
        input$subj_pd_dose_boxplot_xlab
        subj_pd_dose_boxplot_xlab$value <- input$subj_pd_dose_boxplot_xlab
    })
    
    output$subj_pd_dose_lineplot_xlab <- renderUI({
        req(subj_pd_dose_show$lineplot)
        value <- input$subj_pd_dose_x
        textareaInput('subj_pd_dose_lineplot_xlab', 'X-axis label', value = value)
    })
    subj_pd_dose_lineplot_xlab <- reactiveValues(value = NULL)
    observe({
        req(subj_pd_dose_show$lineplot)
        subj_pd_dose_lineplot_xlab$value <- input$subj_pd_dose_x
    })
    observe({
        input$subj_pd_dose_lineplot_xlab
        subj_pd_dose_lineplot_xlab$value <- input$subj_pd_dose_lineplot_xlab
    })
    
    output$subj_pd_corr_scatterplot_xlab <- renderUI({
        req(subj_pd_corr_show$scatterplot)
        value <- input$subj_pd_corr_param1
        textareaInput('subj_pd_corr_scatterplot_xlab', 'X-axis label', value = value)
    })
    subj_pd_corr_scatterplot_xlab <- reactiveValues(value = NULL)
    observe({
        req(subj_pd_corr_show$scatterplot)
        subj_pd_corr_scatterplot_xlab$value <- input$subj_pd_corr_param1
    })
    observe({
        input$subj_pd_corr_scatterplot_xlab
        subj_pd_corr_scatterplot_xlab$value <- input$subj_pd_corr_scatterplot_xlab
    })
    
    output$subj_pd_corr_forestplot_xlab <- renderUI({
        req(subj_pd_corr_show$forestplot)
        value <- input$subj_pd_corr_x
        textareaInput('subj_pd_corr_forestplot_xlab', 'X-axis label', value = value)
    })
    subj_pd_corr_forestplot_xlab <- reactiveValues(value = NULL)
    observe({
        req(subj_pd_corr_show$forestplot)
        subj_pd_corr_forestplot_xlab$value <- input$subj_pd_corr_x
    })
    observe({
        input$subj_pd_corr_forestplot_xlab
        subj_pd_corr_forestplot_xlab$value <- input$subj_pd_corr_forestplot_xlab
    })
    
    output$subj_pd_corr_forestplot2d_xlab <- renderUI({
        req(subj_pd_corr_show$forestplot2d)
        value <- input$subj_pd_corr_param1
        textareaInput('subj_pd_corr_forestplot2d_xlab', 'X-axis label', value = value)
    })
    subj_pd_corr_forestplot2d_xlab <- reactiveValues(value = NULL)
    observe({
        req(subj_pd_corr_show$forestplot)
        subj_pd_corr_forestplot2d_xlab$value <- input$subj_pd_corr_param1
    })
    observe({
        input$subj_pd_corr_forestplot2d_xlab
        subj_pd_corr_forestplot2d_xlab$value <- input$subj_pd_corr_forestplot2d_xlab
    })
    
    output$subj_pkpd_scatterplot_xlab <- renderUI({
        req(subj_pkpd_show$scatterplot)
        value <- input$subj_pkpd_pk
        textareaInput('subj_pkpd_scatterplot_xlab', 'X-axis label', value = value)
    })
    subj_pkpd_scatterplot_xlab <- reactiveValues(value = NULL)
    observe({
        req(subj_pkpd_show$scatterplot)
        subj_pkpd_scatterplot_xlab$value <- input$subj_pkpd_pk
    })
    observe({
        input$subj_pkpd_scatterplot_xlab
        subj_pkpd_scatterplot_xlab$value <- input$subj_pkpd_scatterplot_xlab
    })
    
    output$subj_pkpd_forestplot_xlab <- renderUI({
        req(subj_pkpd_show$forestplot)
        value <- input$subj_pkpd_x
        textareaInput('subj_pkpd_forestplot_xlab', 'X-axis label', value = value)
    })
    subj_pkpd_forestplot_xlab <- reactiveValues(value = NULL)
    observe({
        req(subj_pkpd_show$forestplot)
        subj_pkpd_forestplot_xlab$value <- input$subj_pkpd_x
    })
    observe({
        input$subj_pkpd_forestplot_xlab
        subj_pkpd_forestplot_xlab$value <- input$subj_pkpd_forestplot_xlab
    })
    
    output$subj_pkpd_forestplot2d_xlab <- renderUI({
        req(subj_pkpd_show$forestplot2d)
        value <- input$subj_pkpd_pk
        textareaInput('subj_pkpd_forestplot2d_xlab', 'X-axis label',
                      value = value)
    })
    subj_pkpd_forestplot2d_xlab <- reactiveValues(value = NULL)
    observe({
        req(subj_pkpd_show$forestplot2d)
        subj_pkpd_forestplot2d_xlab$value <- input$subj_pkpd_pk
    })
    observe({
        input$subj_pkpd_forestplot2d_xlab
        subj_pkpd_forestplot2d_xlab$value <- input$subj_pkpd_forestplot2d_xlab
    })
    
    output$subj_pkpd_quartileplot_xlab <- renderUI({
        req(subj_pkpd_show$quartileplot)
        value <- input$subj_pkpd_pk
        textareaInput('subj_pkpd_quartileplot_xlab', 'X-axis label',
                      value = value)
    })
    subj_pkpd_quartileplot_xlab <- reactiveValues(value = NULL)
    observe({
        req(subj_pkpd_show$quartileplot)
        subj_pkpd_quartileplot_xlab$value <- input$subj_pkpd_pk
    })
    observe({
        input$subj_pkpd_quartileplot_xlab
        subj_pkpd_quartileplot_xlab$value <- input$subj_pkpd_quartileplot_xlab
    })
    
    # text area input for specifying y-axis label
    output$subj_pk_boxplot_ylab <- renderUI({
        req(subj_pk_show$boxplot)
        pk_param_unit <- unique(data_subj_pk()[[subj_pk_unit_col]])
        value <- stringr::str_trim(paste(
            input$subj_pk_param, add_parenthesis(pk_param_unit)
        ))
        textareaInput('subj_pk_boxplot_ylab', 'Y-axis label', value = value)
    })
    subj_pk_boxplot_ylab <- reactiveValues(value = NULL)
    observe({
        req(subj_pk_show$boxplot)
        pk_param_unit <- unique(data_subj_pk()[[subj_pk_unit_col]])
        subj_pk_boxplot_ylab$value <- stringr::str_trim(paste(
            input$subj_pk_param, add_parenthesis(pk_param_unit)
        ))
    })
    observe({
        input$subj_pk_boxplot_ylab
        subj_pk_boxplot_ylab$value <- input$subj_pk_boxplot_ylab
    })
    
    output$subj_pk_lineplot_ylab <- renderUI({
        req(subj_pk_show$lineplot)
        pk_param_unit <- unique(data_subj_pk()[[subj_pk_unit_col]])
        value <- stringr::str_trim(paste(
            input$subj_pk_param, add_parenthesis(pk_param_unit)
        ))
        textareaInput('subj_pk_lineplot_ylab', 'Y-axis label', value = value)
    })
    subj_pk_lineplot_ylab <- reactiveValues(value = NULL)
    observe({
        req(subj_pk_show$lineplot)
        pk_param_unit <- unique(data_subj_pk()[[subj_pk_unit_col]])
        subj_pk_lineplot_ylab$value <- stringr::str_trim(paste(
            input$subj_pk_param, add_parenthesis(pk_param_unit)
        ))
    })
    observe({
        input$subj_pk_lineplot_ylab
        subj_pk_lineplot_ylab$value <- input$subj_pk_lineplot_ylab
    })
    
    output$subj_pk_doseprop_ylab <- renderUI({
        req(subj_pk_show$doseprop)
        value <- paste0('log(', input$subj_pk_param, ')')
        textareaInput('subj_pk_doseprop_ylab', 'Y-axis label', value = value)
    })
    subj_pk_doseprop_ylab <- reactiveValues(value = NULL)
    observe({
        req(subj_pk_show$doseprop)
        value <- paste0('log(', input$subj_pk_param, ')')
        subj_pk_doseprop_ylab$value <- value
    })
    observe({
        input$subj_pk_doseprop_ylab
        subj_pk_doseprop_ylab$value <- input$subj_pk_doseprop_ylab
    })
    
    output$subj_pd_dose_boxplot_ylab <- renderUI({
        req(subj_pd_dose_show$boxplot)
        value <- input$subj_pd_dose_param
        textareaInput('subj_pd_dose_boxplot_ylab', 'Y-axis label', value = value)
    })
    subj_pd_dose_boxplot_ylab <- reactiveValues(value = NULL)
    observe({
        req(subj_pd_dose_show$boxplot)
        subj_pd_dose_boxplot_ylab$value <- input$subj_pd_dose_param
    })
    observe({
        input$subj_pd_dose_boxplot_ylab
        subj_pd_dose_boxplot_ylab$value <- input$subj_pd_dose_boxplot_ylab
    })
    
    output$subj_pd_dose_lineplot_ylab <- renderUI({
        req(subj_pd_dose_show$lineplot)
        value <- input$subj_pd_dose_param
        textareaInput('subj_pd_dose_lineplot_ylab', 'Y-axis label', value = value)
    })
    subj_pd_dose_lineplot_ylab <- reactiveValues(value = NULL)
    observe({
        req(subj_pd_dose_show$lineplot)
        subj_pd_dose_lineplot_ylab$value <- input$subj_pd_dose_param
    })
    observe({
        input$subj_pd_dose_lineplot_ylab
        subj_pd_dose_lineplot_ylab$value <- input$subj_pd_dose_lineplot_ylab
    })
    
    output$subj_pd_corr_scatterplot_ylab <- renderUI({
        req(subj_pd_corr_show$scatterplot)
        value <- input$subj_pd_corr_param2
        textareaInput('subj_pd_corr_scatterplot_ylab', 'Y-axis label', value = value)
    })
    subj_pd_corr_scatterplot_ylab <- reactiveValues(value = NULL)
    observe({
        req(subj_pd_corr_show$scatterplot)
        subj_pd_corr_scatterplot_ylab$value <- input$subj_pd_corr_param2
    })
    observe({
        input$subj_pd_corr_scatterplot_ylab
        subj_pd_corr_scatterplot_ylab$value <- input$subj_pd_corr_scatterplot_ylab
    })
    
    output$subj_pd_corr_forestplot_ylab1 <- renderUI({
        req(subj_pd_corr_show$forestplot)
        value <- input$subj_pd_corr_param1
        textareaInput('subj_pd_corr_forestplot_ylab1', ' Left Y-axis label',
                      value = value)
    })
    subj_pd_corr_forestplot_ylab1 <- reactiveValues(value = NULL)
    observe({
        req(subj_pd_corr_show$forestplot)
        subj_pd_corr_forestplot_ylab1$value <- input$subj_pd_corr_param1
    })
    observe({
        input$subj_pd_corr_forestplot_ylab1
        subj_pd_corr_forestplot_ylab1$value <- input$subj_pd_corr_forestplot_ylab1
    })
    
    output$subj_pd_corr_forestplot_ylab2 <- renderUI({
        req(subj_pd_corr_show$forestplot)
        value <- input$subj_pd_corr_param2
        textareaInput('subj_pd_corr_forestplot_ylab2', 'Right Y-axis label',
                      value = value)
    })
    subj_pd_corr_forestplot_ylab2 <- reactiveValues(value = NULL)
    observe({
        req(subj_pd_corr_show$forestplot)
        subj_pd_corr_forestplot_ylab2$value <- input$subj_pd_corr_param2
    })
    observe({
        input$subj_pd_corr_forestplot_ylab2
        subj_pd_corr_forestplot_ylab2$value <- input$subj_pd_corr_forestplot_ylab2
    })
    
    output$subj_pd_corr_forestplot2d_ylab <- renderUI({
        req(subj_pd_corr_show$forestplot2d)
        value <- input$subj_pd_corr_param2
        textareaInput('subj_pd_corr_forestplot2d_ylab', 'Y-axis label', value = value)
    })
    subj_pd_corr_forestplot2d_ylab <- reactiveValues(value = NULL)
    observe({
        req(subj_pd_corr_show$forestplot)
        subj_pd_corr_forestplot2d_ylab$value <- input$subj_pd_corr_param2
    })
    observe({
        input$subj_pd_corr_forestplot2d_ylab
        subj_pd_corr_forestplot2d_ylab$value <- input$subj_pd_corr_forestplot2d_ylab
    })
    
    output$subj_pkpd_scatterplot_ylab <- renderUI({
        req(subj_pkpd_show$scatterplot)
        value <- input$subj_pkpd_pd
        textareaInput('subj_pkpd_scatterplot_ylab', 'Y-axis label', value = value)
    })
    subj_pkpd_scatterplot_ylab <- reactiveValues(value = NULL)
    observe({
        req(subj_pkpd_show$scatterplot)
        subj_pkpd_scatterplot_ylab$value <- input$subj_pkpd_pd
    })
    observe({
        input$subj_pkpd_scatterplot_ylab
        subj_pkpd_scatterplot_ylab$value <- input$subj_pkpd_scatterplot_ylab
    })
    
    output$subj_pkpd_forestplot_ylab1 <- renderUI({
        req(subj_pkpd_show$forestplot)
        value <- input$subj_pkpd_pk
        textareaInput('subj_pkpd_forestplot_ylab1', 'Left Y-axis label',
                      value = value)
    })
    subj_pkpd_forestplot_ylab1 <- reactiveValues(value = NULL)
    observe({
        req(subj_pkpd_show$forestplot)
        subj_pkpd_forestplot_ylab1$value <- input$subj_pkpd_pk
    })
    observe({
        input$subj_pkpd_forestplot_ylab1
        subj_pkpd_forestplot_ylab1$value <- input$subj_pkpd_forestplot_ylab1
    })
    
    output$subj_pkpd_forestplot_ylab2 <- renderUI({
        req(subj_pkpd_show$forestplot)
        value <- input$subj_pkpd_pd
        textareaInput('subj_pkpd_forestplot_ylab2', 'Right Y-axis label',
                      value = value)
    })
    subj_pkpd_forestplot_ylab2 <- reactiveValues(value = NULL)
    observe({
        req(subj_pkpd_show$forestplot)
        subj_pkpd_forestplot_ylab2$value <- input$subj_pkpd_pd
    })
    observe({
        input$subj_pkpd_forestplot_ylab2
        subj_pkpd_forestplot_ylab2$value <- input$subj_pkpd_forestplot_ylab2
    })
    
    output$subj_pkpd_forestplot2d_ylab <- renderUI({
        req(subj_pkpd_show$forestplot2d)
        value <- input$subj_pkpd_pd
        textareaInput('subj_pkpd_forestplot2d_ylab', 'Y-axis label',
                      value = value)
    })
    subj_pkpd_forestplot2d_ylab <- reactiveValues(value = NULL)
    observe({
        req(subj_pkpd_show$forestplot2d)
        subj_pkpd_forestplot2d_ylab$value <- input$subj_pkpd_pd
    })
    observe({
        input$subj_pkpd_forestplot2d_ylab
        subj_pkpd_forestplot2d_ylab$value <- input$subj_pkpd_forestplot2d_ylab
    })
    
    output$subj_pkpd_quartileplot_ylab <- renderUI({
        req(subj_pkpd_show$quartileplot)
        value <- input$subj_pkpd_pd
        textareaInput('subj_pkpd_quartileplot_ylab', 'Y-axis label',
                      value = value)
    })
    subj_pkpd_quartileplot_ylab <- reactiveValues(value = NULL)
    observe({
        req(subj_pkpd_show$quartileplot)
        subj_pkpd_quartileplot_ylab$value <- input$subj_pkpd_pd
    })
    observe({
        input$subj_pkpd_quartileplot_ylab
        subj_pkpd_quartileplot_ylab$value <- input$subj_pkpd_quartileplot_ylab
    })
    
    # text area input for specifying plot title
    output$subj_pk_boxplot_main <- renderUI({
        req(subj_pk_show$boxplot)
        value <- paste(input$subj_pk_param, 'by', input$subj_pk_x)
        textareaInput('subj_pk_boxplot_main', 'Plot title', value = value)
    })
    subj_pk_boxplot_main <- reactiveValues(value = NULL)
    observe({
        req(subj_pk_show$boxplot)
        subj_pk_boxplot_main$value <- paste(
            input$subj_pk_param, 'by', input$subj_pk_x
        )
    })
    observe({
        input$subj_pk_boxplot_main
        subj_pk_boxplot_main$value <- input$subj_pk_boxplot_main
    })
    
    output$subj_pk_lineplot_main <- renderUI({
        req(subj_pk_show$lineplot)
        value <- paste(summary_title_dict[[input$subj_pk_summary]], 'of',
                       input$subj_pk_param, 'by', input$subj_pk_x)
        textareaInput('subj_pk_lineplot_main', 'Plot title', value = value)
    })
    subj_pk_lineplot_main <- reactiveValues(value = NULL)
    observe({
        req(subj_pk_show$lineplot)
        subj_pk_lineplot_main$value <- paste(
            summary_title_dict[[input$subj_pk_summary]], 'of',
            input$subj_pk_param, 'by', input$subj_pk_x
        )
    })
    observe({
        input$subj_pk_lineplot_main
        subj_pk_lineplot_main$value <- input$subj_pk_lineplot_main
    })
    
    output$subj_pk_doseprop_main <- renderUI({
        req(subj_pk_show$doseprop)
        value <- paste0('Log-scale of ', input$subj_pk_param, ' vs Log-scale ',
                        'of Dose')
        textareaInput('subj_pk_doseprop_main', 'Plot title', value = value)
    })
    subj_pk_doseprop_main <- reactiveValues(value = NULL)
    observe({
        req(subj_pk_show$doseprop)
        value <- paste0('Log-scale of ', input$subj_pk_param, ' vs Log-scale ',
                        'of Dose')
        subj_pk_doseprop_main$value <- value
    })
    observe({
        input$subj_pk_doseprop_main
        subj_pk_doseprop_main$value <- input$subj_pk_doseprop_main
    })
    
    output$subj_pd_dose_boxplot_main <- renderUI({
        req(subj_pd_dose_show$boxplot)
        value <- paste(input$subj_pd_dose_param, 'by', input$subj_pd_dose_x)
        textareaInput('subj_pd_dose_boxplot_main', 'Plot title', value = value)
    })
    subj_pd_dose_boxplot_main <- reactiveValues(value = NULL)
    observe({
        req(subj_pd_dose_show$boxplot)
        subj_pd_dose_boxplot_main$value <- paste(
            input$subj_pd_dose_param, 'by', input$subj_pd_dose_x
        )
    })
    observe({
        input$subj_pd_dose_boxplot_main
        subj_pd_dose_boxplot_main$value <- input$subj_pd_dose_boxplot_main
    })
    
    output$subj_pd_dose_lineplot_main <- renderUI({
        req(subj_pd_dose_show$lineplot)
        value <- paste(summary_title_dict[[input$subj_pd_dose_summary]], 'of',
                       input$subj_pd_dose_param, 'by', input$subj_pd_dose_x)
        textareaInput('subj_pd_dose_lineplot_main', 'Plot title', value = value)
    })
    subj_pd_dose_lineplot_main <- reactiveValues(value = NULL)
    observe({
        req(subj_pd_dose_show$lineplot)
        subj_pd_dose_lineplot_main$value <- paste(
            summary_title_dict[[input$subj_pd_dose_summary]], 'of',
            input$subj_pd_dose_param, 'by', input$subj_pd_dose_x
        )
    })
    observe({
        input$subj_pd_dose_lineplot_main
        subj_pd_dose_lineplot_main$value <- input$subj_pd_dose_lineplot_main
    })
    
    output$subj_pd_corr_scatterplot_main <- renderUI({
        req(subj_pd_corr_show$scatterplot)
        value <- paste(
            'Scatter plot between', input$subj_pd_corr_param1, 'and',
            input$subj_pd_corr_param2
        )
        textareaInput('subj_pd_corr_scatterplot_main', 'Plot title', value = value)
    })
    subj_pd_corr_scatterplot_main <- reactiveValues(value = NULL)
    observe({
        req(subj_pd_corr_show$scatterplot)
        value <- paste(
            'Scatter plot between', input$subj_pd_corr_param1, 'and',
            input$subj_pd_corr_param2
        )
        subj_pd_corr_scatterplot_main$value <- value
    })
    observe({
        input$subj_pd_corr_scatterplot_main
        subj_pd_corr_scatterplot_main$value <- input$subj_pd_corr_scatterplot_main
    })
    
    output$subj_pd_corr_forestplot_main <- renderUI({
        req(subj_pd_corr_show$forestplot)
        value <- paste(
            summary_title_dict[[input$subj_pd_corr_summary]],
            'of', input$subj_pd_corr_param1, 'and',
            input$subj_pd_corr_param2, 'by', input$subj_pd_corr_x
        )
        textareaInput('subj_pd_corr_forestplot_main', 'Plot title', value = value)
    })
    subj_pd_corr_forestplot_main <- reactiveValues(value = NULL)
    observe({
        req(subj_pd_corr_show$forestplot)
        value <- paste(
            summary_title_dict[[input$subj_pd_corr_summary]],
            'of', input$subj_pd_corr_param1, 'and',
            input$subj_pd_corr_param2, 'by', input$subj_pd_corr_x
        )
        subj_pd_corr_forestplot_main$value <- value
    })
    observe({
        input$subj_pd_corr_forestplot_main
        subj_pd_corr_forestplot_main$value <- input$subj_pd_corr_forestplot_main
    })
    
    output$subj_pd_corr_forestplot2d_main <- renderUI({
        req(subj_pd_corr_show$forestplot2d)
        value <- paste(
            summary_title_dict[[input$subj_pd_corr_summary]], 'of',
            input$subj_pd_corr_param2, 'vs', input$subj_pd_corr_param1,
            'by', input$subj_pd_corr_group
        )
        textareaInput('subj_pd_corr_forestplot2d_main', 'Plot title', value = value)
    })
    subj_pd_corr_forestplot2d_main <- reactiveValues(value = NULL)
    observe({
        req(subj_pd_corr_show$forestplot)
        value <- paste(
            summary_title_dict[[input$subj_pd_corr_summary]], 'of',
            input$subj_pd_corr_param2, 'vs', input$subj_pd_corr_param1,
            'by', input$subj_pd_corr_group
        )
        subj_pd_corr_forestplot2d_main$value <- value
    })
    observe({
        input$subj_pd_corr_forestplot2d_main
        subj_pd_corr_forestplot2d_main$value <- input$subj_pd_corr_forestplot2d_main
    })
    
    output$subj_pkpd_scatterplot_main <- renderUI({
        req(subj_pkpd_show$scatterplot)
        value <- paste(
            'Scatter plot between', input$subj_pkpd_pk, 'and', input$subj_pkpd_pd
        )
        if(!is_blank(input$subj_pkpd_group))
            value <- paste(value, 'by', input$subj_pkpd_group)
        textareaInput('subj_pkpd_scatterplot_main', 'Plot title', value = value)
    })
    subj_pkpd_scatterplot_main <- reactiveValues(value = NULL)
    observe({
        req(subj_pkpd_show$scatterplot)
        value <- paste(
            'Scatter plot between', input$subj_pkpd_pk, 'and', input$subj_pkpd_pd
        )
        if(!is_blank(input$subj_pkpd_group))
            value <- paste(value, 'by', input$subj_pkpd_group)
        subj_pkpd_scatterplot_main$value <- value
    })
    observe({
        input$subj_pkpd_scatterplot_main
        subj_pkpd_scatterplot_main$value <- input$subj_pkpd_scatterplot_main
    })
    
    output$subj_pkpd_forestplot_main <- renderUI({
        req(subj_pkpd_show$forestplot)
        value <- paste(
            summary_title_dict[[input$subj_pkpd_summary]], 'of',
            input$subj_pkpd_pk, 'and', input$subj_pkpd_pd,
            'by', input$subj_pkpd_group
        )
        textareaInput('subj_pkpd_forestplot_main', 'Plot title', value = value)
    })
    subj_pkpd_forestplot_main <- reactiveValues(value = NULL)
    observe({
        req(subj_pkpd_show$forestplot)
        value <- paste(
            summary_title_dict[[input$subj_pkpd_summary]], 'of',
            input$subj_pkpd_pk, 'and', input$subj_pkpd_pd,
            'by', input$subj_pkpd_group
        )
        subj_pkpd_forestplot_main$value <- value
    })
    observe({
        input$subj_pkpd_forestplot_main
        subj_pkpd_forestplot_main$value <- input$subj_pkpd_forestplot_main
    })
    
    output$subj_pkpd_forestplot2d_main <- renderUI({
        req(subj_pkpd_show$forestplot2d)
        value <- paste(
            summary_title_dict[[input$subj_pkpd_summary]], 'of',
            input$subj_pkpd_pd, 'vs', input$subj_pkpd_pk
        )
        if(!is_blank(input$subj_pkpd_group))
            value <- paste(value, 'by', input$subj_pkpd_group)
        textareaInput('subj_pkpd_forestplot2d_main', 'Plot title', value = value)
    })
    subj_pkpd_forestplot2d_main <- reactiveValues(value = NULL)
    observe({
        req(subj_pkpd_show$forestplot2d)
        value <- paste(
            summary_title_dict[[input$subj_pkpd_summary]], 'of',
            input$subj_pkpd_pd, 'vs', input$subj_pkpd_pk
        )
        if(!is_blank(input$subj_pkpd_group))
            value <- paste(value, 'by', input$subj_pkpd_group)
        subj_pkpd_forestplot2d_main$value <- value
    })
    observe({
        input$subj_pkpd_forestplot2d_main
        subj_pkpd_forestplot2d_main$value <- input$subj_pkpd_forestplot2d_main
    })
    
    output$subj_pkpd_quartileplot_main <- renderUI({
        req(subj_pkpd_show$quartileplot)
        value <- paste(
            'Boxplot of', input$subj_pkpd_pd, 'by', 'quartiles of',
            input$subj_pkpd_pk
        )
        if(!is_blank(input$subj_pkpd_group))
            value <- paste(value, 'and', input$subj_pkpd_group)
        textareaInput('subj_pkpd_quartileplot_main', 'Plot title', value = value)
    })
    subj_pkpd_quartileplot_main <- reactiveValues(value = NULL)
    observe({
        req(subj_pkpd_show$quartileplot)
        value <- paste(
            'Boxplot of', input$subj_pkpd_pd, 'by', 'quartiles of',
            input$subj_pkpd_pk
        )
        if(!is_blank(input$subj_pkpd_group))
            value <- paste(value, 'and', input$subj_pkpd_group)
        subj_pkpd_quartileplot_main$value <- value
    })
    observe({
        input$subj_pkpd_quartileplot_main
        subj_pkpd_quartileplot_main$value <- input$subj_pkpd_quartileplot_main
    })
    
    # text area input for specifying plot footnote
    output$subj_pk_boxplot_footnote <- renderUI({
        req(subj_pk_show$boxplot)
        value <- default_footnote$subj_pk_param
        textareaInput('subj_pk_boxplot_footnote', 'Plot footnote', value = value)
    })
    subj_pk_boxplot_footnote <- reactiveValues(value = '')
    observe({
        input$subj_pk_boxplot_footnote
        subj_pk_boxplot_footnote$value <- input$subj_pk_boxplot_footnote
    })
    
    output$subj_pk_lineplot_footnote <- renderUI({
        req(subj_pk_show$lineplot)
        value <- default_footnote$subj_pk_param
        textareaInput('subj_pk_lineplot_footnote', 'Plot footnote', value = value)
    })
    subj_pk_lineplot_footnote <- reactiveValues(value = '')
    observe({
        input$subj_pk_lineplot_footnote
        subj_pk_lineplot_footnote$value <- input$subj_pk_lineplot_footnote
    })
    
    output$subj_pk_doseprop_footnote <- renderUI({
        req(subj_pk_show$doseprop)
        value <- default_footnote$subj_pk_param
        textareaInput('subj_pk_doseprop_footnote', 'Plot footnote', value = value)
    })
    subj_pk_doseprop_footnote <- reactiveValues(value = '')
    observe({
        input$subj_pk_doseprop_footnote
        subj_pk_doseprop_footnote$value <- input$subj_pk_doseprop_footnote
    })
    
    output$subj_pd_dose_boxplot_footnote <- renderUI({
        req(subj_pd_dose_show$boxplot)
        value <- default_footnote$subj_pd
        textareaInput('subj_pd_dose_boxplot_footnote', 'Plot footnote',
                      value = value)
    })
    subj_pd_dose_boxplot_footnote <- reactiveValues(value = '')
    observe({
        input$subj_pd_dose_boxplot_footnote
        subj_pd_dose_boxplot_footnote$value <-
            input$subj_pd_dose_boxplot_footnote
    })
    
    output$subj_pd_dose_lineplot_footnote <- renderUI({
        req(subj_pd_dose_show$lineplot)
        value <- default_footnote$subj_pd
        textareaInput('subj_pd_dose_lineplot_footnote', 'Plot footnote',
                      value = value)
    })
    subj_pd_dose_lineplot_footnote <- reactiveValues(value = '')
    observe({
        input$subj_pd_dose_lineplot_footnote
        subj_pd_dose_lineplot_footnote$value <-
            input$subj_pd_dose_lineplot_footnote
    })
    
    output$subj_pd_corr_scatterplot_footnote <- renderUI({
        req(subj_pd_corr_show$scatterplot)
        value <- default_footnote$subj_pd
        textareaInput('subj_pd_corr_scatterplot_footnote', 'Plot footnote',
                      value = value)
    })
    subj_pd_corr_scatterplot_footnote <- reactiveValues(value = '')
    observe({
        input$subj_pd_corr_scatterplot_footnote
        subj_pd_corr_scatterplot_footnote$value <-
            input$subj_pd_corr_scatterplot_footnote
    })
    
    output$subj_pd_corr_forestplot_footnote <- renderUI({
        req(subj_pd_corr_show$forestplot)
        value <- default_footnote$subj_pd
        textareaInput('subj_pd_corr_forestplot_footnote', 'Plot footnote',
                      value = value)
    })
    subj_pd_corr_forestplot_footnote <- reactiveValues(value = '')
    observe({
        input$subj_pd_corr_forestplot_footnote
        subj_pd_corr_forestplot_footnote$value <-
            input$subj_pd_corr_forestplot_footnote
    })
    
    output$subj_pd_corr_forestplot2d_footnote <- renderUI({
        req(subj_pd_corr_show$forestplot2d)
        value <- default_footnote$subj_pd
        textareaInput('subj_pd_corr_forestplot2d_footnote', 'Plot footnote',
                      value = value)
    })
    subj_pd_corr_forestplot2d_footnote <- reactiveValues(value = '')
    observe({
        input$subj_pd_corr_forestplot2d_footnote
        subj_pd_corr_forestplot2d_footnote$value <-
            input$subj_pd_corr_forestplot2d_footnote
    })
    
    output$subj_pkpd_scatterplot_footnote <- renderUI({
        req(subj_pkpd_show$scatterplot)
        value <- default_footnote$subj_pkpd
        textareaInput('subj_pkpd_scatterplot_footnote', 'Plot footnote',
                      value = value)
    })
    subj_pkpd_scatterplot_footnote <- reactiveValues(value = '')
    observe({
        input$subj_pkpd_scatterplot_footnote
        subj_pkpd_scatterplot_footnote$value <-
            input$subj_pkpd_scatterplot_footnote
    })
    
    output$subj_pkpd_forestplot_footnote <- renderUI({
        req(subj_pkpd_show$forestplot)
        value <- default_footnote$subj_pkpd
        textareaInput('subj_pkpd_forestplot_footnote', 'Plot footnote',
                      value = value)
    })
    subj_pkpd_forestplot_footnote <- reactiveValues(value = '')
    observe({
        input$subj_pkpd_forestplot_footnote
        subj_pkpd_forestplot_footnote$value <-
            input$subj_pkpd_forestplot_footnote
    })
    
    output$subj_pkpd_forestplot2d_footnote <- renderUI({
        req(subj_pkpd_show$forestplot2d)
        value <- default_footnote$subj_pkpd
        textareaInput('subj_pkpd_forestplot2d_footnote', 'Plot footnote',
                      value = value)
    })
    subj_pkpd_forestplot2d_footnote <- reactiveValues(value = '')
    observe({
        input$subj_pkpd_forestplot2d_footnote
        subj_pkpd_forestplot2d_footnote$value <-
            input$subj_pkpd_forestplot2d_footnote
    })
    
    output$subj_pkpd_quartileplot_footnote <- renderUI({
        req(subj_pkpd_show$quartileplot)
        value <- default_footnote$subj_pkpd
        textareaInput('subj_pkpd_quartileplot_footnote', 'Plot footnote',
                      value = value)
    })
    subj_pkpd_quartileplot_footnote <- reactiveValues(value = '')
    observe({
        input$subj_pkpd_quartileplot_footnote
        subj_pkpd_quartileplot_footnote$value <-
            input$subj_pkpd_quartileplot_footnote
    })
    
    # checkbox for specifying whether to make dual y axis same scale
    output$subj_pd_corr_forestplot_same_y <- renderUI({
        req(subj_pd_corr_show$forestplot)
        checkboxInput('subj_pd_corr_forestplot_same_y', 'Same y axis', value = F)
    })
    subj_pd_corr_forestplot_same_y <- reactiveValues(value = FALSE)
    observe({
        input$subj_pd_corr_forestplot_same_y
        subj_pd_corr_forestplot_same_y$value <- input$subj_pd_corr_forestplot_same_y
    })
    
    output$subj_pkpd_forestplot_same_y <- renderUI({
        req(subj_pkpd_show$forestplot)
        checkboxInput('subj_pkpd_forestplot_same_y', 'Same y axis', value = F)
    })
    subj_pkpd_forestplot_same_y <- reactiveValues(value = FALSE)
    observe({
        input$subj_pkpd_forestplot_same_y
        subj_pkpd_forestplot_same_y$value <- input$subj_pkpd_forestplot_same_y
    })
    
    # text area input for specifying table title
    output$subj_pk_table_title <- renderUI({
        req(subj_pk_show$sumtable)
        value <- paste('Summary Statistics for ', input$subj_pk_param)
        if(!is_blank(input$subj_pk_x))
            value <- paste(value, 'by', input$subj_pk_x)
        textareaInput('subj_pk_table_title', 'Table title', value = value)
    })
    subj_pk_table_title <- reactiveValues(value = NULL)
    observe({
        req(subj_pk_show$sumtable)
        value <- paste('Summary Statistics for ', input$subj_pk_param)
        if(!is_blank(input$subj_pk_x))
            value <- paste(value, 'by', input$subj_pk_x)
        subj_pk_table_title$value <- value
    })
    observe({
        input$subj_pk_table_title
        subj_pk_table_title$value <- input$subj_pk_table_title
    })
    
    output$subj_pd_dose_table_title <- renderUI({
        req(subj_pd_dose_show$sumtable)
        value <- paste('Summary Statistics for ', input$subj_pd_dose_param)
        if(!is_blank(input$subj_pd_dose_x))
            value <- paste(value, 'by', input$subj_pd_dose_x)
        textareaInput('subj_pd_dose_table_title', 'Table title', value = value)
    })
    subj_pd_dose_table_title <- reactiveValues(value = NULL)
    observe({
        req(subj_pd_dose_show$sumtable)
        value <- paste('Summary Statistics for ', input$subj_pd_dose_param)
        if(!is_blank(input$subj_pd_dose_x))
            value <- paste(value, 'by', input$subj_pd_dose_x)
        subj_pd_dose_table_title$value <- value
    })
    observe({
        input$subj_pd_dose_table_title
        subj_pd_dose_table_title$value <- input$subj_pd_dose_table_title
    })
    
    # text area input for specifying table footnote
    output$subj_pk_table_footnote <- renderUI({
        req(subj_pk_show$sumtable)
        value <- default_footnote$subj_pk_param
        textareaInput('subj_pk_table_footnote', 'Table footnote', value = value)
    })
    subj_pk_table_footnote <- reactiveValues(value = '')
    observe({
        input$subj_pk_table_footnote
        subj_pk_table_footnote$value <- input$subj_pk_table_footnote
    })
    
    output$subj_pd_dose_table_footnote <- renderUI({
        req(subj_pd_dose_show$sumtable)
        value <- default_footnote$subj_pd
        textareaInput('subj_pd_dose_table_footnote', 'Table footnote',
                      value = value)
    })
    subj_pd_dose_table_footnote <- reactiveValues(value = '')
    observe({
        input$subj_pd_dose_table_footnote
        subj_pd_dose_table_footnote$value <- input$subj_pd_dose_table_footnote
    })
    
    #-----------------------------------------------
    # Outputs for PK analysis
    
    # dataset for PK analysis
    data_subj_pk <- reactive({
        req(data_import_status$subj_pk_param, input$subj_pk_param)
        data <- data_$subj_pk_param
        data <- data[!is.na(data[[subj_pk_estm_col]]), , drop = F]
        data <- data[data[[subj_pk_param_col]] %in% input$subj_pk_param, ]
        if(!is_blank(input$subj_pk_x) && !is_blank(subj_pk_x_type$value)) {
            data <- arrange_(data, input$subj_pk_x)
            if(subj_pk_x_type$value == 'Continuous')
                data[[input$subj_pk_x]] <- as.numeric(data[[input$subj_pk_x]])
            else if(subj_pk_x_type$value == 'Categorical')
                data[[input$subj_pk_x]] <- as.factor(data[[input$subj_pk_x]])
        }
        return(data)
    })
    
    # dataTable output for analysis data
    output$subj_pk_datatable <- renderDataTable({
        DT::datatable(data_subj_pk(), options = list(paging = FALSE))
    })
    
    # UI output for PK parameter summary
    subj_pk_table <- reactive({
        data <- data_subj_pk()
        req(data, !is.null(input$subj_pk_decimal))
        dgt <- input$subj_pk_decimal
        subj_pk_summary_func <- c(
            'N' = n_nna,
            'Mean (SD)' = partial(mean_sd_str, digits = dgt),
            '%CV' = partial(coeff_var_str, digits = dgt),
            'Median' = partial(median_str, digits = dgt),
            'Q1, Q3' = partial(q1_q3_str, digits = dgt),
            'Min, Max' = partial(min_max_str, digits = dgt),
            'Geom Mean (%CV)' = partial(geo_mean_cv_str, digits = dgt),
            'Mean (SD) of LN' = partial(mean_sd_ln_str, digits = dgt)
        )
        if(is_blank(input$subj_pk_x)) {
            data <- select_(data, subj_pk_estm_col)
            summary_tbl <- summary_table(
                data, collabel = 'Value',
                caption = subj_pk_table_title$value,
                footnote = subj_pk_table_footnote$value,
                func_list = subj_pk_summary_func
            )
        } else {
            data <- select_(data, subj_pk_estm_col, input$subj_pk_x)
            data[[input$subj_pk_x]] <- factor(
                data[[input$subj_pk_x]],
                levels = sort(unique(data[[input$subj_pk_x]]))
            )
            summary_tbl <- summary_table_col(
                data, col_var = input$subj_pk_x,
                col_names = paste(input$subj_pk_x, '=',
                                  levels(data[[input$subj_pk_x]])),
                caption = subj_pk_table_title$value,
                footnote = subj_pk_table_footnote$value,
                func_list = subj_pk_summary_func
            )
        }
        return(summary_tbl)
    })
    output$subj_pk_table <- renderUI({ HTML(subj_pk_table()) })
    
    # Boxplot output of PK parameter
    subj_pk_boxplot <- reactive({
        data <- data_subj_pk()
        req(data, input$subj_pk_x, !is.null(input$subj_pk_group),
            !is.null(input$subj_pk_log_y), !is.null(input$subj_pk_points),
            !is.null(subj_pk_boxplot_xlab$value),
            !is.null(subj_pk_boxplot_ylab$value),
            !is.null(subj_pk_boxplot_main$value),
            !is.null(subj_pk_boxplot_footnote$value))
        data <- data[!is.na(data[[input$subj_pk_x]]), , drop = F]
        plot_ <- gg_boxplot(data, input$subj_pk_x, subj_pk_estm_col,
                   group = input$subj_pk_group, log_y = input$subj_pk_log_y,
                   x_lab = subj_pk_boxplot_xlab$value,
                   y_lab = subj_pk_boxplot_ylab$value,
                   title = subj_pk_boxplot_main$value,
                   with_points = input$subj_pk_points)
        return(plot_)
    })
    output$subj_pk_boxplot_ui <- renderUI({
        shiny::tagList(
            plotOutput('subj_pk_boxplot'),
            uiOutput('subj_pk_boxplot_fn_out'),
            tags$head(tags$style(
                "#subj_pk_boxplot_fn_out{font-size: 9px;}"
            ))
        )
    })
    output$subj_pk_boxplot <- renderPlot({
        subj_pk_boxplot()
    })
    output$subj_pk_boxplot_fn_out <- renderUI({
        req(subj_pk_boxplot_footnote$value)
        HTML(paste(
            strsplit(subj_pk_boxplot_footnote$value, '\n')[[1]],
            collapse = '<br/>'
        ))
    })
    
    # Summary Line plot of PK parameter
    subj_pk_lineplot <- reactive({
        data <- data_subj_pk()
        req(data, input$subj_pk_x,
            !is.null(input$subj_pk_summary), !is.null(input$subj_pk_group),
            !is.null(input$subj_pk_log_y), !is.null(input$subj_pk_points),
            !is.null(subj_pk_lineplot_xlab$value),
            !is.null(subj_pk_lineplot_ylab$value),
            !is.null(subj_pk_lineplot_main$value),
            !is.null(subj_pk_lineplot_footnote$value))
        data <- data[!is.na(data[[input$subj_pk_x]]), , drop = F]
        plot_ <- gg_lineplot(data, input$subj_pk_x, subj_pk_estm_col,
                  group = input$subj_pk_group, log_y = input$subj_pk_log_y,
                  x_lab = subj_pk_lineplot_xlab$value,
                  y_lab = subj_pk_lineplot_ylab$value,
                  title = subj_pk_lineplot_main$value,
                  summary_method = input$subj_pk_summary,
                  with_points = input$subj_pk_points)
        return(plot_)
    })
    output$subj_pk_lineplot_ui <- renderUI({
        shiny::tagList(
            plotOutput('subj_pk_lineplot'),
            uiOutput('subj_pk_lineplot_fn_out'),
            tags$head(tags$style(
                "#subj_pk_lineplot_fn_out{font-size: 9px;}"
            ))
        )
    })
    output$subj_pk_lineplot <- renderPlot({
        subj_pk_lineplot()
    })
    output$subj_pk_lineplot_fn_out <- renderUI({
        req(subj_pk_lineplot_footnote$value)
        HTML(paste(
            strsplit(subj_pk_lineplot_footnote$value, '\n')[[1]],
            collapse = '<br/>'
        ))
    })
    
    # dose proportionality model of PK parameter
    subj_pk_doseprop_model <- reactive({
        data <- data_subj_pk()
        req(input$subj_pk_dose_level)
        data <- data[data[[subj_pk_dose_col]] %in% input$subj_pk_dose_level, ,
                     drop = FALSE]
        data[[subj_pk_estm_col]] <- as.numeric(data[[subj_pk_estm_col]])
        data[[subj_pk_dose_col]] <- as.numeric(data[[subj_pk_dose_col]])
        log_pk <- log(data[[subj_pk_estm_col]])
        log_dose <- log(data[[subj_pk_dose_col]])
        return(lm(log_pk ~ log_dose))
    })
    
    # dose proportionality plot of PK parameter
    subj_pk_doseprop_plot <- reactive({
        data <- data_subj_pk()
        lm_model <- subj_pk_doseprop_model()
        req(input$subj_pk_dose_level, lm_model)
        req(!is.null(subj_pk_doseprop_xlab$value),
            !is.null(subj_pk_doseprop_ylab$value),
            !is.null(subj_pk_doseprop_main$value),
            !is.null(subj_pk_doseprop_footnote$value))
        
        dose_level <- as.numeric(input$subj_pk_dose_level)
        data <- data[data[[subj_pk_dose_col]] %in% dose_level, , drop = FALSE]
        data[[subj_pk_estm_col]] <- as.numeric(data[[subj_pk_estm_col]])
        data[[subj_pk_dose_col]] <- as.numeric(data[[subj_pk_dose_col]])
        if(!is_blank(input$subj_pk_group)) {
            data[[input$subj_pk_group]] <- factor(
                data[[input$subj_pk_group]],
                levels = sort(unique(data[[input$subj_pk_group]]))
            )
        }
        
        lm_intercept <- lm_model$coefficients[1]
        lm_slope <- lm_model$coefficients[2]
        log_dose_name <- 'log(Dose)'
        log_pk_name <- paste0('log(', input$subj_pk_param, ')')
        data[[log_dose_name]] <- log(data[[subj_pk_dose_col]])
        data[[log_pk_name]] <- log(data[[subj_pk_estm_col]])
        data <- data[
            !is.na(data[[log_dose_name]]) & !is.na(data[[log_pk_name]]),
            , drop = FALSE
        ]
        plot_ <- gg_wrapper(
            data, aes_string(x = paste0('`', log_dose_name, '`'),
                             y = paste0('`', log_pk_name, '`'))
        )
        if(!is_blank(input$subj_pk_group)) {
            plot_ <- plot_ + aes_string(colour = input$subj_pk_group)
        }
        plot_ <- plot_ + geom_point(size = 2) +
            geom_abline(intercept = lm_intercept, slope = lm_slope) +
            scale_x_continuous(breaks = log(sort(dose_level)),
                               labels = paste0('log(', dose_level, ')')) +
            labs(x = subj_pk_doseprop_xlab$value,
                 y = subj_pk_doseprop_ylab$value,
                 title = subj_pk_doseprop_main$value)
        # plot_ <- add_footnote(plot_, subj_pk_doseprop_footnote$value)
        return(plot_)
    })
    output$subj_pk_doseprop_plot_ui <- renderUI({
        shiny::tagList(
            plotOutput('subj_pk_doseprop_plot'),
            uiOutput('subj_pk_doseprop_plot_fn_out'),
            tags$head(tags$style(
                "#subj_pk_doseprop_plot_fn_out{font-size: 9px;}"
            )),
            tags$br(),
            tags$br(),
            textOutput('subj_pk_doseprop_slope'),
            textOutput('subj_pk_doseprop_ci')
        )
    })
    output$subj_pk_doseprop_plot <- renderPlot({
        # grid.draw(subj_pk_doseprop_plot())
        subj_pk_doseprop_plot()
    })
    output$subj_pk_doseprop_plot_fn_out <- renderUI({
        req(subj_pk_doseprop_footnote$value)
        HTML(paste(
            strsplit(subj_pk_doseprop_footnote$value, '\n')[[1]],
            collapse = '<br/>'
        ))
    })
    
    # dose proportionality slope value of PK parameter
    output$subj_pk_doseprop_slope <- renderText({
        data <- data_subj_pk()
        lm_model <- subj_pk_doseprop_model()
        req(input$subj_pk_dose_level, lm_model)
        lm_slope <- lm_model$coefficients[2]
        message <- paste('Slope =', round(lm_slope, 3))
        return(message)
    })
    
    # dose proportionality CI value of PK parameter
    output$subj_pk_doseprop_ci <- renderText({
        data <- data_subj_pk()
        lm_model <- subj_pk_doseprop_model()
        req(input$subj_pk_dose_level, lm_model)
        ci_interval <- confint(lm_model, 'log_dose', level = 0.95)
        message <- paste0('A 95% CI for Slope is (',
                          paste(round(ci_interval, 3), collapse = ', '), ')')
        return(message)
    })
    
    #-----------------------------------------------
    # Outputs for PD analysis - 1 PD analysis
    
    # dataset for PD parameter evaluation
    data_subj_pd <- reactive({
        req(data_import_status$subj_pd, input$subj_pd_dose_param,
            input$subj_pd_dose_y)
        data <- data_$subj_pd
        data <- data[!is.na(input$subj_pd_dose_y), , drop = FALSE]
        data <- data[data[[subj_pd_param_col]] %in% input$subj_pd_dose_param, ]
        if(!is_blank(input$subj_pd_dose_x)) {
            req(subj_pd_dose_x_type$value)
            data <- arrange_(data, input$subj_pd_dose_x)
            if(subj_pd_dose_x_type$value == 'Continuous')
                data[[input$subj_pd_dose_x]] <- as.numeric(data[[input$subj_pd_dose_x]])
            else if(subj_pd_dose_x_type$value == 'Categorical')
                data[[input$subj_pd_dose_x]] <- as.factor(data[[input$subj_pd_dose_x]])
        }
        return(data)
    })
    
    # dataTable output for analysis data
    output$subj_pd_dose_datatable <- renderDataTable({
        DT::datatable(data_subj_pd(), options = list(paging = FALSE))
    })
    
    # dataTable output for PD parameter summary
    subj_pd_dose_table <- reactive({
        data <- data_subj_pd()
        req(data, !is.null(input$subj_pd_dose_decimal))
        dgt <- input$subj_pd_dose_decimal
        subj_pd_dose_summary_func <- c(
            'N' = n_nna,
            'Mean (SD)' = partial(mean_sd_str, digits = dgt),
            '%CV' = partial(coeff_var_str, digits = dgt),
            'Median' = partial(median_str, digits = dgt),
            'Q1, Q3' = partial(q1_q3_str, digits = dgt),
            'Min, Max' = partial(min_max_str, digits = dgt),
            'Geom Mean' = partial(geo_mean_str, digits = dgt),
            'Mean (SD) of LN' = partial(mean_sd_ln_str, digits = dgt)
        )
        if(is_blank(input$subj_pd_dose_x)) {
            data <- select_(data, input$subj_pd_dose_y)
            summary_tbl <- summary_table(
                data, caption = subj_pd_dose_table_title$value,
                footnote = subj_pd_dose_table_footnote$value,
                func_list = subj_pd_dose_summary_func
            )
        } else {
            data[[input$subj_pd_dose_x]] <- factor(
                data[[input$subj_pd_dose_x]],
                levels = sort(unique(data[[input$subj_pd_dose_x]]))
            )
            data <- select_(data, input$subj_pd_dose_y, input$subj_pd_dose_x)
            summary_tbl <- summary_table_col(
                data, col_var = input$subj_pd_dose_x,
                col_names = paste(input$subj_pd_dose_x, '=',
                                  levels(data[[input$subj_pd_dose_x]])),
                caption = subj_pd_dose_table_title$value,
                footnote = subj_pd_dose_table_footnote$value,
                func_list = subj_pd_dose_summary_func
            )
        }
        return(summary_tbl)
    })
    output$subj_pd_dose_table <- renderUI({ HTML(subj_pd_dose_table()) })
    
    # Boxplot output of PD parameter
    subj_pd_dose_boxplot <- reactive({
        data <- data_subj_pd()
        req(data, input$subj_pd_dose_x, !is.null(input$subj_pd_dose_group),
            !is.null(input$subj_pd_dose_log_y), !is.null(input$subj_pd_dose_points),
            !is.null(subj_pd_dose_boxplot_xlab$value),
            !is.null(subj_pd_dose_boxplot_ylab$value),
            !is.null(subj_pd_dose_boxplot_main$value),
            !is.null(subj_pd_dose_boxplot_footnote$value))
        data <- data[!is.na(input$subj_pd_dose_x), , drop = FALSE]
        plot_ <- gg_boxplot(data, input$subj_pd_dose_x, input$subj_pd_dose_y,
                   group = input$subj_pd_dose_group, log_y = input$subj_pd_dose_log_y,
                   x_lab = subj_pd_dose_boxplot_xlab$value,
                   y_lab = subj_pd_dose_boxplot_ylab$value,
                   title = subj_pd_dose_boxplot_main$value,
                   with_points = input$subj_pd_dose_points)
        # plot_ <- add_footnote(plot_, subj_pd_dose_boxplot_footnote$value)
        return(plot_)
    })
    output$subj_pd_dose_boxplot_ui <- renderUI({
        shiny::tagList(
            plotOutput('subj_pd_dose_boxplot'),
            uiOutput('subj_pd_dose_boxplot_fn_out'),
            tags$head(tags$style(
                "#subj_pd_dose_boxplot_fn_out{font-size: 9px;}"
            ))
        )
    })
    output$subj_pd_dose_boxplot <- renderPlot({
        # grid.draw(subj_pd_dose_boxplot())
        subj_pd_dose_boxplot()
    })
    output$subj_pd_dose_boxplot_fn_out <- renderUI({
        req(subj_pd_dose_boxplot_footnote$value)
        HTML(paste(
            strsplit(subj_pd_dose_boxplot_footnote$value, '\n')[[1]],
            collapse = '<br/>'
        ))
    })
    
    # Summary Line plot of PD parameter
    subj_pd_dose_lineplot <- reactive({
        data <- data_subj_pd()
        req(data, input$subj_pd_dose_x,
            !is.null(input$subj_pd_dose_summary), !is.null(input$subj_pd_dose_group),
            !is.null(input$subj_pd_dose_log_y), !is.null(input$subj_pd_dose_points),
            !is.null(subj_pd_dose_lineplot_xlab$value),
            !is.null(subj_pd_dose_lineplot_ylab$value),
            !is.null(subj_pd_dose_lineplot_main$value),
            !is.null(subj_pd_dose_lineplot_footnote$value))
        data <- data[!is.na(input$subj_pd_dose_x), , drop = FALSE]
        plot_ <- gg_lineplot(data, input$subj_pd_dose_x, input$subj_pd_dose_y,
                    group = input$subj_pd_dose_group, log_y = input$subj_pd_dose_log_y,
                    x_lab = subj_pd_dose_lineplot_xlab$value,
                    y_lab = subj_pd_dose_lineplot_ylab$value,
                    title = subj_pd_dose_lineplot_main$value,
                    summary_method = input$subj_pd_dose_summary,
                    with_points = input$subj_pd_dose_points)
        # plot_ <- add_footnote(plot_, subj_pd_dose_lineplot_footnote$value)
        return(plot_)
    })
    output$subj_pd_dose_lineplot_ui <- renderUI({
        shiny::tagList(
            plotOutput('subj_pd_dose_lineplot'),
            uiOutput('subj_pd_dose_lineplot_fn_out'),
            tags$head(tags$style(
                "#subj_pd_dose_lineplot_fn_out{font-size: 9px;}"
            ))
        )
    })
    output$subj_pd_dose_lineplot <- renderPlot({
        # grid.draw(subj_pd_dose_lineplot())
        subj_pd_dose_lineplot()
    })
    output$subj_pd_dose_lineplot_fn_out <- renderUI({
        req(subj_pd_dose_lineplot_footnote$value)
        HTML(paste(
            strsplit(subj_pd_dose_lineplot_footnote$value, '\n')[[1]],
            collapse = '<br/>'
        ))
    })
    
    
    #-----------------------------------------------
    # Outputs for PD analysis - 2 PD analysis
    
    # Scatter plot PD- PD correlation
    subj_pd_corr_scatter <- reactive({
        data <- data_$subj_pd
        req(data, input$subj_pd_corr_param1, input$subj_pd_corr_param2,
            input$subj_pd_corr_y1, input$subj_pd_corr_y2,
            !is.null(input$subj_pd_corr_group),
            !is.null(input$subj_pd_corr_refline),
            !is.null(input$subj_pd_corr_log_1),
            !is.null(input$subj_pd_corr_log_2))
        req(!is.null(subj_pd_corr_scatterplot_xlab$value),
            !is.null(subj_pd_corr_scatterplot_ylab$value),
            !is.null(subj_pd_corr_scatterplot_main$value),
            !is.null(subj_pd_corr_scatterplot_footnote$value))
        if(!is_blank(input$subj_pd_corr_group)) {
            formula_ <- paste(
                input$subj_pd_corr_group, '+', subj_pd_subj_col, 
                '~', subj_pd_param_col
            )
        } else {
            formula_ <- paste(subj_pd_subj_col, '~', subj_pd_param_col)
        }
        # data <- reshape2::dcast(data, formula_, fun.aggregate = mean,
        #                         na.rm = TRUE, value.var = subj_pd_aval_col)
        # data <- data[!is.na(data[[input$subj_pd_corr_param1]]) &
        #                  !is.na(data[[input$subj_pd_corr_param2]]), ]
        data <- data.table::dcast(
            data.table::as.data.table(data), formula_,
            fun.aggregate = mean, na.rm = TRUE,
            value.var = c(input$subj_pd_corr_y1, input$subj_pd_corr_y2)
        )
        if(input$subj_pd_corr_y1 == input$subj_pd_corr_y2) {
            pd_var_1 <- input$subj_pd_corr_param1
            pd_var_2 <- input$subj_pd_corr_param2
        } else {
            pd_var_1 <- paste(input$subj_pd_corr_y1, 'mean',
                              input$subj_pd_corr_param1, sep = '_')
            pd_var_2 <- paste(input$subj_pd_corr_y2, 'mean',
                              input$subj_pd_corr_param2, sep = '_')
        }
        data <- data[!is.na(data[[pd_var_1]]) & !is.na(data[[pd_var_2]]), ]
        if(!is_blank(input$subj_pd_corr_group)) {
            data[[input$subj_pd_corr_group]] <- factor(
                data[[input$subj_pd_corr_group]]
            )
        }
        baseplot <- gg_wrapper(
            data, aes_string(x = paste0('`', pd_var_1, '`'),
                             y = paste0('`', pd_var_2, '`')),
            log_x = input$subj_pd_corr_log_1,
            log_y = input$subj_pd_corr_log_2
        )
        if(!is_blank(input$subj_pd_corr_group)) {
            baseplot <- baseplot +
                geom_point(aes_string(colour = input$subj_pd_corr_group,
                                      fill = input$subj_pd_corr_group),
                           size = 2)
        } else baseplot <- baseplot + geom_point(size = 2)
        if(!is_blank(input$subj_pd_corr_refline)) {
            if('Loess' %in% input$subj_pd_corr_refline)
                baseplot <- baseplot + geom_smooth()
            if('Linear regression' %in% input$subj_pd_corr_refline)
                baseplot <- baseplot + geom_smooth(method = 'lm')
        }
        baseplot <- baseplot +
            labs(x = subj_pd_corr_scatterplot_xlab$value,
                 y = subj_pd_corr_scatterplot_ylab$value,
                 title = subj_pd_corr_scatterplot_main$value)
        # baseplot <- add_footnote(
        #     baseplot, subj_pd_corr_scatterplot_footnote$value
        # )
        return(baseplot)
    })
    output$subj_pd_corr_scatter_ui <- renderUI({
        # req(subj_pd_corr_scatter())
        shiny::tagList(
            plotOutput('subj_pd_corr_scatter'),
            uiOutput('subj_pd_corr_scatter_fn_out'),
            tags$head(tags$style(
                "#subj_pd_corr_scatter_fn_out{font-size: 9px;}"
            ))
        )
    })
    output$subj_pd_corr_scatter <- renderPlot({
        # grid.draw(subj_pd_corr_scatter())
        subj_pd_corr_scatter()
    })
    output$subj_pd_corr_scatter_fn_out <- renderUI({
        req(subj_pd_corr_scatterplot_footnote$value)
        HTML(paste(
            strsplit(subj_pd_corr_scatterplot_footnote$value, '\n')[[1]],
            collapse = '<br/>'
        ))
    })
    
    
    # Forest-plot Dose response for two PD parameters
    output$subj_pd_corr_forest_ui <- renderUI({
        plotOutput('subj_pd_corr_forest')
    })
    output$subj_pd_corr_forest <- renderPlot({
        data <- data_$subj_pd
        req(data, input$subj_pd_corr_param1, input$subj_pd_corr_param2,
            input$subj_pd_corr_y1, input$subj_pd_corr_y2,
            input$subj_pd_corr_x, subj_pd_corr_x_type$value,
            input$subj_pd_corr_summary)
        req(!is.null(subj_pd_corr_forestplot_xlab$value),
            !is.null(subj_pd_corr_forestplot_ylab1$value),
            !is.null(subj_pd_corr_forestplot_ylab2$value),
            !is.null(subj_pd_corr_forestplot_main$value),
            !is.null(subj_pd_corr_forestplot_footnote$value))
        formula_ <- paste(
            input$subj_pd_corr_x, '+', subj_pd_subj_col, '~', subj_pd_param_col
        )
        # data <- reshape2::dcast(data, formula_, fun.aggregate = mean,
        #                         na.rm = TRUE, value.var = subj_pd_aval_col)
        data <- data.table::dcast(
            data.table::as.data.table(data), formula_,
            fun.aggregate = mean, na.rm = TRUE,
            value.var = c(input$subj_pd_corr_y1, input$subj_pd_corr_y2)
        )
        if(input$subj_pd_corr_y1 == input$subj_pd_corr_y2) {
            pd_var_1 <- input$subj_pd_corr_param1
            pd_var_2 <- input$subj_pd_corr_param2
        } else {
            pd_var_1 <- paste(input$subj_pd_corr_y1, 'mean',
                              input$subj_pd_corr_param1, sep = '_')
            pd_var_2 <- paste(input$subj_pd_corr_y2, 'mean',
                              input$subj_pd_corr_param2, sep = '_')
        }
        if(subj_pd_corr_x_type$value == 'Continuous') {
            data[[input$subj_pd_corr_x]] <-
                as.numeric(data[[input$subj_pd_corr_x]])
        } else if(subj_pd_corr_x_type$value == 'Categorical') {
            data[[input$subj_pd_corr_x]] <-
                factor(data[[input$subj_pd_corr_x]])
        }
        if(input$subj_pd_corr_summary == 'Mean + SD') {
            method <- 'mean_sd'
            method_title <- 'Mean (SD)'
        } else if(input$subj_pd_corr_summary == 'Mean + SE') {
            method <- 'mean_se'
            method_title <- 'Mean (SE)'
        } else if(input$subj_pd_corr_summary == 'Median + IQR'){
            method <- 'median_iqr'
            method_title <- 'Median (Q1, Q3)'
        }
        forest_plot <- dual_y_axis_sumline(
            data, input$subj_pd_corr_x, pd_var_1, var_y2 = pd_var_2,
            xlab = subj_pd_corr_forestplot_xlab$value,
            ylab1 = subj_pd_corr_forestplot_ylab1$value,
            ylab2 = subj_pd_corr_forestplot_ylab2$value,
            title = subj_pd_corr_forestplot_main$value,
            footnote = subj_pd_corr_forestplot_footnote$value,
            method = method, type = 'p',
            same_y_axis = subj_pd_corr_forestplot_same_y$value,
            save_plot = FALSE
        )
    })
    
    
    # 2D forest-plot Efficacy - PD by dose plot
    subj_pd_corr_2dforest <- reactive({
        data <- data <- data_$subj_pd
        req(data, input$subj_pd_corr_param1, input$subj_pd_corr_param2,
            input$subj_pd_corr_y1, input$subj_pd_corr_y2,
            input$subj_pd_corr_summary, !is.null(input$subj_pd_corr_group))
        req(!is.null(subj_pd_corr_forestplot2d_xlab$value),
            !is.null(subj_pd_corr_forestplot2d_ylab$value),
            !is.null(subj_pd_corr_forestplot2d_main$value),
            !is.null(subj_pd_corr_forestplot2d_footnote$value))
        if(!is_blank(input$subj_pd_corr_group)) {
            formula_ <- paste(
                input$subj_pd_corr_group, '+', subj_pd_subj_col, 
                '~', subj_pd_param_col
            )
        } else {
            formula_ <- paste(subj_pd_subj_col, '~', subj_pd_param_col)
        }
        # data <- reshape2::dcast(data, formula_, fun.aggregate = mean,
        #                         na.rm = TRUE, value.var = subj_pd_aval_col)
        # data <- data[!is.na(data[[input$subj_pd_corr_param1]]) &
        #                  !is.na(data[[input$subj_pd_corr_param2]]), ]
        data <- data.table::dcast(
            data.table::as.data.table(data), formula_,
            fun.aggregate = mean, na.rm = TRUE,
            value.var = c(input$subj_pd_corr_y1, input$subj_pd_corr_y2)
        )
        if(input$subj_pd_corr_y1 == input$subj_pd_corr_y2) {
            pd_var_1 <- input$subj_pd_corr_param1
            pd_var_2 <- input$subj_pd_corr_param2
        } else {
            pd_var_1 <- paste(input$subj_pd_corr_y1, 'mean',
                              input$subj_pd_corr_param1, sep = '_')
            pd_var_2 <- paste(input$subj_pd_corr_y2, 'mean',
                              input$subj_pd_corr_param2, sep = '_')
        }
        data <- data[!is.na(data[[pd_var_1]]) & !is.na(data[[pd_var_2]]), ]
        if(!is_blank(input$subj_pd_corr_group)) {
            data[[input$subj_pd_corr_group]] <- factor(
                data[[input$subj_pd_corr_group]]
            )
            data <- data %>% group_by_(input$subj_pd_corr_group)
        }
        
        if(input$subj_pd_corr_summary %in% c('Mean + SD', 'Mean + SE')) {
            avg_expr <- ~mean_na(var)
            if(input$subj_pd_corr_summary == 'Mean + SD') {
                lower_expr <- ~mean_na(var) - sd_na(var)
                upper_expr <- ~mean_na(var) + sd_na(var)
            } else {
                lower_expr <- ~mean_na(var) - std_err(var)
                upper_expr <- ~mean_na(var) + std_err(var)
            }
        } else if(input$subj_pd_corr_summary == 'Median + IQR') {
            avg_expr <- ~median_na(var)
            lower_expr <- ~q1_na(var)
            upper_expr <- ~q3_na(var)
        }
        expr <- list(
            lazyeval::interp(avg_expr,var=as.name(pd_var_1)),
            lazyeval::interp(lower_expr,var=as.name(pd_var_1)),
            lazyeval::interp(upper_expr,var=as.name(pd_var_1)),
            lazyeval::interp(avg_expr,var=as.name(pd_var_2)),
            lazyeval::interp(lower_expr,var=as.name(pd_var_2)),
            lazyeval::interp(upper_expr,var=as.name(pd_var_2))
        )
        dots <- setNames(expr, c('avg_x', 'lower_x', 'upper_x',
                                 'avg_y', 'lower_y', 'upper_y'))
        data <- data %>% summarise_(.dots = dots)
        
        width_x <- diff(range(data$upper_x, data$lower_x)) / 40
        width_y <- diff(range(data$upper_y, data$lower_y)) / 40
        baseplot <- gg_wrapper(data, aes(x = avg_x, y = avg_y))
        if(!is_blank(input$subj_pd_corr_group)) {
            baseplot <- baseplot +
                aes_string(colour = input$subj_pd_corr_group)
        }
        forest2d_plot <- baseplot + geom_point(size = 2) +
            labs(x = subj_pd_corr_forestplot2d_xlab$value,
                 y = subj_pd_corr_forestplot2d_ylab$value,
                 title = subj_pd_corr_forestplot2d_main$value)
        if(any(!is.na(data$lower_x) & !is.na(data$upper_x))) {
            forest2d_plot <- forest2d_plot +
                geom_errorbarh(aes(xmin = lower_x, xmax = upper_x),
                               height = width_y)
        }
        if(any(!is.na(data$lower_y) & !is.na(data$upper_y))) {
            forest2d_plot <- forest2d_plot +
                geom_errorbar(aes(ymin = lower_y, ymax = upper_y),
                              width = width_x)
        }
        # forest2d_plot <- add_footnote(
        #     forest2d_plot, subj_pd_corr_forestplot2d_footnote$value
        # )
        return(forest2d_plot)
    })
    output$subj_pd_corr_2dforest_ui <- renderUI({
        # req(subj_pd_corr_2dforest())
        shiny::tagList(
            plotOutput('subj_pd_corr_2dforest'),
            uiOutput('subj_pd_corr_2dforest_fn_out'),
            tags$head(tags$style(
                "#subj_pd_corr_2dforest_fn_out{font-size: 9px;}"
            ))
        )
    })
    output$subj_pd_corr_2dforest <- renderPlot({
        # grid.draw(subj_pd_corr_2dforest())
        subj_pd_corr_2dforest()
    })
    output$subj_pd_corr_2dforest_fn_out <- renderUI({
        req(subj_pd_corr_forestplot2d_footnote$value)
        HTML(paste(
            strsplit(subj_pd_corr_forestplot2d_footnote$value, '\n')[[1]],
            collapse = '<br/>'
        ))
    })
    
    
    #-----------------------------------------------
    # Outputs for PK - PD analysis
    
    # Scatter plot PK - PD analysis
    subj_pkpd_scatter <- reactive({
        data <- data_$subj_pkpd
        req(data, input$subj_pkpd_pk, input$subj_pkpd_pd,
            !is.null(input$subj_pkpd_group),
            !is.null(input$subj_pkpd_log_pk),
            !is.null(input$subj_pkpd_log_pd))
        req(!is.null(subj_pkpd_scatterplot_xlab$value),
            !is.null(subj_pkpd_scatterplot_ylab$value),
            !is.null(subj_pkpd_scatterplot_main$value),
            !is.null(subj_pkpd_scatterplot_footnote$value))
        data <- data[
            !is.na(data[[subj_pk_estm_col]]) &
                !is.na(data[[input$subj_pkpd_pd_y]]), , drop = FALSE
        ]
        data <- data[data[[subj_pk_param_col]] %in% input$subj_pkpd_pk, ]
        data <- data[data[[subj_pd_param_col]] %in% input$subj_pkpd_pd, ]
        if(!is_blank(input$subj_pkpd_group)) {
            data[[input$subj_pkpd_group]] <- factor(
                data[[input$subj_pkpd_group]]
            )
        }
        baseplot <- gg_wrapper(data, aes_string(x = subj_pk_estm_col,
                                                y = input$subj_pkpd_pd_y),
                               log_x = input$subj_pkpd_log_pk,
                               log_y = input$subj_pkpd_log_pd)
        if(!is_blank(input$subj_pkpd_group)) {
            baseplot <- baseplot +
                geom_point(aes_string(colour = input$subj_pkpd_group,
                                      fill = input$subj_pkpd_group),
                           size = 2)
        } else baseplot <- baseplot + geom_point(size = 2)
        baseplot <- baseplot +
            labs(x = subj_pkpd_scatterplot_xlab$value,
                 y = subj_pkpd_scatterplot_ylab$value,
                 title = subj_pkpd_scatterplot_main$value)
        if(!is_blank(input$subj_pkpd_refline)) {
            if('Loess' %in% input$subj_pkpd_refline)
                baseplot <- baseplot + geom_smooth()
            if('Linear regression' %in% input$subj_pkpd_refline)
                baseplot <- baseplot + geom_smooth(method = 'lm')
        }
        # baseplot <- add_footnote(baseplot, subj_pkpd_scatterplot_footnote$value)
        return(baseplot)
    })
    output$subj_pkpd_scatter_ui <- renderUI({
        # req(subj_pkpd_scatter())
        shiny::tagList(
            plotOutput('subj_pkpd_scatter'),
            uiOutput('subj_pkpd_scatter_fn_out'),
            tags$head(tags$style(
                "#subj_pkpd_scatter_fn_out{font-size: 9px;}"
            ))
        )
    })
    output$subj_pkpd_scatter <- renderPlot({
        # grid.draw(subj_pkpd_scatter())
        subj_pkpd_scatter()
    })
    output$subj_pkpd_scatter_fn_out <- renderUI({
        req(subj_pkpd_scatterplot_footnote$value)
        HTML(paste(
            strsplit(subj_pkpd_scatterplot_footnote$value, '\n')[[1]],
            collapse = '<br/>'
        ))
    })
    
    
    # Forest-plot Dose response for PK - PD parameters
    output$subj_pkpd_forest_ui <- renderUI({
        plotOutput('subj_pkpd_forest')
    })
    output$subj_pkpd_forest <- renderPlot({
        data <- data_$subj_pkpd
        req(data, input$subj_pkpd_x, subj_pkpd_x_type$value,
            input$subj_pkpd_pk, input$subj_pkpd_pd, input$subj_pkpd_summary)
        req(!is.null(subj_pkpd_forestplot_xlab$value),
            !is.null(subj_pkpd_forestplot_ylab1$value),
            !is.null(subj_pkpd_forestplot_ylab2$value),
            !is.null(subj_pkpd_forestplot_main$value),
            !is.null(subj_pkpd_forestplot_footnote$value))
        cond_pk <- data[[subj_pk_param_col]] %in% input$subj_pkpd_pk
        cond_pd <- data[[subj_pd_param_col]] %in% input$subj_pkpd_pd
        data <- data[cond_pk & cond_pd, ]
        data[[input$subj_pkpd_pk]] <- data[[subj_pk_estm_col]]
        data[[input$subj_pkpd_pd]] <- data[[input$subj_pkpd_pd_y]]
        if(subj_pkpd_x_type$value == 'Continuous') {
            data[[input$subj_pkpd_x]] <-
                as.numeric(data[[input$subj_pkpd_x]])
        } else if(subj_pkpd_x_type$value == 'Categorical') {
            data[[input$subj_pkpd_x]] <-
                factor(data[[input$subj_pkpd_x]])
        }
        if(input$subj_pkpd_summary == 'Mean + SD') {
            method <- 'mean_sd'
            method_title <- 'Mean (SD)'
        } else if(input$subj_pkpd_summary == 'Mean + SE') {
            method <- 'mean_se'
            method_title <- 'Mean (SE)'
        } else if(input$subj_pkpd_summary == 'Median + IQR'){
            method <- 'median_iqr'
            method_title <- 'Median (Q1, Q3)'
        }
        title <- paste(method_title, 'of', input$subj_pkpd_pk,
                       'and', input$subj_pkpd_pd, 'by',
                       input$subj_pkpd_x)
        forest_plot <- dual_y_axis_sumline(
            data, input$subj_pkpd_x,
            input$subj_pkpd_pk, var_y2 = input$subj_pkpd_pd,
            xlab = subj_pkpd_forestplot_xlab$value,
            ylab1 = subj_pkpd_forestplot_ylab1$value,
            ylab2 = subj_pkpd_forestplot_ylab2$value,
            title = subj_pkpd_forestplot_main$value,
            footnote = subj_pkpd_forestplot_footnote$value,
            method = method, type = 'p',
            same_y_axis = subj_pkpd_forestplot_same_y$value,
            save_plot = FALSE
        )
    })
    
    
    # 2D forest-plot for PK - PD parameters
    subj_pkpd_2dforest <- reactive({
        data <- data_$subj_pkpd
        req(data, input$subj_pkpd_pk, input$subj_pkpd_pd,
            input$subj_pkpd_summary, !is.null(input$subj_pkpd_group))
        req(!is.null(subj_pkpd_forestplot2d_xlab$value),
            !is.null(subj_pkpd_forestplot2d_ylab$value),
            !is.null(subj_pkpd_forestplot2d_main$value),
            !is.null(subj_pkpd_forestplot2d_footnote$value))
        data <- data[
            !is.na(data[[subj_pk_estm_col]]) &
                !is.na(data[[input$subj_pkpd_pd_y]]), , drop = FALSE
        ]
        cond_pk <- data[[subj_pk_param_col]] %in% input$subj_pkpd_pk
        cond_pd <- data[[subj_pd_param_col]] %in% input$subj_pkpd_pd
        data <- data[cond_pk & cond_pd, ]
        data[[input$subj_pkpd_pk]] <- data[[subj_pk_estm_col]]
        data[[input$subj_pkpd_pd]] <- data[[input$subj_pkpd_pd_y]]
        data <- data[!is.na(data[[input$subj_pkpd_pk]]) &
                         !is.na(data[[input$subj_pkpd_pd]]), ]
        if(!is_blank(input$subj_pkpd_group)) {
            data[[input$subj_pkpd_group]] <- factor(
                data[[input$subj_pkpd_group]]
            )
            data <- data %>% group_by_(input$subj_pkpd_group)
        }
        
        if(input$subj_pkpd_summary %in% c('Mean + SD', 'Mean + SE')) {
            avg_expr <- ~mean_na(var)
            if(input$subj_pkpd_summary == 'Mean + SD') {
                lower_expr <- ~mean_na(var) - sd_na(var)
                upper_expr <- ~mean_na(var) + sd_na(var)
            } else {
                lower_expr <- ~mean_na(var) - std_err(var)
                upper_expr <- ~mean_na(var) + std_err(var)
            }
        } else if(input$subj_pkpd_summary == 'Median + IQR') {
            avg_expr <- ~median_na(var)
            lower_expr <- ~q1_na(var)
            upper_expr <- ~q3_na(var)
        }
        expr <- list(
            lazyeval::interp(avg_expr,var=as.name(input$subj_pkpd_pk)),
            lazyeval::interp(lower_expr,var=as.name(input$subj_pkpd_pk)),
            lazyeval::interp(upper_expr,var=as.name(input$subj_pkpd_pk)),
            lazyeval::interp(avg_expr,var=as.name(input$subj_pkpd_pd)),
            lazyeval::interp(lower_expr,var=as.name(input$subj_pkpd_pd)),
            lazyeval::interp(upper_expr,var=as.name(input$subj_pkpd_pd))
        )
        dots <- setNames(expr, c('avg_x', 'lower_x', 'upper_x',
                                 'avg_y', 'lower_y', 'upper_y'))
        data <- data %>% summarise_(.dots = dots)
        
        width_x <- diff(range(data$upper_x, data$lower_x)) / 40
        width_y <- diff(range(data$upper_y, data$lower_y)) / 40
        baseplot <- gg_wrapper(data, aes(x = avg_x, y = avg_y))
        if(!is_blank(input$subj_pkpd_group)) {
            baseplot <- baseplot +
                aes_string(colour = input$subj_pkpd_group)
        }
        forest2d_plot <- baseplot + geom_point(size = 2) +
            labs(x = subj_pkpd_forestplot2d_xlab$value,
                 y = subj_pkpd_forestplot2d_ylab$value,
                 title = subj_pkpd_forestplot2d_main$value)
        if(any(!is.na(data$lower_x) & !is.na(data$upper_x))) {
            forest2d_plot <- forest2d_plot +
                geom_errorbarh(aes(xmin = lower_x, xmax = upper_x),
                               height = width_y)
        }
        if(any(!is.na(data$lower_y) & !is.na(data$upper_y))) {
            forest2d_plot <- forest2d_plot +
                geom_errorbar(aes(ymin = lower_y, ymax = upper_y),
                              width = width_x)
        }
        # forest2d_plot <- add_footnote(
        #     forest2d_plot, subj_pkpd_forestplot2d_footnote$value
        # )
        return(forest2d_plot)
    })
    output$subj_pkpd_2dforest_ui <- renderUI({
        # req(subj_pkpd_2dforest())
        shiny::tagList(
            plotOutput('subj_pkpd_2dforest'),
            uiOutput('subj_pkpd_2dforest_fn_out'),
            tags$head(tags$style(
                "#subj_pkpd_2dforest_fn_out{font-size: 9px;}"
            ))
        )
    })
    output$subj_pkpd_2dforest <- renderPlot({
        # grid.draw(subj_pkpd_2dforest())
        subj_pkpd_2dforest()
    })
    output$subj_pkpd_2dforest_fn_out <- renderUI({
        req(subj_pkpd_forestplot2d_footnote$value)
        HTML(paste(
            strsplit(subj_pkpd_forestplot2d_footnote$value, '\n')[[1]],
            collapse = '<br/>'
        ))
    })
    
    # quartile plot for PK - PD analysis
    subj_pkpd_quartile <- reactive({
        data <- data_$subj_pkpd
        req(data, input$subj_pkpd_pk, input$subj_pkpd_pd,
            !is.null(input$subj_pkpd_group), !is.null(input$subj_pkpd_log_pd),
            !is.null(input$subj_pkpd_points))
        req(!is.null(subj_pkpd_quartileplot_xlab$value),
            !is.null(subj_pkpd_quartileplot_ylab$value),
            !is.null(subj_pkpd_quartileplot_main$value),
            !is.null(subj_pkpd_quartileplot_footnote$value))
        data <- data[
            !is.na(data[[subj_pk_estm_col]]) &
                !is.na(data[[input$subj_pkpd_pd_y]]), , drop = FALSE
        ]
        cond_pk <- data[[subj_pk_param_col]] %in% input$subj_pkpd_pk
        cond_pd <- data[[subj_pd_param_col]] %in% input$subj_pkpd_pd
        data <- data[cond_pk & cond_pd, ]
        data[[input$subj_pkpd_pk]] <- data[[subj_pk_estm_col]]
        data[[input$subj_pkpd_pd]] <- data[[input$subj_pkpd_pd_y]]
        data <- data[!is.na(data[[input$subj_pkpd_pk]]) &
                         !is.na(data[[input$subj_pkpd_pd]]), ]
        if(!is_blank(input$subj_pkpd_group)) {
            data[[input$subj_pkpd_group]] <- factor(
                data[[input$subj_pkpd_group]]
            )
            # data <- data %>% group_by_(input$subj_pkpd_group)
        }
        pk_quartile_value <- c(quantile(data[[input$subj_pkpd_pk]],
                                        probs = seq(0, 1, by = 0.25)))
        pk_quartile_label <- trimws(paste0(
            'Q', seq_len(4), '\n', round(head(pk_quartile_value, -1), 2),
            ' - ', round(pk_quartile_value[-1], 2)
        ))
        data[[input$subj_pkpd_pk]] <- factor(cut(
            data[[input$subj_pkpd_pk]], breaks = pk_quartile_value,
            labels = pk_quartile_label, include.lowest = TRUE
        ))
        
        quartile_plot <- gg_boxplot(
            data, input$subj_pkpd_pk, input$subj_pkpd_pd,
            group = input$subj_pkpd_group, log_y = input$subj_pkpd_log_pd,
            x_lab = subj_pkpd_quartileplot_xlab$value,
            y_lab = subj_pkpd_quartileplot_ylab$value,
            title = subj_pkpd_quartileplot_main$value,
            with_points = input$subj_pkpd_points,
            with_line = input$subj_pkpd_line
        )
        # quartile_plot <- add_footnote(
        #     quartile_plot, subj_pkpd_quartileplot_footnote$value
        # )
        return(quartile_plot)
    })
    output$subj_pkpd_quartile_ui <- renderUI({
        # req(subj_pkpd_quartile())
        shiny::tagList(
            plotOutput('subj_pkpd_quartile'),
            uiOutput('subj_pkpd_quartile_fn_out'),
            tags$head(tags$style(
                "#subj_pkpd_quartile_fn_out{font-size: 9px;}"
            ))
        )
    })
    output$subj_pkpd_quartile <- renderPlot({
        # grid.draw(subj_pkpd_quartile())
        subj_pkpd_quartile()
    })
    output$subj_pkpd_quartile_fn_out <- renderUI({
        req(subj_pkpd_quartileplot_footnote$value)
        HTML(paste(
            strsplit(subj_pkpd_quartileplot_footnote$value, '\n')[[1]],
            collapse = '<br/>'
        ))
    })
    
    
    #-----------------------------------------------
    # 'Add to TNF' button for PK analysis
    
    # subject-level PK summary table 'add to TNF' button
    output$subj_pk_table_add_to_tnf <- renderUI({
        req(subj_pk_show$sumtable)
        actionButton('subj_pk_table_add_to_tnf', 'Add to TNF',
                     icon = icon('cloud-upload'))
    })
    observeEvent(input$subj_pk_table_add_to_tnf, {
        if(is.null(n_tfl$table)) n_tfl$table <- 1
        else n_tfl$table <- n_tfl$table + 1
        if(is.null(n_tfl$subj_pk_param_sumtable))
            n_tfl$subj_pk_param_sumtable <- 1
        else n_tfl$subj_pk_param_sumtable <- n_tfl$subj_pk_param_sumtable + 1
        
        title_key <- paste0('subj_pk_summary_table_',n_tfl$subj_pk_param_sumtable)
        tfln <- paste0('2.', n_tfl$table)
        row_list <- list(
            title_key, 'Table', tfln, 'Summary table', input$subj_pk_param,
            NULL, NULL, NULL, input$subj_pk_group, input$subj_pk_decimal,
            subj_pk_table_title$value, subj_pk_table_footnote$value,
            NULL, NULL, NULL, NULL, NULL, NULL, NULL
        )
        row_df <- setNames(
            data.frame(
                lapply(row_list, function(x) replace(x, is.null(x), NA)),
                check.names = FALSE, stringsAsFactors = FALSE
            ),
            out_subj_pk_cols
        )
        if(is.null(tnf_rows$subj_pk)) {
            tnf_rows$subj_pk <- row_df
        } else {
            tnf_rows$subj_pk <- rbind(tnf_rows$subj_pk, row_df)
        }
    })
    
    # subject-level PK box plot 'add to TNF' button
    output$subj_pk_boxplot_add_to_tnf <- renderUI({
        req(subj_pk_show$boxplot)
        actionButton('subj_pk_boxplot_add_to_tnf', 'Add to TNF',
                     icon = icon('cloud-upload'))
    })
    observeEvent(input$subj_pk_boxplot_add_to_tnf, {
        if(is.null(n_tfl$figure)) n_tfl$figure <- 1
        else n_tfl$figure <- n_tfl$figure + 1
        if(is.null(n_tfl$subj_pk_param_boxplot))
            n_tfl$subj_pk_param_boxplot <- 1
        else n_tfl$subj_pk_param_boxplot <- n_tfl$subj_pk_param_boxplot + 1
        
        title_key <- paste0('subj_pk_boxplot_',n_tfl$subj_pk_param_boxplot)
        tfln <- paste0('1.', n_tfl$figure)
        row_list <- list(
            title_key, 'Figure', tfln, 'Box plot', input$subj_pk_param,
            input$subj_pk_x, subj_pk_x_type$value, NULL, input$subj_pk_group,
            NULL, NULL, NULL, input$subj_pk_log_y, input$subj_pk_points,
            NULL, subj_pk_boxplot_xlab$value, subj_pk_boxplot_ylab$value,
            subj_pk_boxplot_main$value, subj_pk_boxplot_footnote$value
        )
        row_df <- setNames(
            data.frame(
                lapply(row_list, function(x) replace(x, is.null(x), NA)),
                check.names = FALSE, stringsAsFactors = FALSE
            ),
            out_subj_pk_cols
        )
        if(is.null(tnf_rows$subj_pk)) {
            tnf_rows$subj_pk <- row_df
        } else {
            tnf_rows$subj_pk <- rbind(tnf_rows$subj_pk, row_df)
        }
    })
    
    # subject-level PK line plot 'add to TNF' button
    output$subj_pk_lineplot_add_to_tnf <- renderUI({
        req(subj_pk_show$lineplot)
        actionButton('subj_pk_lineplot_add_to_tnf', 'Add to TNF',
                     icon = icon('cloud-upload'))
    })
    observeEvent(input$subj_pk_lineplot_add_to_tnf, {
        if(is.null(n_tfl$figure)) n_tfl$figure <- 1
        else n_tfl$figure <- n_tfl$figure + 1
        if(is.null(n_tfl$subj_pk_param_lineplot))
            n_tfl$subj_pk_param_lineplot <- 1
        else n_tfl$subj_pk_param_lineplot <- n_tfl$subj_pk_param_lineplot + 1
        
        title_key <- paste0('subj_pk_lineplot_',n_tfl$subj_pk_param_lineplot)
        tfln <- paste0('1.', n_tfl$figure)
        row_list <- list(
            title_key, 'Figure', tfln, 'Line plot', input$subj_pk_param,
            input$subj_pk_x, subj_pk_x_type$value, NULL, input$subj_pk_group,
            NULL, NULL, NULL, input$subj_pk_log_y, input$subj_pk_points,
            input$subj_pk_summary, subj_pk_lineplot_xlab$value,
            subj_pk_lineplot_ylab$value, subj_pk_lineplot_main$value,
            subj_pk_lineplot_footnote$value
        )
        row_df <- setNames(
            data.frame(
                lapply(row_list, function(x) replace(x, is.null(x), NA)),
                check.names = FALSE, stringsAsFactors = FALSE
            ),
            out_subj_pk_cols
        )
        if(is.null(tnf_rows$subj_pk)) {
            tnf_rows$subj_pk <- row_df
        } else {
            tnf_rows$subj_pk <- rbind(tnf_rows$subj_pk, row_df)
        }
    })
    
    # subject-level PK dose proportionality 'add to TNF' button
    output$subj_pk_doseprop_add_to_tnf <- renderUI({
        req(subj_pk_show$doseprop)
        actionButton('subj_pk_doseprop_add_to_tnf', 'Add to TNF',
                     icon = icon('cloud-upload'))
    })
    observeEvent(input$subj_pk_doseprop_add_to_tnf, {
        if(is.null(n_tfl$figure)) n_tfl$figure <- 1
        else n_tfl$figure <- n_tfl$figure + 1
        if(is.null(n_tfl$subj_pk_param_doseprop))
            n_tfl$subj_pk_param_doseprop <- 1
        else n_tfl$subj_pk_param_doseprop <- n_tfl$subj_pk_param_doseprop + 1
        
        title_key <- paste0('subj_pk_doseprop_',n_tfl$subj_pk_param_doseprop)
        tfln <- paste0('1.', n_tfl$figure)
        dose_levels <- trimws(paste(input$subj_pk_dose_level, collapse = '\\n'))
        row_list <- list(
            title_key, 'Figure', tfln, 'Dose proportionality',
            input$subj_pk_param, NULL, NULL, dose_levels,
            input$subj_pk_group, NULL, NULL, NULL, NULL, NULL,
            NULL, subj_pk_doseprop_xlab$value,
            subj_pk_doseprop_ylab$value, subj_pk_doseprop_main$value,
            subj_pk_doseprop_footnote$value
        )
        row_df <- setNames(
            data.frame(
                lapply(row_list, function(x) replace(x, is.null(x), NA)),
                check.names = FALSE, stringsAsFactors = FALSE
            ),
            out_subj_pk_cols
        )
        if(is.null(tnf_rows$subj_pk)) {
            tnf_rows$subj_pk <- row_df
        } else {
            tnf_rows$subj_pk <- rbind(tnf_rows$subj_pk, row_df)
        }
    })
    
    #-----------------------------------------------
    # 'Add to TNF' button for PD analysis
    
    # subject-level PD summary table 'add to TNF' button
    output$subj_pd_dose_table_add_to_tnf <- renderUI({
        req(subj_pd_dose_show$sumtable)
        actionButton('subj_pd_dose_table_add_to_tnf', 'Add to TNF',
                     icon = icon('cloud-upload'))
    })
    observeEvent(input$subj_pd_dose_table_add_to_tnf, {
        if(is.null(n_tfl$table)) n_tfl$table <- 1
        else n_tfl$table <- n_tfl$table + 1
        if(is.null(n_tfl$subj_pd_dose_table))
            n_tfl$subj_pd_dose_table <- 1
        else n_tfl$subj_pd_dose_table <- n_tfl$subj_pd_dose_table + 1
        
        title_key <- paste0('subj_pd_summary_table_',n_tfl$subj_pd_dose_table)
        tfln <- paste0('2.', n_tfl$table)
        row_list <- list(
            title_key, 'Table', tfln, 'Summary table', input$subj_pd_dose_param,
            input$subj_pd_dose_y, NULL, NULL, input$subj_pd_dose_x,
            subj_pd_dose_x_type$value, input$subj_pd_dose_group,
            input$subj_pd_dose_decimal, subj_pd_dose_table_title$value,
            subj_pd_dose_table_footnote$value,
            NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL
        )
        row_df <- setNames(
            data.frame(
                lapply(row_list, function(x) replace(x, is.null(x), NA)),
                check.names = FALSE, stringsAsFactors = FALSE
            ),
            out_subj_pd_cols
        )
        if(is.null(tnf_rows$subj_pd)) {
            tnf_rows$subj_pd <- row_df
        } else {
            tnf_rows$subj_pd <- rbind(tnf_rows$subj_pd, row_df)
        }
    })
    
    # subject-level PD box plot 'add to TNF' button
    output$subj_pd_dose_boxplot_add_to_tnf <- renderUI({
        req(subj_pd_dose_show$boxplot)
        actionButton('subj_pd_dose_boxplot_add_to_tnf', 'Add to TNF',
                     icon = icon('cloud-upload'))
    })
    observeEvent(input$subj_pd_dose_boxplot_add_to_tnf, {
        if(is.null(n_tfl$figure)) n_tfl$figure <- 1
        else n_tfl$figure <- n_tfl$figure + 1
        if(is.null(n_tfl$subj_pd_dose_boxplot))
            n_tfl$subj_pd_dose_boxplot <- 1
        else n_tfl$subj_pd_dose_boxplot <- n_tfl$subj_pd_dose_boxplot + 1
        
        title_key <- paste0('subj_pd_boxplot_', n_tfl$subj_pd_dose_boxplot)
        tfln <- paste0('1.', n_tfl$figure)
        row_list <- list(
            title_key, 'Figure', tfln, 'Box plot', input$subj_pd_dose_param,
            input$subj_pd_dose_y, NULL, NULL, input$subj_pd_dose_x,
            subj_pd_dose_x_type$value, input$subj_pd_dose_group,
            NULL, NULL, NULL, NULL, input$subj_pd_dose_log_y,
            input$subj_pd_dose_points, NULL, NULL, NULL,
            subj_pk_boxplot_xlab$value, subj_pk_boxplot_ylab$value, NULL,
            subj_pk_boxplot_main$value, subj_pk_boxplot_footnote$value
        )
        row_df <- setNames(
            data.frame(
                lapply(row_list, function(x) replace(x, is.null(x), NA)),
                check.names = FALSE, stringsAsFactors = FALSE
            ),
            out_subj_pd_cols
        )
        if(is.null(tnf_rows$subj_pd)) {
            tnf_rows$subj_pd <- row_df
        } else {
            tnf_rows$subj_pd <- rbind(tnf_rows$subj_pd, row_df)
        }
    })
    
    # subject-level PD line plot 'add to TNF' button
    output$subj_pd_dose_lineplot_add_to_tnf <- renderUI({
        req(subj_pd_dose_show$lineplot)
        actionButton('subj_pd_dose_lineplot_add_to_tnf', 'Add to TNF',
                     icon = icon('cloud-upload'))
    })
    observeEvent(input$subj_pd_dose_lineplot_add_to_tnf, {
        if(is.null(n_tfl$figure)) n_tfl$figure <- 1
        else n_tfl$figure <- n_tfl$figure + 1
        if(is.null(n_tfl$subj_pd_dose_lineplot))
            n_tfl$subj_pd_dose_lineplot <- 1
        else n_tfl$subj_pd_dose_lineplot <- n_tfl$subj_pd_dose_lineplot + 1
        
        title_key <- paste0('subj_pd_lineplot_', n_tfl$subj_pd_dose_lineplot)
        tfln <- paste0('1.', n_tfl$figure)
        row_list <- list(
            title_key, 'Figure', tfln, 'Line plot', input$subj_pd_dose_param,
            input$subj_pd_dose_y, NULL, NULL, input$subj_pd_dose_x,
            subj_pd_dose_x_type$value, input$subj_pd_dose_group,
            NULL, NULL, NULL, NULL, input$subj_pd_dose_log_y,
            input$subj_pd_dose_points, NULL, input$subj_pd_dose_summary, NULL,
            subj_pk_lineplot_xlab$value, subj_pk_lineplot_ylab$value, NULL,
            subj_pk_lineplot_main$value, subj_pk_lineplot_footnote$value
        )
        row_df <- setNames(
            data.frame(
                lapply(row_list, function(x) replace(x, is.null(x), NA)),
                check.names = FALSE, stringsAsFactors = FALSE
            ),
            out_subj_pd_cols
        )
        if(is.null(tnf_rows$subj_pd)) {
            tnf_rows$subj_pd <- row_df
        } else {
            tnf_rows$subj_pd <- rbind(tnf_rows$subj_pd, row_df)
        }
    })
    
    # subject-level PD scatter plot 'add to TNF' button
    output$subj_pd_corr_scatter_add_to_tnf <- renderUI({
        req(subj_pd_corr_show$scatterplot)
        actionButton('subj_pd_corr_scatter_add_to_tnf', 'Add to TNF',
                     icon = icon('cloud-upload'))
    })
    observeEvent(input$subj_pd_corr_scatter_add_to_tnf, {
        if(is.null(n_tfl$figure)) n_tfl$figure <- 1
        else n_tfl$figure <- n_tfl$figure + 1
        if(is.null(n_tfl$subj_pd_corr_scatter))
            n_tfl$subj_pd_corr_scatter <- 1
        else n_tfl$subj_pd_corr_scatter <- n_tfl$subj_pd_corr_scatter + 1
        
        title_key <- paste0('subj_pd_scatterplot_', n_tfl$subj_pd_corr_scatter)
        tfln <- paste0('1.', n_tfl$figure)
        row_list <- list(
            title_key, 'Figure', tfln, 'Scatter plot', input$subj_pd_corr_param1,
            input$subj_pd_corr_y1, input$subj_pd_corr_param2,
            input$subj_pd_corr_y2, NULL, NULL, input$subj_pd_corr_group,
            NULL, NULL, NULL, input$subj_pd_corr_log_1, input$subj_pd_corr_log_2,
            NULL, input$subj_pd_corr_refline, NULL, NULL,
            subj_pd_corr_scatterplot_xlab$value,
            subj_pd_corr_scatterplot_ylab$value, NULL,
            subj_pd_corr_scatterplot_main$value,
            subj_pd_corr_scatterplot_footnote$value
        )
        row_df <- setNames(
            data.frame(
                lapply(row_list, function(x) replace(x, is.null(x), NA)),
                check.names = FALSE, stringsAsFactors = FALSE
            ),
            out_subj_pd_cols
        )
        if(is.null(tnf_rows$subj_pd)) {
            tnf_rows$subj_pd <- row_df
        } else {
            tnf_rows$subj_pd <- rbind(tnf_rows$subj_pd, row_df)
        }
    })
    
    # subject-level PD forest plot 'add to TNF' button
    output$subj_pd_corr_forest_add_to_tnf <- renderUI({
        req(subj_pd_corr_show$forestplot)
        actionButton('subj_pd_corr_forest_add_to_tnf', 'Add to TNF',
                     icon = icon('cloud-upload'))
    })
    observeEvent(input$subj_pd_corr_forest_add_to_tnf, {
        if(is.null(n_tfl$figure)) n_tfl$figure <- 1
        else n_tfl$figure <- n_tfl$figure + 1
        if(is.null(n_tfl$subj_pd_corr_forest))
            n_tfl$subj_pd_corr_forest <- 1
        else n_tfl$subj_pd_corr_forest <- n_tfl$subj_pd_corr_forest + 1
        
        title_key <- paste0('subj_pd_forestplot_', n_tfl$subj_pd_corr_forest)
        tfln <- paste0('1.', n_tfl$figure)
        row_list <- list(
            title_key, 'Figure', tfln, 'Forest plot', input$subj_pd_corr_param1,
            input$subj_pd_corr_y1, input$subj_pd_corr_param2,
            input$subj_pd_corr_y2, input$subj_pd_corr_x, subj_pd_corr_x_type$value,
            NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
            input$subj_pd_corr_summary, subj_pkpd_forestplot_same_y$value,
            subj_pd_corr_forestplot_xlab$value,
            subj_pd_corr_forestplot_ylab1$value,
            subj_pd_corr_forestplot_ylab2$value,
            subj_pd_corr_forestplot_main$value,
            subj_pd_corr_forestplot_footnote$value
        )
        row_df <- setNames(
            data.frame(
                lapply(row_list, function(x) replace(x, is.null(x), NA)),
                check.names = FALSE, stringsAsFactors = FALSE
            ),
            out_subj_pd_cols
        )
        if(is.null(tnf_rows$subj_pd)) {
            tnf_rows$subj_pd <- row_df
        } else {
            tnf_rows$subj_pd <- rbind(tnf_rows$subj_pd, row_df)
        }
    })
    
    # subject-level PD 2D forest plot 'add to TNF' button
    output$subj_pd_corr_2dforest_add_to_tnf <- renderUI({
        req(subj_pd_corr_show$forestplot2d)
        actionButton('subj_pd_corr_2dforest_add_to_tnf', 'Add to TNF',
                     icon = icon('cloud-upload'))
    })
    observeEvent(input$subj_pd_corr_2dforest_add_to_tnf, {
        if(is.null(n_tfl$figure)) n_tfl$figure <- 1
        else n_tfl$figure <- n_tfl$figure + 1
        if(is.null(n_tfl$subj_pd_corr_2dforest))
            n_tfl$subj_pd_corr_2dforest <- 1
        else n_tfl$subj_pd_corr_2dforest <- n_tfl$subj_pd_corr_2dforest + 1
        
        title_key <- paste0('subj_pd_2dforestplot_', n_tfl$subj_pd_corr_2dforest)
        tfln <- paste0('1.', n_tfl$figure)
        row_list <- list(
            title_key, 'Figure', tfln, '2D Forest plot',
            input$subj_pd_corr_param1, input$subj_pd_corr_y1,
            input$subj_pd_corr_param2, input$subj_pd_corr_y2, NULL, NULL,
            input$subj_pd_corr_group, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
            input$subj_pd_corr_summary, NULL,
            subj_pkpd_forestplot2d_xlab$value,
            subj_pkpd_forestplot2d_ylab$value, NULL,
            subj_pkpd_forestplot2d_main$value,
            subj_pkpd_forestplot2d_footnote$value
        )
        row_df <- setNames(
            data.frame(
                lapply(row_list, function(x) replace(x, is.null(x), NA)),
                check.names = FALSE, stringsAsFactors = FALSE
            ),
            out_subj_pd_cols
        )
        if(is.null(tnf_rows$subj_pd)) {
            tnf_rows$subj_pd <- row_df
        } else {
            tnf_rows$subj_pd <- rbind(tnf_rows$subj_pd, row_df)
        }
    })
    
    #-----------------------------------------------
    # 'Add to TNF' button for PKPD analysis
    
    # subject-level PKPD scatter plot 'add to TNF' button
    output$subj_pkpd_scatter_add_to_tnf <- renderUI({
        req(subj_pkpd_show$scatterplot)
        actionButton('subj_pkpd_scatter_add_to_tnf', 'Add to TNF',
                     icon = icon('cloud-upload'))
    })
    observeEvent(input$subj_pkpd_scatter_add_to_tnf, {
        if(is.null(n_tfl$figure)) n_tfl$figure <- 1
        else n_tfl$figure <- n_tfl$figure + 1
        if(is.null(n_tfl$subj_pkpd_scatter))
            n_tfl$subj_pkpd_scatter <- 1
        else n_tfl$subj_pkpd_scatter <- n_tfl$subj_pkpd_scatter + 1
        
        title_key <- paste0('subj_pkpd_scatterplot_', n_tfl$subj_pkpd_scatter)
        tfln <- paste0('1.', n_tfl$figure)
        row_list <- list(
            title_key, 'Figure', tfln, 'Scatter plot', input$subj_pkpd_pk,
            input$subj_pkpd_pd, input$subj_pkpd_pd_y, NULL, NULL,
            input$subj_pkpd_group, input$subj_pkpd_log_pk,
            input$subj_pkpd_log_pd, NULL, NULL, input$subj_pkpd_refline, NULL,
            NULL, subj_pkpd_scatterplot_xlab$value,
            subj_pkpd_scatterplot_ylab$value, NULL,
            subj_pkpd_scatterplot_main$value,
            subj_pkpd_scatterplot_footnote$value
        )
        row_df <- setNames(
            data.frame(
                lapply(row_list, function(x) replace(x, is.null(x), NA)),
                check.names = FALSE, stringsAsFactors = FALSE
            ),
            out_subj_pkpd_cols
        )
        if(is.null(tnf_rows$subj_pkpd)) {
            tnf_rows$subj_pkpd <- row_df
        } else {
            tnf_rows$subj_pkpd <- rbind(tnf_rows$subj_pkpd, row_df)
        }
    })
    
    # subject-level PKPD forest plot 'add to TNF' button
    output$subj_pkpd_forest_add_to_tnf <- renderUI({
        req(subj_pkpd_show$forestplot)
        actionButton('subj_pkpd_forest_add_to_tnf', 'Add to TNF',
                     icon = icon('cloud-upload'))
    })
    observeEvent(input$subj_pkpd_forest_add_to_tnf, {
        if(is.null(n_tfl$figure)) n_tfl$figure <- 1
        else n_tfl$figure <- n_tfl$figure + 1
        if(is.null(n_tfl$subj_pkpd_forest))
            n_tfl$subj_pkpd_forest <- 1
        else n_tfl$subj_pkpd_forest <- n_tfl$subj_pkpd_forest + 1
        
        title_key <- paste0('subj_pkpd_forestplot_', n_tfl$subj_pkpd_forest)
        tfln <- paste0('1.', n_tfl$figure)
        row_list <- list(
            title_key, 'Figure', tfln, 'Forest plot', input$subj_pkpd_pk,
            input$subj_pkpd_pd, input$subj_pkpd_pd_y, input$subj_pkpd_x,
            subj_pkpd_x_type$value, NULL, NULL, NULL, NULL, NULL, NULL,
            input$subj_pkpd_summary, input$subj_pkpd_forestplot_same_y,
            subj_pkpd_forestplot_xlab$value, subj_pkpd_forestplot_ylab1$value,
            subj_pkpd_forestplot_ylab2$value, subj_pkpd_forestplot_main$value,
            subj_pkpd_forestplot_footnote$value
        )
        row_df <- setNames(
            data.frame(
                lapply(row_list, function(x) replace(x, is.null(x), NA)),
                check.names = FALSE, stringsAsFactors = FALSE
            ),
            out_subj_pkpd_cols
        )
        if(is.null(tnf_rows$subj_pkpd)) {
            tnf_rows$subj_pkpd <- row_df
        } else {
            tnf_rows$subj_pkpd <- rbind(tnf_rows$subj_pkpd, row_df)
        }
    })
    
    # subject-level PD 2D forest plot 'add to TNF' button
    output$subj_pkpd_2dforest_add_to_tnf <- renderUI({
        req(subj_pkpd_show$forestplot2d)
        actionButton('subj_pkpd_2dforest_add_to_tnf', 'Add to TNF',
                     icon = icon('cloud-upload'))
    })
    observeEvent(input$subj_pkpd_2dforest_add_to_tnf, {
        if(is.null(n_tfl$figure)) n_tfl$figure <- 1
        else n_tfl$figure <- n_tfl$figure + 1
        if(is.null(n_tfl$subj_pkpd_2dforest))
            n_tfl$subj_pkpd_2dforest <- 1
        else n_tfl$subj_pkpd_2dforest <- n_tfl$subj_pkpd_2dforest + 1
        
        title_key <- paste0('subj_pkpd_2dforestplot_', n_tfl$subj_pkpd_2dforest)
        tfln <- paste0('1.', n_tfl$figure)
        row_list <- list(
            title_key, 'Figure', tfln, '2D Forest plot', input$subj_pkpd_pk,
            input$subj_pkpd_pd, input$subj_pkpd_pd_y, NULL,
            NULL, input$subj_pkpd_group, NULL, NULL, NULL, NULL, NULL,
            input$subj_pkpd_summary, NULL, subj_pkpd_forestplot2d_xlab$value,
            subj_pkpd_forestplot2d_ylab$value, NULL,
            subj_pkpd_forestplot2d_main$value,
            subj_pkpd_forestplot2d_footnote$value
        )
        row_df <- setNames(
            data.frame(
                lapply(row_list, function(x) replace(x, is.null(x), NA)),
                check.names = FALSE, stringsAsFactors = FALSE
            ),
            out_subj_pkpd_cols
        )
        if(is.null(tnf_rows$subj_pkpd)) {
            tnf_rows$subj_pkpd <- row_df
        } else {
            tnf_rows$subj_pkpd <- rbind(tnf_rows$subj_pkpd, row_df)
        }
    })
    
    # subject-level PD quartile plot 'add to TNF' button
    output$subj_pkpd_quartile_add_to_tnf <- renderUI({
        req(subj_pkpd_show$quartileplot)
        actionButton('subj_pkpd_quartile_add_to_tnf', 'Add to TNF',
                     icon = icon('cloud-upload'))
    })
    observeEvent(input$subj_pkpd_quartile_add_to_tnf, {
        if(is.null(n_tfl$figure)) n_tfl$figure <- 1
        else n_tfl$figure <- n_tfl$figure + 1
        if(is.null(n_tfl$subj_pkpd_quartile))
            n_tfl$subj_pkpd_quartile <- 1
        else n_tfl$subj_pkpd_quartile <- n_tfl$subj_pkpd_quartile + 1
        
        title_key <- paste0('subj_pkpd_quartileplot_', n_tfl$subj_pkpd_quartile)
        tfln <- paste0('1.', n_tfl$figure)
        row_list <- list(
            title_key, 'Figure', tfln, 'Quartile plot', input$subj_pkpd_pk,
            input$subj_pkpd_pd, input$subj_pkpd_pd_y, NULL,
            NULL, input$subj_pkpd_group, NULL, input$subj_pkpd_log_pd,
            input$subj_pkpd_points, input$subj_pkpd_line, NULL, NULL, NULL,
            subj_pkpd_quartileplot_xlab$value,
            subj_pkpd_quartileplot_ylab$value, NULL,
            subj_pkpd_quartileplot_main$value,
            subj_pkpd_quartileplot_footnote$value
        )
        row_df <- setNames(
            data.frame(
                lapply(row_list, function(x) replace(x, is.null(x), NA)),
                check.names = FALSE, stringsAsFactors = FALSE
            ),
            out_subj_pkpd_cols
        )
        if(is.null(tnf_rows$subj_pkpd)) {
            tnf_rows$subj_pkpd <- row_df
        } else {
            tnf_rows$subj_pkpd <- rbind(tnf_rows$subj_pkpd, row_df)
        }
    })
    
    #-----------------------------------------------
    # Download button for PK analysis

    # subject-level PK summary table download
    output$subj_pk_table_download_button <- renderUI({
        req(subj_pk_show$sumtable)
        downloadButton('subj_pk_table_download', 'Download table')
    })
    output$subj_pk_table_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'subj_pk_summary_table_', format(Sys.Date(),format = '%Y%m%d'),
                '.rtf'
            )
            return(file_name)
        },
        content = function(file) {
            data <- data_subj_pk()
            dgt <- input$subj_pk_decimal
            subj_pk_summary_func <- c(
                'N' = n_nna,
                'Mean (SD)' = partial(mean_sd_str, digits = dgt),
                '%CV' = partial(coeff_var_str, digits = dgt),
                'Median' = partial(median_str, digits = dgt),
                'Q1, Q3' = partial(q1_q3_str, digits = dgt),
                'Min, Max' = partial(min_max_str, digits = dgt),
                'Geom Mean (%CV)' = partial(geo_mean_cv_str, digits = dgt),
                'Mean (SD) of LN' = partial(mean_sd_ln_str, digits = dgt)
            )
            if(is_blank(input$subj_pk_x)) {
                data <- select_(data, subj_pk_estm_col)
                summary_tbl <- summary_table(
                    data, collabel = 'Value',
                    caption = subj_pk_table_title$value,
                    footnote = subj_pk_table_footnote$value,
                    func_list = subj_pk_summary_func, format = 'rtf'
                )
            } else {
                data <- select_(data, subj_pk_estm_col, input$subj_pk_x)
                data[[input$subj_pk_x]] <- factor(
                    data[[input$subj_pk_x]],
                    levels = sort(unique(data[[input$subj_pk_x]]))
                )
                summary_tbl <- summary_table_col(
                    data, col_var = input$subj_pk_x,
                    col_names = paste(input$subj_pk_x, '=',
                                      levels(data[[input$subj_pk_x]])),
                    caption = subj_pk_table_title$value,
                    footnote = subj_pk_table_footnote$value,
                    func_list = subj_pk_summary_func, format = 'rtf'
                )
            }
            rtf_table_wrapper(
                file, summary_tbl, block_break = TRUE,
                nline_block = length(subj_pk_summary_func),
                caption = subj_pk_table_title$value,
                footnote = subj_pk_table_footnote$value
            )
        }
    )
    
    # subject-level PK boxplot download
    output$subj_pk_boxplot_download_button <- renderUI({
        req(subj_pk_show$boxplot)
        downloadButton('subj_pk_boxplot_download', 'Download plot')
    })
    output$subj_pk_boxplot_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'subj_pk_boxplot_', format(Sys.Date(),format = '%Y%m%d'), '.pdf'
            )
            return(file_name)
        },
        content = function(file) {
            plot_ <- subj_pk_boxplot()
            plot_ <- add_footnote(plot_, subj_pk_boxplot_footnote$value)
            height_width_ratio <- 3 / 4
            width <- 8
            height <- width * height_width_ratio
            ggsave(filename = file, plot = plot_, width = width,
                   height = height, dpi = 600)
        }
    )
    
    # subject-level PK lineplot download
    output$subj_pk_lineplot_download_button <- renderUI({
        req(subj_pk_show$lineplot)
        downloadButton('subj_pk_lineplot_download', 'Download plot')
    })
    output$subj_pk_lineplot_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'subj_pk_lineplot_', format(Sys.Date(),format = '%Y%m%d'), '.pdf'
            )
            return(file_name)
        },
        content = function(file) {
            plot_ <- subj_pk_lineplot()
            plot_ <- add_footnote(plot_, subj_pk_lineplot_footnote$value)
            height_width_ratio <- 3 / 4
            width <- 8
            height <- width * height_width_ratio
            ggsave(filename = file, plot = plot_, width = width,
                   height = height, dpi = 600)
        }
    )
    
    # subject-level PK dose proportionality plot download
    output$subj_pk_doseprop_download_button <- renderUI({
        req(subj_pk_show$doseprop)
        downloadButton('subj_pk_doseprop_download', 'Download plot')
    })
    output$subj_pk_doseprop_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'subj_pk_dose_proportionality_',
                format(Sys.Date(),format = '%Y%m%d'), '.pdf'
            )
            return(file_name)
        },
        content = function(file) {
            plot_ <- subj_pk_doseprop_plot()
            plot_ <- add_footnote(plot_, subj_pk_doseprop_footnote$value)
            height_width_ratio <- 3 / 4
            width <- 8
            height <- width * height_width_ratio
            ggsave(filename = file, plot = plot_, width = width,
                   height = height, dpi = 600)
        }
    )
    
    #-----------------------------------------------
    # Download button for PD analysis
    
    # summary table download in subject-level 1 PD analysis
    output$subj_pd_dose_table_download_button <- renderUI({
        req(subj_pd_dose_show$sumtable)
        downloadButton('subj_pd_dose_table_download', 'Download table')
    })
    output$subj_pd_dose_table_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'subj_1pd_table_', format(Sys.Date(),format = '%Y%m%d'), '.rtf'
            )
            return(file_name)
        },
        content = function(file) {
            data <- data_subj_pd()
            dgt <- input$subj_pd_dose_decimal
            subj_pd_dose_summary_func <- c(
                'N' = n_nna,
                'Mean (SD)' = partial(mean_sd_str, digits = dgt),
                '%CV' = partial(coeff_var_str, digits = dgt),
                'Median' = partial(median_str, digits = dgt),
                'Q1, Q3' = partial(q1_q3_str, digits = dgt),
                'Min, Max' = partial(min_max_str, digits = dgt),
                'Geom Mean' = partial(geo_mean_str, digits = dgt),
                'Mean (SD) of LN' = partial(mean_sd_ln_str, digits = dgt)
            )
            if(is_blank(input$subj_pd_dose_x)) {
                data <- select_(data, input$subj_pd_dose_y)
                summary_tbl <- summary_table(
                    data, caption = subj_pd_dose_table_title$value,
                    footnote = subj_pd_dose_table_footnote$value,
                    func_list = subj_pd_dose_summary_func, format = 'rtf'
                )
            } else {
                data <- select_(data, input$subj_pd_dose_y, input$subj_pd_dose_x)
                data[[input$subj_pd_dose_x]] <- factor(
                    data[[input$subj_pd_dose_x]],
                    levels = sort(unique(data[[input$subj_pd_dose_x]]))
                )
                summary_tbl <- summary_table_col(
                    data, col_var = input$subj_pd_dose_x,
                    col_names = paste(input$subj_pd_dose_x, '=',
                                      levels(data[[input$subj_pd_dose_x]])),
                    caption = subj_pd_dose_table_title$value,
                    footnote = subj_pd_dose_table_footnote$value,
                    func_list = subj_pd_dose_summary_func, format = 'rtf'
                )
            }
            rtf_table_wrapper(
                file, summary_tbl, block_break = TRUE,
                nline_block = length(subj_pd_dose_summary_func),
                caption = subj_pd_dose_table_title$value,
                footnote = subj_pd_dose_table_footnote$value
            )
        }
    )
    
    # boxplot download in subject-level 1 PD analysis
    output$subj_pd_dose_boxplot_download_button <- renderUI({
        req(subj_pd_dose_show$boxplot)
        downloadButton('subj_pd_dose_boxplot_download', 'Download plot')
    })
    output$subj_pd_dose_boxplot_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'subj_1pd_boxplot_', format(Sys.Date(),format = '%Y%m%d'), '.pdf'
            )
            return(file_name)
        },
        content = function(file) {
            plot_ <- subj_pd_dose_boxplot()
            plot_ <- add_footnote(plot_, subj_pd_dose_boxplot_footnote$value)
            height_width_ratio <- 3 / 4
            width <- 8
            height <- width * height_width_ratio
            ggsave(filename = file, plot = plot_, width = width,
                   height = height, dpi = 600)
        }
    )
    
    # lineplot download in subject-level 1 PD analysis
    output$subj_pd_dose_lineplot_download_button <- renderUI({
        req(subj_pd_dose_show$lineplot)
        downloadButton('subj_pd_dose_lineplot_download', 'Download plot')
    })
    output$subj_pd_dose_lineplot_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'subj_1pd_lineplot_', format(Sys.Date(),format = '%Y%m%d'), '.pdf'
            )
            return(file_name)
        },
        content = function(file) {
            plot_ <- subj_pd_dose_lineplot()
            plot_ <- add_footnote(plot_, subj_pd_dose_lineplot_footnote$value)
            height_width_ratio <- 3 / 4
            width <- 8
            height <- width * height_width_ratio
            ggsave(filename = file, plot = plot_, width = width,
                   height = height, dpi = 600)
        }
    )
    
    # scatter plot download in 2 PD analysis
    output$subj_pd_corr_scatterplot_download_button <- renderUI({
        req(subj_pd_corr_show$scatterplot)
        downloadButton('subj_pd_corr_scatterplot_download', 'Download plot')
    })
    output$subj_pd_corr_scatterplot_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'subj_2pd_scatter_', format(Sys.Date(),format = '%Y%m%d'), '.pdf'
            )
            return(file_name)
        },
        content = function(file) {
            plot_ <- subj_pd_corr_scatter()
            plot_ <- add_footnote(plot_, subj_pd_corr_scatterplot_footnote$value)
            height_width_ratio <- 3 / 4
            width <- 8
            height <- width * height_width_ratio
            ggsave(filename = file, plot = plot_, width = width,
                   height = height, dpi = 600)
        }
    )
    
    # forest plot download in 2 PD analysis
    output$subj_pd_corr_forestplot_download_button <- renderUI({
        req(subj_pd_corr_show$forestplot)
        downloadButton('subj_pd_corr_forestplot_download', 'Download plot')
    })
    output$subj_pd_corr_forestplot_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'subj_2pd_forestplot_', format(Sys.Date(),format = '%Y%m%d'),
                '.pdf'
            )
            return(file_name)
        },
        content = function(file) {
            height_width_ratio <- 3 / 4
            width <- 8
            height <- width * height_width_ratio
            pdf(file = file, width = width, height = height)
            data <- data_$subj_pd
            formula_ <- paste(
                input$subj_pd_corr_x, '+', subj_pd_subj_col, '~', subj_pd_param_col
            )
            data <- data.table::dcast(
                data.table::as.data.table(data), formula_,
                fun.aggregate = mean, na.rm = TRUE,
                value.var = c(input$subj_pd_corr_y1, input$subj_pd_corr_y2)
            )
            if(input$subj_pd_corr_y1 == input$subj_pd_corr_y2) {
                pd_var_1 <- input$subj_pd_corr_param1
                pd_var_2 <- input$subj_pd_corr_param2
            } else {
                pd_var_1 <- paste(input$subj_pd_corr_y1, 'mean',
                                  input$subj_pd_corr_param1, sep = '_')
                pd_var_2 <- paste(input$subj_pd_corr_y2, 'mean',
                                  input$subj_pd_corr_param2, sep = '_')
            }
            if(subj_pd_corr_x_type$value == 'Continuous') {
                data[[input$subj_pd_corr_x]] <-
                    as.numeric(data[[input$subj_pd_corr_x]])
            } else if(subj_pd_corr_x_type$value == 'Categorical') {
                data[[input$subj_pd_corr_x]] <-
                    factor(data[[input$subj_pd_corr_x]])
            }
            if(input$subj_pd_corr_summary == 'Mean + SD') {
                method <- 'mean_sd'
                method_title <- 'Mean (SD)'
            } else if(input$subj_pd_corr_summary == 'Mean + SE') {
                method <- 'mean_se'
                method_title <- 'Mean (SE)'
            } else if(input$subj_pd_corr_summary == 'Median + IQR'){
                method <- 'median_iqr'
                method_title <- 'Median (Q1, Q3)'
            }
            forest_plot <- dual_y_axis_sumline(
                data, input$subj_pd_corr_x, pd_var_1, var_y2 = pd_var_2,
                xlab = subj_pd_corr_forestplot_xlab$value,
                ylab1 = subj_pd_corr_forestplot_ylab1$value,
                ylab2 = subj_pd_corr_forestplot_ylab2$value,
                title = subj_pd_corr_forestplot_main$value,
                footnote = subj_pd_corr_forestplot_footnote$value,
                method = method, type = 'p',
                same_y_axis = subj_pd_corr_forestplot_same_y$value,
                save_plot = FALSE
            )
            dev.off()
        }
    )
    
    # 2D forest plot download in 2 PD analysis
    output$subj_pd_corr_forestplot2d_download_button <- renderUI({
        req(subj_pd_corr_show$forestplot2d)
        downloadButton('subj_pd_corr_forestplot2d_download', 'Download plot')
    })
    output$subj_pd_corr_forestplot2d_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'subj_2pd_forestplot2d_', format(Sys.Date(),format = '%Y%m%d'),
                '.pdf'
            )
            return(file_name)
        },
        content = function(file) {
            plot_ <- subj_pd_corr_2dforest()
            plot_ <- add_footnote(plot_, subj_pd_corr_forestplot2d_footnote$value)
            height_width_ratio <- 3 / 4
            width <- 8
            height <- width * height_width_ratio
            ggsave(filename = file, plot = plot_, width = width,
                   height = height, dpi = 600)
        }
    )
    
    
    #-----------------------------------------------
    # Download button for PK-PD analysis
    
    # scatter plot download in PK-PD analysis
    output$subj_pkpd_scatterplot_download_button <- renderUI({
        req(subj_pkpd_show$scatterplot)
        downloadButton('subj_pkpd_scatterplot_download', 'Download plot')
    })
    output$subj_pkpd_scatterplot_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'subj_pkpd_scatterplot_', format(Sys.Date(),format = '%Y%m%d'),
                '.pdf'
            )
            return(file_name)
        },
        content = function(file) {
            plot_ <- subj_pkpd_scatter()
            plot_ <- add_footnote(plot_, subj_pkpd_scatterplot_footnote$value)
            height_width_ratio <- 3 / 4
            width <- 8
            height <- width * height_width_ratio
            ggsave(filename = file, plot = plot_, width = width,
                   height = height, dpi = 600)
        }
    )
    
    # forest plot download in PK-PD analysis
    output$subj_pkpd_forestplot_download_button <- renderUI({
        req(subj_pkpd_show$forestplot)
        downloadButton('subj_pkpd_forestplot_download', 'Download plot')
    })
    output$subj_pkpd_forestplot_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'subj_pkpd_forestplot_', format(Sys.Date(),format = '%Y%m%d'),
                '.pdf'
            )
            return(file_name)
        },
        content = function(file) {
            height_width_ratio <- 3 / 4
            width <- 8
            height <- width * height_width_ratio
            pdf(file = file, width = width, height = height)
            data <- data_$subj_pkpd
            cond_pk <- data[[subj_pk_param_col]] %in% input$subj_pkpd_pk
            cond_pd <- data[[subj_pd_param_col]] %in% input$subj_pkpd_pd
            data <- data[cond_pk & cond_pd, ]
            data[[input$subj_pkpd_pk]] <- data[[subj_pk_estm_col]]
            data[[input$subj_pkpd_pd]] <- data[[input$subj_pkpd_pd_y]]
            if(subj_pkpd_x_type$value == 'Continuous') {
                data[[input$subj_pkpd_x]] <-
                    as.numeric(data[[input$subj_pkpd_x]])
            } else if(subj_pkpd_x_type$value == 'Categorical') {
                data[[input$subj_pkpd_x]] <-
                    factor(data[[input$subj_pkpd_x]])
            }
            if(input$subj_pkpd_summary == 'Mean + SD') {
                method <- 'mean_sd'
                method_title <- 'Mean (SD)'
            } else if(input$subj_pkpd_summary == 'Mean + SE') {
                method <- 'mean_se'
                method_title <- 'Mean (SE)'
            } else if(input$subj_pkpd_summary == 'Median + IQR'){
                method <- 'median_iqr'
                method_title <- 'Median (Q1, Q3)'
            }
            title <- paste(method_title, 'of', input$subj_pkpd_pk,
                           'and', input$subj_pkpd_pd, 'by',
                           input$subj_pkpd_x)
            forest_plot <- dual_y_axis_sumline(
                data, input$subj_pkpd_x,
                input$subj_pkpd_pk, var_y2 = input$subj_pkpd_pd,
                xlab = subj_pkpd_forestplot_xlab$value,
                ylab1 = subj_pkpd_forestplot_ylab1$value,
                ylab2 = subj_pkpd_forestplot_ylab2$value,
                title = subj_pkpd_forestplot_main$value,
                footnote = subj_pkpd_forestplot_footnote$value,
                method = method, type = 'p',
                same_y_axis = subj_pkpd_forestplot_same_y$value,
                save_plot = FALSE
            )
            dev.off()
        }
    )
    
    # 2D forest plot download in PK-PD analysis
    output$subj_pkpd_forestplot2d_download_button <- renderUI({
        req(subj_pkpd_show$forestplot2d)
        downloadButton('subj_pkpd_forestplot2d_download', 'Download plot')
    })
    output$subj_pkpd_forestplot2d_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'subj_pkpd_forestplot2d_', format(Sys.Date(),format = '%Y%m%d'),
                '.pdf'
            )
            return(file_name)
        },
        content = function(file) {
            plot_ <- subj_pkpd_2dforest()
            plot_ <- add_footnote(plot_, subj_pkpd_forestplot2d_footnote$value)
            height_width_ratio <- 3 / 4
            width <- 8
            height <- width * height_width_ratio
            ggsave(filename = file, plot = plot_, width = width,
                   height = height, dpi = 600)
        }
    )
    
    # Quartile plot download in PK-PD analysis
    output$subj_pkpd_quartileplot_download_button <- renderUI({
        req(subj_pkpd_show$quartileplot)
        downloadButton('subj_pkpd_quartileplot_download', 'Download plot')
    })
    output$subj_pkpd_quartileplot_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'subj_pkpd_quartileplot_', format(Sys.Date(),format = '%Y%m%d'),
                '.pdf'
            )
            return(file_name)
        },
        content = function(file) {
            plot_ <- subj_pkpd_quartile()
            plot_ <- add_footnote(plot_, subj_pkpd_quartileplot_footnote$value)
            height_width_ratio <- 3 / 4
            width <- 8
            height <- width * height_width_ratio
            ggsave(filename = file, plot = plot_, width = width,
                   height = height, dpi = 600)
        }
    )
    
    
    #-----------------------------------------------
    # 2.	Sample-level PKPD analysis
    #-----------------------------------------------
    
    # Dynamically select which tab to open based on file uploads
    observe({
        if(isTRUE(data_import_status$sample_pk_con))
            updateCollapse(session, 'sample_panel', 'PK analysis')
        else if(isTRUE(data_import_status$sample_pk_param))
            updateCollapse(session, 'sample_panel', 'PK analysis')
        else if(isTRUE(data_import_status$sample_pd))
            updateCollapse(session, 'sample_panel', 'PD analysis')
    })
    
    #-----------------------------------------------
    # UI widgets for PK concentration time profile
    
    # selectInput for choosing PK concentration visit
    output$sample_pk_con_visit <- renderUI({
        req(data_import_status$sample_pk_con,
            input$file_sample_pk_type == 'PK concentration')
        choices <- paste(
            unique(data_$sample_pk_con[[sample_pk_param_visit_col]])
        )
        selectizeInput(
            'sample_pk_con_visit', 'Visit',
            c('Choose'='', choices), multiple = TRUE,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    # selectInput for choosing group variable
    output$sample_pk_con_group <- renderUI({
        req(data_import_status$sample_pk_con,
            input$file_sample_pk_type == 'PK concentration')
        choices <- names(data_$sample_pk_con)
        to_exclude <- c(sample_pk_con_subj_col, sample_pk_con_visit_col,
                        sample_pk_con_con_col, sample_pk_con_time_col)
        choices <- c('Choose' = '', sort(setdiff(choices, to_exclude)))
        selectInput('sample_pk_con_group', 'Group', choices,
                    selected = sample_pk_con_dose_col)
    })
    # a selectInput for choosing summary statistics
    output$sample_pk_con_summary <- renderUI({
        req(data_import_status$sample_pk_con,
            input$file_sample_pk_type == 'PK concentration',
            input$sample_pk_con_tabs == 'Summary line')
        selectInput('sample_pk_con_summary', 'Statistics',
                    sample_pk_con_summary_opts)
    })
    # a selectizeInput for choosing subject ID(s)
    output$sample_pk_con_subjid <- renderUI({
        req(data_import_status$sample_pk_con,
            input$file_sample_pk_type == 'PK concentration',
            input$sample_pk_con_tabs == 'Individual line')
        data <- data_$sample_pk_con
        if(!is.null(input$sample_pk_con_visit)) {
            cond <- data[[sample_pk_param_visit_col]]%in%input$sample_pk_con_visit
            data <- data[cond, , drop = FALSE]
        }
        choices <- sort(unique(data[[sample_pk_con_subj_col]]))
        selected <- isolate(ternary(
            is.null(input$sample_pk_con_subjid), choices,
            input$sample_pk_con_subjid[input$sample_pk_con_subjid %in% choices]
        ))
        selectizeInput(
            'sample_pk_con_subjid', 'Choose Subject ID(s)',
            c('Choose' = '', choices), selected = selected, multiple = T,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    # a numericInput for specifying decimal place
    output$sample_pk_con_decimal <- renderUI({
        req(data_import_status$sample_pk_con,
            input$file_sample_pk_type == 'PK concentration',
            req(input$sample_pk_con_tabs) == 'Summary table')
        stillSelected <- isolate(ifelse(
            is.null(input$sample_pk_con_decimal), 2, input$sample_pk_con_decimal
        ))
        numericInput('sample_pk_con_decimal', 'Decimal places', value = stillSelected)
    })
    sample_pk_con_decimal <- reactiveValues(value = 2)
    observe({
        input$sample_pk_con_decimal
        sample_pk_con_decimal$value <- input$sample_pk_con_decimal
    })
    # a checkbox for specifying log of y
    output$sample_pk_con_log <- renderUI({
        req(data_import_status$sample_pk_con,
            input$file_sample_pk_type == 'PK concentration',
            input$sample_pk_con_tabs != 'Summary table')
        checkboxInput('sample_pk_con_log', 'Log of Y', value = FALSE)
    })
    
    #-----------------------------------------------
    # Further table/graph refine for PK concentration time profile
    
    # a textareaInput for specifying summary table title
    output$sample_pk_con_table_title <- renderUI({
        data <- data_$sample_pk_con
        req(data, req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK concentration',
            req(input$sample_pk_con_tabs) == 'Summary table',
            input$sample_pk_con_visit, sample_pk_con_decimal$value,
            !is.null(input$sample_pk_con_group))
        value <- 'Summary statistics for PK concentration by'
        if(!is_blank(input$sample_pk_con_group))
            value <- paste(value, input$sample_pk_con_group, 'and')
        value <- paste(value, 'timepoint')
        textareaInput('sample_pk_con_table_title', 'Table title', value = value)
    })
    sample_pk_con_table_title <- reactiveValues(value = NULL)
    observe({
        data <- data_$sample_pk_con
        req(data, req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK concentration',
            req(input$sample_pk_con_tabs) == 'Summary table',
            input$sample_pk_con_visit, sample_pk_con_decimal$value,
            !is.null(input$sample_pk_con_group))
        value <- 'Summary statistics for PK concentration by'
        if(!is_blank(input$sample_pk_con_group))
            value <- paste(value, input$sample_pk_con_group, 'and')
        value <- paste(value, 'timepoint')
        sample_pk_con_table_title$value <- value
    })
    observe({
        input$sample_pk_con_table_title
        sample_pk_con_table_title$value <- input$sample_pk_con_table_title
    })
    
    # a textareaInput for specifying summary table footnote
    output$sample_pk_con_table_footnote <- renderUI({
        data <- data_$sample_pk_con
        req(data, req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK concentration',
            req(input$sample_pk_con_tabs) == 'Summary table',
            input$sample_pk_con_visit, sample_pk_con_decimal$value,
            !is.null(input$sample_pk_con_group))
        value <- default_footnote$sample_pk_con
        textareaInput('sample_pk_con_table_footnote', 'Table footnote',
                      value = value)
    })
    sample_pk_con_table_footnote <- reactiveValues(value = '')
    observe({
        input$sample_pk_con_table_footnote
        sample_pk_con_table_footnote$value <- input$sample_pk_con_table_footnote
    })
    
    # a textareaInput for specifying xlab for summary line
    output$sample_pk_con_sumline_xlab <- renderUI({
        req(req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK concentration',
            input$sample_pk_con_tabs == 'Summary line',
            input$sample_pk_con_visit, input$sample_pk_con_summary,
            !is.null(input$sample_pk_con_group),
            !is.null(input$sample_pk_con_log))
        value <- 'Time'
        textareaInput('sample_pk_con_sumline_xlab', 'X-axis label', value = value)
    })
    sample_pk_con_sumline_xlab <- reactiveValues(value = 'Time')
    observe({
        input$sample_pk_con_sumline_xlab
        sample_pk_con_sumline_xlab$value <- input$sample_pk_con_sumline_xlab
    })
    
    # a textareaInput for specifying ylab for summary line
    output$sample_pk_con_sumline_ylab <- renderUI({
        req(req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK concentration',
            input$sample_pk_con_tabs == 'Summary line',
            input$sample_pk_con_visit, input$sample_pk_con_summary,
            !is.null(input$sample_pk_con_group),
            !is.null(input$sample_pk_con_log))
        value <- 'PK concentration'
        textareaInput('sample_pk_con_sumline_ylab', 'Y-axis label', value = value)
    })
    sample_pk_con_sumline_ylab <- reactiveValues(value = 'PK concentration')
    observe({
        input$sample_pk_con_sumline_ylab
        sample_pk_con_sumline_ylab$value <- input$sample_pk_con_sumline_ylab
    })
    
    # a textareaInput for specifying main for summary line
    output$sample_pk_con_sumline_main <- renderUI({
        req(req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK concentration',
            input$sample_pk_con_tabs == 'Summary line',
            input$sample_pk_con_visit, input$sample_pk_con_summary,
            !is.null(input$sample_pk_con_group),
            !is.null(input$sample_pk_con_log))
        value <- paste(
            summary_title_dict[[input$sample_pk_con_summary]],
            'PK concentration time profiles'
        )
        if(!is_blank(input$sample_pk_con_group))
            value <- paste(value, 'by', input$sample_pk_con_group)
        textareaInput('sample_pk_con_sumline_main', 'Plot title', value = value)
    })
    sample_pk_con_sumline_main <- reactiveValues(value = NULL)
    observe({
        req(req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK concentration',
            input$sample_pk_con_tabs == 'Summary line',
            input$sample_pk_con_visit, input$sample_pk_con_summary,
            !is.null(input$sample_pk_con_group),
            !is.null(input$sample_pk_con_log))
        value <- paste(
            summary_title_dict[[input$sample_pk_con_summary]],
            'PK concentration time profiles'
        )
        if(!is_blank(input$sample_pk_con_group))
            value <- paste(value, 'by', input$sample_pk_con_group)
        sample_pk_con_sumline_main$value <- value
    })
    observe({
        input$sample_pk_con_sumline_main
        sample_pk_con_sumline_main$value <- input$sample_pk_con_sumline_main
    })
    
    # a textareaInput for specifying footnote for summary line
    output$sample_pk_con_sumline_footnote <- renderUI({
        req(req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK concentration',
            input$sample_pk_con_tabs == 'Summary line',
            input$sample_pk_con_visit, input$sample_pk_con_summary,
            !is.null(input$sample_pk_con_group),
            !is.null(input$sample_pk_con_log))
        value <- default_footnote$sample_pk_con
        textareaInput('sample_pk_con_sumline_footnote', 'Plot footnote',
                      value = value)
    })
    sample_pk_con_sumline_footnote <- reactiveValues(value = '')
    observe({
        input$sample_pk_con_sumline_footnote
        sample_pk_con_sumline_footnote$value <- input$sample_pk_con_sumline_footnote
    })
    
    # a textareaInput for specifying xlab for individual line
    output$sample_pk_con_indline_xlab <- renderUI({
        req(req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK concentration',
            input$sample_pk_con_tabs == 'Individual line',
            input$sample_pk_con_visit, input$sample_pk_con_subjid, 
            !is.null(input$sample_pk_con_group),
            !is.null(input$sample_pk_con_log))
        value <- 'Time'
        textareaInput('sample_pk_con_indline_xlab', 'X-axis label', value = value)
    })
    sample_pk_con_indline_xlab <- reactiveValues(value = 'Time')
    observe({
        input$sample_pk_con_indline_xlab
        sample_pk_con_indline_xlab$value <- input$sample_pk_con_indline_xlab
    })
    
    # a textareaInput for specifying ylab for individual line
    output$sample_pk_con_indline_ylab <- renderUI({
        req(req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK concentration',
            input$sample_pk_con_tabs == 'Individual line',
            input$sample_pk_con_visit, input$sample_pk_con_subjid, 
            !is.null(input$sample_pk_con_group),
            !is.null(input$sample_pk_con_log))
        value <- 'PK concentration'
        textareaInput('sample_pk_con_indline_ylab', 'Y-axis label', value = value)
    })
    sample_pk_con_indline_ylab <- reactiveValues(value = 'PK concentration')
    observe({
        input$sample_pk_con_indline_ylab
        sample_pk_con_indline_ylab$value <- input$sample_pk_con_indline_ylab
    })
    
    # a textareaInput for specifying main for individual line
    output$sample_pk_con_indline_main <- renderUI({
        req(req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK concentration',
            input$sample_pk_con_tabs == 'Individual line',
            input$sample_pk_con_visit, input$sample_pk_con_subjid, 
            !is.null(input$sample_pk_con_group),
            !is.null(input$sample_pk_con_log))
        value <- 'Individual PK concentration time profiles'
        if(!is_blank(input$sample_pk_con_group))
            value <- paste(value, 'by', input$sample_pk_con_group)
        textareaInput('sample_pk_con_indline_main', 'Plot title', value = value)
    })
    sample_pk_con_indline_main <- reactiveValues(value = NULL)
    observe({
        req(req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK concentration',
            input$sample_pk_con_tabs == 'Individual line',
            input$sample_pk_con_visit, input$sample_pk_con_subjid, 
            !is.null(input$sample_pk_con_group),
            !is.null(input$sample_pk_con_log))
        value <- 'Individual PK concentration time profiles'
        if(!is_blank(input$sample_pk_con_group))
            value <- paste(value, 'by', input$sample_pk_con_group)
        sample_pk_con_indline_main$value <- value
    })
    observe({
        input$sample_pk_con_indline_main
        sample_pk_con_indline_main$value <- input$sample_pk_con_indline_main
    })
    
    # a textareaInput for specifying footnote for individual line
    output$sample_pk_con_indline_footnote <- renderUI({
        req(req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK concentration',
            input$sample_pk_con_tabs == 'Individual line',
            input$sample_pk_con_visit, input$sample_pk_con_subjid, 
            !is.null(input$sample_pk_con_group),
            !is.null(input$sample_pk_con_log))
        value <- default_footnote$sample_pk_con
        textareaInput('sample_pk_con_indline_footnote', 'Plot footnote',
                      value = value)
    })
    sample_pk_con_indline_footnote <- reactiveValues(value = '')
    observe({
        input$sample_pk_con_indline_footnote
        sample_pk_con_indline_footnote$value <- input$sample_pk_con_indline_footnote
    })
    
    
    #---------------------------------------
    # Add to TNF buttons
    
    # summary table for PK concentration
    output$sample_pk_con_sumtable_att <- renderUI({
        data <- data_$sample_pk_con
        req(data, req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK concentration',
            req(input$sample_pk_con_tabs) == 'Summary table',
            input$sample_pk_con_visit, sample_pk_con_decimal$value,
            !is.null(input$sample_pk_con_group))
        actionButton('sample_pk_con_sumtable_att', 'Add to TNF',
                     icon = icon('cloud-upload'))
    })
    observeEvent(input$sample_pk_con_sumtable_att, {
        if(is.null(n_tfl$table)) n_tfl$table <- 1
        else n_tfl$table <- n_tfl$table + 1
        if(is.null(n_tfl$sample_pk_con_sumtable))
            n_tfl$sample_pk_con_sumtable <- 1
        else n_tfl$sample_pk_con_sumtable <- n_tfl$sample_pk_con_sumtable + 1
        
        title_key <- paste0('sample_pk_summary_table_',
                            n_tfl$sample_pk_con_sumtable)
        tfln <- paste0('2.', n_tfl$table)
        row_list <- list(
            title_key, 'Table', tfln, 'Summary table', 'PK concentration',
            NULL, input$sample_pk_con_visit, input$sample_pk_con_group,
            input$sample_pk_con_decimal, sample_pk_con_table_title$value,
            sample_pk_con_table_footnote$value, NULL, NULL, NULL, NULL,
            NULL, NULL, NULL
        )
        row_df <- setNames(
            data.frame(
                lapply(row_list, function(x) replace(x, is.null(x), NA)),
                check.names = FALSE, stringsAsFactors = FALSE
            ),
            out_sample_pk_cols
        )
        if(is.null(tnf_rows$sample_pk)) {
            tnf_rows$sample_pk <- row_df
        } else {
            tnf_rows$sample_pk <- rbind(tnf_rows$sample_pk, row_df)
        }
    })
    
    # summary line for PK concentration
    output$sample_pk_con_sumline_att <- renderUI({
        req(req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK concentration',
            input$sample_pk_con_tabs == 'Summary line',
            input$sample_pk_con_visit, input$sample_pk_con_summary,
            !is.null(input$sample_pk_con_group),
            !is.null(input$sample_pk_con_log))
        actionButton('sample_pk_con_sumline_att', 'Add to TNF',
                     icon = icon('cloud-upload'))
    })
    observeEvent(input$sample_pk_con_sumline_att, {
        if(is.null(n_tfl$figure)) n_tfl$figure <- 1
        else n_tfl$figure <- n_tfl$figure + 1
        if(is.null(n_tfl$sample_pk_con_sumline))
            n_tfl$sample_pk_con_sumline <- 1
        else n_tfl$sample_pk_con_sumline <- n_tfl$sample_pk_con_sumline + 1
        
        title_key <- paste0('sample_pk_summary_line_',
                            n_tfl$sample_pk_con_sumline)
        tfln <- paste0('1.', n_tfl$figure)
        row_list <- list(
            title_key, 'Figure', tfln, 'Summary line', 'PK concentration',
            NULL, input$sample_pk_con_visit, input$sample_pk_con_group,
            NULL, NULL, NULL, input$sample_pk_con_summary, NULL,
            input$sample_pk_con_log, sample_pk_con_sumline_xlab$value,
            sample_pk_con_sumline_ylab$value, sample_pk_con_sumline_main$value,
            sample_pk_con_sumline_footnote$value
        )
        row_df <- setNames(
            data.frame(
                lapply(row_list, function(x) replace(x, is.null(x), NA)),
                check.names = FALSE, stringsAsFactors = FALSE
            ),
            out_sample_pk_cols
        )
        if(is.null(tnf_rows$sample_pk)) {
            tnf_rows$sample_pk <- row_df
        } else {
            tnf_rows$sample_pk <- rbind(tnf_rows$sample_pk, row_df)
        }
    })
    
    # individual line for PK concentration
    output$sample_pk_con_indline_att <- renderUI({
        req(req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK concentration',
            input$sample_pk_con_tabs == 'Individual line',
            input$sample_pk_con_visit, input$sample_pk_con_subjid, 
            !is.null(input$sample_pk_con_group),
            !is.null(input$sample_pk_con_log))
        actionButton('sample_pk_con_indline_att', 'Add to TNF',
                     icon = icon('cloud-upload'))
    })
    observeEvent(input$sample_pk_con_indline_att, {
        if(is.null(n_tfl$figure)) n_tfl$figure <- 1
        else n_tfl$figure <- n_tfl$figure + 1
        if(is.null(n_tfl$sample_pk_con_indline))
            n_tfl$sample_pk_con_indline <- 1
        else n_tfl$sample_pk_con_indline <- n_tfl$sample_pk_con_indline + 1
        
        title_key <- paste0('sample_pk_individual_line_',
                            n_tfl$sample_pk_con_indline)
        tfln <- paste0('1.', n_tfl$figure)
        subjid <- paste(input$sample_pk_con_subjid, collapse = '\\n')
        row_list <- list(
            title_key, 'Figure', tfln, 'Individual line', 'PK concentration',
            NULL, input$sample_pk_con_visit, input$sample_pk_con_group,
            NULL, NULL, NULL, NULL, subjid, input$sample_pk_con_log,
            sample_pk_con_indline_xlab$value, sample_pk_con_indline_ylab$value,
            sample_pk_con_indline_main$value,
            sample_pk_con_indline_footnote$value
        )
        row_df <- setNames(
            data.frame(
                lapply(row_list, function(x) replace(x, is.null(x), NA)),
                check.names = FALSE, stringsAsFactors = FALSE
            ),
            out_sample_pk_cols
        )
        if(is.null(tnf_rows$sample_pk)) {
            tnf_rows$sample_pk <- row_df
        } else {
            tnf_rows$sample_pk <- rbind(tnf_rows$sample_pk, row_df)
        }
    })
    
    
    # summary table for PK concentration
    output$sample_pk_param_sumtable_att <- renderUI({
        data <- data_$sample_pk_param
        req(data, req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK parameter',
            req(input$sample_pk_param_tabs) == 'Summary table',
            input$sample_pk_param_param, sample_pk_param_decimal$value,
            !is.null(input$sample_pk_param_group))
        actionButton('sample_pk_param_sumtable_att', 'Add to TNF',
                     icon = icon('cloud-upload'))
    })
    observeEvent(input$sample_pk_param_sumtable_att, {
        if(is.null(n_tfl$table)) n_tfl$table <- 1
        else n_tfl$table <- n_tfl$table + 1
        if(is.null(n_tfl$sample_pk_param_sumtable))
            n_tfl$sample_pk_param_sumtable <- 1
        else n_tfl$sample_pk_param_sumtable <- n_tfl$sample_pk_param_sumtable + 1
        
        title_key <- paste0('sample_pk_summary_table_',
                            n_tfl$sample_pk_param_sumtable)
        tfln <- paste0('2.', n_tfl$table)
        row_list <- list(
            title_key, 'Table', tfln, 'Summary table', 'PK parameter',
            input$sample_pk_param_param, NULL, input$sample_pk_param_group,
            input$sample_pk_param_decimal, sample_pk_param_table_title$value,
            sample_pk_param_table_footnote$value, NULL, NULL, NULL, NULL,
            NULL, NULL, NULL
        )
        row_df <- setNames(
            data.frame(
                lapply(row_list, function(x) replace(x, is.null(x), NA)),
                check.names = FALSE, stringsAsFactors = FALSE
            ),
            out_sample_pk_cols
        )
        if(is.null(tnf_rows$sample_pk)) {
            tnf_rows$sample_pk <- row_df
        } else {
            tnf_rows$sample_pk <- rbind(tnf_rows$sample_pk, row_df)
        }
    })
    
    # summary line for PK concentration
    output$sample_pk_param_sumline_att <- renderUI({
        req(input$file_sample_pk_type == 'PK parameter',
            input$sample_pk_param_tabs == 'Summary line',
            input$sample_pk_param_param, input$sample_pk_param_summary,
            !is.null(input$sample_pk_param_group),
            !is.null(input$sample_pk_param_log))
        actionButton('sample_pk_param_sumline_att', 'Add to TNF',
                     icon = icon('cloud-upload'))
    })
    observeEvent(input$sample_pk_param_sumline_att, {
        if(is.null(n_tfl$figure)) n_tfl$figure <- 1
        else n_tfl$figure <- n_tfl$figure + 1
        if(is.null(n_tfl$sample_pk_param_sumline))
            n_tfl$sample_pk_param_sumline <- 1
        else n_tfl$sample_pk_param_sumline <- n_tfl$sample_pk_param_sumline + 1
        
        title_key <- paste0('sample_pk_summary_line_',
                            n_tfl$sample_pk_param_sumline)
        tfln <- paste0('1.', n_tfl$figure)
        row_list <- list(
            title_key, 'Figure', tfln, 'Summary line', 'PK parameter',
            input$sample_pk_param_param, NULL, input$sample_pk_param_group,
            NULL, NULL, NULL, input$sample_pk_param_summary, NULL,
            input$sample_pk_param_log, sample_pk_param_sumline_xlab$value,
            sample_pk_param_sumline_ylab$value, sample_pk_param_sumline_main$value,
            sample_pk_param_sumline_footnote$value
        )
        row_df <- setNames(
            data.frame(
                lapply(row_list, function(x) replace(x, is.null(x), NA)),
                check.names = FALSE, stringsAsFactors = FALSE
            ),
            out_sample_pk_cols
        )
        if(is.null(tnf_rows$sample_pk)) {
            tnf_rows$sample_pk <- row_df
        } else {
            tnf_rows$sample_pk <- rbind(tnf_rows$sample_pk, row_df)
        }
    })
    
    # individual line for PK concentration
    output$sample_pk_param_indline_att <- renderUI({
        req(req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK parameter',
            input$sample_pk_param_tabs == 'Individual line',
            input$sample_pk_param_param, input$sample_pk_param_subjid,
            !is.null(input$sample_pk_param_group),
            !is.null(input$sample_pk_param_log))
        actionButton('sample_pk_param_indline_att', 'Add to TNF',
                     icon = icon('cloud-upload'))
    })
    observeEvent(input$sample_pk_param_indline_att, {
        if(is.null(n_tfl$figure)) n_tfl$figure <- 1
        else n_tfl$figure <- n_tfl$figure + 1
        if(is.null(n_tfl$sample_pk_param_indline))
            n_tfl$sample_pk_param_indline <- 1
        else n_tfl$sample_pk_param_indline <- n_tfl$sample_pk_param_indline + 1
        
        title_key <- paste0('sample_pk_individual_line_',
                            n_tfl$sample_pk_param_indline)
        tfln <- paste0('1.', n_tfl$figure)
        subjid <- paste(input$sample_pk_param_subjid, collapse = '\\n')
        row_list <- list(
            title_key, 'Figure', tfln, 'Individual line', 'PK parameter',
            input$sample_pk_param_param, NULL, input$sample_pk_param_group,
            NULL, NULL, NULL, NULL, subjid, input$sample_pk_param_log,
            sample_pk_param_indline_xlab$value, sample_pk_param_indline_ylab$value,
            sample_pk_param_indline_main$value,
            sample_pk_param_indline_footnote$value
        )
        row_df <- setNames(
            data.frame(
                lapply(row_list, function(x) replace(x, is.null(x), NA)),
                check.names = FALSE, stringsAsFactors = FALSE
            ),
            out_sample_pk_cols
        )
        if(is.null(tnf_rows$sample_pk)) {
            tnf_rows$sample_pk <- row_df
        } else {
            tnf_rows$sample_pk <- rbind(tnf_rows$sample_pk, row_df)
        }
    })
    
    # summary table for PD analysis
    output$sample_pd_time_sumtable_att <- renderUI({
        data <- data_$sample_pd
        req(data, req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '1 PD analysis',
            req(input$sample_pd_time_tabs) == 'Summary table',
            input$sample_pd_time_param, input$sample_pd_time_y,
            input$sample_pd_time_x,
            (input$sample_pd_time_x == sample_pd_avisitn_col ||
                 (input$sample_pd_time_x == sample_pd_atptn_col &&
                      !is_blank(input$sample_pd_time_visit))),
            sample_pd_time_decimal$value,
            !is.null(input$sample_pd_time_group))
        actionButton('sample_pd_time_sumtable_att', 'Add to TNF',
                     icon = icon('cloud-upload'))
    })
    observeEvent(input$sample_pd_time_sumtable_att, {
        if(is.null(n_tfl$table)) n_tfl$table <- 1
        else n_tfl$table <- n_tfl$table + 1
        if(is.null(n_tfl$sample_pd_time_sumtable))
            n_tfl$sample_pd_time_sumtable <- 1
        else n_tfl$sample_pd_time_sumtable <- n_tfl$sample_pd_time_sumtable + 1
        
        title_key <- paste0('sample_pd_summary_table_',
                            n_tfl$sample_pd_time_sumtable)
        tfln <- paste0('2.', n_tfl$table)
        row_list <- list(
            title_key, 'Table', tfln, 'Summary table',
            input$sample_pd_time_param, input$sample_pd_time_y, NULL, NULL,
            input$sample_pd_time_x, input$sample_pd_time_visit,
            input$sample_pd_time_group, input$sample_pd_time_decimal,
            sample_pd_time_table_title$value,
            sample_pd_time_table_footnote$value, NULL, NULL, NULL, NULL, NULL,
            NULL, NULL, NULL, NULL, NULL, NULL
        )
        row_df <- setNames(
            data.frame(
                lapply(row_list, function(x) replace(x, is.null(x), NA)),
                check.names = FALSE, stringsAsFactors = FALSE
            ),
            out_sample_pd_cols
        )
        if(is.null(tnf_rows$sample_pd)) {
            tnf_rows$sample_pd <- row_df
        } else {
            tnf_rows$sample_pd <- rbind(tnf_rows$sample_pd, row_df)
        }
    })
    
    # summary line for PD analysis
    output$sample_pd_time_sumline_att <- renderUI({
        is_visit <- input$sample_pd_time_x == sample_pd_avisitn_col
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '1 PD analysis',
            input$sample_pd_time_param, input$sample_pd_time_y,
            input$sample_pd_time_summary,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_time_visit))),
            !is.null(input$sample_pd_time_group),
            !is.null(input$sample_pd_time_log),
            req(input$sample_pd_time_tabs) == 'Summary line')
        actionButton('sample_pd_time_sumline_att', 'Add to TNF',
                     icon = icon('cloud-upload'))
    })
    observeEvent(input$sample_pd_time_sumline_att, {
        if(is.null(n_tfl$figure)) n_tfl$figure <- 1
        else n_tfl$figure <- n_tfl$figure + 1
        if(is.null(n_tfl$sample_pd_time_sumline))
            n_tfl$sample_pd_time_sumline <- 1
        else n_tfl$sample_pd_time_sumline <- n_tfl$sample_pd_time_sumline + 1
        
        title_key <- paste0('sample_pd_summary_line_',
                            n_tfl$sample_pd_time_sumline)
        tfln <- paste0('1.', n_tfl$figure)
        row_list <- list(
            title_key, 'Figure', tfln, 'Summary line',
            input$sample_pd_time_param, input$sample_pd_time_y, NULL, NULL,
            input$sample_pd_time_x, input$sample_pd_time_visit,
            input$sample_pd_time_group, NULL, NULL, NULL,
            input$sample_pd_time_summary, NULL, NULL, input$sample_pd_time_log,
            NULL, NULL, sample_pd_time_sumline_xlab$value,
            sample_pd_time_sumline_ylab$value,
            NULL, sample_pd_time_sumline_main$value,
            sample_pd_time_sumline_footnote$value
        )
        row_df <- setNames(
            data.frame(
                lapply(row_list, function(x) replace(x, is.null(x), NA)),
                check.names = FALSE, stringsAsFactors = FALSE
            ),
            out_sample_pd_cols
        )
        if(is.null(tnf_rows$sample_pd)) {
            tnf_rows$sample_pd <- row_df
        } else {
            tnf_rows$sample_pd <- rbind(tnf_rows$sample_pd, row_df)
        }
    })
    
    # individual line for PD analysis
    output$sample_pd_time_indline_att <- renderUI({
        is_visit <- input$sample_pd_time_x == sample_pd_avisitn_col
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '1 PD analysis',
            input$sample_pd_time_param, input$sample_pd_time_y,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_time_visit))),
            input$sample_pd_time_subjid, !is.null(input$sample_pd_time_group),
            !is.null(input$sample_pd_time_log),
            req(input$sample_pd_time_tabs) == 'Individual line')
        actionButton('sample_pd_time_indline_att', 'Add to TNF',
                     icon = icon('cloud-upload'))
    })
    observeEvent(input$sample_pd_time_indline_att, {
        if(is.null(n_tfl$figure)) n_tfl$figure <- 1
        else n_tfl$figure <- n_tfl$figure + 1
        if(is.null(n_tfl$sample_pd_time_indline))
            n_tfl$sample_pd_time_indline <- 1
        else n_tfl$sample_pd_time_indline <- n_tfl$sample_pd_time_indline + 1
        
        title_key <- paste0('sample_pd_individual_line_',
                            n_tfl$sample_pd_time_indline)
        tfln <- paste0('1.', n_tfl$figure)
        subjid <- paste(input$sample_pd_time_subjid, collapse = '\\n')
        row_list <- list(
            title_key, 'Figure', tfln, 'Individual line',
            input$sample_pd_time_param, input$sample_pd_time_y, NULL, NULL,
            input$sample_pd_time_x, input$sample_pd_time_visit,
            input$sample_pd_time_group, NULL, NULL, NULL,
            NULL, subjid, NULL, input$sample_pd_time_log,
            NULL, NULL, sample_pd_time_indline_xlab$value,
            sample_pd_time_indline_ylab$value,
            NULL, sample_pd_time_indline_main$value,
            sample_pd_time_indline_footnote$value
        )
        row_df <- setNames(
            data.frame(
                lapply(row_list, function(x) replace(x, is.null(x), NA)),
                check.names = FALSE, stringsAsFactors = FALSE
            ),
            out_sample_pd_cols
        )
        if(is.null(tnf_rows$sample_pd)) {
            tnf_rows$sample_pd <- row_df
        } else {
            tnf_rows$sample_pd <- rbind(tnf_rows$sample_pd, row_df)
        }
    })
    
    # forest plot for PD analysis
    output$sample_pd_corr_sumline_att <- renderUI({
        is_visit <- input$sample_pd_corr_x == sample_pd_avisitn_col
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '2 PD analysis',
            input$sample_pd_corr_param1, input$sample_pd_corr_y1,
            input$sample_pd_corr_param2, input$sample_pd_corr_y2,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_corr_visit))),
            input$sample_pd_corr_summary,
            !is.null(input$sample_pd_corr_group), 
            !is.null(input$sample_pd_corr_log_1),
            !is.null(input$sample_pd_corr_log_2),
            req(input$sample_pd_corr_tabs) == 'Summary line')
        actionButton('sample_pd_corr_sumline_att', 'Add to TNF',
                     icon = icon('cloud-upload'))
    })
    observeEvent(input$sample_pd_corr_sumline_att, {
        if(is.null(n_tfl$figure)) n_tfl$figure <- 1
        else n_tfl$figure <- n_tfl$figure + 1
        if(is.null(n_tfl$sample_pd_corr_sumline))
            n_tfl$sample_pd_corr_sumline <- 1
        else n_tfl$sample_pd_corr_sumline <- n_tfl$sample_pd_corr_sumline + 1
        
        title_key <- paste0('sample_pd_forest_plot_',
                            n_tfl$sample_pd_corr_sumline)
        tfln <- paste0('1.', n_tfl$figure)
        row_list <- list(
            title_key, 'Figure', tfln, 'Forest plot',
            input$sample_pd_corr_param1, input$sample_pd_corr_y1,
            input$sample_pd_corr_param2, input$sample_pd_corr_y2,
            input$sample_pd_corr_x, input$sample_pd_corr_visit,
            input$sample_pd_corr_group, NULL, NULL, NULL,
            input$sample_pd_corr_summary, NULL, NULL,
            input$sample_pd_corr_log_1, input$sample_pd_corr_log_2,
            sample_pd_corr_sumline_same_y$value,
            sample_pd_corr_sumline_xlab$value,
            sample_pd_corr_sumline_ylabl$value,
            sample_pd_corr_sumline_ylabr$value,
            sample_pd_corr_sumline_main$value,
            sample_pd_corr_sumline_footnote$value
        )
        row_df <- setNames(
            data.frame(
                lapply(row_list, function(x) replace(x, is.null(x), NA)),
                check.names = FALSE, stringsAsFactors = FALSE
            ),
            out_sample_pd_cols
        )
        if(is.null(tnf_rows$sample_pd)) {
            tnf_rows$sample_pd <- row_df
        } else {
            tnf_rows$sample_pd <- rbind(tnf_rows$sample_pd, row_df)
        }
    })
    
    # scatter plot for PD analysis
    output$sample_pd_corr_scatter_att <- renderUI({
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '2 PD analysis',
            input$sample_pd_corr_param1, input$sample_pd_corr_y1,
            input$sample_pd_corr_param2, input$sample_pd_corr_y2,
            !is.null(input$sample_pd_corr_group),
            !is.null(input$sample_pd_corr_refline),
            !is.null(input$sample_pd_corr_log_1),
            !is.null(input$sample_pd_corr_log_2),
            req(input$sample_pd_corr_tabs) == 'Scatter plot')
        actionButton('sample_pd_corr_scatter_att', 'Add to TNF',
                     icon = icon('cloud-upload'))
    })
    observeEvent(input$sample_pd_corr_scatter_att, {
        if(is.null(n_tfl$figure)) n_tfl$figure <- 1
        else n_tfl$figure <- n_tfl$figure + 1
        if(is.null(n_tfl$sample_pd_corr_scatter))
            n_tfl$sample_pd_corr_scatter <- 1
        else n_tfl$sample_pd_corr_scatter <- n_tfl$sample_pd_corr_scatter + 1
        
        title_key <- paste0('sample_pd_scatter_plot_',
                            n_tfl$sample_pd_corr_scatter)
        tfln <- paste0('1.', n_tfl$figure)
        row_list <- list(
            title_key, 'Figure', tfln, 'Scatter plot',
            input$sample_pd_corr_param1, input$sample_pd_corr_y1,
            input$sample_pd_corr_param2, input$sample_pd_corr_y2,
            NULL, NULL, input$sample_pd_corr_group, NULL, NULL, NULL,
            NULL, NULL, input$sample_pd_corr_refline,
            input$sample_pd_corr_log_1, input$sample_pd_corr_log_2, NULL,
            sample_pd_corr_scatter_xlab$value,
            sample_pd_corr_scatter_ylab$value, NULL,
            sample_pd_corr_scatter_main$value,
            sample_pd_corr_scatter_footnote$value
        )
        row_df <- setNames(
            data.frame(
                lapply(row_list, function(x) replace(x, is.null(x), NA)),
                check.names = FALSE, stringsAsFactors = FALSE
            ),
            out_sample_pd_cols
        )
        if(is.null(tnf_rows$sample_pd)) {
            tnf_rows$sample_pd <- row_df
        } else {
            tnf_rows$sample_pd <- rbind(tnf_rows$sample_pd, row_df)
        }
    })
    
    
    # forest plot for PK - PD analysis
    output$sample_pkpd_sumline_att <- renderUI({
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        is_visit <- input$sample_pkpd_x == sample_pk_con_visit_col
        req((is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            (is_visit || (!is_visit && !is_blank(input$sample_pkpd_visit))),
            input$sample_pkpd_summary,
            !is.null(input$sample_pkpd_group), 
            !is.null(input$sample_pkpd_log_pk),
            !is.null(input$sample_pkpd_log_pd),
            req(input$sample_pkpd_tabs) == 'Summary line')
        actionButton('sample_pkpd_sumline_att', 'Add to TNF',
                     icon = icon('cloud-upload'))
    })
    observeEvent(input$sample_pkpd_sumline_att, {
        if(is.null(n_tfl$figure)) n_tfl$figure <- 1
        else n_tfl$figure <- n_tfl$figure + 1
        if(is.null(n_tfl$sample_pkpd_sumline))
            n_tfl$sample_pkpd_sumline <- 1
        else n_tfl$sample_pkpd_sumline <- n_tfl$sample_pkpd_sumline + 1
        
        title_key <- paste0('sample_pkpd_forest_plot_',
                            n_tfl$sample_pkpd_sumline)
        tfln <- paste0('1.', n_tfl$figure)
        row_list <- list(
            title_key, 'Figure', tfln, 'Forest plot', input$file_sample_pk_type,
            input$sample_pkpd_pk, input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            input$sample_pkpd_x, input$sample_pkpd_visit,
            input$sample_pkpd_group, input$sample_pkpd_summary, NULL,
            NULL, NULL, input$sample_pkpd_log_pk, input$sample_pkpd_log_pd,
            sample_pkpd_sumline_same_y$value,
            sample_pkpd_sumline_xlab$value, sample_pkpd_sumline_ylabl$value,
            sample_pkpd_sumline_ylabr$value, sample_pkpd_sumline_main$value,
            sample_pkpd_sumline_footnote$value
        )
        row_df <- setNames(
            data.frame(
                lapply(row_list, function(x) replace(x, is.null(x), NA)),
                check.names = FALSE, stringsAsFactors = FALSE
            ),
            out_sample_pkpd_cols
        )
        if(is.null(tnf_rows$sample_pkpd)) {
            tnf_rows$sample_pkpd <- row_df
        } else {
            tnf_rows$sample_pkpd <- rbind(tnf_rows$sample_pkpd, row_df)
        }
    })
    
    # scatter plot for PK - PD analysis
    output$sample_pkpd_scatter_att <- renderUI({
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        req(data, (is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            !is.null(input$sample_pkpd_refline),
            !is.null(input$sample_pkpd_group), 
            !is.null(input$sample_pkpd_log_pk),
            !is.null(input$sample_pkpd_log_pd),
            req(input$sample_pkpd_tabs) == 'Scatter plot')
        actionButton('sample_pkpd_scatter_att', 'Add to TNF',
                     icon = icon('cloud-upload'))
    })
    observeEvent(input$sample_pkpd_scatter_att, {
        if(is.null(n_tfl$figure)) n_tfl$figure <- 1
        else n_tfl$figure <- n_tfl$figure + 1
        if(is.null(n_tfl$sample_pkpd_scatter))
            n_tfl$sample_pkpd_scatter <- 1
        else n_tfl$sample_pkpd_scatter <- n_tfl$sample_pkpd_scatter + 1
        
        title_key <- paste0('sample_pkpd_scatter_plot_',
                            n_tfl$sample_pkpd_scatter)
        tfln <- paste0('1.', n_tfl$figure)
        row_list <- list(
            title_key, 'Figure', tfln, 'Scatter plot', input$file_sample_pk_type,
            input$sample_pkpd_pk, input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            NULL, NULL, input$sample_pkpd_group, NULL, input$sample_pkpd_refline,
            NULL, NULL, input$sample_pkpd_log_pk, input$sample_pkpd_log_pd,
            NULL, sample_pkpd_scatter_xlab$value, sample_pkpd_scatter_ylab$value,
            NULL, sample_pkpd_scatter_main$value,
            sample_pkpd_scatter_footnote$value
        )
        row_df <- setNames(
            data.frame(
                lapply(row_list, function(x) replace(x, is.null(x), NA)),
                check.names = FALSE, stringsAsFactors = FALSE
            ),
            out_sample_pkpd_cols
        )
        if(is.null(tnf_rows$sample_pkpd)) {
            tnf_rows$sample_pkpd <- row_df
        } else {
            tnf_rows$sample_pkpd <- rbind(tnf_rows$sample_pkpd, row_df)
        }
    })
    
    # quartile plot for PK - PD analysis
    output$sample_pkpd_quartile_att <- renderUI({
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        req(data, (is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            !is.null(input$sample_pkpd_group),
            !is.null(input$sample_pkpd_add_points),
            !is.null(input$sample_pkpd_add_line),
            !is.null(input$sample_pkpd_log_pd),
            req(input$sample_pkpd_tabs) == 'Quartile plot')
        actionButton('sample_pkpd_quartile_att', 'Add to TNF',
                     icon = icon('cloud-upload'))
    })
    observeEvent(input$sample_pkpd_quartile_att, {
        if(is.null(n_tfl$figure)) n_tfl$figure <- 1
        else n_tfl$figure <- n_tfl$figure + 1
        if(is.null(n_tfl$sample_pkpd_quartile))
            n_tfl$sample_pkpd_quartile <- 1
        else n_tfl$sample_pkpd_quartile <- n_tfl$sample_pkpd_quartile + 1
        
        title_key <- paste0('sample_pkpd_quartile_plot_',
                            n_tfl$sample_pkpd_quartile)
        tfln <- paste0('1.', n_tfl$figure)
        row_list <- list(
            title_key, 'Figure', tfln, 'Quartile plot', input$file_sample_pk_type,
            input$sample_pkpd_pk, input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            NULL, NULL, input$sample_pkpd_group, NULL, NULL,
            input$sample_pkpd_add_points, input$sample_pkpd_add_line,
            NULL, input$sample_pkpd_log_pd, NULL, sample_pkpd_quartile_xlab$value,
            sample_pkpd_quartile_ylab$value, NULL,
            sample_pkpd_quartile_main$value,
            sample_pkpd_quartile_footnote$value
        )
        row_df <- setNames(
            data.frame(
                lapply(row_list, function(x) replace(x, is.null(x), NA)),
                check.names = FALSE, stringsAsFactors = FALSE
            ),
            out_sample_pkpd_cols
        )
        if(is.null(tnf_rows$sample_pkpd)) {
            tnf_rows$sample_pkpd <- row_df
        } else {
            tnf_rows$sample_pkpd <- rbind(tnf_rows$sample_pkpd, row_df)
        }
    })
    
    #-----------------------------------------------
    # Output for PK concentration time profile
    
    # a tabset UI for lab and AE safety analysis output
    output$sample_tabpanel <- renderUI({
        req(input$sample_panel)
        if(input$sample_panel == 'PK analysis') {
            if(req(input$file_sample_pk_type) == 'PK concentration') {
                req(data_import_status$sample_pk_con, input$sample_pk_con_visit)
                tabsetPanel(
                    tabPanel('Summary table', uiOutput('sample_pk_con_sumtable')),
                    tabPanel('Summary line', uiOutput('sample_pk_con_sumline_ui')),
                    tabPanel('Individual line', uiOutput('sample_pk_con_indline_ui')),
                    id = 'sample_pk_con_tabs',
                    selected = isolate(
                        input$sample_pk_con_tabs[input$sample_pk_con_tabs %in%
                                                     sample_pk_con_tabnames]
                    )
                )
            } else if(req(input$file_sample_pk_type) == 'PK parameter') {
                req(data_import_status$sample_pk_param, input$sample_pk_param_param)
                tabsetPanel(
                    tabPanel('Summary table', uiOutput('sample_pk_param_sumtable')),
                    tabPanel('Summary line', uiOutput('sample_pk_param_sumline_ui')),
                    tabPanel('Individual line', uiOutput('sample_pk_param_indline_ui')),
                    id = 'sample_pk_param_tabs',
                    selected = isolate(
                        input$sample_pk_param_tabs[input$sample_pk_param_tabs %in%
                                                       sample_pk_param_tabnames]
                    )
                )
            }
        } else if(input$sample_panel == 'PD analysis') {
            if(req(input$sample_pd_analysis_type) == '1 PD analysis') {
                req(data_import_status$sample_pd, input$sample_pd_time_param,
                    input$sample_pd_time_x,
                    (input$sample_pd_time_x == sample_pd_avisitn_col ||
                         (input$sample_pd_time_x == sample_pd_atptn_col &&
                              !is_blank(input$sample_pd_time_visit))))
                tabsetPanel(
                    tabPanel('Summary table', uiOutput('sample_pd_time_sumtable_ui')),
                    tabPanel('Summary line', uiOutput('sample_pd_time_sumline_ui')),
                    tabPanel('Individual line', uiOutput('sample_pd_time_indline_ui')),
                    id = 'sample_pd_time_tabs',
                    selected = isolate(
                        input$sample_pd_time_tabs[input$sample_pd_time_tabs %in%
                                                      sample_pd_time_tabnames]
                    )
                )
            } else if(req(input$sample_pd_analysis_type) == '2 PD analysis') {
                req(data_import_status$sample_pd, input$sample_pd_corr_param1,
                    input$sample_pd_corr_param2)
                tabsetPanel(
                    tabPanel('Summary line', plotOutput('sample_pd_corr_sumline')),
                    tabPanel('Scatter plot', uiOutput('sample_pd_corr_scatter_ui')),
                    id = 'sample_pd_corr_tabs',
                    selected = isolate(
                        input$sample_pd_corr_tabs[input$sample_pd_corr_tabs %in%
                                                      sample_pd_corr_tabnames]
                    )
                )
            }
        } else if(input$sample_panel == 'PK-PD analysis') {
            req(data_import_status$sample_pkpd,
                ((input$file_sample_pk_type == 'PK parameter' &&
                      !is_blank(input$sample_pkpd_pk)) ||
                     input$file_sample_pk_type == 'PK concentration'),
                input$sample_pkpd_pd)
            tabsetPanel(
                tabPanel('Summary line', plotOutput('sample_pkpd_sumline')),
                tabPanel('Scatter plot', uiOutput('sample_pkpd_scatter_ui')),
                tabPanel('Quartile plot', uiOutput('sample_pkpd_quartile_ui')),
                id = 'sample_pkpd_tabs',
                selected = isolate(
                    input$sample_pkpd_tabs[input$sample_pkpd_tabs %in%
                                               sample_pkpd_tabnames]
                )
            )
        }
    })
    
    # summary table for PK concentration time profile
    sample_pk_con_sumtable <- reactive({
        data <- data_$sample_pk_con
        req(data, input$sample_pk_con_visit, sample_pk_con_decimal$value,
            !is.null(input$sample_pk_con_group))
        req(!is.null(sample_pk_con_table_title$value),
            !is.null(sample_pk_con_table_footnote$value))
        data <- data[
            !is.na(data[[sample_pk_con_time_col]]) &
                !is.na(data[[sample_pk_con_con_col]]), , drop = FALSE
        ]
        cond <- data[[sample_pk_param_visit_col]] %in% input$sample_pk_con_visit
        data <- data[cond, , drop = FALSE]
        if(!is_blank(input$sample_pk_con_group)) {
            data[[input$sample_pk_con_group]] <- as.factor(
                data[[input$sample_pk_con_group]]
            )
        }
        data[[sample_pk_con_con_col]] <- as.numeric(
            data[[sample_pk_con_con_col]]
        )
        data[[sample_pk_param_visit_col]] <- as.numeric(
            data[[sample_pk_param_visit_col]]
        )
        data[[sample_pk_con_time_col]] <- as.numeric(
            data[[sample_pk_con_time_col]]
        )
        data[['time_']] <- paste0(
            'Visit ', to_character(data[[sample_pk_param_visit_col]]), ', ',
            'Timepoint ', to_character(data[[sample_pk_con_time_col]])
        )
        time_order <- order(data[[sample_pk_param_visit_col]],
                            data[[sample_pk_con_time_col]])
        data[['time_']] <- factor(
            data[['time_']], levels = unique(data[['time_']][time_order])
        )
        
        dgt <- sample_pk_con_decimal$value
        sample_pk_con_summary_func <- c(
            'N' = n_nna,
            'Mean (SD)' = partial(mean_sd_str, digits = dgt),
            '%CV' = partial(coeff_var_str, digits = dgt),
            'Median' = partial(median_str, digits = dgt),
            'Q1, Q3' = partial(q1_q3_str, digits = dgt),
            'Min, Max' = partial(min_max_str, digits = dgt),
            'Geom Mean' = partial(geo_mean_str, digits = dgt),
            'Mean (SD) of LN' = partial(mean_sd_ln_str, digits = dgt)
        )
        if(!is_blank(input$sample_pk_con_group)) {
            data <- select_(data, sample_pk_con_con_col, 'time_',
                            input$sample_pk_con_group)
            summary_tbl <- summary_table_2d(
                data, 'time_', input$sample_pk_con_group,
                row_names = levels(data[['time_']]),
                col_names = paste(input$sample_pk_con_group, '=',
                                  levels(data[[input$sample_pk_con_group]])),
                rowlabel = '', caption = sample_pk_con_table_title$value,
                footnote = sample_pk_con_table_footnote$value,
                func_list = sample_pk_con_summary_func
            )
        } else {
            data <- select_(data, sample_pk_con_con_col, 'time_')
            summary_tbl <- summary_table_row(
                data, 'time_', row_names = levels(data[['time_']]),
                rowlabel = '', caption = sample_pk_con_table_title$value,
                footnote = sample_pk_con_table_footnote$value,
                func_list = sample_pk_con_summary_func
            )
        }
        return(summary_tbl)
    })
    output$sample_pk_con_sumtable <- renderUI({ HTML(sample_pk_con_sumtable()) })
    
    # download summary table for PK concentration time profile
    output$sample_pk_con_table_download_button <- renderUI({
        data <- data_$sample_pk_con
        req(data, req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK concentration',
            req(input$sample_pk_con_tabs) == 'Summary table',
            input$sample_pk_con_visit, sample_pk_con_decimal$value,
            !is.null(input$sample_pk_con_group))
        downloadButton('sample_pk_con_table_download', 'Download table')
    })
    output$sample_pk_con_table_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'sample_pk_summary_table_',
                format(Sys.Date(),format = '%Y%m%d'), '.rtf'
            )
            return(file_name)
        },
        content = function(file) {
            data <- data_$sample_pk_con
            data <- data[
                !is.na(data[[sample_pk_con_time_col]]) &
                    !is.na(data[[sample_pk_con_con_col]]), , drop = FALSE
            ]
            cond <- data[[sample_pk_param_visit_col]] %in% input$sample_pk_con_visit
            data <- data[cond, , drop = FALSE]
            if(!is_blank(input$sample_pk_con_group)) {
                data[[input$sample_pk_con_group]] <- as.factor(
                    data[[input$sample_pk_con_group]]
                )
            }
            data[[sample_pk_con_con_col]] <- as.numeric(
                data[[sample_pk_con_con_col]]
            )
            data[[sample_pk_param_visit_col]] <- as.numeric(
                data[[sample_pk_param_visit_col]]
            )
            data[[sample_pk_con_time_col]] <- as.numeric(
                data[[sample_pk_con_time_col]]
            )
            data[['time_']] <- paste0(
                'Visit ', to_character(data[[sample_pk_param_visit_col]]), ', ',
                'Timepoint ', to_character(data[[sample_pk_con_time_col]])
            )
            time_order <- order(data[[sample_pk_param_visit_col]],
                                data[[sample_pk_con_time_col]])
            data[['time_']] <- factor(
                data[['time_']], levels = unique(data[['time_']][time_order])
            )

            dgt <- sample_pk_con_decimal$value
            sample_pk_con_summary_func <- c(
                'N' = n_nna,
                'Mean (SD)' = partial(mean_sd_str, digits = dgt),
                '%CV' = partial(coeff_var_str, digits = dgt),
                'Median' = partial(median_str, digits = dgt),
                'Q1, Q3' = partial(q1_q3_str, digits = dgt),
                'Min, Max' = partial(min_max_str, digits = dgt),
                'Geom Mean' = partial(geo_mean_str, digits = dgt),
                'Mean (SD) of LN' = partial(mean_sd_ln_str, digits = dgt)
            )
            if(!is_blank(input$sample_pk_con_group)) {
                data <- select_(data, sample_pk_con_con_col,
                                'time_', input$sample_pk_con_group)
            } else {
                data <- select_(data, sample_pk_con_con_col, 'time_')
            }
            summary_table <- summary_table_all(
                data, row_var = 'time_',
                col_var = input$sample_pk_con_group,
                col_names = paste(input$sample_pk_con_group, '=',
                                  levels(data[[input$sample_pk_con_group]])),
                val_var = sample_pk_con_con_col,
                n_in_header = FALSE, func_list = sample_pk_con_summary_func,
                caption = sample_pk_con_table_title$value,
                footnote = sample_pk_con_table_footnote$value,
                rowlabel = ' ', format = 'rtf'
            )
            rtf_table_wrapper(
                file, summary_table, block_break = TRUE,
                nline_block = length(sample_pk_con_summary_func) + 1,
                caption = sample_pk_con_table_title$value,
                footnote = sample_pk_con_table_footnote$value
            )
        }
    )
    
    # summary line for PK concentration time profile
    sample_pk_con_sumline <- reactive({
        req(input$sample_pk_con_visit, input$sample_pk_con_summary,
            !is.null(input$sample_pk_con_group), !is.null(input$sample_pk_con_log))
        req(!is.null(sample_pk_con_sumline_xlab$value),
            !is.null(sample_pk_con_sumline_ylab$value),
            !is.null(sample_pk_con_sumline_main$value),
            !is.null(sample_pk_con_sumline_footnote$value))
        data <- data_$sample_pk_con
        data <- data[
            !is.na(data[[sample_pk_con_time_col]]) &
                !is.na(data[[sample_pk_con_con_col]]), , drop = FALSE
        ]
        cond <- data[[sample_pk_param_visit_col]]%in%input$sample_pk_con_visit
        data <- data[cond, , drop = FALSE]
        
        if(!is_blank(input$sample_pk_con_group)) {
            data[[input$sample_pk_con_group]] <- as.factor(
                data[[input$sample_pk_con_group]]
            )
        }
        data[[sample_pk_con_con_col]] <- as.numeric(data[[sample_pk_con_con_col]])
        if(length(input$sample_pk_con_visit) > 1) {
            data[[sample_pk_param_visit_col]] <- as.factor(
                data[[sample_pk_param_visit_col]]
            )
            levels(data[[sample_pk_param_visit_col]]) <- paste(
                sample_pk_param_visit_col, '=',
                levels(data[[sample_pk_param_visit_col]])
            )
        }
        if(!is_blank(input$sample_pk_con_group)) {
            data <- group_by_(data, sample_pk_param_visit_col,
                              input$sample_pk_con_group,
                              sample_pk_con_time_col)
        } else {
            data <- group_by_(data, sample_pk_param_visit_col,
                              sample_pk_con_time_col)
        }
        
        if(input$sample_pk_con_summary %in% c('Mean + SD', 'Mean + SE')) {
            avg_expr <- ~mean_na(var)
            if(input$sample_pk_con_summary == 'Mean + SD') {
                lower_expr <- ~mean_na(var) - sd_na(var)
                upper_expr <- ~mean_na(var) + sd_na(var)
            } else {
                lower_expr <- ~mean_na(var) - std_err(var)
                upper_expr <- ~mean_na(var) + std_err(var)
            }
        } else if(input$sample_pk_con_summary == 'Median + IQR') {
            avg_expr <- ~median_na(var)
            lower_expr <- ~q1_na(var)
            upper_expr <- ~q3_na(var)
        }
        expr <- list(
            lazyeval::interp(avg_expr, var = as.name(sample_pk_con_con_col)),
            lazyeval::interp(lower_expr, var = as.name(sample_pk_con_con_col)),
            lazyeval::interp(upper_expr, var = as.name(sample_pk_con_con_col))
        )
        dots <- setNames(expr, c('avg', 'lower', 'upper'))
        data <- data %>% summarise_(.dots = dots)
        data[[sample_pk_con_time_col]] <- as.numeric(data[[sample_pk_con_time_col]])
        
        timepoint <- sort(unique(data[[sample_pk_con_time_col]]))
        ngroups <- ifelse(is_blank(input$sample_pk_con_group), 1,
                          length(unique(data[[input$sample_pk_con_group]])))
        dodge_width <- min(
            diff(range(timepoint)) / 40, min(diff(timepoint)) / (3 * (ngroups - 1))
        )
        plt <- gg_wrapper(
            data, aes_string(x = sample_pk_con_time_col, y = 'avg'),
            log_y = input$sample_pk_con_log
        )
        if(length(input$sample_pk_con_visit) > 1) {
            plt <- plt +
                facet_grid(formula(paste(sample_pk_param_visit_col, '~ .')))
        }
        if(!is_blank(input$sample_pk_con_group)) {
            plt <- plt + aes_string(colour = input$sample_pk_con_group)
        }
        plt <- plt +
            geom_line(position = position_dodge(dodge_width)) +
            geom_point(position = position_dodge(dodge_width), size = 2) +
            labs(x = sample_pk_con_sumline_xlab$value,
                 y = sample_pk_con_sumline_ylab$value,
                 title = sample_pk_con_sumline_main$value)
        
        if(any(!is.na(data$lower) & !is.na(data$upper))) {
            plt <- plt + geom_errorbar(aes(ymin = lower, ymax = upper),
                                       width = dodge_width,
                                       position = position_dodge(dodge_width))
        }
        # plt <- add_footnote(plt, sample_pk_con_sumline_footnote$value)
        return(plt)
    })
    output$sample_pk_con_sumline_ui <- renderUI({
        # req(sample_pk_con_sumline())
        shiny::tagList(
            plotOutput('sample_pk_con_sumline'),
            uiOutput('sample_pk_con_sumline_fn_out'),
            tags$head(tags$style(
                "#sample_pk_con_sumline_fn_out{font-size: 9px;}"
            ))
        )
    })
    output$sample_pk_con_sumline <- renderPlot({
        # grid.draw(sample_pk_con_sumline())
        sample_pk_con_sumline()
    })
    output$sample_pk_con_sumline_fn_out <- renderUI({
        req(sample_pk_con_sumline_footnote$value)
        HTML(paste(
            strsplit(sample_pk_con_sumline_footnote$value, '\n')[[1]],
            collapse = '<br/>'
        ))
    })
    
    # download summary line for PK concentration time profile
    output$sample_pk_con_sumline_download_button <- renderUI({
        req(req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK concentration',
            input$sample_pk_con_tabs == 'Summary line',
            input$sample_pk_con_visit, input$sample_pk_con_summary,
            !is.null(input$sample_pk_con_group),
            !is.null(input$sample_pk_con_log))
        downloadButton('sample_pk_con_sumline_download', 'Download plot')
    })
    output$sample_pk_con_sumline_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'sample_pk_summary_line_',
                format(Sys.Date(),format = '%Y%m%d'), '.pdf'
            )
            return(file_name)
        },
        content = function(file) {
            plot_ <- sample_pk_con_sumline()
            plot_ <- add_footnote(plot_, sample_pk_con_sumline_footnote$value)
            height_width_ratio <- 3 / 4
            width <- 8
            height <- width * height_width_ratio
            ggsave(filename = file, plot = plot_, width = width,
                   height = height, dpi = 600)
        }
    )
    
    # individual line for PK concentration time profile
    sample_pk_con_indline <- reactive({
        req(input$sample_pk_con_visit, input$sample_pk_con_subjid, 
            !is.null(input$sample_pk_con_group), !is.null(input$sample_pk_con_log))
        req(!is.null(sample_pk_con_indline_xlab$value),
            !is.null(sample_pk_con_indline_ylab$value),
            !is.null(sample_pk_con_indline_main$value),
            !is.null(sample_pk_con_indline_footnote$value))
        data <- data_$sample_pk_con
        data <- data[
            !is.na(data[[sample_pk_con_time_col]]) &
                !is.na(data[[sample_pk_con_con_col]]), , drop = FALSE
        ]
        data <- data[
            data[[sample_pk_param_visit_col]] %in% input$sample_pk_con_visit, ,
            drop = FALSE
        ]
        data <- data[
            data[[sample_pk_con_subj_col]] %in% input$sample_pk_con_subjid, ,
            drop = F
        ]
        data[[sample_pk_con_con_col]] <- as.numeric(data[[sample_pk_con_con_col]])
        data[[sample_pk_con_time_col]] <- as.numeric(data[[sample_pk_con_time_col]])
        data[[sample_pk_con_subj_col]] <- trimws(as.character(
            data[[sample_pk_con_subj_col]]
        ))
        
        if(!is_blank(input$sample_pk_con_group)) {
            data[[input$sample_pk_con_group]] <- factor(
                data[[input$sample_pk_con_group]],
                levels = sort(unique(data[[input$sample_pk_con_group]]))
            )
            data <- arrange_(data, input$sample_pk_con_group,
                             sample_pk_con_subj_col,
                             sample_pk_param_visit_col)
        } else {
            data <- arrange_(data, sample_pk_con_subj_col,
                             sample_pk_param_visit_col)
        }
        
        data[[sample_pk_con_subj_col]] <- factor(
            data[[sample_pk_con_subj_col]],
            levels = unique(data[[sample_pk_con_subj_col]])
        )
        if(length(input$sample_pk_con_visit) > 1) {
            data[[sample_pk_param_visit_col]] <- as.factor(
                data[[sample_pk_param_visit_col]]
            )
            levels(data[[sample_pk_param_visit_col]]) <- paste(
                sample_pk_param_visit_col, '=',
                levels(data[[sample_pk_param_visit_col]])
            )
        }
        
        nsubj <- length(levels(data[[sample_pk_con_subj_col]]))
        shape_value <- seq_len(nsubj)
        plt <- gg_wrapper(
            data, aes_string(x = sample_pk_con_time_col,
                             y = sample_pk_con_con_col,
                             group = sample_pk_con_subj_col),
            log_y = input$sample_pk_con_log
        )
        if(!is_blank(input$sample_pk_con_group)) {
            nsubj_group <- sapply(by(
                data, data[[input$sample_pk_con_group]],
                function(df) {unique(df[[sample_pk_con_subj_col]])}
            ), length)
            shape_value <- sequence(nsubj_group)
            ngroups <- length(nsubj_group)
            all_colors <- gg_color_hue(ngroups)
            color_value <- all_colors[rep.int(1:ngroups, times = nsubj_group)]
            plt <- plt + aes_string(colour = sample_pk_con_subj_col,
                                    fill = input$sample_pk_con_group) +
                scale_colour_manual(
                    name = sample_pk_con_subj_col,
                    values = color_value
                ) +
                scale_fill_manual(
                    name = input$sample_pk_con_group,
                    labels = levels(data[[input$sample_pk_con_group]]),
                    values = all_colors,
                    guide = guide_legend(
                        override.aes = list(colour = all_colors),
                        order = 2
                    )
                )
        }
        if(length(input$sample_pk_con_visit) > 1) {
            plt <- plt + 
                facet_grid(formula(paste(sample_pk_param_visit_col, '~ .')))
        }
        plt <- plt +
            stat_summary(fun.y = mean_na, geom = 'line') +
            stat_summary(aes_string(shape = sample_pk_con_subj_col),
                         fun.y = mean_na, geom = 'point') +
            scale_shape_manual(
                name = sample_pk_con_subj_col,
                values = unname(shape_value)
            ) +
            labs(x = sample_pk_con_indline_xlab$value,
                 y = sample_pk_con_indline_ylab$value,
                 title = sample_pk_con_indline_main$value)
        return(plt)
    })
    output$sample_pk_con_indline_ui <- renderUI({
        # req(sample_pk_con_indline())
        shiny::tagList(
            plotOutput('sample_pk_con_indline'),
            uiOutput('sample_pk_con_indline_fn_out'),
            tags$head(tags$style(
                "#sample_pk_con_indline_fn_out{font-size: 9px;}"
            ))
        )
    })
    output$sample_pk_con_indline <- renderPlot({
        # grid.draw(sample_pk_con_indline())
        sample_pk_con_indline()
    })
    output$sample_pk_con_indline_fn_out <- renderUI({
        req(sample_pk_con_indline_footnote$value)
        HTML(paste(
            strsplit(sample_pk_con_indline_footnote$value, '\n')[[1]],
            collapse = '<br/>'
        ))
    })
    
    # download individual line for PK concentration time profile
    output$sample_pk_con_indline_download_button <- renderUI({
        req(req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK concentration',
            input$sample_pk_con_tabs == 'Individual line',
            input$sample_pk_con_visit, input$sample_pk_con_subjid, 
            !is.null(input$sample_pk_con_group),
            !is.null(input$sample_pk_con_log))
        downloadButton('sample_pk_con_indline_download', 'Download plot')
    })
    output$sample_pk_con_indline_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'sample_pk_individual_line_',
                format(Sys.Date(),format = '%Y%m%d'), '.pdf'
            )
            return(file_name)
        },
        content = function(file) {
            plot_ <- sample_pk_con_indline()
            plot_ <- add_footnote(plot_, sample_pk_con_indline_footnote$value)
            height_width_ratio <- 3 / 4
            width <- 8
            height <- width * height_width_ratio
            ggsave(filename = file, plot = plot_, width = width,
                   height = height, dpi = 600)
        }
    )
    
    #-----------------------------------------------
    # UI widgets for PK parameter time profile
    
    # a selectInput for choosing PK parameter
    output$sample_pk_param_param <- renderUI({
        req(data_import_status$sample_pk_param,
            input$file_sample_pk_type == 'PK parameter')
        data <- data_$sample_pk_param
        choices <- c(
            'Choose' = '', sort(unique(data[[sample_pk_param_param_col]]))
        )
        selectInput('sample_pk_param_param', 'PK parameter', choices)
    })
    # a selectInput for choosing group variable
    output$sample_pk_param_group <- renderUI({
        req(data_import_status$sample_pk_param,
            input$file_sample_pk_type == 'PK parameter')
        data <- data_$sample_pk_param
        choices <- names(data)
        to_exclude <- c(subj_pk_param_col, subj_pk_estm_col)
        choices <- c('Choose' = '', sort(setdiff(choices, to_exclude)))
        stillSelected <- isolate(ifelse(
            is.null(input$sample_pk_param_group), sample_pk_param_dose_col,
            input$sample_pk_param_group[input$sample_pk_param_group %in% choices]
        ))
        selectInput('sample_pk_param_group', 'Group', choices, stillSelected)
    })
    # a selectInput for choosing summary statistics
    output$sample_pk_param_summary <- renderUI({
        req(data_import_status$sample_pk_param,
            input$file_sample_pk_type == 'PK parameter',
            input$sample_pk_param_tabs == 'Summary line')
        selectInput('sample_pk_param_summary', 'Statistics',
                    sample_pk_param_summary_opts)
    })
    # a selectizeInput for choosing subject ID(s)
    output$sample_pk_param_subjid <- renderUI({
        req(data_import_status$sample_pk_param,
            input$file_sample_pk_type == 'PK parameter',
            input$sample_pk_param_tabs == 'Individual line')
        data <- data_$sample_pk_param
        if(!is.null(input$sample_pk_param_param)) {
            cond <- data[[sample_pk_param_param_col]] %in%
                input$sample_pk_param_param
            data <- data[cond, , drop = FALSE]
        }
        choices <- sort(unique(data[[sample_pk_param_subj_col]]))
        selected <- isolate(ternary(
            is.null(input$sample_pk_param_subjid), choices,
            input$sample_pk_param_subjid[input$sample_pk_param_subjid %in% choices]
        ))
        selectizeInput(
            'sample_pk_param_subjid', 'Choose Subject ID(s)',
            c('Choose' = '', choices), selected = selected, multiple = T,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    # a numericInput for specifying decimal place
    output$sample_pk_param_decimal <- renderUI({
        req(data_import_status$sample_pk_param,
            input$file_sample_pk_type == 'PK parameter',
            req(input$sample_pk_param_tabs) == 'Summary table')
        stillSelected <- isolate(ifelse(
            is.null(input$sample_pk_param_decimal), 2,
            input$sample_pk_param_decimal
        ))
        numericInput('sample_pk_param_decimal', 'Decimal places',
                     value = stillSelected)
    })
    sample_pk_param_decimal <- reactiveValues(value = 2)
    observe({
        input$sample_pk_param_decimal
        sample_pk_param_decimal$value <- input$sample_pk_param_decimal
    })
    # a checkbox for specifying log of y
    output$sample_pk_param_log <- renderUI({
        req(data_import_status$sample_pk_param,
            input$file_sample_pk_type == 'PK parameter',
            req(input$sample_pk_param_tabs) != 'Summary table')
        checkboxInput('sample_pk_param_log', 'Log of Y', value = FALSE)
    })
    
    #-----------------------------------------------
    # Further table/graph refine for PK parameter time profile
    
    # a textareaInput for specifying summary table title
    output$sample_pk_param_table_title <- renderUI({
        data <- data_$sample_pk_param
        req(data, req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK parameter',
            req(input$sample_pk_param_tabs) == 'Summary table',
            input$sample_pk_param_param, sample_pk_param_decimal$value,
            !is.null(input$sample_pk_param_group))
        value <- paste(
            'Summary statistics for', input$sample_pk_param_param, 'by'
        )
        if(!is_blank(input$sample_pk_param_group))
            value <- paste(value, input$sample_pk_param_group, 'and')
        value <- paste(value, 'visit')
        textareaInput('sample_pk_param_table_title', 'Table title', value = value)
    })
    sample_pk_param_table_title <- reactiveValues(value = NULL)
    observe({
        data <- data_$sample_pk_param
        req(data, req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK parameter',
            req(input$sample_pk_param_tabs) == 'Summary table',
            input$sample_pk_param_param, sample_pk_param_decimal$value,
            !is.null(input$sample_pk_param_group))
        value <- paste(
            'Summary statistics for', input$sample_pk_param_param, 'by'
        )
        if(!is_blank(input$sample_pk_param_group))
            value <- paste(value, input$sample_pk_param_group, 'and')
        value <- paste(value, 'visit')
        sample_pk_param_table_title$value <- value
    })
    observe({
        input$sample_pk_param_table_title
        sample_pk_param_table_title$value <- input$sample_pk_param_table_title
    })
    
    # a textareaInput for specifying summary table footnote
    output$sample_pk_param_table_footnote <- renderUI({
        data <- data_$sample_pk_param
        req(data, req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK parameter',
            req(input$sample_pk_param_tabs) == 'Summary table',
            input$sample_pk_param_param, sample_pk_param_decimal$value,
            !is.null(input$sample_pk_param_group))
        value <- default_footnote$sample_pk_param
        textareaInput('sample_pk_param_table_footnote', 'Table footnote',
                      value = value)
    })
    sample_pk_param_table_footnote <- reactiveValues(value = '')
    observe({
        input$sample_pk_param_table_footnote
        sample_pk_param_table_footnote$value <- input$sample_pk_param_table_footnote
    })
    
    # a textareaInput for specifying xlab for summary line
    output$sample_pk_param_sumline_xlab <- renderUI({
        req(input$file_sample_pk_type == 'PK parameter',
            input$sample_pk_param_tabs == 'Summary line',
            input$sample_pk_param_param, input$sample_pk_param_summary,
            !is.null(input$sample_pk_param_group),
            !is.null(input$sample_pk_param_log))
        value <- 'Visit'
        textareaInput('sample_pk_param_sumline_xlab', 'X-axis label', value = value)
    })
    sample_pk_param_sumline_xlab <- reactiveValues(value = 'Visit')
    observe({
        input$sample_pk_param_sumline_xlab
        sample_pk_param_sumline_xlab$value <- input$sample_pk_param_sumline_xlab
    })
    
    # a textareaInput for specifying ylab for summary line
    output$sample_pk_param_sumline_ylab <- renderUI({
        req(req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK parameter',
            input$sample_pk_param_tabs == 'Summary line',
            input$sample_pk_param_param, input$sample_pk_param_summary,
            !is.null(input$sample_pk_param_group),
            !is.null(input$sample_pk_param_log))
        value <- input$sample_pk_param_param
        textareaInput('sample_pk_param_sumline_ylab', 'Y-axis label', value = value)
    })
    sample_pk_param_sumline_ylab <- reactiveValues(value = NULL)
    observe({
        req(req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK parameter',
            input$sample_pk_param_tabs == 'Summary line',
            input$sample_pk_param_param, input$sample_pk_param_summary,
            !is.null(input$sample_pk_param_group),
            !is.null(input$sample_pk_param_log))
        value <- input$sample_pk_param_param
        sample_pk_param_sumline_ylab$value <- value
    })
    observe({
        input$sample_pk_param_sumline_ylab
        sample_pk_param_sumline_ylab$value <- input$sample_pk_param_sumline_ylab
    })
    
    # a textareaInput for specifying main for summary line
    output$sample_pk_param_sumline_main <- renderUI({
        req(req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK parameter',
            input$sample_pk_param_tabs == 'Summary line',
            input$sample_pk_param_param, input$sample_pk_param_summary,
            !is.null(input$sample_pk_param_group),
            !is.null(input$sample_pk_param_log))
        value <- paste(
            summary_title_dict[[input$sample_pk_param_summary]],
            input$sample_pk_param_param, 'time profiles'
        )
        if(!is_blank(input$sample_pk_param_group))
            value <- paste(value, 'by', input$sample_pk_param_group)
        textareaInput('sample_pk_param_sumline_main', 'Plot title', value = value)
    })
    sample_pk_param_sumline_main <- reactiveValues(value = NULL)
    observe({
        req(req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK parameter',
            input$sample_pk_param_tabs == 'Summary line',
            input$sample_pk_param_param, input$sample_pk_param_summary,
            !is.null(input$sample_pk_param_group),
            !is.null(input$sample_pk_param_log))
        value <- paste(
            summary_title_dict[[input$sample_pk_param_summary]],
            input$sample_pk_param_param, 'time profiles'
        )
        if(!is_blank(input$sample_pk_param_group))
            value <- paste(value, 'by', input$sample_pk_param_group)
        sample_pk_param_sumline_main$value <- value
    })
    observe({
        input$sample_pk_param_sumline_main
        sample_pk_param_sumline_main$value <- input$sample_pk_param_sumline_main
    })
    
    # a textareaInput for specifying footnote for summary line
    output$sample_pk_param_sumline_footnote <- renderUI({
        req(req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK parameter',
            input$sample_pk_param_tabs == 'Summary line',
            input$sample_pk_param_param, input$sample_pk_param_summary,
            !is.null(input$sample_pk_param_group),
            !is.null(input$sample_pk_param_log))
        value <- default_footnote$sample_pk_param
        textareaInput('sample_pk_param_sumline_footnote', 'Plot footnote',
                      value = value)
    })
    sample_pk_param_sumline_footnote <- reactiveValues(value = '')
    observe({
        input$sample_pk_param_sumline_footnote
        sample_pk_param_sumline_footnote$value <-
            input$sample_pk_param_sumline_footnote
    })
    
    # a textareaInput for specifying xlab for individual line
    output$sample_pk_param_indline_xlab <- renderUI({
        req(req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK parameter',
            input$sample_pk_param_tabs == 'Individual line',
            input$sample_pk_param_param, input$sample_pk_param_subjid,
            !is.null(input$sample_pk_param_group),
            !is.null(input$sample_pk_param_log))
        value <- 'Visit'
        textareaInput('sample_pk_param_indline_xlab', 'X-axis label',
                      value = value)
    })
    sample_pk_param_indline_xlab <- reactiveValues(value = 'Visit')
    observe({
        input$sample_pk_param_indline_xlab
        sample_pk_param_indline_xlab$value <- input$sample_pk_param_indline_xlab
    })
    
    # a textareaInput for specifying ylab for individual line
    output$sample_pk_param_indline_ylab <- renderUI({
        req(req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK parameter',
            input$sample_pk_param_tabs == 'Individual line',
            input$sample_pk_param_param, input$sample_pk_param_subjid,
            !is.null(input$sample_pk_param_group),
            !is.null(input$sample_pk_param_log))
        value <- input$sample_pk_param_param
        textareaInput('sample_pk_param_indline_ylab', 'Y-axis label', value = value)
    })
    sample_pk_param_indline_ylab <- reactiveValues(value = NULL)
    observe({
        req(req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK parameter',
            input$sample_pk_param_tabs == 'Individual line',
            input$sample_pk_param_param, input$sample_pk_param_subjid,
            !is.null(input$sample_pk_param_group),
            !is.null(input$sample_pk_param_log))
        value <- input$sample_pk_param_param
        sample_pk_param_indline_ylab$value <- value
    })
    observe({
        input$sample_pk_param_indline_ylab
        sample_pk_param_indline_ylab$value <- input$sample_pk_param_indline_ylab
    })
    
    # a textareaInput for specifying main for individual line
    output$sample_pk_param_indline_main <- renderUI({
        req(req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK parameter',
            input$sample_pk_param_tabs == 'Individual line',
            input$sample_pk_param_param, input$sample_pk_param_subjid,
            !is.null(input$sample_pk_param_group),
            !is.null(input$sample_pk_param_log))
        value <- paste(
            'Individual', input$sample_pk_param_param, 'time profiles'
        )
        if(!is_blank(input$sample_pk_param_group))
            value <- paste(value, 'by', input$sample_pk_param_group)
        textareaInput('sample_pk_param_indline_main', 'Plot title', value = value)
    })
    sample_pk_param_indline_main <- reactiveValues(value = NULL)
    observe({
        req(req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK parameter',
            input$sample_pk_param_tabs == 'Individual line',
            input$sample_pk_param_param, input$sample_pk_param_subjid,
            !is.null(input$sample_pk_param_group),
            !is.null(input$sample_pk_param_log))
        value <- paste(
            'Individual', input$sample_pk_param_param, 'time profiles'
        )
        if(!is_blank(input$sample_pk_param_group))
            value <- paste(value, 'by', input$sample_pk_param_group)
        sample_pk_param_indline_main$value <- value
    })
    observe({
        input$sample_pk_param_indline_main
        sample_pk_param_indline_main$value <- input$sample_pk_param_indline_main
    })
    
    # a textareaInput for specifying footnote for individual line
    output$sample_pk_param_indline_footnote <- renderUI({
        req(req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK parameter',
            input$sample_pk_param_tabs == 'Individual line',
            input$sample_pk_param_param, input$sample_pk_param_subjid,
            !is.null(input$sample_pk_param_group),
            !is.null(input$sample_pk_param_log))
        value <- default_footnote$sample_pk_param
        textareaInput('sample_pk_param_indline_footnote', 'Plot footnote',
                      value = value)
    })
    sample_pk_param_indline_footnote <- reactiveValues(value = '')
    observe({
        input$sample_pk_param_indline_footnote
        sample_pk_param_indline_footnote$value <- input$sample_pk_param_indline_footnote
    })
    
    #-----------------------------------------------
    # Output for PK parameter time profile
    
    # summary table for PK parameter time profile
    sample_pk_param_sumtable <- reactive({
        data <- data_$sample_pk_param
        req(data, input$sample_pk_param_param, sample_pk_param_decimal$value,
            !is.null(input$sample_pk_param_group))
        req(!is.null(sample_pk_param_table_title$value),
            !is.null(sample_pk_param_table_footnote$value))
        data <- data[
            !is.na(data[[sample_pk_param_visit_col]]) &
                !is.na(data[[sample_pk_param_estm_col]]), , drop = FALSE
        ]
        cond <- data[[sample_pk_param_param_col]] %in% input$sample_pk_param_param
        data <- data[cond, , drop = FALSE]
        if(!is_blank(input$sample_pk_param_group)) {
            data[[input$sample_pk_param_group]] <- factor(
                data[[input$sample_pk_param_group]],
                levels = sort(unique(data[[input$sample_pk_param_group]]))
            )
        }
        data[[sample_pk_param_estm_col]] <- as.numeric(
            data[[sample_pk_param_estm_col]]
        )
        data[[sample_pk_param_visit_col]] <- as.numeric(
            data[[sample_pk_param_visit_col]]
        )
        data[[sample_pk_param_visit_col]] <- factor(
            data[[sample_pk_param_visit_col]],
            levels = sort(unique(data[[sample_pk_param_visit_col]]))
        )
        
        dgt <- sample_pk_param_decimal$value
        sample_pk_param_summary_func <- c(
            'N' = n_nna,
            'Mean (SD)' = partial(mean_sd_str, digits = dgt),
            '%CV' = partial(coeff_var_str, digits = dgt),
            'Median' = partial(median_str, digits = dgt),
            'Q1, Q3' = partial(q1_q3_str, digits = dgt),
            'Min, Max' = partial(min_max_str, digits = dgt),
            'Geom Mean' = partial(geo_mean_str, digits = dgt),
            'Mean (SD) of LN' = partial(mean_sd_ln_str, digits = dgt)
        )
        if(!is_blank(input$sample_pk_param_group)) {
            data <- select_(data, sample_pk_param_estm_col,
                            sample_pk_param_visit_col,
                            input$sample_pk_param_group)
            summary_tbl <- summary_table_2d(
                data, sample_pk_param_visit_col, input$sample_pk_param_group,
                row_names = paste('Visit',
                                  levels(data[[sample_pk_param_visit_col]])),
                col_names = paste(input$sample_pk_param_group, '=',
                                  levels(data[[input$sample_pk_param_group]])),
                rowlabel = '',
                caption = sample_pk_param_table_title$value,
                footnote = sample_pk_param_table_footnote$value,
                func_list = sample_pk_param_summary_func
            )
        } else {
            data <- select_(data, sample_pk_param_estm_col,
                            sample_pk_param_visit_col)
            summary_tbl <- summary_table_row(
                data, sample_pk_param_visit_col,
                row_names = paste('Visit',
                                  levels(data[[sample_pk_param_visit_col]])),
                rowlabel = '',
                caption = sample_pk_param_table_title$value,
                footnote = sample_pk_param_table_footnote$value,
                func_list = sample_pk_param_summary_func
            )
        }
        return(summary_tbl)
    })
    output$sample_pk_param_sumtable <- renderUI({ HTML(sample_pk_param_sumtable()) })
    
    # download summary table for PK parameter time profile
    output$sample_pk_param_table_download_button <- renderUI({
        data <- data_$sample_pk_param
        req(data, req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK parameter',
            req(input$sample_pk_param_tabs) == 'Summary table',
            input$sample_pk_param_param, sample_pk_param_decimal$value,
            !is.null(input$sample_pk_param_group))
        downloadButton('sample_pk_param_table_download', 'Download table')
    })
    output$sample_pk_param_table_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'sample_pk_summary_table_',
                format(Sys.Date(),format = '%Y%m%d'), '.rtf'
            )
            return(file_name)
        },
        content = function(file) {
            data <- data_$sample_pk_param
            data <- data[
                !is.na(data[[sample_pk_param_visit_col]]) &
                    !is.na(data[[sample_pk_param_estm_col]]), , drop = FALSE
            ]
            cond <- data[[sample_pk_param_param_col]] %in% input$sample_pk_param_param
            data <- data[cond, , drop = FALSE]
            if(!is_blank(input$sample_pk_param_group)) {
                data[[input$sample_pk_param_group]] <- factor(
                    data[[input$sample_pk_param_group]],
                    levels = sort(unique(data[[input$sample_pk_param_group]]))
                )
            }
            data[[sample_pk_param_estm_col]] <- as.numeric(
                data[[sample_pk_param_estm_col]]
            )
            data[[sample_pk_param_visit_col]] <- as.numeric(
                data[[sample_pk_param_visit_col]]
            )
            data[[sample_pk_param_visit_col]] <- factor(
                data[[sample_pk_param_visit_col]],
                levels = sort(unique(data[[sample_pk_param_visit_col]]))
            )
            
            dgt <- sample_pk_param_decimal$value
            sample_pk_param_summary_func <- c(
                'N' = n_nna,
                'Mean (SD)' = partial(mean_sd_str, digits = dgt),
                '%CV' = partial(coeff_var_str, digits = dgt),
                'Median' = partial(median_str, digits = dgt),
                'Q1, Q3' = partial(q1_q3_str, digits = dgt),
                'Min, Max' = partial(min_max_str, digits = dgt),
                'Geom Mean' = partial(geo_mean_str, digits = dgt),
                'Mean (SD) of LN' = partial(mean_sd_ln_str, digits = dgt)
            )
            if(!is_blank(input$sample_pk_param_group)) {
                data <- select_(data, sample_pk_param_estm_col,
                                sample_pk_param_visit_col,
                                input$sample_pk_param_group)
            } else {
                data <- select_(data, sample_pk_param_estm_col,
                                sample_pk_param_visit_col)
            }
            
            summary_table <- summary_table_all(
                data, row_var = sample_pk_param_visit_col,
                row_names = paste('Visit',
                                  levels(data[[sample_pk_param_visit_col]])),
                col_var = input$sample_pk_param_group,
                col_names = paste(input$sample_pk_param_group, '=',
                                  levels(data[[input$sample_pk_param_group]])),
                val_var = sample_pk_param_estm_col,
                n_in_header = FALSE, func_list = sample_pk_param_summary_func,
                caption = sample_pk_param_table_title$value,
                footnote = sample_pk_param_table_footnote$value,
                rowlabel = ' ', format = 'rtf'
            )
            rtf_table_wrapper(
                file, summary_table, block_break = TRUE,
                nline_block = length(sample_pk_con_summary_func) + 1,
                caption = sample_pk_param_table_title$value,
                footnote = sample_pk_param_table_footnote$value
            )
        }
    )
    
    # summary line for PK parameter time profile
    sample_pk_param_sumline <- reactive({
        req(input$sample_pk_param_param, input$sample_pk_param_summary,
            !is.null(input$sample_pk_param_group),
            !is.null(input$sample_pk_param_log))
        req(!is.null(sample_pk_param_sumline_xlab$value),
            !is.null(sample_pk_param_sumline_ylab$value),
            !is.null(sample_pk_param_sumline_main$value),
            !is.null(sample_pk_param_sumline_footnote$value))
        data <- data_$sample_pk_param
        data <- data[
            !is.na(data[[sample_pk_param_visit_col]]) &
                !is.na(data[[sample_pk_param_estm_col]]), , drop = FALSE
        ]
        data <- data[
            data[[sample_pk_param_param_col]]%in%input$sample_pk_param_param,
            , drop = F
        ]
        data[[sample_pk_param_estm_col]] <- as.numeric(data[[sample_pk_param_estm_col]])
        if(!is_blank(input$sample_pk_param_group)) {
            data[[input$sample_pk_param_group]] <- as.factor(
                data[[input$sample_pk_param_group]]
            )
            data <- group_by_(data, input$sample_pk_param_group,
                              sample_pk_param_visit_col)
        } else {
            data <- group_by_(data, sample_pk_param_visit_col)
        }
        
        if(input$sample_pk_param_summary %in% c('Mean + SD', 'Mean + SE')) {
            avg_expr <- ~mean_na(var)
            if(input$sample_pk_param_summary == 'Mean + SD') {
                lower_expr <- ~mean_na(var) - sd_na(var)
                upper_expr <- ~mean_na(var) + sd_na(var)
            } else {
                lower_expr <- ~mean_na(var) - std_err(var)
                upper_expr <- ~mean_na(var) + std_err(var)
            }
        } else if(input$sample_pk_param_summary == 'Median + IQR') {
            avg_expr <- ~median_na(var)
            lower_expr <- ~q1_na(var)
            upper_expr <- ~q3_na(var)
        }
        expr <- list(
            lazyeval::interp(avg_expr, var = as.name(sample_pk_param_estm_col)),
            lazyeval::interp(lower_expr, var = as.name(sample_pk_param_estm_col)),
            lazyeval::interp(upper_expr, var = as.name(sample_pk_param_estm_col))
        )
        dots <- setNames(expr, c('avg', 'lower', 'upper'))
        data <- data %>% summarise_(.dots = dots)
        data[[sample_pk_param_visit_col]] <- as.numeric(data[[sample_pk_param_visit_col]])
        
        title_ <- paste(
            summary_title_dict[[input$sample_pk_param_summary]],
            'PK parameter time profiles'
        )
        visit_day <- sort(unique(data[[sample_pk_param_visit_col]]))
        ngroups <- ifelse(is_blank(input$sample_pk_param_group), 1,
                          length(unique(data[[input$sample_pk_param_group]])))
        dodge_width <- min(
            diff(range(visit_day)) / 40, min(diff(visit_day)) / (3 * (ngroups - 1))
        )
        plt <- gg_wrapper(
            data, aes_string(x = sample_pk_param_visit_col, y = 'avg'),
            log_y = input$sample_pk_param_log
        )
        if(!is_blank(input$sample_pk_param_group)) {
            title_ <- paste(title_, 'by', input$sample_pk_param_group)
            plt <- plt + aes_string(colour = input$sample_pk_param_group,
                                    shape = input$sample_pk_param_group)
        }
        plt <- plt +
            geom_line(position = position_dodge(dodge_width)) +
            geom_point(position = position_dodge(dodge_width), size = 2) +
            scale_x_continuous(breaks = visit_day, labels = visit_day) +
            labs(x = sample_pk_param_sumline_xlab$value,
                 y = sample_pk_param_sumline_ylab$value,
                 title = sample_pk_param_sumline_main$value)
        
        if(any(!is.na(data$lower) & !is.na(data$upper))) {
            plt <- plt + geom_errorbar(aes(ymin = lower, ymax = upper),
                                       width = dodge_width,
                                       position = position_dodge(dodge_width))
        }
        # plt <- add_footnote(plt, sample_pk_param_sumline_footnote$value)
        return(plt)
    })
    output$sample_pk_param_sumline_ui <- renderUI({
        # req(sample_pk_param_sumline())
        shiny::tagList(
            plotOutput('sample_pk_param_sumline'),
            uiOutput('sample_pk_param_sumline_fn_out'),
            tags$head(tags$style(
                "#sample_pk_param_sumline_fn_out{font-size: 9px;}"
            ))
        )
    })
    output$sample_pk_param_sumline <- renderPlot({
        # grid.draw(sample_pk_param_sumline())
        sample_pk_param_sumline()
    })
    output$sample_pk_param_sumline_fn_out <- renderUI({
        req(sample_pk_param_sumline_footnote$value)
        HTML(paste(
            strsplit(sample_pk_param_sumline_footnote$value, '\n')[[1]],
            collapse = '<br/>'
        ))
    })
    
    # download summary line for PK parameter time profile
    output$sample_pk_param_sumline_download_button <- renderUI({
        req(req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK parameter',
            input$sample_pk_param_tabs == 'Summary line',
            input$sample_pk_param_param, input$sample_pk_param_summary,
            !is.null(input$sample_pk_param_group),
            !is.null(input$sample_pk_param_log))
        downloadButton('sample_pk_param_sumline_download', 'Download plot')
    })
    output$sample_pk_param_sumline_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'sample_pk_summary_line_',
                format(Sys.Date(),format = '%Y%m%d'), '.pdf'
            )
            return(file_name)
        },
        content = function(file) {
            plot_ <- sample_pk_param_sumline()
            plot_ <- add_footnote(plot_, sample_pk_param_sumline_footnote$value)
            height_width_ratio <- 3 / 4
            width <- 8
            height <- width * height_width_ratio
            ggsave(filename = file, plot = plot_, width = width,
                   height = height, dpi = 600)
        }
    )
    
    # individual line for PK parameter time profile
    sample_pk_param_indline <- reactive({
        req(input$sample_pk_param_param, input$sample_pk_param_subjid,
            !is.null(input$sample_pk_param_group),
            !is.null(input$sample_pk_param_log))
        req(!is.null(sample_pk_param_indline_xlab$value),
            !is.null(sample_pk_param_indline_ylab$value),
            !is.null(sample_pk_param_indline_main$value),
            !is.null(sample_pk_param_indline_footnote$value))
        data <- data_$sample_pk_param
        data <- data[
            !is.na(data[[sample_pk_param_visit_col]]) &
                !is.na(data[[sample_pk_param_estm_col]]), , drop = FALSE
        ]
        data <- data[
            data[[sample_pk_param_param_col]]%in%input$sample_pk_param_param,
            , drop = F
        ]
        data <- data[
            data[[sample_pk_param_subj_col]]%in%input$sample_pk_param_subjid, ,
            drop = F
        ]
        data[[sample_pk_param_estm_col]] <- as.numeric(data[[sample_pk_param_estm_col]])
        data[[sample_pk_param_visit_col]] <- as.numeric(data[[sample_pk_param_visit_col]])
        data[[sample_pk_param_subj_col]] <- trimws(as.character(
            data[[sample_pk_param_subj_col]]
        ))
        
        if(!is_blank(input$sample_pk_param_group)) {
            data[[input$sample_pk_param_group]] <- factor(
                data[[input$sample_pk_param_group]],
                levels = sort(unique(data[[input$sample_pk_param_group]]))
            )
            data <- arrange_(data, input$sample_pk_param_group,
                             sample_pk_param_subj_col)
        } else {
            data <- arrange_(data, sample_pk_param_subj_col)
        }
        
        data[[sample_pk_param_subj_col]] <- factor(
            data[[sample_pk_param_subj_col]],
            levels = unique(data[[sample_pk_param_subj_col]])
        )
        
        visit_day <- sort(unique(data[[sample_pk_param_visit_col]]))
        nsubj <- length(levels(data[[sample_pk_param_subj_col]]))
        shape_value <- seq_len(nsubj)
        plt <- gg_wrapper(
            data, aes_string(x = sample_pk_param_visit_col,
                             y = sample_pk_param_estm_col,
                             group = sample_pk_param_subj_col),
            log_y = input$sample_pk_param_log
        )
        if(!is_blank(input$sample_pk_param_group)) {
            nsubj_group <- sapply(by(
                data, data[[input$sample_pk_param_group]],
                function(df) {unique(df[[sample_pk_param_subj_col]])}
            ), length)
            shape_value <- unname(sequence(nsubj_group))
            ngroups <- length(nsubj_group)
            all_colors <- gg_color_hue(ngroups)
            color_value <- all_colors[rep.int(1:ngroups, times = nsubj_group)]
            
            plt <- plt + aes_string(colour = sample_pk_param_subj_col,
                                    fill = input$sample_pk_param_group) +
                scale_colour_manual(
                    name = sample_pk_param_subj_col,
                    labels = levels(data[[sample_pk_param_subj_col]]),
                    values = color_value
                ) +
                scale_fill_manual(
                    name = input$sample_pk_param_group,
                    labels = levels(data[[input$sample_pk_param_group]]),
                    values = all_colors,
                    guide = guide_legend(
                        override.aes = list(colour = all_colors),
                        order = 2
                    )
                )
        }
        plt <- plt +
            stat_summary(fun.y = mean_na, geom = 'line') +
            stat_summary(aes_string(shape = sample_pk_param_subj_col),
                         fun.y = mean_na, geom = 'point') +
            scale_x_continuous(breaks = visit_day, labels = visit_day) +
            scale_shape_manual(
                name = sample_pk_param_subj_col,
                labels = levels(data[[sample_pk_param_subj_col]]),
                values = shape_value
            ) +
            labs(x = sample_pk_param_indline_xlab$value,
                 y = sample_pk_param_indline_ylab$value,
                 title = sample_pk_param_indline_main$value)
        return(plt)
    })
    output$sample_pk_param_indline_ui <- renderUI({
        # req(sample_pk_param_indline())
        shiny::tagList(
            plotOutput('sample_pk_param_indline'),
            uiOutput('sample_pk_param_indline_fn_out'),
            tags$head(tags$style(
                "#sample_pk_param_indline_fn_out{font-size: 9px;}"
            ))
        )
    })
    output$sample_pk_param_indline <- renderPlot({
        # grid.draw(sample_pk_param_indline())
        sample_pk_param_indline()
    })
    output$sample_pk_param_indline_fn_out <- renderUI({
        req(sample_pk_param_indline_footnote$value)
        HTML(paste(
            strsplit(sample_pk_param_indline_footnote$value, '\n')[[1]],
            collapse = '<br/>'
        ))
    })
    
    # download individual line for PK parameter time profile
    output$sample_pk_param_indline_download_button <- renderUI({
        req(req(input$sample_panel) == 'PK analysis',
            input$file_sample_pk_type == 'PK parameter',
            input$sample_pk_param_tabs == 'Individual line',
            input$sample_pk_param_param, input$sample_pk_param_subjid,
            !is.null(input$sample_pk_param_group),
            !is.null(input$sample_pk_param_log))
        downloadButton('sample_pk_param_indline_download', 'Download plot')
    })
    output$sample_pk_param_indline_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'sample_pk_individual_line_',
                format(Sys.Date(),format = '%Y%m%d'), '.pdf'
            )
            return(file_name)
        },
        content = function(file) {
            plot_ <- sample_pk_param_indline()
            plot_ <- add_footnote(plot_, sample_pk_param_indline_footnote$value)
            height_width_ratio <- 3 / 4
            width <- 8
            height <- width * height_width_ratio
            ggsave(filename = file, plot = plot_, width = width,
                   height = height, dpi = 600)
        }
    )
    
    #-----------------------------------------------
    # UI widgets for PD analysis
    
    # a radioButtons for choosing analysis type
    output$sample_pd_analysis_type <- renderUI({
        req(data_import_status$sample_pd)
        radioButtons('sample_pd_analysis_type', 'Analysis type',
                     sample_pd_analysis_types, selected = character(0), inline = T)
    }) 
    
    #-----------------------------------------------
    # UI widgets for PD analysis - time profile
    
    # a selectInput for choosing PD parameter
    output$sample_pd_time_param <- renderUI({
        req(data_import_status$sample_pd,
            input$sample_pd_analysis_type == '1 PD analysis')
        data <- data_$sample_pd
        choices <- c('Choose' = '', sort(unique(data[[sample_pd_param_col]])))
        selectInput('sample_pd_time_param', 'PD parameter', choices)
    })
    # a selectInput for choosing y for PD parameter
    output$sample_pd_time_y <- renderUI({
        req(data_import_status$sample_pd,
            input$sample_pd_analysis_type == '1 PD analysis')
        selectInput('sample_pd_time_y', 'PD value', sample_pd_dose_y_types)
    })
    # selectInput for choosing x variable
    output$sample_pd_time_x <- renderUI({
        req(data_import_status$sample_pd,
            input$sample_pd_analysis_type == '1 PD analysis')
        choices <- c('Choose' = '', sample_pd_avisitn_col, sample_pd_atptn_col)
        selectInput(
            'sample_pd_time_x',
            tags$html(
                'X variable',
                tags$style(type = 'text/css',
                           '#qsample_pd_time_x {vertical-align: top;}'),
                shinyBS::bsButton('qsample_pd_time_x', label = '',
                                  icon = icon('question'),
                                  style = 'info', size = 'extra-small')
            ), choices
        )
    })
    observe({
        input$qsample_pd_time_x
        shinyBS::addTooltip(
            session, 'qsample_pd_time_x', sample_pd_time_x_htext,
            placement = 'right', trigger = 'hover',
            options = list(container = 'body')
        )
    })
    # a selectizeInput for choosing visit(s)
    output$sample_pd_time_visit <- renderUI({
        req(data_import_status$sample_pd,
            input$sample_pd_analysis_type == '1 PD analysis',
            req(input$sample_pd_time_x) == sample_pd_atptn_col)
        data <- data_$sample_pd
        choices <- sort(unique(data[[sample_pd_avisitn_col]]))
        selectizeInput(
            'sample_pd_time_visit',
            tags$html(
                'Visit',
                tags$style(type = 'text/css',
                           '#qsample_pd_time_visit {vertical-align: top;}'),
                shinyBS::bsButton('qsample_pd_time_visit', label = '',
                                  icon = icon('question'),
                                  style = 'info', size = 'extra-small')
            ),
            c('Choose' = '', choices), multiple = T,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    observe({
        input$qsample_pd_time_visit
        shinyBS::addTooltip(
            session, 'qsample_pd_time_visit', sample_pd_time_visit_htext,
            placement = 'right', trigger = 'hover',
            options = list(container = 'body')
        )
    })
    # a selectInput for choosing group variable
    output$sample_pd_time_group <- renderUI({
        req(data_import_status$sample_pd,
            input$sample_pd_analysis_type == '1 PD analysis')
        data <- data_$sample_pd
        choices <- names(data)
        to_exclude <- c(sample_pd_param_col, sample_pd_aval_col)
        choices <- c('Choose' = '', sort(setdiff(choices, to_exclude)))
        stillSelected <- isolate(ifelse(
            is.null(input$sample_pd_time_group), sample_pd_dose_col,
            input$sample_pd_time_group[input$sample_pd_time_group %in% choices]
        ))
        selectInput('sample_pd_time_group', 'Group', choices, stillSelected)
    })
    # a selectInput for choosing summary statistics
    output$sample_pd_time_summary <- renderUI({
        req(data_import_status$sample_pd,
            input$sample_pd_analysis_type == '1 PD analysis',
            input$sample_pd_time_tabs == 'Summary line')
        selectInput('sample_pd_time_summary', 'Statistics', sample_pd_time_summary_opts)
    })
    # a selectizeInput for choosing subject ID(s)
    output$sample_pd_time_subjid <- renderUI({
        req(data_import_status$sample_pd,
            input$sample_pd_analysis_type == '1 PD analysis',
            input$sample_pd_time_tabs == 'Individual line')
        data <- data_$sample_pd
        if(!is.null(input$sample_pd_time_param)) {
            data <- data[
                data[[sample_pd_param_col]]%in%input$sample_pd_time_param,
                , drop = F
            ]
        }
        if(!is.null(input$sample_pd_time_x) &&
           input$sample_pd_time_x == sample_pd_atptn_col &&
           !is.null(input$sample_pd_time_visit)) {
            data <- data[
                data[[sample_pd_avisitn_col]] %in%  input$sample_pd_time_visit,
                , drop = F
            ]
        }
        choices <- sort(unique(data[[sample_pd_subj_col]]))
        selected <- isolate(ternary(
            is.null(input$sample_pd_time_subjid), choices,
            input$sample_pd_time_subjid[input$sample_pd_time_subjid %in% choices]
        ))
        selectizeInput(
            'sample_pd_time_subjid', 'Choose Subject ID(s)',
            c('Choose' = '', choices), selected = selected, multiple = T,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    # a numericInput for specifying decimal place
    output$sample_pd_time_decimal <- renderUI({
        req(data_import_status$sample_pd,
            input$sample_pd_analysis_type == '1 PD analysis',
            req(input$sample_pd_time_tabs) == 'Summary table')
        stillSelected <- isolate(ifelse(
            is.null(input$sample_pd_time_decimal), 2,
            input$sample_pd_time_decimal
        ))
        numericInput('sample_pd_time_decimal', 'Decimal places',
                     value = stillSelected)
    })
    sample_pd_time_decimal <- reactiveValues(value = 2)
    observe({
        input$sample_pd_time_decimal
        sample_pd_time_decimal$value <- input$sample_pd_time_decimal
    })
    # a checkbox for specifying log of y
    output$sample_pd_time_log <- renderUI({
        req(data_import_status$sample_pd,
            input$sample_pd_analysis_type == '1 PD analysis',
            req(input$sample_pd_time_tabs) != 'Summary table')
        checkboxInput('sample_pd_time_log', 'Log of Y', value = FALSE)
    })
    
    #-----------------------------------------------
    # Further table/graph refine for PD analysis - time profile
    
    # a textareaInput for specifying summary table title
    output$sample_pd_time_table_title <- renderUI({
        data <- data_$sample_pd
        req(data, req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '1 PD analysis',
            req(input$sample_pd_time_tabs) == 'Summary table',
            input$sample_pd_time_param, input$sample_pd_time_y,
            input$sample_pd_time_x,
            (input$sample_pd_time_x == sample_pd_avisitn_col ||
                 (input$sample_pd_time_x == sample_pd_atptn_col &&
                      !is_blank(input$sample_pd_time_visit))),
            sample_pd_time_decimal$value,
            !is.null(input$sample_pd_time_group))
        value <- paste(
            'Summary statistics for', input$sample_pd_time_param, 'by'
        )
        if(!is_blank(input$sample_pd_time_group))
            value <- paste(value, input$sample_pd_time_group, 'and')
        if(input$sample_pd_time_x == sample_pd_avisitn_col)
            value <- paste(value, 'visit')
        else
            value <- paste(value, 'timepoint')
        textareaInput('sample_pd_time_table_title', 'Table title', value = value)
    })
    sample_pd_time_table_title <- reactiveValues(value = NULL)
    observe({
        data <- data_$sample_pd
        req(data, req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '1 PD analysis',
            req(input$sample_pd_time_tabs) == 'Summary table',
            input$sample_pd_time_param, input$sample_pd_time_y,
            input$sample_pd_time_x,
            (input$sample_pd_time_x == sample_pd_avisitn_col ||
                 (input$sample_pd_time_x == sample_pd_atptn_col &&
                      !is_blank(input$sample_pd_time_visit))),
            sample_pd_time_decimal$value,
            !is.null(input$sample_pd_time_group))
        value <- paste(
            'Summary statistics for', input$sample_pd_time_param, 'by'
        )
        if(!is_blank(input$sample_pd_time_group))
            value <- paste(value, input$sample_pd_time_group, 'and')
        if(input$sample_pd_time_x == sample_pd_avisitn_col)
            value <- paste(value, 'visit')
        else
            value <- paste(value, 'timepoint')
        sample_pd_time_table_title$value <- value
    })
    observe({
        input$sample_pd_time_table_title
        sample_pd_time_table_title$value <- input$sample_pd_time_table_title
    })

    # a textareaInput for specifying summary table footnote
    output$sample_pd_time_table_footnote <- renderUI({
        data <- data_$sample_pd
        req(data, req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '1 PD analysis',
            req(input$sample_pd_time_tabs) == 'Summary table',
            input$sample_pd_time_param, input$sample_pd_time_y,
            input$sample_pd_time_x,
            (input$sample_pd_time_x == sample_pd_avisitn_col ||
                 (input$sample_pd_time_x == sample_pd_atptn_col &&
                      !is_blank(input$sample_pd_time_visit))),
            sample_pd_time_decimal$value,
            !is.null(input$sample_pd_time_group))
        value <- ''
        textareaInput('sample_pd_time_table_footnote', 'Table footnote',
                      value = value)
    })
    sample_pd_time_table_footnote <- reactiveValues(value = '')
    observe({
        input$sample_pd_time_table_footnote
        sample_pd_time_table_footnote$value <- input$sample_pd_time_table_footnote
    })
    
    # a textareaInput for specifying xlab for summary line
    output$sample_pd_time_sumline_xlab <- renderUI({
        is_visit <- input$sample_pd_time_x == sample_pd_avisitn_col
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '1 PD analysis',
            input$sample_pd_time_param, input$sample_pd_time_y,
            input$sample_pd_time_summary,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_time_visit))),
            !is.null(input$sample_pd_time_group),
            !is.null(input$sample_pd_time_log),
            req(input$sample_pd_time_tabs) == 'Summary line')
        value <- ifelse(is_visit, 'Visit', 'Timepoint')
        textareaInput('sample_pd_time_sumline_xlab', 'X-axis label',
                      value = value)
    })
    sample_pd_time_sumline_xlab <- reactiveValues(value = NULL)
    observe({
        is_visit <- input$sample_pd_time_x == sample_pd_avisitn_col
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '1 PD analysis',
            input$sample_pd_time_param, input$sample_pd_time_y,
            input$sample_pd_time_summary,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_time_visit))),
            !is.null(input$sample_pd_time_group),
            !is.null(input$sample_pd_time_log),
            req(input$sample_pd_time_tabs) == 'Summary line')
        value <- ifelse(is_visit, 'Visit', 'Timepoint')
        sample_pd_time_sumline_xlab$value <- value
    })
    observe({
        input$sample_pd_time_sumline_xlab
        sample_pd_time_sumline_xlab$value <- input$sample_pd_time_sumline_xlab
    })
    
    # a textareaInput for specifying ylab for summary line
    output$sample_pd_time_sumline_ylab <- renderUI({
        is_visit <- input$sample_pd_time_x == sample_pd_avisitn_col
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '1 PD analysis',
            input$sample_pd_time_param, input$sample_pd_time_y,
            input$sample_pd_time_summary,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_time_visit))),
            !is.null(input$sample_pd_time_group),
            !is.null(input$sample_pd_time_log),
            req(input$sample_pd_time_tabs) == 'Summary line')
        value <- input$sample_pd_time_param
        textareaInput('sample_pd_time_sumline_ylab', 'Y-axis label',
                      value = value)
    })
    sample_pd_time_sumline_ylab <- reactiveValues(value = NULL)
    observe({
        is_visit <- input$sample_pd_time_x == sample_pd_avisitn_col
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '1 PD analysis',
            input$sample_pd_time_param, input$sample_pd_time_y,
            input$sample_pd_time_summary,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_time_visit))),
            !is.null(input$sample_pd_time_group),
            !is.null(input$sample_pd_time_log),
            req(input$sample_pd_time_tabs) == 'Summary line')
        value <- input$sample_pd_time_param
        sample_pd_time_sumline_ylab$value <- value
    })
    observe({
        input$sample_pd_time_sumline_ylab
        sample_pd_time_sumline_ylab$value <- input$sample_pd_time_sumline_ylab
    })
    
    # a textareaInput for specifying main for summary line
    output$sample_pd_time_sumline_main <- renderUI({
        is_visit <- input$sample_pd_time_x == sample_pd_avisitn_col
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '1 PD analysis',
            input$sample_pd_time_param, input$sample_pd_time_y,
            input$sample_pd_time_summary,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_time_visit))),
            !is.null(input$sample_pd_time_group),
            !is.null(input$sample_pd_time_log),
            req(input$sample_pd_time_tabs) == 'Summary line')
        value <- paste(
            summary_title_dict[[input$sample_pd_time_summary]],
            input$sample_pd_time_param, 'time profiles'
        )
        if(!is_blank(input$sample_pd_time_group))
            value <- paste(value, 'by', input$sample_pd_time_group)
        textareaInput('sample_pd_time_sumline_main', 'Plot title',
                      value = value)
    })
    sample_pd_time_sumline_main <- reactiveValues(value = NULL)
    observe({
        is_visit <- input$sample_pd_time_x == sample_pd_avisitn_col
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '1 PD analysis',
            input$sample_pd_time_param, input$sample_pd_time_y,
            input$sample_pd_time_summary,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_time_visit))),
            !is.null(input$sample_pd_time_group),
            !is.null(input$sample_pd_time_log),
            req(input$sample_pd_time_tabs) == 'Summary line')
        value <- paste(
            summary_title_dict[[input$sample_pd_time_summary]],
            input$sample_pd_time_param, 'time profiles'
        )
        if(!is_blank(input$sample_pd_time_group))
            value <- paste(value, 'by', input$sample_pd_time_group)
        sample_pd_time_sumline_main$value <- value
    })
    observe({
        input$sample_pd_time_sumline_main
        sample_pd_time_sumline_main$value <- input$sample_pd_time_sumline_main
    })
    
    # a textareaInput for specifying footnote for summary line
    output$sample_pd_time_sumline_footnote <- renderUI({
        is_visit <- input$sample_pd_time_x == sample_pd_avisitn_col
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '1 PD analysis',
            input$sample_pd_time_param, input$sample_pd_time_y,
            input$sample_pd_time_summary,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_time_visit))),
            !is.null(input$sample_pd_time_group),
            !is.null(input$sample_pd_time_log),
            req(input$sample_pd_time_tabs) == 'Summary line')
        value <- default_footnote$sample_pd
        textareaInput('sample_pd_time_sumline_footnote', 'Plot footnote',
                      value = value)
    })
    sample_pd_time_sumline_footnote <- reactiveValues(value = '')
    observe({
        input$sample_pd_time_sumline_footnote
        sample_pd_time_sumline_footnote$value <-
            input$sample_pd_time_sumline_footnote
    })
    
    # a textareaInput for specifying xlab for individual line
    output$sample_pd_time_indline_xlab <- renderUI({
        is_visit <- input$sample_pd_time_x == sample_pd_avisitn_col
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '1 PD analysis',
            input$sample_pd_time_param, input$sample_pd_time_y,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_time_visit))),
            input$sample_pd_time_subjid, !is.null(input$sample_pd_time_group),
            !is.null(input$sample_pd_time_log),
            req(input$sample_pd_time_tabs) == 'Individual line')
        value <- ifelse(is_visit, 'Visit', 'Timepoint')
        textareaInput('sample_pd_time_indline_xlab', 'X-axis label',
                      value = value)
    })
    sample_pd_time_indline_xlab <- reactiveValues(value = NULL)
    observe({
        is_visit <- input$sample_pd_time_x == sample_pd_avisitn_col
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '1 PD analysis',
            input$sample_pd_time_param, input$sample_pd_time_y,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_time_visit))),
            input$sample_pd_time_subjid, !is.null(input$sample_pd_time_group),
            !is.null(input$sample_pd_time_log),
            req(input$sample_pd_time_tabs) == 'Individual line')
        value <- ifelse(is_visit, 'Visit', 'Timepoint')
        sample_pd_time_indline_xlab$value <- value
    })
    observe({
        input$sample_pd_time_indline_xlab
        sample_pd_time_indline_xlab$value <- input$sample_pd_time_indline_xlab
    })
    
    # a textareaInput for specifying ylab for individual line
    output$sample_pd_time_indline_ylab <- renderUI({
        is_visit <- input$sample_pd_time_x == sample_pd_avisitn_col
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '1 PD analysis',
            input$sample_pd_time_param, input$sample_pd_time_y,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_time_visit))),
            input$sample_pd_time_subjid, !is.null(input$sample_pd_time_group),
            !is.null(input$sample_pd_time_log),
            req(input$sample_pd_time_tabs) == 'Individual line')
        value <- input$sample_pd_time_param
        textareaInput('sample_pd_time_indline_ylab', 'Y-axis label',
                      value = value)
    })
    sample_pd_time_indline_ylab <- reactiveValues(value = NULL)
    observe({
        is_visit <- input$sample_pd_time_x == sample_pd_avisitn_col
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '1 PD analysis',
            input$sample_pd_time_param, input$sample_pd_time_y,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_time_visit))),
            input$sample_pd_time_subjid, !is.null(input$sample_pd_time_group),
            !is.null(input$sample_pd_time_log),
            req(input$sample_pd_time_tabs) == 'Individual line')
        value <- input$sample_pd_time_param
        sample_pd_time_indline_ylab$value <- value
    })
    observe({
        input$sample_pd_time_indline_ylab
        sample_pd_time_indline_ylab$value <- input$sample_pd_time_indline_ylab
    })
    
    # a textareaInput for specifying main for individual line
    output$sample_pd_time_indline_main <- renderUI({
        is_visit <- input$sample_pd_time_x == sample_pd_avisitn_col
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '1 PD analysis',
            input$sample_pd_time_param, input$sample_pd_time_y,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_time_visit))),
            input$sample_pd_time_subjid, !is.null(input$sample_pd_time_group),
            !is.null(input$sample_pd_time_log),
            req(input$sample_pd_time_tabs) == 'Individual line')
        value <- paste('Individual', input$sample_pd_time_param, 'time profiles')
        if(!is_blank(input$sample_pd_time_group))
            value <- paste(value, 'by', input$sample_pd_time_group)
        textareaInput('sample_pd_time_indline_main', 'Plot title',
                      value = value)
    })
    sample_pd_time_indline_main <- reactiveValues(value = NULL)
    observe({
        is_visit <- input$sample_pd_time_x == sample_pd_avisitn_col
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '1 PD analysis',
            input$sample_pd_time_param, input$sample_pd_time_y,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_time_visit))),
            input$sample_pd_time_subjid, !is.null(input$sample_pd_time_group),
            !is.null(input$sample_pd_time_log),
            req(input$sample_pd_time_tabs) == 'Individual line')
        value <- paste('Individual', input$sample_pd_time_param, 'time profiles')
        if(!is_blank(input$sample_pd_time_group))
            value <- paste(value, 'by', input$sample_pd_time_group)
        sample_pd_time_indline_main$value <- value
    })
    observe({
        input$sample_pd_time_indline_main
        sample_pd_time_indline_main$value <- input$sample_pd_time_indline_main
    })
    
    # a textareaInput for specifying footnote for individual line
    output$sample_pd_time_indline_footnote <- renderUI({
        is_visit <- input$sample_pd_time_x == sample_pd_avisitn_col
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '1 PD analysis',
            input$sample_pd_time_param, input$sample_pd_time_y,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_time_visit))),
            input$sample_pd_time_subjid, !is.null(input$sample_pd_time_group),
            !is.null(input$sample_pd_time_log),
            req(input$sample_pd_time_tabs) == 'Individual line')
        value <- default_footnote$sample_pd
        textareaInput('sample_pd_time_indline_footnote', 'Plot footnote',
                      value = value)
    })
    sample_pd_time_indline_footnote <- reactiveValues(value = '')
    observe({
        input$sample_pd_time_indline_footnote
        sample_pd_time_indline_footnote$value <-
            input$sample_pd_time_indline_footnote
    })
    
    #-----------------------------------------------
    # Output for PD analysis - time profile
    
    # summary table for 1 PD analysis
    sample_pd_time_sumtable <- reactive({
        data <- data_$sample_pd
        is_visit <- input$sample_pd_time_x == sample_pd_avisitn_col
        req(data, req(input$sample_pd_time_tabs) == 'Summary table',
            input$sample_pd_time_param, input$sample_pd_time_y,
            input$sample_pd_time_x,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_time_visit))),
            sample_pd_time_decimal$value,
            !is.null(input$sample_pd_time_group))
        req(!is.null(sample_pd_time_table_title$value),
            !is.null(sample_pd_time_table_footnote$value))
        x_var <- ifelse(is_visit, sample_pd_avisitn_col, sample_pd_atptn_col)
        data <- data[
            !is.na(data[[x_var]]) &
                !is.na(data[[input$sample_pd_time_y]]), , drop = FALSE
        ]
        data <- data[
            data[[sample_pd_param_col]] %in% input$sample_pd_time_param, ,
            drop = F
        ]
        if(!is_visit) {
            data <- data[
                data[[sample_pd_avisitn_col]] %in%  input$sample_pd_time_visit, ,
                drop = F
            ]
        }
        
        if(!is_blank(input$sample_pd_time_group)) {
            data[[input$sample_pd_time_group]] <- as.factor(
                data[[input$sample_pd_time_group]]
            )
        }
        data[[input$sample_pd_time_y]] <- as.numeric(data[[input$sample_pd_time_y]])
        data[[sample_pd_avisitn_col]] <- as.numeric(data[[sample_pd_avisitn_col]])
        dgt <- sample_pd_time_decimal$value
        sample_pd_time_summary_func <- c(
            'N' = n_nna,
            'Mean (SD)' = partial(mean_sd_str, digits = dgt),
            '%CV' = partial(coeff_var_str, digits = dgt),
            'Median' = partial(median_str, digits = dgt),
            'Q1, Q3' = partial(q1_q3_str, digits = dgt),
            'Min, Max' = partial(min_max_str, digits = dgt),
            'Geom Mean' = partial(geo_mean_str, digits = dgt),
            'Mean (SD) of LN' = partial(mean_sd_ln_str, digits = dgt)
        )
        
        if(is_visit) {
            data[[sample_pd_avisitn_col]] <- factor(
                data[[sample_pd_avisitn_col]],
                levels = sort(unique(data[[sample_pd_avisitn_col]]))
            )
            if(!is_blank(input$sample_pd_time_group)) {
                data <- select_(data, input$sample_pd_time_y, sample_pd_avisitn_col,
                                input$sample_pd_time_group)
                summary_tbl <- summary_table_2d(
                    data, sample_pd_avisitn_col, input$sample_pd_time_group,
                    row_names = paste('Visit',
                                      levels(data[[sample_pd_avisitn_col]])),
                    col_names = paste(input$sample_pd_time_group, '=',
                                      levels(data[[input$sample_pd_time_group]])),
                    rowlabel = '',
                    caption = sample_pd_time_table_title$value,
                    footnote = sample_pd_time_table_footnote$value,
                    func_list = sample_pd_time_summary_func
                )
            } else {
                data <- select_(data, input$sample_pd_time_y, sample_pd_avisitn_col)
                summary_tbl <- summary_table_row(
                    data, sample_pd_avisitn_col,
                    row_names = paste('Visit',
                                      levels(data[[sample_pd_avisitn_col]])),
                    rowlabel = '', caption = sample_pd_time_table_title$value,
                    footnote = sample_pd_time_table_footnote$value,
                    func_list = sample_pd_time_summary_func
                )
            }
            return(summary_tbl)
        } else {
            cond <- data[[sample_pd_avisitn_col]] %in% input$sample_pd_time_visit
            data <- data[cond, , drop = FALSE]
            
            data[[sample_pd_atptn_col]] <- as.numeric(data[[sample_pd_atptn_col]])
            data[['time_']] <- paste0(
                'Visit ', to_character(data[[sample_pd_avisitn_col]]), ', ',
                'Timepoint', to_character(data[[sample_pd_atptn_col]])
            )
            order_time <- order(data[[sample_pd_avisitn_col]],
                                data[[sample_pd_atptn_col]])
            data[['time_']] <- factor(
                data[['time_']], levels = unique(data[['time_']][order_time])
            )
            
            if(!is_blank(input$sample_pd_time_group)) {
                data <- select_(data, input$sample_pd_time_y, 'time_',
                                input$sample_pd_time_group)
                summary_tbl <- summary_table_2d(
                    data, 'time_', input$sample_pd_time_group,
                    row_names = levels(data[['time_']]),
                    col_names = paste(input$sample_pd_time_group, '=',
                                      levels(data[[input$sample_pd_time_group]])),
                    rowlabel = '', caption = sample_pd_time_table_title$value,
                    footnote = sample_pd_time_table_footnote$value,
                    func_list = sample_pd_time_summary_func
                )
            } else {
                data <- select_(data, input$sample_pd_time_y, 'time_')
                summary_tbl <- summary_table_row(
                    data, 'time_', row_names = levels(data[['time_']]),
                    rowlabel = '', caption = sample_pd_time_table_title$value,
                    footnote = sample_pd_time_table_footnote$value,
                    func_list = sample_pd_time_summary_func
                )
            }
            return(summary_tbl)
        }
    })
    output$sample_pd_time_sumtable_ui <- renderUI({HTML(sample_pd_time_sumtable())})
    
    # download summary table for 1 PD analysis
    output$sample_pd_time_table_download_button <- renderUI({
        data <- data_$sample_pd
        req(data, req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '1 PD analysis',
            req(input$sample_pd_time_tabs) == 'Summary table',
            input$sample_pd_time_param, input$sample_pd_time_y,
            input$sample_pd_time_x,
            (input$sample_pd_time_x == sample_pd_avisitn_col ||
                 (input$sample_pd_time_x == sample_pd_atptn_col &&
                      !is_blank(input$sample_pd_time_visit))),
            sample_pd_time_decimal$value,
            !is.null(input$sample_pd_time_group))
        downloadButton('sample_pd_time_table_download', 'Download table')
    })
    output$sample_pd_time_table_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'sample_pd_summary_table_',
                format(Sys.Date(),format = '%Y%m%d'), '.rtf'
            )
            return(file_name)
        },
        content = function(file) {
            data <- data_$sample_pd
            is_visit <- input$sample_pd_time_x == sample_pd_avisitn_col
            x_var <- ifelse(is_visit, sample_pd_avisitn_col, sample_pd_atptn_col)
            data <- data[
                !is.na(data[[x_var]]) &
                    !is.na(data[[input$sample_pd_time_y]]), , drop = FALSE
            ]
            data <- data[
                data[[sample_pd_param_col]] %in% input$sample_pd_time_param, ,
                drop = F
            ]
            if(!is_visit) {
                data <- data[
                    data[[sample_pd_avisitn_col]] %in%  input$sample_pd_time_visit,
                    , drop = F
                ]
            }
            
            if(!is_blank(input$sample_pd_time_group)) {
                data[[input$sample_pd_time_group]] <- factor(
                    data[[input$sample_pd_time_group]],
                    levels = sort(unique(data[[input$sample_pd_time_group]]))
                )
            }
            data[[input$sample_pd_time_y]] <- as.numeric(
                data[[input$sample_pd_time_y]]
            )
            data[[sample_pd_avisitn_col]] <- as.numeric(
                data[[sample_pd_avisitn_col]]
            )
            dgt <- sample_pd_time_decimal$value
            sample_pd_time_summary_func <- c(
                'N' = n_nna,
                'Mean (SD)' = partial(mean_sd_str, digits = dgt),
                '%CV' = partial(coeff_var_str, digits = dgt),
                'Median' = partial(median_str, digits = dgt),
                'Q1, Q3' = partial(q1_q3_str, digits = dgt),
                'Min, Max' = partial(min_max_str, digits = dgt),
                'Geom Mean' = partial(geo_mean_str, digits = dgt),
                'Mean (SD) of LN' = partial(mean_sd_ln_str, digits = dgt)
            )
            
            if(is_visit) {
                data[[sample_pd_avisitn_col]] <- factor(
                    data[[sample_pd_avisitn_col]],
                    levels = sort(unique(data[[sample_pd_avisitn_col]]))
                )
                if(!is_blank(input$sample_pd_time_group)) {
                    data <- select_(data, input$sample_pd_time_y,
                                    sample_pd_avisitn_col,
                                    input$sample_pd_time_group)
                } else {
                    data <- select_(data, input$sample_pd_time_y,
                                    sample_pd_avisitn_col)
                }
                summary_table <- summary_table_all(
                    data, row_var = sample_pd_avisitn_col,
                    row_names = paste('Visit',
                                      levels(data[[sample_pd_avisitn_col]])),
                    col_var = input$sample_pd_time_group,
                    col_names = paste(input$sample_pd_time_group, '=',
                                      levels(data[[input$sample_pd_time_group]])),
                    val_var = input$sample_pd_time_y,
                    n_in_header = FALSE, func_list = sample_pd_time_summary_func,
                    caption = sample_pd_time_table_title$value,
                    footnote = sample_pd_time_table_footnote$value,
                    rowlabel = ' ', format = 'rtf'
                )
                rtf_table_wrapper(
                    file, summary_table, block_break = TRUE,
                    nline_block = length(sample_pd_time_summary_func) + 1,
                    caption = sample_pd_time_table_title$value,
                    footnote = sample_pd_time_table_footnote$value
                )
            } else {
                cond <- data[[sample_pd_avisitn_col]] %in% input$sample_pd_time_visit
                data <- data[cond, , drop = FALSE]
                
                data[[sample_pd_atptn_col]] <- as.numeric(
                    data[[sample_pd_atptn_col]]
                )
                data[['time_']] <- paste0(
                    'Visit ', to_character(data[[sample_pd_avisitn_col]]), ', ',
                    'Timepoint', to_character(data[[sample_pd_atptn_col]])
                )
                order_time <- order(data[[sample_pd_avisitn_col]],
                                    data[[sample_pd_atptn_col]])
                data[['time_']] <- factor(
                    data[['time_']], levels = unique(data[['time_']][order_time])
                )
                
                if(!is_blank(input$sample_pd_time_group)) {
                    data <- select_(data, input$sample_pd_time_y, 'time_',
                                    input$sample_pd_time_group)
                } else {
                    data <- select_(data, input$sample_pd_time_y, 'time_')
                }
                summary_table <- summary_table_all(
                    data, row_var = 'time_', row_names = levels(data[['time_']]),
                    col_var = input$sample_pd_time_group,
                    col_names = paste(input$sample_pd_time_group, '=',
                                      levels(data[[input$sample_pd_time_group]])),
                    val_var = input$sample_pd_time_y,
                    n_in_header = FALSE, func_list = sample_pd_time_summary_func,
                    caption = sample_pd_time_table_title$value,
                    footnote = sample_pd_time_table_footnote$value,
                    rowlabel = ' ', format = 'rtf'
                )
                rtf_table_wrapper(
                    file, summary_table, block_break = TRUE,
                    nline_block = length(sample_pd_time_summary_func) + 1,
                    caption = sample_pd_time_table_title$value,
                    footnote = sample_pd_time_table_footnote$value
                )
            }
        }
    )
    
    # summary line for 1 PD analysis
    sample_pd_time_sumline <- reactive({
        is_visit <- input$sample_pd_time_x == sample_pd_avisitn_col
        req(input$sample_pd_time_param, input$sample_pd_time_y,
            input$sample_pd_time_summary,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_time_visit))),
            !is.null(input$sample_pd_time_group),
            !is.null(input$sample_pd_time_log))
        req(!is.null(sample_pd_time_sumline_xlab$value),
            !is.null(sample_pd_time_sumline_ylab$value),
            !is.null(sample_pd_time_sumline_main$value),
            !is.null(sample_pd_time_sumline_footnote$value))
        data <- data_$sample_pd
        x_var <- ifelse(is_visit, sample_pd_avisitn_col, sample_pd_atptn_col)
        data <- data[
            !is.na(data[[x_var]]) &
                !is.na(data[[input$sample_pd_time_y]]), , drop = FALSE
        ]
        data[[input$sample_pd_time_y]] <- as.numeric(data[[input$sample_pd_time_y]])
        data <- data[
            data[[sample_pd_param_col]]%in%input$sample_pd_time_param, , drop = F
        ]
        if(!is_visit) {
            data <- data[data[[sample_pd_avisitn_col]] %in%
                             input$sample_pd_time_visit, , drop = F]
        }
        if(!is_blank(input$sample_pd_time_group)) {
            data[[input$sample_pd_time_group]] <- as.factor(
                data[[input$sample_pd_time_group]]
            )
            if(!is_visit) {
                data <- group_by_(data, sample_pd_avisitn_col,
                                  input$sample_pd_time_group,
                                  sample_pd_atptn_col)
            } else {
                data <- group_by_(data, input$sample_pd_time_group,
                                  sample_pd_avisitn_col)
            }
        } else {
            if(!is_visit) {
                data <- group_by_(data, sample_pd_avisitn_col,
                                  sample_pd_atptn_col)
            } else {
                data <- group_by_(data, sample_pd_avisitn_col)
            }
        }
        
        if(input$sample_pd_time_summary %in% c('Mean + SD', 'Mean + SE')) {
            avg_expr <- ~mean_na(var)
            if(input$sample_pd_time_summary == 'Mean + SD') {
                lower_expr <- ~mean_na(var) - sd_na(var)
                upper_expr <- ~mean_na(var) + sd_na(var)
            } else {
                lower_expr <- ~mean_na(var) - std_err(var)
                upper_expr <- ~mean_na(var) + std_err(var)
            }
        } else if(input$sample_pd_time_summary == 'Median + IQR') {
            avg_expr <- ~median_na(var)
            lower_expr <- ~q1_na(var)
            upper_expr <- ~q3_na(var)
        }
        expr <- list(
            lazyeval::interp(avg_expr, var = as.name(input$sample_pd_time_y)),
            lazyeval::interp(lower_expr, var = as.name(input$sample_pd_time_y)),
            lazyeval::interp(upper_expr, var = as.name(input$sample_pd_time_y))
        )
        dots <- setNames(expr, c('avg', 'lower', 'upper'))
        data <- data %>% summarise_(.dots = dots)
        
        if(!is_visit) {
            data[[sample_pd_avisitn_col]] <- as.factor(
                data[[sample_pd_avisitn_col]]
            )
            levels(data[[sample_pd_avisitn_col]]) <- paste(
                'Visit =', levels(data[[sample_pd_avisitn_col]])
            )
            x_var <- sample_pd_atptn_col
            data[[sample_pd_atptn_col]] <- as.numeric(data[[sample_pd_atptn_col]])
            visits <- sort(unique(data[[sample_pd_atptn_col]]))
        } else {
            x_var <- sample_pd_avisitn_col
            data[[sample_pd_avisitn_col]] <- as.numeric(data[[sample_pd_avisitn_col]])
            visits <- sort(unique(data[[sample_pd_avisitn_col]]))
        }
        ngroups <- ifelse(is_blank(input$sample_pd_time_group), 1,
                          length(unique(data[[input$sample_pd_time_group]])))
        dodge_width <- min(
            diff(range(visits)) / 40, min(diff(visits)) / (ngroups - 1)
        )
        plt <- gg_wrapper(
            data, aes_string(x = x_var, y = 'avg'),
            log_y = input$sample_pd_time_log
        )
        if(!is_blank(input$sample_pd_time_group)) {
            plt <- plt + aes_string(colour = input$sample_pd_time_group,
                                    shape = input$sample_pd_time_group)
            # plt <- plt + aes_string(colour = input$sample_pd_time_group) 
        }
        if(!is_visit && length(input$sample_pd_time_visit) > 1) {
            plt <- plt + facet_grid(formula(paste(sample_pd_avisitn_col, '~ .')))
        }
        plt <- plt +
            geom_line(position = position_dodge(dodge_width)) +
            geom_point(position = position_dodge(dodge_width), size = 2) +
            scale_x_continuous(breaks = visits, labels = visits) +
            labs(x = sample_pd_time_sumline_xlab$value,
                 y = sample_pd_time_sumline_ylab$value,
                 title = sample_pd_time_sumline_main$value)
        if(any(!is.na(data$lower) & !is.na(data$upper))) {
            plt <- plt + geom_errorbar(aes(ymin = lower, ymax = upper),
                                       width = dodge_width,
                                       position = position_dodge(dodge_width))
        }
        # plt <- add_footnote(plt, sample_pd_time_sumline_footnote$value)
        return(plt)
    })
    output$sample_pd_time_sumline_ui <- renderUI({
        # req(sample_pd_time_sumline())
        shiny::tagList(
            plotOutput('sample_pd_time_sumline'),
            uiOutput('sample_pd_time_sumline_fn_out'),
            tags$head(tags$style(
                "#sample_pd_time_sumline_fn_out{font-size: 9px;}"
            ))
        )
    })
    output$sample_pd_time_sumline <- renderPlot({
        # grid.draw(sample_pd_time_sumline())
        sample_pd_time_sumline()
    })
    output$sample_pd_time_sumline_fn_out <- renderUI({
        req(sample_pd_time_sumline_footnote$value)
        HTML(paste(
            strsplit(sample_pd_time_sumline_footnote$value, '\n')[[1]],
            collapse = '<br/>'
        ))
    })
    
    # download summary line for 1 PD analysis
    output$sample_pd_time_sumline_download_button <- renderUI({
        is_visit <- input$sample_pd_time_x == sample_pd_avisitn_col
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '1 PD analysis',
            input$sample_pd_time_param, input$sample_pd_time_y,
            input$sample_pd_time_summary,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_time_visit))),
            !is.null(input$sample_pd_time_group),
            !is.null(input$sample_pd_time_log),
            req(input$sample_pd_time_tabs) == 'Summary line')
        downloadButton('sample_pd_time_sumline_download', 'Download plot')
    })
    output$sample_pd_time_sumline_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'sample_1pd_summary_line_',
                format(Sys.Date(),format = '%Y%m%d'), '.pdf'
            )
            return(file_name)
        },
        content = function(file) {
            plot_ <- sample_pd_time_sumline()
            plot_ <- add_footnote(plot_, sample_pd_time_sumline_footnote$value)
            height_width_ratio <- 3 / 4
            width <- 8
            height <- width * height_width_ratio
            ggsave(filename = file, plot = plot_, width = width,
                   height = height, dpi = 600)
        }
    )
    
    # individual line for 1 PD analysis
    sample_pd_time_indline <- reactive({
        is_visit <- input$sample_pd_time_x == sample_pd_avisitn_col
        req(input$sample_pd_time_param, input$sample_pd_time_y,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_time_visit))),
            input$sample_pd_time_subjid, !is.null(input$sample_pd_time_group),
            !is.null(input$sample_pd_time_log))
        req(!is.null(sample_pd_time_indline_xlab$value),
            !is.null(sample_pd_time_indline_ylab$value),
            !is.null(sample_pd_time_indline_main$value),
            !is.null(sample_pd_time_indline_footnote$value))
        data <- data_$sample_pd
        x_var <- ifelse(is_visit, sample_pd_avisitn_col, sample_pd_atptn_col)
        data <- data[
            !is.na(data[[x_var]]) &
                !is.na(data[[input$sample_pd_time_y]]), , drop = FALSE
        ]
        data <- data[
            data[[sample_pd_param_col]] %in% input$sample_pd_time_param, ,
            drop = F
        ]
        if(!is_visit) {
            data <- data[
                data[[sample_pd_avisitn_col]] %in%  input$sample_pd_time_visit, ,
                drop = F
            ]
        }
        data <- data[
            data[[sample_pd_subj_col]]%in%input$sample_pd_time_subjid, ,
            drop = FALSE
        ]
        
        data[[input$sample_pd_time_y]] <- as.numeric(data[[input$sample_pd_time_y]])
        data[[sample_pd_avisitn_col]] <- as.numeric(data[[sample_pd_avisitn_col]])
        data[[sample_pd_subj_col]] <- trimws(as.character(
            data[[sample_pd_subj_col]]
        ))
        
        if(!is_blank(input$sample_pd_time_group)) {
            data[[input$sample_pd_time_group]] <- factor(
                data[[input$sample_pd_time_group]],
                levels = sort(unique(data[[input$sample_pd_time_group]]))
            )
            if(!is_visit) {
                data <- arrange_(data, input$sample_pd_time_group,
                                 sample_pd_subj_col,
                                 sample_pd_avisitn_col,
                                 sample_pd_atptn_col)
            } else {
                data <- arrange_(data, input$sample_pd_time_group,
                                 sample_pd_subj_col,
                                 sample_pd_avisitn_col)
            }
        } else {
            if(!is_visit) {
                data <- arrange_(data, sample_pd_subj_col,
                                 sample_pd_avisitn_col,
                                 sample_pd_atptn_col)
            } else {
                data <- arrange_(data, sample_pd_subj_col,
                                 sample_pd_avisitn_col)
            }
        }
        
        data[[sample_pd_subj_col]] <- factor(
            data[[sample_pd_subj_col]],
            levels = unique(data[[sample_pd_subj_col]])
        )
        
        # expr <- list(
        #     lazyeval::interp(~mean_na(var), var = as.name(input$sample_pd_time_y))
        # )
        # dots <- setNames(expr, 'avg')
        # data <- data %>% summarise_(.dots = dots)
        
        if(!is_visit) {
            data[[sample_pd_avisitn_col]] <- factor(
                data[[sample_pd_avisitn_col]]
            )
            levels(data[[sample_pd_avisitn_col]]) <- paste(
                'Visit =', levels(data[[sample_pd_avisitn_col]])
            )
            data[[sample_pd_atptn_col]] <- as.numeric(data[[sample_pd_atptn_col]])
            visits <- sort(unique(data[[sample_pd_atptn_col]]))
        } else {
            data[[sample_pd_avisitn_col]] <- as.numeric(data[[sample_pd_avisitn_col]])
            visits <- sort(unique(data[[sample_pd_avisitn_col]]))
        }

        nsubj <- length(levels(data[[sample_pd_subj_col]]))
        shape_value <- seq_len(nsubj)
        plt <- gg_wrapper(
            data, aes_string(x = x_var, y = input$sample_pd_time_y,
                             group = sample_pd_subj_col),
            log_y = input$sample_pd_time_log
        )
        if(!is_blank(input$sample_pd_time_group)) {
            nsubj_group <- sapply(by(
                data, data[[input$sample_pd_time_group]],
                function(df) {unique(df[[sample_pd_subj_col]])}
            ), length)
            shape_value <- unname(sequence(nsubj_group))
            ngroups <- length(nsubj_group)
            all_colors <- gg_color_hue(ngroups)
            color_value <- all_colors[rep.int(1:ngroups, times = nsubj_group)]

            plt <- plt + aes_string(colour = sample_pd_subj_col,
                                    fill = input$sample_pd_time_group) +
                scale_colour_manual(
                    name = sample_pd_subj_col,
                    labels = levels(data[[sample_pd_subj_col]]),
                    values = color_value
                ) +
                scale_fill_manual(
                    name = input$sample_pd_time_group,
                    labels = levels(data[[input$sample_pd_time_group]]),
                    values = all_colors,
                    guide = guide_legend(
                        override.aes = list(colour = all_colors),
                        order = 2
                    )
                )
        }
        if((!is_visit) && length(input$sample_pd_time_visit) > 1) {
            plt <- plt + facet_grid(formula(paste(sample_pd_avisitn_col, '~ .')))
        }
        plt <- plt +
            stat_summary(fun.y = mean_na, geom = 'line') +
            stat_summary(aes_string(shape = sample_pd_subj_col),
                         fun.y = mean_na, geom = 'point') +
            scale_x_continuous(breaks = visits, labels = visits) +
            scale_shape_manual(
                name = sample_pd_subj_col,
                labels = levels(data[[sample_pd_subj_col]]),
                values = shape_value
            ) +
            labs(x = sample_pd_time_indline_xlab$value,
                 y = sample_pd_time_indline_ylab$value,
                 title = sample_pd_time_indline_main$value)
        # plt <- add_footnote(plt, sample_pd_time_indline_footnote$value)
        return(plt)
    })
    output$sample_pd_time_indline_ui <- renderUI({
        # req(sample_pd_time_indline())
        shiny::tagList(
            plotOutput('sample_pd_time_indline'),
            uiOutput('sample_pd_time_indline_fn_out'),
            tags$head(tags$style(
                "#sample_pd_time_indline_fn_out{font-size: 9px;}"
            ))
        )
    })
    output$sample_pd_time_indline <- renderPlot({
        # grid.draw(sample_pd_time_indline())
        sample_pd_time_indline()
    })
    output$sample_pd_time_indline_fn_out <- renderUI({
        req(sample_pd_time_indline_footnote$value)
        HTML(paste(
            strsplit(sample_pd_time_indline_footnote$value, '\n')[[1]],
            collapse = '<br/>'
        ))
    })
    
    # download individual line for 1 PD analysis
    output$sample_pd_time_indline_download_button <- renderUI({
        is_visit <- input$sample_pd_time_x == sample_pd_avisitn_col
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '1 PD analysis',
            input$sample_pd_time_param, input$sample_pd_time_y,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_time_visit))),
            input$sample_pd_time_subjid, !is.null(input$sample_pd_time_group),
            !is.null(input$sample_pd_time_log),
            req(input$sample_pd_time_tabs) == 'Individual line')
        downloadButton('sample_pd_time_indline_download', 'Download plot')
    })
    output$sample_pd_time_indline_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'sample_1pd_individual_line_',
                format(Sys.Date(),format = '%Y%m%d'), '.pdf'
            )
            return(file_name)
        },
        content = function(file) {
            plot_ <- sample_pd_time_indline()
            plot_ <- add_footnote(plot_, sample_pd_time_indline_footnote$value)
            height_width_ratio <- 3 / 4
            width <- 8
            height <- width * height_width_ratio
            ggsave(filename = file, plot = plot_, width = width,
                   height = height, dpi = 600)
        }
    )
    
    
    #-----------------------------------------------
    # UI widgets for PD analysis - correlation
    
    # a selectInput for choosing PD parameter 1
    output$sample_pd_corr_param1 <- renderUI({
        req(data_import_status$sample_pd,
            input$sample_pd_analysis_type == '2 PD analysis')
        data <- data_$sample_pd
        choices <- c('Choose' = '', sort(unique(data[[sample_pd_param_col]])))
        selectInput('sample_pd_corr_param1', 'PD parameter 1', choices)
    })
    # a selectInput for choosing y for PD parameter 1
    output$sample_pd_corr_y1 <- renderUI({
        req(data_import_status$sample_pd,
            input$sample_pd_analysis_type == '2 PD analysis')
        selectInput('sample_pd_corr_y1', 'PD value 1', sample_pd_dose_y_types)
    })
    # a selectInput for choosing PD parameter 2
    output$sample_pd_corr_param2 <- renderUI({
        req(data_import_status$sample_pd,
            input$sample_pd_analysis_type == '2 PD analysis')
        data <- data_$sample_pd
        choices <- c('Choose' = '', sort(unique(data[[sample_pd_param_col]])))
        selectInput('sample_pd_corr_param2', 'PD parameter 2', choices)
    })
    # a selectInput for choosing y for PD parameter 2
    output$sample_pd_corr_y2 <- renderUI({
        req(data_import_status$sample_pd,
            input$sample_pd_analysis_type == '2 PD analysis')
        selectInput('sample_pd_corr_y2', 'PD value 2', sample_pd_dose_y_types)
    })
    # selectInput for choosing x variable
    output$sample_pd_corr_x <- renderUI({
        req(data_import_status$sample_pd,
            req(input$sample_pd_analysis_type) == '2 PD analysis',
            input$sample_pd_corr_tabs == 'Summary line')
        choices <- c('Choose' = '', sample_pd_avisitn_col, sample_pd_atptn_col)
        selected <- isolate(ifelse(
            is.null(input$sample_pd_corr_x), '',
            input$sample_pd_corr_x[input$sample_pd_corr_x %in% choices]
        ))
        selectInput('sample_pd_corr_x', 'X variable', choices, selected)
    })
    # a selectizeInput for choosing visit(s)
    output$sample_pd_corr_visit <- renderUI({
        req(data_import_status$sample_pd,
            req(input$sample_pd_analysis_type) == '2 PD analysis',
            input$sample_pd_corr_tabs == 'Summary line',
            req(input$sample_pd_corr_x) == sample_pd_atptn_col)
        data <- data_$sample_pd
        choices <- sort(unique(data[[sample_pd_avisitn_col]]))
        selected <- isolate(ifelse(
            is.null(input$sample_pd_corr_visit), '',
            input$sample_pd_corr_visit[input$sample_pd_corr_visit %in% choices]
        ))
        selectizeInput(
            'sample_pd_corr_visit', 'Visit',
            c('Choose' = '', choices), selected = selected, multiple = T,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    # a selectInput for choosing group variable
    output$sample_pd_corr_group <- renderUI({
        req(data_import_status$sample_pd,
            input$sample_pd_analysis_type == '2 PD analysis')
        data <- data_$sample_pd
        choices <- names(data)
        to_exclude <- c(sample_pd_param_col, sample_pd_aval_col)
        choices <- c('Choose' = '', sort(setdiff(choices, to_exclude)))
        stillSelected <- isolate(ifelse(
            is.null(input$sample_pd_corr_group), sample_pd_dose_col,
            input$sample_pd_corr_group[input$sample_pd_corr_group %in% choices]
        ))
        selectInput('sample_pd_corr_group', 'Group', choices, stillSelected)
    })
    # a selectInput for choosing summary statistics
    output$sample_pd_corr_summary <- renderUI({
        req(data_import_status$sample_pd,
            input$sample_pd_analysis_type == '2 PD analysis',
            input$sample_pd_corr_tabs == 'Summary line')
        selectInput('sample_pd_corr_summary', 'Statistics',
                    sample_pd_corr_summary_opts)
    })
    # a radioButtons for adding reference line to the scatter plot
    output$sample_pd_corr_refline <- renderUI({
        req(data_import_status$sample_pd,
            req(input$sample_pd_analysis_type) == '2 PD analysis',
            req(input$sample_pd_corr_tabs) == 'Scatter plot')
        choices <- c('None', 'Loess', 'Linear regression')
        radioButtons('sample_pd_corr_refline', 'Add reference line',
                     choices = choices, inline = TRUE)
    })
    # a checkbox for specifying log of PD parameter 1
    output$sample_pd_corr_log_1 <- renderUI({
        req(data_import_status$sample_pd,
            input$sample_pd_analysis_type == '2 PD analysis')
        checkboxInput('sample_pd_corr_log_1', 'Log of PD parameter 1', value = FALSE)
    })
    # a checkbox for specifying log of PD parameter 2
    output$sample_pd_corr_log_2 <- renderUI({
        req(data_import_status$sample_pd,
            input$sample_pd_analysis_type == '2 PD analysis')
        checkboxInput('sample_pd_corr_log_2', 'Log of parameter 2', value = FALSE)
    })
    
    #-----------------------------------------------
    # Further table/graph refine for PD analysis - correlation
    
    # a textareaInput for specifying xlab for summary line
    output$sample_pd_corr_sumline_xlab <- renderUI({
        is_visit <- input$sample_pd_corr_x == sample_pd_avisitn_col
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '2 PD analysis',
            input$sample_pd_corr_param1, input$sample_pd_corr_y1,
            input$sample_pd_corr_param2, input$sample_pd_corr_y2,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_corr_visit))),
            input$sample_pd_corr_summary,
            !is.null(input$sample_pd_corr_group), 
            !is.null(input$sample_pd_corr_log_1),
            !is.null(input$sample_pd_corr_log_2),
            req(input$sample_pd_corr_tabs) == 'Summary line')
        value <- ifelse(is_visit, 'Visit', 'Timepoint')
        textareaInput('sample_pd_corr_sumline_xlab', 'X-axis label',
                      value = value)
    })
    sample_pd_corr_sumline_xlab <- reactiveValues(value = NULL)
    observe({
        is_visit <- input$sample_pd_corr_x == sample_pd_avisitn_col
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '2 PD analysis',
            input$sample_pd_corr_param1, input$sample_pd_corr_y1,
            input$sample_pd_corr_param2, input$sample_pd_corr_y2,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_corr_visit))),
            input$sample_pd_corr_summary,
            !is.null(input$sample_pd_corr_group), 
            !is.null(input$sample_pd_corr_log_1),
            !is.null(input$sample_pd_corr_log_2),
            req(input$sample_pd_corr_tabs) == 'Summary line')
        value <- ifelse(is_visit, 'Visit', 'Timepoint')
        sample_pd_corr_sumline_xlab$value <- value
    })
    observe({
        input$sample_pd_corr_sumline_xlab
        sample_pd_corr_sumline_xlab$value <- input$sample_pd_corr_sumline_xlab
    })
    
    # a textareaInput for specifying left ylab for summary line
    output$sample_pd_corr_sumline_ylabl <- renderUI({
        is_visit <- input$sample_pd_corr_x == sample_pd_avisitn_col
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '2 PD analysis',
            input$sample_pd_corr_param1, input$sample_pd_corr_y1,
            input$sample_pd_corr_param2, input$sample_pd_corr_y2,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_corr_visit))),
            input$sample_pd_corr_summary,
            !is.null(input$sample_pd_corr_group), 
            !is.null(input$sample_pd_corr_log_1),
            !is.null(input$sample_pd_corr_log_2),
            req(input$sample_pd_corr_tabs) == 'Summary line')
        value <- input$sample_pd_corr_param1
        textareaInput('sample_pd_corr_sumline_ylabl', 'Left Y-axis label',
                      value = value)
    })
    sample_pd_corr_sumline_ylabl <- reactiveValues(value = NULL)
    observe({
        is_visit <- input$sample_pd_corr_x == sample_pd_avisitn_col
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '2 PD analysis',
            input$sample_pd_corr_param1, input$sample_pd_corr_y1,
            input$sample_pd_corr_param2, input$sample_pd_corr_y2,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_corr_visit))),
            input$sample_pd_corr_summary,
            !is.null(input$sample_pd_corr_group), 
            !is.null(input$sample_pd_corr_log_1),
            !is.null(input$sample_pd_corr_log_2),
            req(input$sample_pd_corr_tabs) == 'Summary line')
        value <- input$sample_pd_corr_param1
        sample_pd_corr_sumline_ylabl$value <- value
    })
    observe({
        input$sample_pd_corr_sumline_ylabl
        sample_pd_corr_sumline_ylabl$value <- input$sample_pd_corr_sumline_ylabl
    })
    
    # a textareaInput for specifying right ylab for summary line
    output$sample_pd_corr_sumline_ylabr <- renderUI({
        is_visit <- input$sample_pd_corr_x == sample_pd_avisitn_col
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '2 PD analysis',
            input$sample_pd_corr_param1, input$sample_pd_corr_y1,
            input$sample_pd_corr_param2, input$sample_pd_corr_y2,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_corr_visit))),
            input$sample_pd_corr_summary,
            !is.null(input$sample_pd_corr_group), 
            !is.null(input$sample_pd_corr_log_1),
            !is.null(input$sample_pd_corr_log_2),
            req(input$sample_pd_corr_tabs) == 'Summary line')
        value <- input$sample_pd_corr_param2
        textareaInput('sample_pd_corr_sumline_ylabr', 'Right Y-axis label',
                      value = value)
    })
    sample_pd_corr_sumline_ylabr <- reactiveValues(value = NULL)
    observe({
        is_visit <- input$sample_pd_corr_x == sample_pd_avisitn_col
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '2 PD analysis',
            input$sample_pd_corr_param1, input$sample_pd_corr_y1,
            input$sample_pd_corr_param2, input$sample_pd_corr_y2,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_corr_visit))),
            input$sample_pd_corr_summary,
            !is.null(input$sample_pd_corr_group), 
            !is.null(input$sample_pd_corr_log_1),
            !is.null(input$sample_pd_corr_log_2),
            req(input$sample_pd_corr_tabs) == 'Summary line')
        value <- input$sample_pd_corr_param2
        sample_pd_corr_sumline_ylabr$value <- value
    })
    observe({
        input$sample_pd_corr_sumline_ylabr
        sample_pd_corr_sumline_ylabr$value <- input$sample_pd_corr_sumline_ylabr
    })
    
    # a textareaInput for specifying main for summary line
    output$sample_pd_corr_sumline_main <- renderUI({
        is_visit <- input$sample_pd_corr_x == sample_pd_avisitn_col
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '2 PD analysis',
            input$sample_pd_corr_param1, input$sample_pd_corr_y1,
            input$sample_pd_corr_param2, input$sample_pd_corr_y2,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_corr_visit))),
            input$sample_pd_corr_summary,
            !is.null(input$sample_pd_corr_group), 
            !is.null(input$sample_pd_corr_log_1),
            !is.null(input$sample_pd_corr_log_2),
            req(input$sample_pd_corr_tabs) == 'Summary line')
        value <- paste(
            summary_title_dict[[input$sample_pd_corr_summary]], 'of', 
            input$sample_pd_corr_param1, 'and', input$sample_pd_corr_param2
        )
        if(!is_blank(input$sample_pd_corr_group))
            value <- paste(value, 'by', input$sample_pd_corr_group)
        textareaInput('sample_pd_corr_sumline_main', 'Plot title',
                      value = value)
    })
    sample_pd_corr_sumline_main <- reactiveValues(value = NULL)
    observe({
        is_visit <- input$sample_pd_corr_x == sample_pd_avisitn_col
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '2 PD analysis',
            input$sample_pd_corr_param1, input$sample_pd_corr_y1,
            input$sample_pd_corr_param2, input$sample_pd_corr_y2,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_corr_visit))),
            input$sample_pd_corr_summary,
            !is.null(input$sample_pd_corr_group),
            !is.null(input$sample_pd_corr_log_1),
            !is.null(input$sample_pd_corr_log_2),
            req(input$sample_pd_corr_tabs) == 'Summary line')
        value <- paste(
            summary_title_dict[[input$sample_pd_corr_summary]], 'of', 
            input$sample_pd_corr_param1, 'and', input$sample_pd_corr_param2
        )
        if(!is_blank(input$sample_pd_corr_group))
            value <- paste(value, 'by', input$sample_pd_corr_group)
        sample_pd_corr_sumline_main$value <- value
    })
    observe({
        input$sample_pd_corr_sumline_main
        sample_pd_corr_sumline_main$value <- input$sample_pd_corr_sumline_main
    })
    
    # a textareaInput for specifying footnote for summary line
    output$sample_pd_corr_sumline_footnote <- renderUI({
        is_visit <- input$sample_pd_corr_x == sample_pd_avisitn_col
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '2 PD analysis',
            input$sample_pd_corr_param1, input$sample_pd_corr_y1,
            input$sample_pd_corr_param2, input$sample_pd_corr_y2,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_corr_visit))),
            input$sample_pd_corr_summary,
            !is.null(input$sample_pd_corr_group), 
            !is.null(input$sample_pd_corr_log_1),
            !is.null(input$sample_pd_corr_log_2),
            req(input$sample_pd_corr_tabs) == 'Summary line')
        value <- default_footnote$sample_pd
        textareaInput('sample_pd_corr_sumline_footnote', 'Plot footnote',
                      value = value)
    })
    sample_pd_corr_sumline_footnote <- reactiveValues(value = '')
    observe({
        input$sample_pd_corr_sumline_footnote
        sample_pd_corr_sumline_footnote$value <-
            input$sample_pd_corr_sumline_footnote
    })
    
    # a checkbox for specifying same dual y axis for summary line
    output$sample_pd_corr_sumline_same_y <- renderUI({
        is_visit <- input$sample_pd_corr_x == sample_pd_avisitn_col
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '2 PD analysis',
            input$sample_pd_corr_param1, input$sample_pd_corr_y1,
            input$sample_pd_corr_param2, input$sample_pd_corr_y2,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_corr_visit))),
            input$sample_pd_corr_summary,
            !is.null(input$sample_pd_corr_group),
            !is.null(input$sample_pd_corr_log_1),
            !is.null(input$sample_pd_corr_log_2),
            req(input$sample_pd_corr_tabs) == 'Summary line')
        checkboxInput('sample_pd_corr_sumline_same_y', 'Same y axis', value = F)
    })
    sample_pd_corr_sumline_same_y <- reactiveValues(value = FALSE)
    observe({
        input$sample_pd_corr_sumline_same_y
        sample_pd_corr_sumline_same_y$value <- input$sample_pd_corr_sumline_same_y
    })
    
    # a textareaInput for specifying xlab for scatter plot
    output$sample_pd_corr_scatter_xlab <- renderUI({
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '2 PD analysis',
            input$sample_pd_corr_param1, input$sample_pd_corr_y1,
            input$sample_pd_corr_param2, input$sample_pd_corr_y2,
            !is.null(input$sample_pd_corr_group),
            !is.null(input$sample_pd_corr_refline),
            !is.null(input$sample_pd_corr_log_1),
            !is.null(input$sample_pd_corr_log_2),
            req(input$sample_pd_corr_tabs) == 'Scatter plot')
        value <- input$sample_pd_corr_param1
        textareaInput('sample_pd_corr_scatter_xlab', 'X-axis label',
                      value = value)
    })
    sample_pd_corr_scatter_xlab <- reactiveValues(value = NULL)
    observe({
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '2 PD analysis',
            input$sample_pd_corr_param1, input$sample_pd_corr_y1,
            input$sample_pd_corr_param2, input$sample_pd_corr_y2,
            !is.null(input$sample_pd_corr_group),
            !is.null(input$sample_pd_corr_refline),
            !is.null(input$sample_pd_corr_log_1),
            !is.null(input$sample_pd_corr_log_2),
            req(input$sample_pd_corr_tabs) == 'Scatter plot')
        value <- input$sample_pd_corr_param1
        sample_pd_corr_scatter_xlab$value <- value
    })
    observe({
        input$sample_pd_corr_scatter_xlab
        sample_pd_corr_scatter_xlab$value <- input$sample_pd_corr_scatter_xlab
    })
    
    # a textareaInput for specifying ylab for scatter plot
    output$sample_pd_corr_scatter_ylab <- renderUI({
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '2 PD analysis',
            input$sample_pd_corr_param1, input$sample_pd_corr_y1,
            input$sample_pd_corr_param2, input$sample_pd_corr_y2,
            !is.null(input$sample_pd_corr_group),
            !is.null(input$sample_pd_corr_refline),
            !is.null(input$sample_pd_corr_log_1),
            !is.null(input$sample_pd_corr_log_2),
            req(input$sample_pd_corr_tabs) == 'Scatter plot')
        value <- input$sample_pd_corr_param2
        textareaInput('sample_pd_corr_scatter_ylab', 'Y-axis label',
                      value = value)
    })
    sample_pd_corr_scatter_ylab <- reactiveValues(value = NULL)
    observe({
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '2 PD analysis',
            input$sample_pd_corr_param1, input$sample_pd_corr_y1,
            input$sample_pd_corr_param2, input$sample_pd_corr_y2,
            !is.null(input$sample_pd_corr_group),
            !is.null(input$sample_pd_corr_refline),
            !is.null(input$sample_pd_corr_log_1),
            !is.null(input$sample_pd_corr_log_2),
            req(input$sample_pd_corr_tabs) == 'Scatter plot')
        value <- input$sample_pd_corr_param2
        sample_pd_corr_scatter_ylab$value <- value
    })
    observe({
        input$sample_pd_corr_scatter_ylab
        sample_pd_corr_scatter_ylab$value <- input$sample_pd_corr_scatter_ylab
    })
    
    # a textareaInput for specifying main for scatter plot
    output$sample_pd_corr_scatter_main <- renderUI({
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '2 PD analysis',
            input$sample_pd_corr_param1, input$sample_pd_corr_y1,
            input$sample_pd_corr_param2, input$sample_pd_corr_y2,
            !is.null(input$sample_pd_corr_group),
            !is.null(input$sample_pd_corr_refline),
            !is.null(input$sample_pd_corr_log_1),
            !is.null(input$sample_pd_corr_log_2),
            req(input$sample_pd_corr_tabs) == 'Scatter plot')
        value <- paste(
            'Scatter plot between', input$sample_pd_corr_param1, 'and',
            input$sample_pd_corr_param2
        )
        if(!is_blank(input$sample_pd_corr_group))
            value <- paste(value, 'by', input$sample_pd_corr_group)
        textareaInput('sample_pd_corr_scatter_main', 'Plot title',
                      value = value)
    })
    sample_pd_corr_scatter_main <- reactiveValues(value = NULL)
    observe({
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '2 PD analysis',
            input$sample_pd_corr_param1, input$sample_pd_corr_y1,
            input$sample_pd_corr_param2, input$sample_pd_corr_y2,
            !is.null(input$sample_pd_corr_group),
            !is.null(input$sample_pd_corr_refline),
            !is.null(input$sample_pd_corr_log_1),
            !is.null(input$sample_pd_corr_log_2),
            req(input$sample_pd_corr_tabs) == 'Scatter plot')
        value <- paste(
            'Scatter plot between', input$sample_pd_corr_param1, 'and',
            input$sample_pd_corr_param2
        )
        if(!is_blank(input$sample_pd_corr_group))
            value <- paste(value, 'by', input$sample_pd_corr_group)
        sample_pd_corr_scatter_main$value <- value
    })
    observe({
        input$sample_pd_corr_scatter_main
        sample_pd_corr_scatter_main$value <- input$sample_pd_corr_scatter_main
    })
    
    # a textareaInput for specifying footnote for scatter plot
    output$sample_pd_corr_scatter_footnote <- renderUI({
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '2 PD analysis',
            input$sample_pd_corr_param1, input$sample_pd_corr_y1,
            input$sample_pd_corr_param2, input$sample_pd_corr_y2,
            !is.null(input$sample_pd_corr_group),
            !is.null(input$sample_pd_corr_refline),
            !is.null(input$sample_pd_corr_log_1),
            !is.null(input$sample_pd_corr_log_2),
            req(input$sample_pd_corr_tabs) == 'Scatter plot')
        value <- default_footnote$sample_pd
        textareaInput('sample_pd_corr_scatter_footnote', 'Plot footnote',
                      value = value)
    })
    sample_pd_corr_scatter_footnote <- reactiveValues(value = '')
    observe({
        input$sample_pd_corr_scatter_footnote
        sample_pd_corr_scatter_footnote$value <-
            input$sample_pd_corr_scatter_footnote
    })
    
    #-----------------------------------------------
    # Output for PD analysis - correlation
    
    # Scatter plot PD- PD correlation
    sample_pd_corr_scatter <- reactive({
        data <- data_$sample_pd
        req(data, input$sample_pd_corr_param1, input$sample_pd_corr_y1,
            input$sample_pd_corr_param2, input$sample_pd_corr_y2,
            !is.null(input$sample_pd_corr_group),
            !is.null(input$sample_pd_corr_refline),
            !is.null(input$sample_pd_corr_log_1),
            !is.null(input$sample_pd_corr_log_2))
        req(!is.null(sample_pd_corr_scatter_xlab$value),
            !is.null(sample_pd_corr_scatter_ylab$value),
            !is.null(sample_pd_corr_scatter_main$value),
            !is.null(sample_pd_corr_scatter_footnote$value))
        is_visit <- !any(!sapply(data[[sample_pd_atptn_col]], is_blank))
        data <- data[
            !is.na(data[[input$sample_pd_corr_y1]]) &
                !is.na(data[[input$sample_pd_corr_y2]]), , drop = FALSE
        ]
        if(!is_blank(input$sample_pd_corr_group)) {
            if(!is_visit) {
                formula_ <- formula(paste(
                    input$sample_pd_corr_group, '+', sample_pd_subj_col, '+',
                    sample_pd_avisitn_col, '+', sample_pd_atptn_col,
                    '~', sample_pd_param_col
                ))
            } else {
                formula_ <- formula(paste(
                    input$sample_pd_corr_group, '+', sample_pd_subj_col, '+',
                    sample_pd_avisitn_col, '~', sample_pd_param_col
                ))
            }
        } else {
            if(!is_visit) {
                formula_ <- formula(paste(
                    sample_pd_subj_col, '+', sample_pd_avisitn_col, '+',
                    sample_pd_atptn_col, '~', sample_pd_param_col
                ))
            } else {
                formula_ <- formula(paste(
                    sample_pd_subj_col, '+', sample_pd_avisitn_col,
                    '~', sample_pd_param_col
                ))
            }
        }
        # data <- reshape2::dcast(data, formula_, fun.aggregate = mean,
        #                         na.rm = TRUE, value.var = sample_pd_aval_col)
        # data <- data[!is.na(data[[input$sample_pd_corr_param1]]) &
        #                  !is.na(data[[input$sample_pd_corr_param2]]), ]
        data <- data.table::dcast(
            data.table::as.data.table(data), formula_,
            fun.aggregate = mean, na.rm = TRUE,
            value.var = c(input$sample_pd_corr_y1, input$sample_pd_corr_y2)
        )
        if(input$sample_pd_corr_y1 == input$sample_pd_corr_y2) {
            pd_var_1 <- input$sample_pd_corr_param1
            pd_var_2 <- input$sample_pd_corr_param2
        } else {
            pd_var_1 <- paste(input$sample_pd_corr_y1, 'mean',
                              input$sample_pd_corr_param1, sep = '_')
            pd_var_2 <- paste(input$sample_pd_corr_y2, 'mean',
                              input$sample_pd_corr_param2, sep = '_')
        }
        data <- data[!is.na(data[[pd_var_1]]) & !is.na(data[[pd_var_2]]), ]
        if(!is_blank(input$sample_pd_corr_group)) {
            data[[input$sample_pd_corr_group]] <- factor(
                as.character(data[[input$sample_pd_corr_group]])
            )
        }
        baseplot <- gg_wrapper(
            data, aes_string(x = paste0('`', pd_var_1, '`'),
                             y = paste0('`', pd_var_2, '`')),
            log_x = input$sample_pd_corr_log_1,
            log_y = input$sample_pd_corr_log_2
        )
        if(!is_blank(input$sample_pd_corr_group)) {
            baseplot <- baseplot +
                geom_point(aes_string(colour = input$sample_pd_corr_group,
                                      fill = input$sample_pd_corr_group),
                           size = 2)
        } else baseplot <- baseplot + geom_point(size = 2)
        if(!is_blank(input$sample_pd_corr_refline)) {
            if('Loess' %in% input$sample_pd_corr_refline)
                baseplot <- baseplot + geom_smooth()
            if('Linear regression' %in% input$sample_pd_corr_refline)
                baseplot <- baseplot + geom_smooth(method = 'lm')
        }
        baseplot <- baseplot +
            labs(x = sample_pd_corr_scatter_xlab$value,
                 y = sample_pd_corr_scatter_ylab$value,
                 title = sample_pd_corr_scatter_main$value)
        # baseplot <- add_footnote(baseplot, sample_pd_corr_scatter_footnote$value)
        return(baseplot)
    })
    output$sample_pd_corr_scatter_ui <- renderUI({
        # req(sample_pd_corr_scatter())
        shiny::tagList(
            plotOutput('sample_pd_corr_scatter'),
            uiOutput('sample_pd_corr_scatter_fn_out'),
            tags$head(tags$style(
                "#sample_pd_corr_scatter_fn_out{font-size: 9px;}"
            ))
        )
    })
    output$sample_pd_corr_scatter <- renderPlot({
        # grid.draw(sample_pd_corr_scatter())
        sample_pd_corr_scatter()
    })
    output$sample_pd_corr_scatter_fn_out <- renderUI({
        req(sample_pd_corr_scatter_footnote$value)
        HTML(paste(
            strsplit(sample_pd_corr_scatter_footnote$value, '\n')[[1]],
            collapse = '<br/>'
        ))
    })
    
    
    # download scatter plot PD-PD correlation
    output$sample_pd_corr_scatter_download_button <- renderUI({
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '2 PD analysis',
            input$sample_pd_corr_param1, input$sample_pd_corr_y1,
            input$sample_pd_corr_param2, input$sample_pd_corr_y2,
            !is.null(input$sample_pd_corr_group),
            !is.null(input$sample_pd_corr_refline),
            !is.null(input$sample_pd_corr_log_1),
            !is.null(input$sample_pd_corr_log_2),
            req(input$sample_pd_corr_tabs) == 'Scatter plot')
        downloadButton('sample_pd_corr_scatter_download', 'Download plot')
    })
    output$sample_pd_corr_scatter_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'sample_2pd_scatter_plot_',
                format(Sys.Date(),format = '%Y%m%d'), '.pdf'
            )
            return(file_name)
        },
        content = function(file) {
            plot_ <- sample_pd_corr_scatter()
            plot_ <- add_footnote(plot_, sample_pd_corr_scatter_footnote$value)
            height_width_ratio <- 3 / 4
            width <- 8
            height <- width * height_width_ratio
            ggsave(filename = file, plot = plot_, width = width,
                   height = height, dpi = 600)
        }
    )
    
    # Summary line plot for two PD parameters
    output$sample_pd_corr_sumline <- renderPlot({
        data <- data_$sample_pd
        is_visit <- input$sample_pd_corr_x == sample_pd_avisitn_col
        req(data, input$sample_pd_corr_param1, input$sample_pd_corr_y1,
            input$sample_pd_corr_param2, input$sample_pd_corr_y2,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_corr_visit))),
            input$sample_pd_corr_summary,
            !is.null(input$sample_pd_corr_group), 
            !is.null(input$sample_pd_corr_log_1),
            !is.null(input$sample_pd_corr_log_2))
        req(!is.null(sample_pd_corr_sumline_xlab$value),
            !is.null(sample_pd_corr_sumline_ylabl$value),
            !is.null(sample_pd_corr_sumline_ylabr$value),
            !is.null(sample_pd_corr_sumline_main$value),
            !is.null(sample_pd_corr_sumline_footnote$value))
        x_var <- ifelse(is_visit, sample_pd_avisitn_col, sample_pd_atptn_col)
        data <- data[
            !is.na(data[[x_var]]) &
                !is.na(data[[input$sample_pd_corr_y1]]) &
                !is.na(data[[input$sample_pd_corr_y2]]), , drop = FALSE
        ]
        if(!is_visit) {
            cond <- data[[sample_pd_avisitn_col]] %in% input$sample_pd_corr_visit
            data <- data[cond, , drop = FALSE]
        }
        formula_ <- ifelse(is_blank(input$sample_pd_corr_group), '',
                           paste(input$sample_pd_corr_group, '+'))
        formula_ <- paste(formula_, sample_pd_subj_col, '+')
        if(!is_visit) {
            formula_ <- paste(
                formula_, sample_pd_avisitn_col, '+', input$sample_pd_corr_x
            )
        } else formula_ <- paste(formula_, sample_pd_avisitn_col)
        formula_ <- paste(formula_, '~', subj_pd_param_col)
        # data <- reshape2::dcast(data, formula_, fun.aggregate = mean,
        #                         na.rm = TRUE, value.var = sample_pd_aval_col)
        data <- data.table::dcast(
            data.table::as.data.table(data), formula_,
            fun.aggregate = mean, na.rm = TRUE,
            value.var = c(input$sample_pd_corr_y1, input$sample_pd_corr_y2)
        )
        if(input$sample_pd_corr_y1 == input$sample_pd_corr_y2) {
            pd_var_1 <- input$sample_pd_corr_param1
            pd_var_2 <- input$sample_pd_corr_param2
        } else {
            pd_var_1 <- paste(input$sample_pd_corr_y1, 'mean',
                              input$sample_pd_corr_param1, sep = '_')
            pd_var_2 <- paste(input$sample_pd_corr_y2, 'mean',
                              input$sample_pd_corr_param2, sep = '_')
        }
        data[[input$sample_pd_corr_x]] <- as.numeric(data[[input$sample_pd_corr_x]])
        if(input$sample_pd_corr_summary == 'Mean + SD') {
            method <- 'mean_sd'
        } else if(input$sample_pd_corr_summary == 'Mean + SE') {
            method <- 'mean_se'
        } else if(input$sample_pd_corr_summary == 'Median + IQR') {
            method <- 'median_iqr'
        }
        nna_x <- !is.na(data[[input$sample_pd_corr_x]])
        cond_na <- (!is.na(data[[pd_var_1]]) & nna_x) ||
            (!is.na(data[[pd_var_2]]) & nna_x)
        data <- data[cond_na, , drop = FALSE]
        group_ <- NULL
        if(!is_blank(input$sample_pd_corr_group)) {
            data[[input$sample_pd_corr_group]] <- factor(
                as.character(data[[input$sample_pd_corr_group]])
            )
            group_ <- input$sample_pd_corr_group
        }
        footnote <- trimws(sample_pd_corr_sumline_footnote$value)
        fnote_lines <- ifelse(is_blank(footnote), 0,
                              length(strsplit(footnote, '\n')[[1]]))
        fnote_size <- 0.8
        fnote_space <- 0.8 + fnote_lines * fnote_size + 0.2
        if(is_visit) {
            has_group <- !is.null(group_)
            if(has_group) {
                if(!is_blank(footnote)) oma_bottom <- 3 + fnote_space
                else oma_bottom <- 3
            } else {
                if(!is_blank(footnote)) oma_bottom <- fnote_space
                else oma_bottom <- 0
            }
            par_opt <- par(oma = c(oma_bottom, 0, 0, 0))
            if(has_group) plot_footnote <- character(0)
            else plot_footnote <- footnote
            forest_plot <- dual_y_axis_sumline(
                data, input$sample_pd_corr_x,
                pd_var_1, var_y2 = pd_var_2,
                group = group_,
                xlab = sample_pd_corr_sumline_xlab$value,
                ylab1 = sample_pd_corr_sumline_ylabl$value,
                ylab2 = sample_pd_corr_sumline_ylabr$value,
                ylog1 = input$sample_pd_corr_log_1,
                ylog2 = input$sample_pd_corr_log_2,
                title = sample_pd_corr_sumline_main$value,
                footnote = plot_footnote,
                method = method, type = 'o',
                same_y_axis = sample_pd_corr_sumline_same_y$value,
                save_plot = FALSE
            )
            if(has_group) {
                oma_bottom_legend <- ifelse(is_blank(footnote), 0, fnote_space)
                ngroups <- length(unique(data[[group_]]))
                all_cols <- colorRampPalette(c('red4', 'white'))(3 * ngroups)
                leg_cols <- all_cols[seq(1, 3 * ngroups, by = 3)]
                par(fig = c(0, 1, 0, 1), oma = c(oma_bottom_legend, 0, 0, 0),
                    mar = c(0, 0, 0, 0), new = TRUE)
                plot(0, 0, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
                legend('bottom', levels(data[[group_]]),
                       xpd = TRUE, horiz = TRUE, inset = c(0, 0),
                       lty = 1, lwd = 1, bty = 'n', col = leg_cols,
                       title = group_)
                if(!is_blank(footnote)) {
                    line_pos <- 0.8 + (fnote_lines - 1) * fnote_size
                    mtext(footnote, side = 1, adj = 0, cex = fnote_size,
                          outer = TRUE, line = line_pos)
                }
            }
        } else {
            n_visits <- length(input$sample_pd_corr_visit)
            if(n_visits == 1) {
                has_group <- !is.null(group_)
                if(has_group) {
                    if(!is_blank(footnote)) oma_bottom <- 3 + fnote_space
                    else oma_bottom <- 3
                } else {
                    if(!is_blank(footnote)) oma_bottom <- fnote_space
                    else oma_bottom <- 0
                }
                par_opt <- par(oma = c(oma_bottom, 0, 0, 0))
                if(has_group) plot_footnote <- character(0)
                else plot_footnote <- footnote
                # par_opt <- par(oma = c(3, 0, 0, 0))
                forest_plot <- dual_y_axis_sumline(
                    data, input$sample_pd_corr_x,
                    pd_var_1, var_y2 = pd_var_2,
                    group = group_, 
                    xlab = sample_pd_corr_sumline_xlab$value,
                    ylab1 = sample_pd_corr_sumline_ylabl$value,
                    ylab2 = sample_pd_corr_sumline_ylabr$value,
                    ylog1 = input$sample_pd_corr_log_1,
                    ylog2 = input$sample_pd_corr_log_2,
                    title = sample_pd_corr_sumline_main$value,
                    footnote = plot_footnote,
                    method = method, type = 'o',
                    same_y_axis = sample_pd_corr_sumline_same_y$value,
                    xaxis = TRUE, mar = c(5.1, 4.1, 4.1, 4.1), save_plot = F
                )
                if(!is.null(group_)) {
                    oma_bottom_legend <- ifelse(is_blank(footnote), 0, fnote_space)
                    ngroups <- length(unique(data[[group_]]))
                    all_cols <- colorRampPalette(c('red4', 'white'))(3 * ngroups)
                    leg_cols <- all_cols[seq(1, 3 * ngroups, by = 3)]
                    par(fig = c(0, 1, 0, 1), oma = c(oma_bottom_legend, 0, 0, 0),
                        mar = c(0, 0, 0, 0), new = TRUE)
                    plot(0, 0, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
                    legend('bottom', levels(data[[group_]]),
                           xpd = TRUE, horiz = TRUE, inset = c(0, 0),
                           lty = 1, lwd = 1, bty = 'n', col = leg_cols,
                           title = group_)
                    if(!is_blank(footnote)) {
                        line_pos <- 0.8 + (fnote_lines - 1) * fnote_size
                        mtext(footnote, side = 1, adj = 0, cex = fnote_size,
                              outer = TRUE, line = line_pos)
                    }
                }
            } else {
                xlim <- range_na(data[[sample_pd_atptn_col]])
                x_tick <- sort(unique(data[[sample_pd_atptn_col]]))
                layout(matrix(seq_len(n_visits), ncol = 1, byrow = TRUE))
                
                has_group <- !is.null(group_)
                if(has_group) {
                    if(!is_blank(footnote)) oma_bottom <- 7 + fnote_space
                    else oma_bottom <- 7
                } else {
                    if(!is_blank(footnote)) oma_bottom <- 4 + fnote_space
                    else oma_bottom <- 4
                }
                par_opt <- par(oma = c(oma_bottom, 4, 4, 4) + 0.1)
                # par_opt <- par(oma = c(7, 4, 4, 4) + 0.1)
                for(idx in seq_along(input$sample_pd_corr_visit)) {
                    visit <- input$sample_pd_corr_visit[idx]
                    data_visit <- data[
                        data[[sample_pd_avisitn_col]] %in% visit, , drop = F
                    ]
                    xaxis <- idx == n_visits
                    forest_plot <- dual_y_axis_sumline(
                        data_visit, input$sample_pd_corr_x,
                        pd_var_1, var_y2 = pd_var_2,
                        group = group_, xlab = '', ylab1 = '', ylab2 = '',
                        ylog1 = input$sample_pd_corr_log_1,
                        ylog2 = input$sample_pd_corr_log_2,
                        title = '', method = method, type = 'o', xlim = xlim,
                        same_y_axis = sample_pd_corr_sumline_same_y$value,
                        xaxis = xaxis, x_tick = x_tick, x_tick_lab = x_tick,
                        mar = c(0, 0, 0, 0) + 0.1,
                        legend_txt = paste('Visit =', visit),
                        save_plot = F
                    )
                }
                title(main = sample_pd_corr_sumline_main$value, outer = TRUE)
                mtext(sample_pd_corr_sumline_xlab$value, side = 1, line = 2.5,
                      outer = TRUE)
                mtext(sample_pd_corr_sumline_ylabl$value, side = 2, line = 3,
                      outer = TRUE, col = 'red4')
                mtext(sample_pd_corr_sumline_ylabr$value, side = 4, line = 3,
                      outer = TRUE, col = 'blue4')
                if(!is.null(group_)) {
                    oma_bottom_legend <- ifelse(is_blank(footnote), 0, fnote_space)
                    ngroups <- length(unique(data[[group_]]))
                    all_cols <- colorRampPalette(c('red4', 'white'))(3 * ngroups)
                    leg_cols <- all_cols[seq(1, 3 * ngroups, by = 3)]
                    par(fig = c(0, 1, 0, 1), oma = c(oma_bottom_legend, 0, 0, 0),
                        mar = c(0, 0, 0, 0), new = TRUE)
                    plot(0, 0, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
                    legend('bottom', levels(data[[group_]]),
                           xpd = TRUE, horiz = TRUE, inset = c(0, 0),
                           lty = 1, lwd = 1, bty = 'n', col = leg_cols,
                           title = group_)
                    if(!is_blank(footnote)) {
                        line_pos <- 0.8 + (fnote_lines - 1) * fnote_size
                        mtext(footnote, side = 1, adj = 0, cex = fnote_size,
                              outer = TRUE, line = line_pos)
                    }
                } else {
                    oma_bottom_legend <- ifelse(is_blank(footnote), 0, fnote_space)
                    if(!is_blank(footnote)) {
                        par(fig = c(0, 1, 0, 1), oma = c(oma_bottom_legend,0,0,0),
                            mar = c(0, 0, 0, 0), new = TRUE)
                        plot(0, 0, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
                        line_pos <- 0.8 + (fnote_lines - 1) * fnote_size
                        mtext(footnote, side = 1, adj = 0, cex = fnote_size,
                              outer = TRUE, line = line_pos)
                    }
                }
            }
        }
        par(par_opt)
    })
    
    # download summary line plot for two PD parameters
    output$sample_pd_corr_sumline_download_button <- renderUI({
        is_visit <- input$sample_pd_corr_x == sample_pd_avisitn_col
        req(req(input$sample_panel) == 'PD analysis',
            input$sample_pd_analysis_type == '2 PD analysis',
            input$sample_pd_corr_param1, input$sample_pd_corr_y1,
            input$sample_pd_corr_param2, input$sample_pd_corr_y2,
            (is_visit || (!is_visit && !is_blank(input$sample_pd_corr_visit))),
            input$sample_pd_corr_summary,
            !is.null(input$sample_pd_corr_group), 
            !is.null(input$sample_pd_corr_log_1),
            !is.null(input$sample_pd_corr_log_2),
            req(input$sample_pd_corr_tabs) == 'Summary line')
        downloadButton('sample_pd_corr_sumline_download', 'Download plot')
    })
    output$sample_pd_corr_sumline_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'sample_2pd_summary_line_',
                format(Sys.Date(),format = '%Y%m%d'), '.pdf'
            )
            return(file_name)
        },
        content = function(file) {
            height_width_ratio <- 3 / 4
            width <- 8
            height <- width * height_width_ratio
            pdf(file = file, width = width, height = height)
            data <- data_$sample_pd
            is_visit <- input$sample_pd_corr_x == sample_pd_avisitn_col
            x_var <- ifelse(is_visit, sample_pd_avisitn_col, sample_pd_atptn_col)
            data <- data[
                !is.na(data[[x_var]]) &
                    !is.na(data[[input$sample_pd_corr_y1]]) &
                    !is.na(data[[input$sample_pd_corr_y2]]), , drop = FALSE
                ]
            if(!is_visit) {
                cond <- data[[sample_pd_avisitn_col]] %in% input$sample_pd_corr_visit
                data <- data[cond, , drop = FALSE]
            }
            formula_ <- ifelse(is_blank(input$sample_pd_corr_group), '',
                               paste(input$sample_pd_corr_group, '+'))
            formula_ <- paste(formula_, sample_pd_subj_col, '+')
            if(!is_visit) {
                formula_ <- paste(
                    formula_, sample_pd_avisitn_col, '+', input$sample_pd_corr_x
                )
            } else formula_ <- paste(formula_, sample_pd_avisitn_col)
            formula_ <- paste(formula_, '~', subj_pd_param_col)
            # data <- reshape2::dcast(data, formula_, fun.aggregate = mean,
            #                         na.rm = TRUE, value.var = sample_pd_aval_col)
            data <- data.table::dcast(
                data.table::as.data.table(data), formula_,
                fun.aggregate = mean, na.rm = TRUE,
                value.var = c(input$sample_pd_corr_y1, input$sample_pd_corr_y2)
            )
            if(input$sample_pd_corr_y1 == input$sample_pd_corr_y2) {
                pd_var_1 <- input$sample_pd_corr_param1
                pd_var_2 <- input$sample_pd_corr_param2
            } else {
                pd_var_1 <- paste(input$sample_pd_corr_y1, 'mean',
                                  input$sample_pd_corr_param1, sep = '_')
                pd_var_2 <- paste(input$sample_pd_corr_y2, 'mean',
                                  input$sample_pd_corr_param2, sep = '_')
            }
            data[[input$sample_pd_corr_x]] <- as.numeric(data[[input$sample_pd_corr_x]])
            if(input$sample_pd_corr_summary == 'Mean + SD') {
                method <- 'mean_sd'
            } else if(input$sample_pd_corr_summary == 'Mean + SE') {
                method <- 'mean_se'
            } else if(input$sample_pd_corr_summary == 'Median + IQR') {
                method <- 'median_iqr'
            }
            nna_x <- !is.na(data[[input$sample_pd_corr_x]])
            cond_na <- (!is.na(data[[pd_var_1]]) & nna_x) ||
                (!is.na(data[[pd_var_2]]) & nna_x)
            data <- data[cond_na, , drop = FALSE]
            group_ <- NULL
            if(!is_blank(input$sample_pd_corr_group)) {
                data[[input$sample_pd_corr_group]] <- factor(
                    as.character(data[[input$sample_pd_corr_group]])
                )
                group_ <- input$sample_pd_corr_group
            }
            footnote <- trimws(sample_pd_corr_sumline_footnote$value)
            fnote_lines <- ifelse(is_blank(footnote), 0,
                                  length(strsplit(footnote, '\n')[[1]]))
            fnote_size <- 0.8
            fnote_space <- 0.8 + fnote_lines * fnote_size + 0.2
            if(is_visit) {
                has_group <- !is.null(group_)
                if(has_group) {
                    if(!is_blank(footnote)) oma_bottom <- 3 + fnote_space
                    else oma_bottom <- 3
                } else {
                    if(!is_blank(footnote)) oma_bottom <- fnote_space
                    else oma_bottom <- 0
                }
                par_opt <- par(oma = c(oma_bottom, 0, 0, 0))
                if(has_group) plot_footnote <- character(0)
                else plot_footnote <- footnote
                forest_plot <- dual_y_axis_sumline(
                    data, input$sample_pd_corr_x,
                    pd_var_1, var_y2 = pd_var_2,
                    group = group_,
                    xlab = sample_pd_corr_sumline_xlab$value,
                    ylab1 = sample_pd_corr_sumline_ylabl$value,
                    ylab2 = sample_pd_corr_sumline_ylabr$value,
                    ylog1 = input$sample_pd_corr_log_1,
                    ylog2 = input$sample_pd_corr_log_2,
                    title = sample_pd_corr_sumline_main$value,
                    footnote = plot_footnote,
                    method = method, type = 'o',
                    same_y_axis = sample_pd_corr_sumline_same_y$value,
                    save_plot = FALSE
                )
                if(has_group) {
                    oma_bottom_legend <- ifelse(is_blank(footnote), 0, fnote_space)
                    ngroups <- length(unique(data[[group_]]))
                    all_cols <- colorRampPalette(c('red4', 'white'))(3 * ngroups)
                    leg_cols <- all_cols[seq(1, 3 * ngroups, by = 3)]
                    par(fig = c(0, 1, 0, 1), oma = c(oma_bottom_legend, 0, 0, 0),
                        mar = c(0, 0, 0, 0), new = TRUE)
                    plot(0, 0, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
                    legend('bottom', levels(data[[group_]]),
                           xpd = TRUE, horiz = TRUE, inset = c(0, 0),
                           lty = 1, lwd = 1, bty = 'n', col = leg_cols,
                           title = group_)
                    if(!is_blank(footnote)) {
                        line_pos <- 0.8 + (fnote_lines - 1) * fnote_size
                        mtext(footnote, side = 1, adj = 0, cex = fnote_size,
                              outer = TRUE, line = line_pos)
                    }
                }
            } else {
                n_visits <- length(input$sample_pd_corr_visit)
                if(n_visits == 1) {
                    has_group <- !is.null(group_)
                    if(has_group) {
                        if(!is_blank(footnote)) oma_bottom <- 3 + fnote_space
                        else oma_bottom <- 3
                    } else {
                        if(!is_blank(footnote)) oma_bottom <- fnote_space
                        else oma_bottom <- 0
                    }
                    par_opt <- par(oma = c(oma_bottom, 0, 0, 0))
                    if(has_group) plot_footnote <- character(0)
                    else plot_footnote <- footnote
                    # par_opt <- par(oma = c(3, 0, 0, 0))
                    forest_plot <- dual_y_axis_sumline(
                        data, input$sample_pd_corr_x,
                        pd_var_1, var_y2 = pd_var_2,
                        group = group_, 
                        xlab = sample_pd_corr_sumline_xlab$value,
                        ylab1 = sample_pd_corr_sumline_ylabl$value,
                        ylab2 = sample_pd_corr_sumline_ylabr$value,
                        ylog1 = input$sample_pd_corr_log_1,
                        ylog2 = input$sample_pd_corr_log_2,
                        title = sample_pd_corr_sumline_main$value,
                        footnote = plot_footnote,
                        method = method, type = 'o',
                        same_y_axis = sample_pd_corr_sumline_same_y$value,
                        xaxis = TRUE, mar = c(5.1, 4.1, 4.1, 4.1), save_plot = F
                    )
                    if(!is.null(group_)) {
                        oma_bottom_legend <- ifelse(is_blank(footnote), 0, fnote_space)
                        ngroups <- length(unique(data[[group_]]))
                        all_cols <- colorRampPalette(c('red4', 'white'))(3 * ngroups)
                        leg_cols <- all_cols[seq(1, 3 * ngroups, by = 3)]
                        par(fig = c(0, 1, 0, 1), oma = c(oma_bottom_legend, 0, 0, 0),
                            mar = c(0, 0, 0, 0), new = TRUE)
                        plot(0, 0, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
                        legend('bottom', levels(data[[group_]]),
                               xpd = TRUE, horiz = TRUE, inset = c(0, 0),
                               lty = 1, lwd = 1, bty = 'n', col = leg_cols,
                               title = group_)
                        if(!is_blank(footnote)) {
                            line_pos <- 0.8 + (fnote_lines - 1) * fnote_size
                            mtext(footnote, side = 1, adj = 0, cex = fnote_size,
                                  outer = TRUE, line = line_pos)
                        }
                    }
                } else {
                    xlim <- range_na(data[[sample_pd_atptn_col]])
                    x_tick <- sort(unique(data[[sample_pd_atptn_col]]))
                    layout(matrix(seq_len(n_visits), ncol = 1, byrow = TRUE))
                    
                    has_group <- !is.null(group_)
                    if(has_group) {
                        if(!is_blank(footnote)) oma_bottom <- 7 + fnote_space
                        else oma_bottom <- 7
                    } else {
                        if(!is_blank(footnote)) oma_bottom <- 4 + fnote_space
                        else oma_bottom <- 4
                    }
                    par_opt <- par(oma = c(oma_bottom, 4, 4, 4) + 0.1)
                    # par_opt <- par(oma = c(7, 4, 4, 4) + 0.1)
                    for(idx in seq_along(input$sample_pd_corr_visit)) {
                        visit <- input$sample_pd_corr_visit[idx]
                        data_visit <- data[
                            data[[sample_pd_avisitn_col]] %in% visit, , drop = F
                            ]
                        xaxis <- idx == n_visits
                        forest_plot <- dual_y_axis_sumline(
                            data_visit, input$sample_pd_corr_x,
                            pd_var_1, var_y2 = pd_var_2,
                            group = group_, xlab = '', ylab1 = '', ylab2 = '',
                            ylog1 = input$sample_pd_corr_log_1,
                            ylog2 = input$sample_pd_corr_log_2,
                            title = '', method = method, type = 'o', xlim = xlim,
                            same_y_axis = sample_pd_corr_sumline_same_y$value,
                            xaxis = xaxis, x_tick = x_tick, x_tick_lab = x_tick,
                            mar = c(0, 0, 0, 0) + 0.1,
                            legend_txt = paste('Visit =', visit),
                            save_plot = F
                        )
                    }
                    title(main = sample_pd_corr_sumline_main$value, outer = TRUE)
                    mtext(sample_pd_corr_sumline_xlab$value, side = 1, line = 2.5,
                          outer = TRUE)
                    mtext(sample_pd_corr_sumline_ylabl$value, side = 2, line = 3,
                          outer = TRUE, col = 'red4')
                    mtext(sample_pd_corr_sumline_ylabr$value, side = 4, line = 3,
                          outer = TRUE, col = 'blue4')
                    if(!is.null(group_)) {
                        oma_bottom_legend <- ifelse(is_blank(footnote), 0, fnote_space)
                        ngroups <- length(unique(data[[group_]]))
                        all_cols <- colorRampPalette(c('red4', 'white'))(3 * ngroups)
                        leg_cols <- all_cols[seq(1, 3 * ngroups, by = 3)]
                        par(fig = c(0, 1, 0, 1), oma = c(oma_bottom_legend, 0, 0, 0),
                            mar = c(0, 0, 0, 0), new = TRUE)
                        plot(0, 0, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
                        legend('bottom', levels(data[[group_]]),
                               xpd = TRUE, horiz = TRUE, inset = c(0, 0),
                               lty = 1, lwd = 1, bty = 'n', col = leg_cols,
                               title = group_)
                        if(!is_blank(footnote)) {
                            line_pos <- 0.8 + (fnote_lines - 1) * fnote_size
                            mtext(footnote, side = 1, adj = 0, cex = fnote_size,
                                  outer = TRUE, line = line_pos)
                        }
                    } else {
                        if(!is_blank(footnote)) {
                            par(fig = c(0, 1, 0, 1), oma = c(oma_bottom_legend,0,0,0),
                                mar = c(0, 0, 0, 0), new = TRUE)
                            plot(0, 0, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
                            line_pos <- 0.8 + (fnote_lines - 1) * fnote_size
                            mtext(footnote, side = 1, adj = 0, cex = fnote_size,
                                  outer = TRUE, line = line_pos)
                        }
                    }
                }
            }
            par(par_opt)
            dev.off()
        }
    )
    
    
    #-----------------------------------------------
    # UI widgets for PK-PD analysis

    # a selectInput for choosing PK parameter
    output$sample_pkpd_pk <- renderUI({
        req(data_import_status$sample_pkpd,
            input$file_sample_pk_type == 'PK parameter')
        data <- data_$sample_pkpd
        choices <- c('Choose' = '', sort(unique(data[[sample_pk_param_param_col]])))
        selectInput('sample_pkpd_pk', 'PK parameter', na.omit(choices))
    })
    observe({
        req(data_$sample_pkpd)
        data <- data_$sample_pkpd
        if(!is_blank(input$sample_pkpd_pd)) {
            is_pk_con <- input$file_sample_pk_type == 'PK concentration'
            pk_y_col <- ifelse(is_pk_con, sample_pk_con_con_col,
                               sample_pk_param_estm_col)
            cond_pk <- !is.na(data[[pk_y_col]])
            cond_pd_param <- data[[sample_pd_param_col]] %in% input$sample_pkpd_pd
            if(!is_blank(input$sample_pkpd_pd_y))
                cond_pd_y <- !is.na(data[[input$sample_pkpd_pd_y]])
            else cond_pd_y <- !is.na(data[[sample_pd_aval_col]])
            data <- data[cond_pk & cond_pd_param & cond_pd_y, ]
        }
        choices <- c('Choose' = '', sort(unique(data[[sample_pk_param_param_col]])))
        if(!is_blank(input$sample_pkpd_pk) && input$sample_pkpd_pk %in% choices)
            selected <- input$sample_pkpd_pk
        else selected <- ''
        updateSelectInput(
            session, 'sample_pkpd_pk', 'PK parameter', choices, selected
        )
    })
    
    # a selectInput for choosing PD parameter
    output$sample_pkpd_pd <- renderUI({
        req(data_import_status$sample_pkpd)
        data <- data_$sample_pkpd
        choices <- c('Choose' = '', sort(unique(data[[sample_pd_param_col]])))
        selectInput('sample_pkpd_pd', 'PD parameter', na.omit(choices))
    })
    observe({
        req(data_$sample_pkpd)
        data <- data_$sample_pkpd
        req(data)
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        pk_y_col <- ifelse(is_pk_con, sample_pk_con_con_col,
                           sample_pk_param_estm_col)
        cond_pk <- !is.na(data[[pk_y_col]])
        if(!is_blank(input$sample_pkpd_pk)) {
            cond_pk_param <- data[[sample_pk_param_param_col]] %in%
                input$sample_pkpd_pk
        } else { cond_pk_param <- TRUE }
        if(!is_blank(input$sample_pkpd_pd_y))
            cond_pd_y <- !is.na(data[[input$sample_pkpd_pd_y]])
        else cond_pd_y <- !is.na(data[[sample_pd_aval_col]])
        data <- data[cond_pk & cond_pk_param & cond_pd_y, ]
        choices <- c('Choose' = '', sort(unique(data[[sample_pd_param_col]])))
        if(!is_blank(input$sample_pkpd_pd) && input$sample_pkpd_pd %in% choices)
            selected <- input$sample_pkpd_pd
        else selected <- ''
        updateSelectInput(
            session, 'sample_pkpd_pd', 'PD parameter', choices, selected
        )
    })
    
    # a selectInput for choosing y for PD parameter
    output$sample_pkpd_pd_y <- renderUI({
        req(data_import_status$sample_pkpd)
        selectInput('sample_pkpd_pd_y', 'PD value', sample_pd_dose_y_types)
    })
    # selectInput for choosing x variable
    output$sample_pkpd_x <- renderUI({
        req(data_import_status$sample_pkpd,
            req(input$sample_pkpd_tabs) == 'Summary line')
        by_visit <- input$file_sample_pkpd_merge_by == 'Visit only'
        if(by_visit) {
            choices <- c('', sample_pk_con_visit_col)
            selected <- sample_pk_con_visit_col
        } else {
            choices <- c('', sample_pk_con_visit_col, sample_pk_con_time_col)
            selected <- sample_pk_con_time_col
        }
        selectInput('sample_pkpd_x', 'X variable', choices,
                    selected = selected)
    })
    # a selectizeInput for choosing visit(s)
    output$sample_pkpd_visit <- renderUI({
        req(data_import_status$sample_pkpd,
            req(input$sample_pkpd_tabs) == 'Summary line',
            req(input$sample_pkpd_x) == sample_pk_con_time_col)
        data <- data_$sample_pkpd
        choices <- sort(unique(data[[sample_pk_con_visit_col]]))
        selected <- isolate(ifelse(
            is.null(input$sample_pkpd_visit), '',
            input$sample_pkpd_visit[input$sample_pkpd_visit %in% choices]
        ))
        selectizeInput(
            'sample_pkpd_visit', 'Visit',
            c('Choose' = '', choices), selected = selected, multiple = T,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    # a selectizeInput for choosing visit(s) for scatter plot
    output$sample_pkpd_visit_scatter <- renderUI({
        req(data_import_status$sample_pkpd,
            req(input$sample_pkpd_tabs) == 'Scatter plot')
        data <- data_$sample_pkpd
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        visit_col <- ifelse(is_pk_con, sample_pk_con_visit_col,
                            sample_pk_param_visit_col)
        
        cond_param <- rep(TRUE, nrow(data))
        if(!is_blank(input$sample_pkpd_pd)) {
            cond_param <- data[[sample_pd_param_col]] %in% input$sample_pkpd_pd
        }
        if(!is_pk_con && !is_blank(input$sample_pkpd_pk)) {
            cond_pk <- data[[sample_pk_param_param_col]] %in% input$sample_pkpd_pk
            cond_param <- cond_param & cond_pk
        }
        data <- data[cond_param, , drop = FALSE]
        choices <- sort(unique(data[[visit_col]]))
        selected <- isolate(ternary(
            is.null(input$sample_pkpd_visit_scatter), choices,
            input$sample_pkpd_visit_scatter
        ))
        selectizeInput(
            'sample_pkpd_visit_scatter', 'Visit',
            c('Choose' = '', choices), selected = selected, multiple = T,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    # a selectInput for choosing group variable
    output$sample_pkpd_group <- renderUI({
        req(data_import_status$sample_pkpd, input$sample_pkpd_tabs)
        data <- data_$sample_pkpd
        choices <- names(data)
        to_exclude <- c(sample_pd_param_col, sample_pd_aval_col)
        choices <- c('Choose' = '', sort(setdiff(choices, to_exclude)))
        # stillSelected <- isolate(ifelse(
        #     is.null(input$sample_pkpd_group), sample_pd_dose_col,
        #     input$sample_pkpd_group[input$sample_pkpd_group %in% choices]
        # ))
        selected <- ifelse(input$sample_pkpd_tabs == 'Quartile plot',
                           c('Choose' = ''), sample_pd_dose_col)
        selectInput('sample_pkpd_group', 'Group', choices, selected)
    })
    # a selectInput for choosing summary statistics
    output$sample_pkpd_summary <- renderUI({
        req(data_import_status$sample_pkpd,
            input$sample_pkpd_tabs == 'Summary line')
        selectInput('sample_pkpd_summary', 'Statistics',
                    sample_pkpd_summary_opts)
    })
    # a radioButtons for adding reference line to the scatter plot
    output$sample_pkpd_refline <- renderUI({
        req(data_import_status$sample_pkpd,
            req(input$sample_pkpd_tabs) == 'Scatter plot')
        choices <- c('None', 'Loess', 'Linear regression')
        radioButtons('sample_pkpd_refline', 'Add reference line',
                     choices = choices, inline = TRUE)
    })
    # a checkbox for toggling points
    output$sample_pkpd_add_points <- renderUI({
        req(data_import_status$sample_pkpd,
            req(input$sample_pkpd_tabs) %in% c('Quartile plot'))
        checkboxInput('sample_pkpd_add_points', 'Add points', value = FALSE)
    })
    # a checkbox for toggling line
    output$sample_pkpd_add_line <- renderUI({
        req(data_import_status$sample_pkpd,
            req(input$sample_pkpd_tabs) %in% c('Quartile plot'))
        stillSelected <- isolate(
            !is_blank(input$sample_pkpd_add_line, false_triggers = TRUE)
        )
        checkboxInput(
            'sample_pkpd_add_line',
            tags$html(
                'Add line',
                tags$style(type = 'text/css',
                           '#qsample_pkpd_line {vertical-align: top;}'),
                shinyBS::bsButton('qsample_pkpd_line', label = '',
                                  icon = icon('question'),
                                  style = 'info', size = 'extra-small')
            ),
            value = stillSelected
        )
    })
    observe({
        input$qsample_pkpd_line
        shinyBS::addTooltip(
            session, 'qsample_pkpd_line', sample_pkpd_line_htext,
            placement = 'right', trigger = 'hover',
            options = list(container = 'body')
        )
    })
    # a checkbox for specifying log of PD parameter 1
    output$sample_pkpd_log_pk <- renderUI({
        req(data_import_status$sample_pkpd,
            req(input$sample_pkpd_tabs) %in% c('Scatter plot', 'Summary line'))
        checkboxInput('sample_pkpd_log_pk', 'Log of PK', value = FALSE)
    })
    # a checkbox for specifying log of PD parameter 2
    output$sample_pkpd_log_pd <- renderUI({
        req(data_import_status$sample_pkpd)
        checkboxInput('sample_pkpd_log_pd', 'Log of PD', value = FALSE)
    })

    #-----------------------------------------------
    # Further table/graph refine for PK-PD analysis

    # a textareaInput for specifying xlab for summary line
    output$sample_pkpd_sumline_xlab <- renderUI({
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        is_visit <- input$sample_pkpd_x == sample_pk_con_visit_col
        req((is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            (is_visit || (!is_visit && !is_blank(input$sample_pkpd_visit))),
            input$sample_pkpd_summary,
            !is.null(input$sample_pkpd_group), 
            !is.null(input$sample_pkpd_log_pk),
            !is.null(input$sample_pkpd_log_pd),
            req(input$sample_pkpd_tabs) == 'Summary line')
        value <- ifelse(is_visit, 'Visit', 'Timepoint')
        textareaInput('sample_pkpd_sumline_xlab', 'X-axis label',
                      value = value)
    })
    sample_pkpd_sumline_xlab <- reactiveValues(value = NULL)
    observe({
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        is_visit <- input$sample_pkpd_x == sample_pk_con_visit_col
        req((is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            (is_visit || (!is_visit && !is_blank(input$sample_pkpd_visit))),
            input$sample_pkpd_summary,
            !is.null(input$sample_pkpd_group), 
            !is.null(input$sample_pkpd_log_pk),
            !is.null(input$sample_pkpd_log_pd),
            req(input$sample_pkpd_tabs) == 'Summary line')
        value <- ifelse(is_visit, 'Visit', 'Timepoint')
        sample_pkpd_sumline_xlab$value <- value
    })
    observe({
        input$sample_pkpd_sumline_xlab
        sample_pkpd_sumline_xlab$value <- input$sample_pkpd_sumline_xlab
    })

    # a textareaInput for specifying left ylab for summary line
    output$sample_pkpd_sumline_ylabl <- renderUI({
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        is_visit <- input$sample_pkpd_x == sample_pk_con_visit_col
        req((is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            (is_visit || (!is_visit && !is_blank(input$sample_pkpd_visit))),
            input$sample_pkpd_summary,
            !is.null(input$sample_pkpd_group), 
            !is.null(input$sample_pkpd_log_pk),
            !is.null(input$sample_pkpd_log_pd),
            req(input$sample_pkpd_tabs) == 'Summary line')
        value <- ifelse(is_pk_con, 'PK concentration', input$sample_pkpd_pk)
        textareaInput('sample_pkpd_sumline_ylabl', 'Left Y-axis label',
                      value = value)
    })
    sample_pkpd_sumline_ylabl <- reactiveValues(value = NULL)
    observe({
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        is_visit <- input$sample_pkpd_x == sample_pk_con_visit_col
        req((is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            (is_visit || (!is_visit && !is_blank(input$sample_pkpd_visit))),
            input$sample_pkpd_summary,
            !is.null(input$sample_pkpd_group), 
            !is.null(input$sample_pkpd_log_pk),
            !is.null(input$sample_pkpd_log_pd),
            req(input$sample_pkpd_tabs) == 'Summary line')
        value <- ifelse(is_pk_con, 'PK concentration', input$sample_pkpd_pk)
        sample_pkpd_sumline_ylabl$value <- value
    })
    observe({
        input$sample_pkpd_sumline_ylabl
        sample_pkpd_sumline_ylabl$value <- input$sample_pkpd_sumline_ylabl
    })

    # a textareaInput for specifying right ylab for summary line
    output$sample_pkpd_sumline_ylabr <- renderUI({
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        is_visit <- input$sample_pkpd_x == sample_pk_con_visit_col
        req((is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            (is_visit || (!is_visit && !is_blank(input$sample_pkpd_visit))),
            input$sample_pkpd_summary,
            !is.null(input$sample_pkpd_group), 
            !is.null(input$sample_pkpd_log_pk),
            !is.null(input$sample_pkpd_log_pd),
            req(input$sample_pkpd_tabs) == 'Summary line')
        value <- input$sample_pkpd_pd
        textareaInput('sample_pkpd_sumline_ylabr', 'Right Y-axis label',
                      value = value)
    })
    sample_pkpd_sumline_ylabr <- reactiveValues(value = NULL)
    observe({
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        is_visit <- input$sample_pkpd_x == sample_pk_con_visit_col
        req((is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            (is_visit || (!is_visit && !is_blank(input$sample_pkpd_visit))),
            input$sample_pkpd_summary,
            !is.null(input$sample_pkpd_group), 
            !is.null(input$sample_pkpd_log_pk),
            !is.null(input$sample_pkpd_log_pd),
            req(input$sample_pkpd_tabs) == 'Summary line')
        value <- input$sample_pkpd_pd
        sample_pkpd_sumline_ylabr$value <- value
    })
    observe({
        input$sample_pkpd_sumline_ylabr
        sample_pkpd_sumline_ylabr$value <- input$sample_pkpd_sumline_ylabr
    })

    # a textareaInput for specifying main for summary line
    output$sample_pkpd_sumline_main <- renderUI({
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        is_visit <- input$sample_pkpd_x == sample_pk_con_visit_col
        req((is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            (is_visit || (!is_visit && !is_blank(input$sample_pkpd_visit))),
            input$sample_pkpd_summary,
            !is.null(input$sample_pkpd_group), 
            !is.null(input$sample_pkpd_log_pk),
            !is.null(input$sample_pkpd_log_pd),
            req(input$sample_pkpd_tabs) == 'Summary line')
        value <- paste(
            summary_title_dict[[input$sample_pkpd_summary]], 'of',
            ifelse(is_pk_con, 'PK concentration', input$sample_pkpd_pk),
            'and', input$sample_pkpd_pd
        )
        if(!is_blank(input$sample_pkpd_group))
            value <- paste(value, 'by', input$sample_pkpd_group)
        textareaInput('sample_pkpd_sumline_main', 'Plot title',
                      value = value)
    })
    sample_pkpd_sumline_main <- reactiveValues(value = NULL)
    observe({
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        is_visit <- input$sample_pkpd_x == sample_pk_con_visit_col
        req((is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            (is_visit || (!is_visit && !is_blank(input$sample_pkpd_visit))),
            input$sample_pkpd_summary,
            !is.null(input$sample_pkpd_group), 
            !is.null(input$sample_pkpd_log_pk),
            !is.null(input$sample_pkpd_log_pd),
            req(input$sample_pkpd_tabs) == 'Summary line')
        value <- paste(
            summary_title_dict[[input$sample_pkpd_summary]], 'of',
            ifelse(is_pk_con, 'PK concentration', input$sample_pkpd_pk),
            'and', input$sample_pkpd_pd
        )
        if(!is_blank(input$sample_pkpd_group))
            value <- paste(value, 'by', input$sample_pkpd_group)
        sample_pkpd_sumline_main$value <- value
    })
    observe({
        input$sample_pkpd_sumline_main
        sample_pkpd_sumline_main$value <- input$sample_pkpd_sumline_main
    })
    
    # a textareaInput for specifying footnote for summary line
    output$sample_pkpd_sumline_footnote <- renderUI({
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        is_visit <- input$sample_pkpd_x == sample_pk_con_visit_col
        req((is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            (is_visit || (!is_visit && !is_blank(input$sample_pkpd_visit))),
            input$sample_pkpd_summary,
            !is.null(input$sample_pkpd_group), 
            !is.null(input$sample_pkpd_log_pk),
            !is.null(input$sample_pkpd_log_pd),
            req(input$sample_pkpd_tabs) == 'Summary line')
        value <- default_footnote$sample_pkpd
        textareaInput('sample_pkpd_sumline_footnote', 'Plot footnote',
                      value = value)
    })
    sample_pkpd_sumline_footnote <- reactiveValues(value = NULL)
    observe({
        input$sample_pkpd_sumline_footnote
        sample_pkpd_sumline_footnote$value <- input$sample_pkpd_sumline_footnote
    })
    
    # a checkboxInput for specifying same y axis for summary line
    output$sample_pkpd_sumline_same_y <- renderUI({
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        is_visit <- input$sample_pkpd_x == sample_pk_con_visit_col
        req((is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            (is_visit || (!is_visit && !is_blank(input$sample_pkpd_visit))),
            input$sample_pkpd_summary,
            !is.null(input$sample_pkpd_group), 
            !is.null(input$sample_pkpd_log_pk),
            !is.null(input$sample_pkpd_log_pd),
            req(input$sample_pkpd_tabs) == 'Summary line')
        checkboxInput('sample_pkpd_sumline_same_y', 'Same y axis', value = F)
    })
    sample_pkpd_sumline_same_y <- reactiveValues(value = FALSE)
    observe({
        input$sample_pkpd_sumline_same_y
        sample_pkpd_sumline_same_y$value <- input$sample_pkpd_sumline_same_y
    })

    # a textareaInput for specifying xlab for scatter plot
    output$sample_pkpd_scatter_xlab <- renderUI({
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        req(data, (is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            !is.null(input$sample_pkpd_refline),
            !is.null(input$sample_pkpd_group), 
            !is.null(input$sample_pkpd_log_pk),
            !is.null(input$sample_pkpd_log_pd),
            req(input$sample_pkpd_tabs) == 'Scatter plot')
        value <- ifelse(is_pk_con, 'PK concentration', input$sample_pkpd_pk)
        textareaInput('sample_pkpd_scatter_xlab', 'X-axis label', value = value)
    })
    sample_pkpd_scatter_xlab <- reactiveValues(value = NULL)
    observe({
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        req(data, (is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            !is.null(input$sample_pkpd_refline),
            !is.null(input$sample_pkpd_group), 
            !is.null(input$sample_pkpd_log_pk),
            !is.null(input$sample_pkpd_log_pd),
            req(input$sample_pkpd_tabs) == 'Scatter plot')
        value <- ifelse(is_pk_con, 'PK concentration', input$sample_pkpd_pk)
        sample_pkpd_scatter_xlab$value <- value
    })
    observe({
        input$sample_pkpd_scatter_xlab
        sample_pkpd_scatter_xlab$value <- input$sample_pkpd_scatter_xlab
    })

    # a textareaInput for specifying ylab for scatter plot
    output$sample_pkpd_scatter_ylab <- renderUI({
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        req(data, (is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            !is.null(input$sample_pkpd_refline),
            !is.null(input$sample_pkpd_group), 
            !is.null(input$sample_pkpd_log_pk),
            !is.null(input$sample_pkpd_log_pd),
            req(input$sample_pkpd_tabs) == 'Scatter plot')
        value <- input$sample_pkpd_pd
        textareaInput('sample_pkpd_scatter_ylab', 'Y-axis label',
                      value = value)
    })
    sample_pkpd_scatter_ylab <- reactiveValues(value = NULL)
    observe({
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        req(data, (is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            !is.null(input$sample_pkpd_refline),
            !is.null(input$sample_pkpd_group), 
            !is.null(input$sample_pkpd_log_pk),
            !is.null(input$sample_pkpd_log_pd),
            req(input$sample_pkpd_tabs) == 'Scatter plot')
        value <- input$sample_pkpd_pd
        sample_pkpd_scatter_ylab$value <- value
    })
    observe({
        input$sample_pkpd_scatter_ylab
        sample_pkpd_scatter_ylab$value <- input$sample_pkpd_scatter_ylab
    })

    # a textareaInput for specifying main for scatter plot
    output$sample_pkpd_scatter_main <- renderUI({
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        req(data, (is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            !is.null(input$sample_pkpd_refline),
            !is.null(input$sample_pkpd_group), 
            !is.null(input$sample_pkpd_log_pk),
            !is.null(input$sample_pkpd_log_pd),
            req(input$sample_pkpd_tabs) == 'Scatter plot')
        value <- paste(
            'Scatter plot between',
            ifelse(is_pk_con, 'PK concentration', input$sample_pkpd_pk),
            'and', input$sample_pd_corr_param2
        )
        if(!is_blank(input$sample_pkpd_group))
            value <- paste(value, 'by', input$sample_pkpd_group)
        textareaInput('sample_pkpd_scatter_main', 'Plot title', value = value)
    })
    sample_pkpd_scatter_main <- reactiveValues(value = NULL)
    observe({
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        req(data, (is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            !is.null(input$sample_pkpd_refline),
            !is.null(input$sample_pkpd_group), 
            !is.null(input$sample_pkpd_log_pk),
            !is.null(input$sample_pkpd_log_pd),
            req(input$sample_pkpd_tabs) == 'Scatter plot')
        value <- paste(
            'Scatter plot between',
            ifelse(is_pk_con, 'PK concentration', input$sample_pkpd_pk),
            'and', input$sample_pd_corr_param2
        )
        if(!is_blank(input$sample_pkpd_group))
            value <- paste(value, 'by', input$sample_pkpd_group)
        sample_pkpd_scatter_main$value <- value
    })
    observe({
        input$sample_pkpd_scatter_main
        sample_pkpd_scatter_main$value <- input$sample_pkpd_scatter_main
    })
    
    # a textareaInput for specifying main for scatter plot
    output$sample_pkpd_scatter_footnote <- renderUI({
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        req(data, (is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            !is.null(input$sample_pkpd_refline),
            !is.null(input$sample_pkpd_group), 
            !is.null(input$sample_pkpd_log_pk),
            !is.null(input$sample_pkpd_log_pd),
            req(input$sample_pkpd_tabs) == 'Scatter plot')
        value <- default_footnote$sample_pkpd
        textareaInput('sample_pkpd_scatter_footnote', 'Plot footnote',
                      value = value)
    })
    sample_pkpd_scatter_footnote <- reactiveValues(value = '')
    observe({
        input$sample_pkpd_scatter_footnote
        sample_pkpd_scatter_footnote$value <- input$sample_pkpd_scatter_footnote
    })
    
    # a textareaInput for specifying xlab for quartile plot
    output$sample_pkpd_quartile_xlab <- renderUI({
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        req(data, (is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            !is.null(input$sample_pkpd_group),
            !is.null(input$sample_pkpd_add_points),
            !is.null(input$sample_pkpd_add_line),
            !is.null(input$sample_pkpd_log_pd),
            req(input$sample_pkpd_tabs) == 'Quartile plot')
        value <- ifelse(is_pk_con, 'PK concentration', input$sample_pkpd_pk)
        textareaInput('sample_pkpd_quartile_xlab', 'X-axis label', value = value)
    })
    sample_pkpd_quartile_xlab <- reactiveValues(value = NULL)
    observe({
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        req(data, (is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            !is.null(input$sample_pkpd_group),
            !is.null(input$sample_pkpd_add_points),
            !is.null(input$sample_pkpd_add_line),
            !is.null(input$sample_pkpd_log_pd),
            req(input$sample_pkpd_tabs) == 'Quartile plot')
        value <- ifelse(is_pk_con, 'PK concentration', input$sample_pkpd_pk)
        sample_pkpd_quartile_xlab$value <- value
    })
    observe({
        input$sample_pkpd_quartile_xlab
        sample_pkpd_quartile_xlab$value <- input$sample_pkpd_quartile_xlab
    })
    
    # a textareaInput for specifying ylab for scatter plot
    output$sample_pkpd_quartile_ylab <- renderUI({
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        req(data, (is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            !is.null(input$sample_pkpd_group),
            !is.null(input$sample_pkpd_add_points),
            !is.null(input$sample_pkpd_add_line),
            !is.null(input$sample_pkpd_log_pd),
            req(input$sample_pkpd_tabs) == 'Quartile plot')
        value <- input$sample_pkpd_pd
        textareaInput('sample_pkpd_quartile_ylab', 'Y-axis label', value = value)
    })
    sample_pkpd_quartile_ylab <- reactiveValues(value = NULL)
    observe({
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        req(data, (is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            !is.null(input$sample_pkpd_group),
            !is.null(input$sample_pkpd_add_points),
            !is.null(input$sample_pkpd_add_line),
            !is.null(input$sample_pkpd_log_pd),
            req(input$sample_pkpd_tabs) == 'Quartile plot')
        value <- input$sample_pkpd_pd
        sample_pkpd_quartile_ylab$value <- value
    })
    observe({
        input$sample_pkpd_quartile_ylab
        sample_pkpd_quartile_ylab$value <- input$sample_pkpd_quartile_ylab
    })
    
    # a textareaInput for specifying main for scatter plot
    output$sample_pkpd_quartile_main <- renderUI({
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        req(data, (is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            !is.null(input$sample_pkpd_group),
            !is.null(input$sample_pkpd_add_points),
            !is.null(input$sample_pkpd_add_line),
            !is.null(input$sample_pkpd_log_pd),
            req(input$sample_pkpd_tabs) == 'Quartile plot')
        value <- paste(
            'Quartile plot of', input$sample_pkpd_pd, 'by',
            ifelse(is_pk_con, 'PK concentration', input$sample_pkpd_pk)
        )
        if(!is_blank(input$sample_pkpd_group))
            value <- paste(value, 'and', input$sample_pkpd_group)
        textareaInput('sample_pkpd_quartile_main', 'Plot title', value = value)
    })
    sample_pkpd_quartile_main <- reactiveValues(value = NULL)
    observe({
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        req(data, (is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            !is.null(input$sample_pkpd_group),
            !is.null(input$sample_pkpd_add_points),
            !is.null(input$sample_pkpd_add_line),
            !is.null(input$sample_pkpd_log_pd),
            req(input$sample_pkpd_tabs) == 'Quartile plot')
        value <- paste(
            'Quartile plot of', input$sample_pkpd_pd, 'by',
            ifelse(is_pk_con, 'PK concentration', input$sample_pkpd_pk)
        )
        if(!is_blank(input$sample_pkpd_group))
            value <- paste(value, 'and', input$sample_pkpd_group)
        sample_pkpd_quartile_main$value <- value
    })
    observe({
        input$sample_pkpd_quartile_main
        sample_pkpd_quartile_main$value <- input$sample_pkpd_quartile_main
    })
    
    # a textareaInput for specifying footnote for scatter plot
    output$sample_pkpd_quartile_footnote <- renderUI({
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        req(data, (is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            !is.null(input$sample_pkpd_group),
            !is.null(input$sample_pkpd_add_points),
            !is.null(input$sample_pkpd_add_line),
            !is.null(input$sample_pkpd_log_pd),
            req(input$sample_pkpd_tabs) == 'Quartile plot')
        value <- default_footnote$sample_pkpd
        textareaInput('sample_pkpd_quartile_footnote', 'Plot footnote',
                      value = value)
    })
    sample_pkpd_quartile_footnote <- reactiveValues(value = '')
    observe({
        input$sample_pkpd_quartile_footnote
        sample_pkpd_quartile_footnote$value <- input$sample_pkpd_quartile_footnote
    })
    
    # summary line for sample-level PK-PD analysis
    output$sample_pkpd_sumline <- renderPlot({
        data <- data_$sample_pkpd
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        left_y <- ifelse(is_pk_con, sample_pk_con_con_col, sample_pk_param_estm_col)
        is_visit <- input$sample_pkpd_x == sample_pk_con_visit_col
        req(data, (is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            (is_visit || (!is_visit && !is_blank(input$sample_pkpd_visit))),
            input$sample_pkpd_summary,
            !is.null(input$sample_pkpd_group), 
            !is.null(input$sample_pkpd_log_pk),
            !is.null(input$sample_pkpd_log_pd))
        req(!is.null(sample_pkpd_sumline_xlab$value),
            !is.null(sample_pkpd_sumline_ylabl$value),
            !is.null(sample_pkpd_sumline_ylabr$value),
            !is.null(sample_pkpd_sumline_main$value))
        if(!is_visit) {
            cond <- data[[sample_pk_con_visit_col]] %in% input$sample_pkpd_visit
            data <- data[cond, , drop = FALSE]
        }
        cond_param <- data[[sample_pd_param_col]] %in% input$sample_pkpd_pd
        if(!is_pk_con) {
            cond_pk <- data[[sample_pk_param_param_col]] %in% input$sample_pkpd_pk
            cond_param <- cond_param & cond_pk
        }
        data <- data[cond_param, , drop = FALSE]
        data[[input$sample_pkpd_x]] <- as.numeric(data[[input$sample_pkpd_x]])
        data[[left_y]] <- as.numeric(data[[left_y]])
        data[[input$sample_pkpd_pd_y]] <- as.numeric(data[[input$sample_pkpd_pd_y]])
        
        if(input$sample_pkpd_summary == 'Mean + SD') {
            method <- 'mean_sd'
        } else if(input$sample_pkpd_summary == 'Mean + SE') {
            method <- 'mean_se'
        } else if(input$sample_pkpd_summary == 'Median + IQR') {
            method <- 'median_iqr'
        }
        group_ <- NULL
        if(!is_blank(input$sample_pkpd_group)) {
            data[[input$sample_pkpd_group]] <- factor(
                data[[input$sample_pkpd_group]]
            )
            group_ <- input$sample_pkpd_group
        }
        footnote <- trimws(sample_pkpd_sumline_footnote$value)
        fnote_lines <- ifelse(is_blank(footnote), 0,
                              length(strsplit(footnote, '\n')[[1]]))
        fnote_size <- 0.8
        fnote_space <- 0.8 + fnote_lines * fnote_size + 0.2
        if(is_visit) {
            has_group <- !is.null(group_)
            if(has_group) {
                if(!is_blank(footnote)) oma_bottom <- 3 + fnote_space
                else oma_bottom <- 3
            } else {
                if(!is_blank(footnote)) oma_bottom <- fnote_space
                else oma_bottom <- 0
            }
            par_opt <- par(oma = c(oma_bottom, 0, 0, 0))
            if(has_group) plot_footnote <- character(0)
            else plot_footnote <- footnote
            
            # par_opt <- par(oma = c(3, 0, 0, 0))
            forest_plot <- dual_y_axis_sumline(
                data, input$sample_pkpd_x,
                left_y, var_y2 = input$sample_pkpd_pd_y, group = group_,
                xlab = sample_pkpd_sumline_xlab$value,
                ylab1 = sample_pkpd_sumline_ylabl$value,
                ylab2 = sample_pkpd_sumline_ylabr$value,
                ylog1 = input$sample_pkpd_log_pk,
                ylog2 = input$sample_pkpd_log_pd,
                title = sample_pkpd_sumline_main$value,
                footnote = plot_footnote,
                method = method, type = 'o',
                same_y_axis = sample_pkpd_sumline_same_y$value,
                save_plot = FALSE
            )
            if(!is.null(group_)) {
                oma_bottom_legend <- ifelse(is_blank(footnote), 0, fnote_space)
                ngroups <- length(unique(data[[group_]]))
                all_cols <- colorRampPalette(c('red4', 'white'))(3 * ngroups)
                leg_cols <- all_cols[seq(1, 3 * ngroups, by = 3)]
                par(fig = c(0, 1, 0, 1), oma = c(oma_bottom_legend, 0, 0, 0),
                    mar = c(0, 0, 0, 0), new = TRUE)
                plot(0, 0, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
                legend('bottom', levels(data[[group_]]),
                       xpd = TRUE, horiz = TRUE, inset = c(0, 0),
                       lty = 1, lwd = 1, bty = 'n', col = leg_cols,
                       title = group_)
                if(!is_blank(footnote)) {
                    line_pos <- 0.8 + (fnote_lines - 1) * fnote_size
                    mtext(footnote, side = 1, adj = 0, cex = fnote_size,
                          outer = TRUE, line = line_pos)
                }
            }
        } else {
            n_visits <- length(input$sample_pkpd_visit)
            if(n_visits == 1) {
                has_group <- !is.null(group_)
                if(has_group) {
                    if(!is_blank(footnote)) oma_bottom <- 3 + fnote_space
                    else oma_bottom <- 3
                } else {
                    if(!is_blank(footnote)) oma_bottom <- fnote_space
                    else oma_bottom <- 0
                }
                par_opt <- par(oma = c(oma_bottom, 0, 0, 0))
                if(has_group) plot_footnote <- character(0)
                else plot_footnote <- footnote
                
                # par_opt <- par(oma = c(3, 0, 0, 0))
                forest_plot <- dual_y_axis_sumline(
                    data, input$sample_pkpd_x,
                    left_y, var_y2 = input$sample_pkpd_pd_y, group = group_, 
                    xlab = sample_pkpd_sumline_xlab$value,
                    ylab1 = sample_pkpd_sumline_ylabl$value,
                    ylab2 = sample_pkpd_sumline_ylabr$value,
                    ylog1 = input$sample_pkpd_log_pk,
                    ylog2 = input$sample_pkpd_log_pd,
                    title = sample_pkpd_sumline_main$value,
                    footnote = plot_footnote,
                    method = method, type = 'o',
                    same_y_axis = sample_pkpd_sumline_same_y$value,
                    xaxis = TRUE, mar = c(5.1, 4.1, 4.1, 4.1), save_plot = F
                )
                if(!is.null(group_)) {
                    oma_bottom_legend <- ifelse(is_blank(footnote), 0, fnote_space)
                    ngroups <- length(unique(data[[group_]]))
                    all_cols <- colorRampPalette(c('red4', 'white'))(3 * ngroups)
                    leg_cols <- all_cols[seq(1, 3 * ngroups, by = 3)]
                    par(fig = c(0, 1, 0, 1), oma = c(oma_bottom_legend, 0, 0, 0),
                        mar = c(0, 0, 0, 0), new = TRUE)
                    plot(0, 0, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
                    legend('bottom', levels(data[[group_]]),
                           xpd = TRUE, horiz = TRUE, inset = c(0, 0),
                           lty = 1, lwd = 1, bty = 'n', col = leg_cols,
                           title = group_)
                    if(!is_blank(footnote)) {
                        line_pos <- 0.8 + (fnote_lines - 1) * fnote_size
                        mtext(footnote, side = 1, adj = 0, cex = fnote_size,
                              outer = TRUE, line = line_pos)
                    }
                }
            } else {
                xlim <- range_na(data[[sample_pk_con_time_col]])
                x_tick <- sort(unique(data[[sample_pk_con_time_col]]))
                layout(matrix(seq_len(n_visits), ncol = 1, byrow = TRUE))
                
                has_group <- !is.null(group_)
                if(has_group) {
                    if(!is_blank(footnote)) oma_bottom <- 7 + fnote_space
                    else oma_bottom <- 7
                } else {
                    if(!is_blank(footnote)) oma_bottom <- 4 + fnote_space
                    else oma_bottom <- 4
                }
                par_opt <- par(oma = c(oma_bottom, 4, 4, 4) + 0.1)
                # par_opt <- par(oma = c(7, 4, 4, 4) + 0.1)
                for(idx in seq_along(input$sample_pkpd_visit)) {
                    visit <- input$sample_pkpd_visit[idx]
                    data_visit <- data[
                        data[[sample_pk_con_visit_col]] %in% visit, , drop = F
                    ]
                    xaxis <- idx == n_visits
                    forest_plot <- dual_y_axis_sumline(
                        data_visit, input$sample_pkpd_x,
                        left_y, var_y2 = input$sample_pkpd_pd_y,
                        group = group_, xlab = '', ylab1 = '', ylab2 = '',
                        ylog1 = input$sample_pkpd_log_pk,
                        ylog2 = input$sample_pkpd_log_pd,
                        title = '', method = method, type = 'o',
                        same_y_axis = sample_pkpd_sumline_same_y$value,
                        xlim = xlim, xaxis = xaxis, x_tick = x_tick,
                        x_tick_lab = x_tick, mar = c(0, 0, 0, 0) + 0.1,
                        legend_txt = paste('Visit =', visit), save_plot = F
                    )
                }
                title(main = sample_pkpd_sumline_main$value, outer = TRUE)
                mtext(sample_pkpd_sumline_xlab$value, side = 1, line = 2.5,
                      outer = TRUE)
                mtext(sample_pkpd_sumline_ylabl$value, side = 2, line = 3,
                      outer = TRUE, col = 'red4')
                mtext(sample_pkpd_sumline_ylabr$value, side = 4, line = 3,
                      outer = TRUE, col = 'blue4')
                if(!is.null(group_)) {
                    oma_bottom_legend <- ifelse(is_blank(footnote), 0, fnote_space)
                    ngroups <- length(unique(data[[group_]]))
                    all_cols <- colorRampPalette(c('red4', 'white'))(3 * ngroups)
                    leg_cols <- all_cols[seq(1, 3 * ngroups, by = 3)]
                    par(fig = c(0, 1, 0, 1), oma = c(oma_bottom_legend, 0, 0, 0),
                        mar = c(0, 0, 0, 0), new = TRUE)
                    plot(0, 0, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
                    legend('bottom', levels(data[[group_]]),
                           xpd = TRUE, horiz = TRUE, inset = c(0, 0),
                           lty = 1, lwd = 1, bty = 'n', col = leg_cols,
                           title = group_)
                } else {
                    oma_bottom_legend <- ifelse(is_blank(footnote), 0, fnote_space)
                    if(!is_blank(footnote)) {
                        par(fig = c(0, 1, 0, 1), oma = c(oma_bottom_legend,0,0,0),
                            mar = c(0, 0, 0, 0), new = TRUE)
                        plot(0, 0, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
                        line_pos <- 0.8 + (fnote_lines - 1) * fnote_size
                        mtext(footnote, side = 1, adj = 0, cex = fnote_size,
                              outer = TRUE, line = line_pos)
                    }
                }
            }
        }
        par(par_opt)
    })
    
    # 
    output$sample_pkpd_sumline_download_button <- renderUI({
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        is_visit <- input$sample_pkpd_x == sample_pk_con_visit_col
        req((is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            (is_visit || (!is_visit && !is_blank(input$sample_pkpd_visit))),
            input$sample_pkpd_summary,
            !is.null(input$sample_pkpd_group), 
            !is.null(input$sample_pkpd_log_pk),
            !is.null(input$sample_pkpd_log_pd),
            req(input$sample_pkpd_tabs) == 'Summary line')
        downloadButton('sample_pkpd_sumline_download', 'Download button')
    })
    output$sample_pkpd_sumline_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'sample_pkpd_summary_line_',
                format(Sys.Date(),format = '%Y%m%d'), '.pdf'
            )
            return(file_name)
        },
        content = function(file) {
            height_width_ratio <- 3 / 4
            width <- 8
            height <- width * height_width_ratio
            pdf(file = file, width = width, height = height)
            data <- data_$sample_pkpd
            is_pk_con <- input$file_sample_pk_type == 'PK concentration'
            left_y <- ifelse(is_pk_con, sample_pk_con_con_col, sample_pk_param_estm_col)
            is_visit <- input$sample_pkpd_x == sample_pk_con_visit_col
            if(!is_visit) {
                cond <- data[[sample_pk_con_visit_col]] %in% input$sample_pkpd_visit
                data <- data[cond, , drop = FALSE]
            }
            cond_param <- data[[sample_pd_param_col]] %in% input$sample_pkpd_pd
            if(!is_pk_con) {
                cond_pk <- data[[sample_pk_param_param_col]] %in% input$sample_pkpd_pk
                cond_param <- cond_param & cond_pk
            }
            data <- data[cond_param, , drop = FALSE]
            data[[input$sample_pkpd_x]] <- as.numeric(data[[input$sample_pkpd_x]])
            data[[left_y]] <- as.numeric(data[[left_y]])
            data[[input$sample_pkpd_pd_y]] <- as.numeric(data[[input$sample_pkpd_pd_y]])
            
            if(input$sample_pkpd_summary == 'Mean + SD') {
                method <- 'mean_sd'
            } else if(input$sample_pkpd_summary == 'Mean + SE') {
                method <- 'mean_se'
            } else if(input$sample_pkpd_summary == 'Median + IQR') {
                method <- 'median_iqr'
            }
            group_ <- NULL
            if(!is_blank(input$sample_pkpd_group)) {
                data[[input$sample_pkpd_group]] <- factor(
                    data[[input$sample_pkpd_group]]
                )
                group_ <- input$sample_pkpd_group
            }
            footnote <- trimws(sample_pkpd_sumline_footnote$value)
            fnote_lines <- ifelse(is_blank(footnote), 0,
                                  length(strsplit(footnote, '\n')[[1]]))
            fnote_size <- 0.8
            fnote_space <- 0.8 + fnote_lines * fnote_size + 0.2
            if(is_visit) {
                has_group <- !is.null(group_)
                if(has_group) {
                    if(!is_blank(footnote)) oma_bottom <- 3 + fnote_space
                    else oma_bottom <- 3
                } else {
                    if(!is_blank(footnote)) oma_bottom <- fnote_space
                    else oma_bottom <- 0
                }
                par_opt <- par(oma = c(oma_bottom, 0, 0, 0))
                if(has_group) plot_footnote <- character(0)
                else plot_footnote <- footnote
                
                # par_opt <- par(oma = c(3, 0, 0, 0))
                forest_plot <- dual_y_axis_sumline(
                    data, input$sample_pkpd_x,
                    left_y, var_y2 = input$sample_pkpd_pd_y, group = group_,
                    xlab = sample_pkpd_sumline_xlab$value,
                    ylab1 = sample_pkpd_sumline_ylabl$value,
                    ylab2 = sample_pkpd_sumline_ylabr$value,
                    ylog1 = input$sample_pkpd_log_pk,
                    ylog2 = input$sample_pkpd_log_pd,
                    title = sample_pkpd_sumline_main$value,
                    footnote = plot_footnote,
                    method = method, type = 'o',
                    same_y_axis = sample_pkpd_sumline_same_y$value,
                    save_plot = FALSE
                )
                if(!is.null(group_)) {
                    oma_bottom_legend <- ifelse(is_blank(footnote), 0,
                                                fnote_space)
                    ngroups <- length(unique(data[[group_]]))
                    all_cols <- colorRampPalette(c('red4', 'white'))(3 * ngroups)
                    leg_cols <- all_cols[seq(1, 3 * ngroups, by = 3)]
                    par(fig = c(0, 1, 0, 1), oma = c(oma_bottom_legend, 0, 0, 0),
                        mar = c(0, 0, 0, 0), new = TRUE)
                    plot(0, 0, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
                    legend('bottom', levels(data[[group_]]),
                           xpd = TRUE, horiz = TRUE, inset = c(0, 0),
                           lty = 1, lwd = 1, bty = 'n', col = leg_cols,
                           title = group_)
                    if(!is_blank(footnote)) {
                        line_pos <- 0.8 + (fnote_lines - 1) * fnote_size
                        mtext(footnote, side = 1, adj = 0, cex = fnote_size,
                              outer = TRUE, line = line_pos)
                    }
                }
            } else {
                n_visits <- length(input$sample_pkpd_visit)
                if(n_visits == 1) {
                    has_group <- !is.null(group_)
                    if(has_group) {
                        if(!is_blank(footnote)) oma_bottom <- 3 + fnote_space
                        else oma_bottom <- 3
                    } else {
                        if(!is_blank(footnote)) oma_bottom <- fnote_space
                        else oma_bottom <- 0
                    }
                    par_opt <- par(oma = c(oma_bottom, 0, 0, 0))
                    if(has_group) plot_footnote <- character(0)
                    else plot_footnote <- footnote
                    
                    # par_opt <- par(oma = c(3, 0, 0, 0))
                    forest_plot <- dual_y_axis_sumline(
                        data, input$sample_pkpd_x,
                        left_y, var_y2 = input$sample_pkpd_pd_y, group = group_, 
                        xlab = sample_pkpd_sumline_xlab$value,
                        ylab1 = sample_pkpd_sumline_ylabl$value,
                        ylab2 = sample_pkpd_sumline_ylabr$value,
                        ylog1 = input$sample_pkpd_log_pk,
                        ylog2 = input$sample_pkpd_log_pd,
                        title = sample_pkpd_sumline_main$value,
                        footnote = plot_footnote,
                        method = method, type = 'o',
                        same_y_axis = sample_pkpd_sumline_same_y$value,
                        xaxis = TRUE, mar = c(5.1, 4.1, 4.1, 4.1), save_plot = F
                    )
                    if(!is.null(group_)) {
                        oma_bottom_legend <- ifelse(is_blank(footnote), 0,
                                                    fnote_space)
                        ngroups <- length(unique(data[[group_]]))
                        all_cols <- colorRampPalette(
                            c('red4', 'white')
                        )(3 * ngroups)
                        leg_cols <- all_cols[seq(1, 3 * ngroups, by = 3)]
                        par(fig = c(0, 1, 0, 1),
                            oma = c(oma_bottom_legend, 0, 0, 0),
                            mar = c(0, 0, 0, 0), new = TRUE)
                        plot(0, 0, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
                        legend('bottom', levels(data[[group_]]),
                               xpd = TRUE, horiz = TRUE, inset = c(0, 0),
                               lty = 1, lwd = 1, bty = 'n', col = leg_cols,
                               title = group_)
                        if(!is_blank(footnote)) {
                            line_pos <- 0.8 + (fnote_lines - 1) * fnote_size
                            mtext(footnote, side = 1, adj = 0, cex = fnote_size,
                                  outer = TRUE, line = line_pos)
                        }
                    }
                } else {
                    xlim <- range_na(data[[sample_pk_con_time_col]])
                    x_tick <- sort(unique(data[[sample_pk_con_time_col]]))
                    layout(matrix(seq_len(n_visits), ncol = 1, byrow = TRUE))
                    
                    has_group <- !is.null(group_)
                    if(has_group) {
                        if(!is_blank(footnote)) oma_bottom <- 7 + fnote_space
                        else oma_bottom <- 7
                    } else {
                        if(!is_blank(footnote)) oma_bottom <- 4 + fnote_space
                        else oma_bottom <- 4
                    }
                    par_opt <- par(oma = c(oma_bottom, 4, 4, 4) + 0.1)
                    # par_opt <- par(oma = c(7, 4, 4, 4) + 0.1)
                    for(idx in seq_along(input$sample_pkpd_visit)) {
                        visit <- input$sample_pkpd_visit[idx]
                        data_visit <- data[
                            data[[sample_pk_con_visit_col]] %in% visit, ,
                            drop = F
                        ]
                        xaxis <- idx == n_visits
                        forest_plot <- dual_y_axis_sumline(
                            data_visit, input$sample_pkpd_x,
                            left_y, var_y2 = input$sample_pkpd_pd_y,
                            group = group_, xlab = '', ylab1 = '', ylab2 = '',
                            ylog1 = input$sample_pkpd_log_pk,
                            ylog2 = input$sample_pkpd_log_pd,
                            title = '', method = method, type = 'o',
                            same_y_axis = sample_pkpd_sumline_same_y$value,
                            xlim = xlim, xaxis = xaxis, x_tick = x_tick,
                            x_tick_lab = x_tick, mar = c(0, 0, 0, 0) + 0.1,
                            legend_txt = paste('Visit =', visit), save_plot = F
                        )
                    }
                    title(main = sample_pkpd_sumline_main$value, outer = TRUE)
                    mtext(sample_pkpd_sumline_xlab$value, side = 1, line = 2.5,
                          outer = TRUE)
                    mtext(sample_pkpd_sumline_ylabl$value, side = 2, line = 3,
                          outer = TRUE, col = 'red4')
                    mtext(sample_pkpd_sumline_ylabr$value, side = 4, line = 3,
                          outer = TRUE, col = 'blue4')
                    if(!is.null(group_)) {
                        oma_bottom_legend <- ifelse(is_blank(footnote), 0,
                                                    fnote_space)
                        ngroups <- length(unique(data[[group_]]))
                        all_cols <- colorRampPalette(
                            c('red4', 'white')
                        )(3 * ngroups)
                        leg_cols <- all_cols[seq(1, 3 * ngroups, by = 3)]
                        par(fig = c(0, 1, 0, 1),
                            oma = c(oma_bottom_legend, 0, 0, 0),
                            mar = c(0, 0, 0, 0), new = TRUE)
                        plot(0, 0, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
                        legend('bottom', levels(data[[group_]]),
                               xpd = TRUE, horiz = TRUE, inset = c(0, 0),
                               lty = 1, lwd = 1, bty = 'n', col = leg_cols,
                               title = group_)
                    } else {
                        oma_bottom_legend <- ifelse(is_blank(footnote), 0,
                                                    fnote_space)
                        if(!is_blank(footnote)) {
                            par(fig = c(0, 1, 0, 1),
                                oma = c(oma_bottom_legend,0,0,0),
                                mar = c(0, 0, 0, 0), new = TRUE)
                            plot(0, 0, type = 'n', bty = 'n', xaxt = 'n',
                                 yaxt = 'n')
                            line_pos <- 0.8 + (fnote_lines - 1) * fnote_size
                            mtext(footnote, side = 1, adj = 0, cex = fnote_size,
                                  outer = TRUE, line = line_pos)
                        }
                    }
                }
            }
            par(par_opt)
            dev.off()
        }
    )
    
    # Scatter plot for PK-PD analysis
    sample_pkpd_scatter <- reactive({
        data <- data_$sample_pkpd
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        req(data, (is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            !is.null(input$sample_pkpd_refline),
            !is.null(input$sample_pkpd_group),
            !is.null(input$sample_pkpd_visit_scatter),
            !is.null(input$sample_pkpd_log_pk),
            !is.null(input$sample_pkpd_log_pd))
        req(!is.null(sample_pkpd_scatter_xlab$value),
            !is.null(sample_pkpd_scatter_ylab$value),
            !is.null(sample_pkpd_scatter_main$value),
            !is.null(sample_pkpd_scatter_footnote$value))
        x_var <- ifelse(is_pk_con, sample_pk_con_con_col, sample_pk_param_estm_col)
        data <- data[
            !is.na(data[[x_var]]) &
                !is.na(data[[input$sample_pkpd_pd_y]]), , drop = FALSE
        ]
        cond_param <- data[[sample_pd_param_col]] %in% input$sample_pkpd_pd
        if(!is_pk_con) {
            cond_pk <- data[[sample_pk_param_param_col]] %in% input$sample_pkpd_pk
            cond_param <- cond_param & cond_pk
        }
        data <- data[cond_param, , drop = FALSE]
        data[[x_var]] <- as.numeric(data[[x_var]])
        data[[input$sample_pkpd_pd_y]] <- as.numeric(data[[input$sample_pkpd_pd_y]])
        data <- data[!is.na(data[[x_var]]) & !is.na(data[[input$sample_pkpd_pd_y]]), ]
        if(!is_blank(input$sample_pkpd_group)) {
            data[[input$sample_pkpd_group]] <- factor(
                as.character(data[[input$sample_pkpd_group]])
            )
        }
        visit_col <- ifelse(is_pk_con, sample_pk_con_visit_col,
                            sample_pk_param_visit_col)
        data <- data[data[[visit_col]] %in% input$sample_pkpd_visit_scatter, ,
                     drop = F]
        
        baseplot <- gg_wrapper(
            data, aes_string(x = paste0('`', x_var, '`'),
                             y = paste0('`', input$sample_pkpd_pd_y, '`')),
            log_x = input$sample_pkpd_log_pk,
            log_y = input$sample_pkpd_log_pd
        )
        if(!is_blank(input$sample_pkpd_group)) {
            baseplot <- baseplot +
                geom_point(aes_string(colour = input$sample_pkpd_group,
                                      fill = input$sample_pkpd_group),
                           size = 2)
        } else baseplot <- baseplot + geom_point(size = 2)
        if(!is_blank(input$sample_pkpd_refline)) {
            if('Loess' %in% input$sample_pkpd_refline)
                baseplot <- baseplot + geom_smooth()
            if('Linear regression' %in% input$sample_pkpd_refline)
                baseplot <- baseplot + geom_smooth(method = 'lm')
        }
        baseplot <- baseplot +
            labs(x = sample_pkpd_scatter_xlab$value,
                 y = sample_pkpd_scatter_ylab$value,
                 title = sample_pkpd_scatter_main$value)
        # baseplot <- add_footnote(baseplot, sample_pkpd_scatter_footnote$value)
        return(baseplot)
    })
    output$sample_pkpd_scatter_ui <- renderUI({
        # req(sample_pkpd_scatter())
        shiny::tagList(
            plotOutput('sample_pkpd_scatter'),
            uiOutput('sample_pkpd_scatter_fn_out'),
            tags$head(tags$style(
                "#sample_pkpd_scatter_fn_out{font-size: 9px;}"
            ))
        )
    })
    output$sample_pkpd_scatter <- renderPlot({
        # grid.draw(sample_pkpd_scatter())
        sample_pkpd_scatter()
    })
    output$sample_pkpd_scatter_fn_out <- renderUI({
        req(sample_pkpd_scatter_footnote$value)
        HTML(paste(
            strsplit(sample_pkpd_scatter_footnote$value, '\n')[[1]],
            collapse = '<br/>'
        ))
    })
    
    # download scatter plot for PK-PD analysis
    output$sample_pkpd_scatter_download_button <- renderUI({
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        req(data, (is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            !is.null(input$sample_pkpd_refline),
            !is.null(input$sample_pkpd_group), 
            !is.null(input$sample_pkpd_log_pk),
            !is.null(input$sample_pkpd_log_pd),
            req(input$sample_pkpd_tabs) == 'Scatter plot')
        downloadButton('sample_pkpd_scatter_download', 'Download plot')
    })
    output$sample_pkpd_scatter_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'sample_pkpd_scatter_plot_',
                format(Sys.Date(),format = '%Y%m%d'), '.pdf'
            )
            return(file_name)
        },
        content = function(file) {
            plot_ <- sample_pkpd_scatter()
            plot_ <- add_footnote(plot_, sample_pkpd_scatter_footnote$value)
            height_width_ratio <- 3 / 4
            width <- 8
            height <- width * height_width_ratio
            ggsave(filename = file, plot = plot_, width = width,
                   height = height, dpi = 600)
        }
    )
    
    # Quartile plot for PK-PD analysis
    sample_pkpd_quartile <- reactive({
        data <- data_$sample_pkpd
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        req(data, (is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            !is.null(input$sample_pkpd_group),
            !is.null(input$sample_pkpd_add_points),
            !is.null(input$sample_pkpd_add_line),
            !is.null(input$sample_pkpd_log_pd))
        req(!is.null(sample_pkpd_quartile_xlab$value),
            !is.null(sample_pkpd_quartile_ylab$value),
            !is.null(sample_pkpd_quartile_main$value),
            !is.null(sample_pkpd_quartile_footnote$value))
        x_var <- ifelse(is_pk_con, sample_pk_con_con_col, sample_pk_param_estm_col)
        data <- data[
            !is.na(data[[x_var]]) &
                !is.na(data[[input$sample_pkpd_pd_y]]), , drop = FALSE
        ]
        cond_param <- data[[sample_pd_param_col]] %in% input$sample_pkpd_pd
        if(!is_pk_con) {
            cond_pk <- data[[sample_pk_param_param_col]] %in% input$sample_pkpd_pk
            cond_param <- cond_param & cond_pk
        }
        data <- data[cond_param, , drop = FALSE]
        data[[x_var]] <- as.numeric(data[[x_var]])
        data[[input$sample_pkpd_pd_y]] <- as.numeric(data[[input$sample_pkpd_pd_y]])
        data <- data[!is.na(data[[x_var]]) & !is.na(data[[input$sample_pkpd_pd_y]]), ]
        if(!is_blank(input$sample_pkpd_group)) {
            data[[input$sample_pkpd_group]] <- factor(
                as.character(data[[input$sample_pkpd_group]])
            )
        }
        pk_quartile_value <- c(quantile(data[[x_var]],
                                        probs = seq(0, 1, by = 0.25)))
        pk_quartile_label <- trimws(paste0(
            'Q', seq_len(4), '\n', round(head(pk_quartile_value, -1), 2),
            ' - ', round(pk_quartile_value[-1], 2)
        ))
        data[[x_var]] <- factor(cut(
            data[[x_var]], breaks = pk_quartile_value,
            labels = pk_quartile_label, include.lowest = TRUE
        ))
        quartile_plot <- gg_boxplot(
            data, xvar = x_var, yvar = input$sample_pkpd_pd_y,
            group = input$sample_pkpd_group,
            log_y = input$sample_pkpd_log_pd,
            x_lab = sample_pkpd_quartile_xlab$value,
            y_lab = sample_pkpd_quartile_ylab$value,
            title = sample_pkpd_quartile_main$value,
            with_points = input$sample_pkpd_add_points,
            with_line = input$sample_pkpd_add_line
        )
        return(quartile_plot)
    })
    output$sample_pkpd_quartile_ui <- renderUI({
        # req(sample_pkpd_quartile())
        shiny::tagList(
            plotOutput('sample_pkpd_quartile'),
            uiOutput('sample_pkpd_quartile_fn_out'),
            tags$head(tags$style(
                "#sample_pkpd_quartile_fn_out{font-size: 9px;}"
            ))
        )
    })
    output$sample_pkpd_quartile <- renderPlot({
        # grid.draw(sample_pkpd_quartile())
        sample_pkpd_quartile()
    })
    output$sample_pkpd_quartile_fn_out <- renderUI({
        req(sample_pkpd_quartile_footnote$value)
        HTML(paste(
            strsplit(sample_pkpd_quartile_footnote$value, '\n')[[1]],
            collapse = '<br/>'
        ))
    })
    
    # download quartile plot for PK-PD analysis
    output$sample_pkpd_quartile_download_button <- renderUI({
        is_pk_con <- input$file_sample_pk_type == 'PK concentration'
        req(data, (is_pk_con || (!is_pk_con && !is_blank(input$sample_pkpd_pk))),
            input$sample_pkpd_pd, input$sample_pkpd_pd_y,
            !is.null(input$sample_pkpd_group),
            !is.null(input$sample_pkpd_add_points),
            !is.null(input$sample_pkpd_add_line),
            !is.null(input$sample_pkpd_log_pd))
        req(!is.null(sample_pkpd_quartile_xlab$value),
            !is.null(sample_pkpd_quartile_ylab$value),
            !is.null(sample_pkpd_quartile_main$value))
        downloadButton('sample_pkpd_quartile_download', 'Download plot')
    })
    output$sample_pkpd_quartile_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'sample_pkpd_quartile_plot_',
                format(Sys.Date(),format = '%Y%m%d'), '.pdf'
            )
            return(file_name)
        },
        content = function(file) {
            plot_ <- sample_pkpd_quartile()
            plot_ <- add_footnote(plot_, sample_pkpd_quartile_footnote$value)
            height_width_ratio <- 3 / 4
            width <- 8
            height <- width * height_width_ratio
            ggsave(filename = file, plot = plot_, width = width,
                   height = height, dpi = 600)
        }
    )
    
    #-----------------------------------------------
    # 3.	Lab and AE safety analysis
    #-----------------------------------------------
    
    # Dynamically select which tab to open based on file uploads
    observe({
        if(isTRUE(data_import_status$lab))
            updateCollapse(session, 'lab_ae_panel', 'Lab data  time profile')
        if(isTRUE(data_import_status$ae))
            updateCollapse(session, 'lab_ae_panel', 'AE analysis')
    })
    
    #-----------------------------------------------
    # UI widgets for lab analysis
    
    # selectInput for choosing lab analyte
    output$lab_analyte <- renderUI({
        req(data_import_status$lab)
        data <- data_$lab
        choices <- c('Choose' = '', sort(unique(data[[lab_tstnam_col]])))
        selectInput('lab_analyte', 'Lab analyte', choices)
    })
    
    # selectInput for choosing group variable
    output$lab_group <- renderUI({
        req(data_import_status$lab)
        data <- data_$lab
        to_exclude <- c(
            lab_visitnum_col, lab_testcd_col, lab_tstnam_col, lab_rptresn_col
        )
        choices <- sort(setdiff(names(data), to_exclude))
        selected <- NULL
        if(lab_dose_col %in% names(data)) {
            choices <- c('', choices)
            selected <- lab_dose_col
        } else choices <- c('Choose' = '', choices)
        selectInput('lab_group', 'Group', choices, selected = selected)
    })
    
    # selectInput for choosing summary method
    output$lab_summary <- renderUI({
        req(data_import_status$lab, req(input$lab_tabs) == 'Summary line')
        stillSelected <- isolate(
            input$lab_summary[input$lab_summary %in% lab_summary_opts]
        )
        selectInput('lab_summary', 'Summary method', lab_summary_opts,
                    selected = stillSelected)
    })
    
    # selectizeInput for choosing choosing subject ID(s)
    output$lab_subjid <- renderUI({
        req(data_import_status$lab, req(input$lab_tabs) == 'Individual line')
        data <- data_$lab
        if(!is.null(input$lab_analyte)) {
            data[data[[lab_tstnam_col]] %in% input$lab_analyte, , drop = F]
        }
        choices <- sort(unique(data[[lab_subj_col]]))
        selected <- isolate(ternary(
            is.null(input$lab_subjid), choices,
            input$lab_subjid[input$lab_subjid %in% choices]
        ))
        selectizeInput(
            'lab_subjid', 'Choose Subject ID(s)',
            c('Choose' = '', choices), selected = selected, multiple = T,
            options = list(plugins = list('drag_drop','remove_button'))
        )
    })
    
    # check box for log scale of y
    output$lab_log_y <- renderUI({
        req(data_import_status$lab, req(input$lab_tabs) != 'Summary table')
        stillSelected <- isolate(
            !is_blank(input$lab_log_y, false_triggers = TRUE)
        )
        checkboxInput('lab_log_y', 'Log of Y', value = stillSelected)
    })
    
    # check box for including points in the graph
    output$lab_points <- renderUI({
        req(data_import_status$lab, req(input$lab_tabs) == 'Summary line')
        stillSelected <- isolate(
            !is_blank(input$lab_points, false_triggers = TRUE)
        )
        checkboxInput('lab_points', 'Add points', value = stillSelected)
    })
    
    # a numericInput for specifying decimal place
    output$lab_decimal <- renderUI({
        req(data_import_status$lab, req(input$lab_tabs) == 'Summary table')
        stillSelected <- isolate(
            ifelse(is.null(input$lab_decimal), 2, input$lab_decimal)
        )
        numericInput('lab_decimal', 'Decimal places', value = stillSelected)
    })
    
    #-----------------------------------------------
    # Table/Graph post refinement for lab analysis
    
    # group checkbox input for choosing download plot format
    output$lab_plot_format <- renderUI({
        req(data_import_status$lab, req(input$lab_tabs) != 'Summary table')
        checkboxGroupInput('lab_plot_format', 'Plot format',
                           c('pdf', 'png', 'jpeg'), 'pdf', inline = TRUE)
    })
    
    # numeric input for choosing download plot height
    output$lab_plot_height <- renderUI({
        req(data_import_status$lab, req(input$lab_tabs) != 'Summary table')
        numericInput('lab_plot_height', 'Plot height (inches)', value = 4)
    })
    
    # numeric input for choosing download plot width
    output$lab_plot_width <- renderUI({
        req(data_import_status$lab, req(input$lab_tabs) != 'Summary table')
        numericInput('lab_plot_width', 'Plot width (inches)', value = 6)
    })
    
    # text area input for specifying table title
    output$lab_table_title <- renderUI({
        req(lab_show$sumtable)
        value <- paste0('Summary table for ', input$lab_analyte, ' by')
        if(!is_blank(input$lab_group)) {
            value <- paste(value, input$lab_group, 'and')
        }
        value <- paste(value, 'timepoint')
        textareaInput('lab_table_title', 'Table title', value = value)
    })
    lab_table_title <- reactiveValues(value = NULL)
    observe({
        req(lab_show$sumtable)
        value <- paste0('Summary table for ', input$lab_analyte, ' by')
        if(!is_blank(input$lab_group)) {
            value <- paste(value, input$lab_group, 'and')
        }
        value <- paste(value, 'timepoint')
        lab_table_title$value <- value
    })
    observe({
        input$lab_table_title
        lab_table_title$value <- input$lab_table_title
    })
    
    # text area input for specifying table footnote
    output$lab_table_footnote <- renderUI({
        req(lab_show$sumtable)
        value <- default_footnote$lab
        textareaInput('lab_table_footnote', 'Table footnote', value = value)
    })
    lab_table_footnote <- reactiveValues(value = '')
    observe({
        input$lab_table_footnote
        lab_table_footnote$value <- input$lab_table_footnote
    })
    
    # checkbox for whether to add an overall column in summary table
    output$lab_table_overall_col <- renderUI({
        req(lab_show$sumtable)
        value <- FALSE
        checkboxInput('lab_table_overall_col', 'Overall column', value = value)
    })
    lab_table_overall_col <- reactiveValues(value = FALSE)
    observe({ lab_table_overall_col$value <- input$lab_table_overall_col })
    
    # text area input for specifying abnormality table title
    output$lab_abntable_title <- renderUI({
        req(lab_show$abntable)
        value <- paste('Summary of lab abnormality of', input$lab_analyte, 'by')
        if(!is_blank(input$lab_group)) {
            value <- paste(value, input$lab_group, 'and')
        }
        value <- paste(value, 'timepoint')
        textareaInput('lab_abntable_title', 'Table title', value = value)
    })
    lab_abntable_title <- reactiveValues(value = NULL)
    observe({
        req(lab_show$abntable)
        value <- paste('Summary of lab abnormality of', input$lab_analyte, 'by')
        if(!is_blank(input$lab_group)) {
            value <- paste(value, input$lab_group, 'and')
        }
        value <- paste(value, 'timepoint')
        lab_abntable_title$value <- value
    })
    observe({
        input$lab_abntable_title
        lab_abntable_title$value <- input$lab_abntable_title
    })
    
    # text area input for specifying abnormality table footnote
    output$lab_abntable_footnote <- renderUI({
        req(lab_show$abntable)
        value <- default_footnote$lab
        textareaInput('lab_abntable_footnote', 'Table footnote', value = value)
    })
    lab_abntable_footnote <- reactiveValues(value = '')
    observe({
        input$lab_abntable_footnote
        lab_abntable_footnote$value <- input$lab_abntable_footnote
    })
    
    # text area input for specifying xlab for summary line
    output$lab_sumplot_xlab <- renderUI({
        req(lab_show$sumline)
        textareaInput('lab_sumplot_xlab', 'X-axis label', value = lab_visitnum_col)
    })
    lab_sumplot_xlab <- reactiveValues(value = lab_visitnum_col)
    observe({
        input$lab_sumplot_xlab
        lab_sumplot_xlab$value <- input$lab_sumplot_xlab
    })
    
    # text area input for specifying ylab for summary line
    output$lab_sumplot_ylab <- renderUI({
        req(lab_show$sumline)
        textareaInput('lab_sumplot_ylab', 'Y-axis label', value = lab_rptresn_col)
    })
    lab_sumplot_ylab <- reactiveValues(value = lab_rptresn_col)
    observe({
        input$lab_sumplot_ylab
        lab_sumplot_ylab$value <- input$lab_sumplot_ylab
    })
    
    # text area input for specifying title for summary line
    output$lab_sumplot_main <- renderUI({
        req(lab_show$sumline)
        value <- 'Summary lab parameter time profiles'
        if(!is_blank(input$lab_group)) {
            value <- paste(value, 'by', input$lab_group)
        }
        textareaInput('lab_sumplot_main', 'Plot title', value = value)
    })
    lab_sumplot_main <- reactiveValues(value = NULL)
    observe({
        req(lab_show$sumline)
        value <- 'Summary lab parameter time profiles'
        if(!is_blank(input$lab_group)) {
            value <- paste(value, 'by', input$lab_group)
        }
        lab_sumplot_main$value <- value
    })
    observe({
        input$lab_sumplot_main
        lab_sumplot_main$value <- input$lab_sumplot_main
    })
    
    # text area input for specifying footnote for summary line
    output$lab_sumplot_footnote <- renderUI({
        req(lab_show$sumline)
        value <- default_footnote$lab
        textareaInput('lab_sumplot_footnote', 'Plot footnote', value = value)
    })
    lab_sumplot_footnote <- reactiveValues(value = '')
    observe({
        input$lab_sumplot_footnote
        lab_sumplot_footnote$value <- input$lab_sumplot_footnote
    })
    
    # text area input for specifying xlab for individual line
    output$lab_indplot_xlab <- renderUI({
        req(lab_show$indline)
        textareaInput('lab_indplot_xlab', 'X-axis label', value = lab_visitnum_col)
    })
    lab_indplot_xlab <- reactiveValues(value = lab_visitnum_col)
    observe({
        input$lab_indplot_xlab
        lab_indplot_xlab$value <- input$lab_indplot_xlab
    })
    
    # text area input for specifying ylab for individual line
    output$lab_indplot_ylab <- renderUI({
        req(lab_show$indline)
        textareaInput('lab_indplot_ylab', 'Y-axis label', value = lab_rptresn_col)
    })
    lab_indplot_ylab <- reactiveValues(value = lab_rptresn_col)
    observe({
        input$lab_indplot_ylab
        lab_indplot_ylab$value <- input$lab_indplot_ylab
    })
    
    # text area input for specifying title for individual line
    output$lab_indplot_main <- renderUI({
        req(lab_show$indline)
        value <- 'Individual lab parameter time profiles'
        if(!is_blank(input$lab_group)) {
            value <- paste(value, 'by', input$lab_group)
        }
        textareaInput('lab_indplot_main', 'Plot title', value = value)
    })
    lab_indplot_main <- reactiveValues(value = NULL)
    observe({
        req(lab_show$indline)
        value <- 'Individual lab parameter time profiles'
        if(!is_blank(input$lab_group)) {
            value <- paste(value, 'by', input$lab_group)
        }
        lab_indplot_main$value <- value
    })
    observe({
        input$lab_indplot_main
        lab_indplot_main$value <- input$lab_indplot_main
    })
    
    # text area input for specifying footnote for individual line
    output$lab_indplot_footnote <- renderUI({
        req(lab_show$indline)
        value <- default_footnote$lab
        textareaInput('lab_indplot_footnote', 'Plot footnote', value = value)
    })
    lab_indplot_footnote <- reactiveValues(value = '')
    observe({
        input$lab_indplot_footnote
        lab_indplot_footnote$value <- input$lab_indplot_footnote
    })
    
    # textInput for specifying the positions of reference line
    output$lab_refline <- renderUI({
        req((lab_show$indline || lab_show$sumline))
        textInput('lab_refline', 'Reference line', value = '')
    })
    lab_refline <- reactiveValues(value = '')
    observe({
        input$lab_refline
        lab_refline$value <- input$lab_refline
    })
    
    #-------------------------------------------
    # add to TNF buttons
    
    # add to TNF button for summary table
    output$lab_table_att <- renderUI({
        req(lab_show$sumtable)
        actionButton('lab_table_att', 'Add to TNF', icon = icon('cloud-upload'))
    })
    observeEvent(input$lab_table_att, {
        if(is.null(n_tfl$table)) n_tfl$table <- 1
        else n_tfl$table <- n_tfl$table + 1
        if(is.null(n_tfl$lab_table)) n_tfl$lab_table <- 1
        else n_tfl$lab_table <- n_tfl$lab_table + 1
        
        title_key <- paste0('lab_summary_table_', n_tfl$lab_table)
        tfln <- paste0('2.', n_tfl$table)
        row_list <- list(
            title_key, 'Table', tfln, 'Summary table', input$lab_analyte,
            input$lab_group, input$lab_decimal, lab_table_title$value,
            lab_table_footnote$value, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
            NULL, NULL
        )
        row_df <- setNames(
            data.frame(
                lapply(row_list, function(x) replace(x, is.null(x), NA)),
                check.names = FALSE, stringsAsFactors = FALSE
            ),
            lab_cols
        )
        if(is.null(tnf_rows$lab)) {
            tnf_rows$lab <- row_df
        } else {
            tnf_rows$lab <- rbind(tnf_rows$lab, row_df)
        }
    })
    
    # add to TNF button for abnormality table
    output$lab_abntable_att <- renderUI({
        req(lab_show$abntable)
        actionButton('lab_abntable_att', 'Add to TNF', icon = icon('cloud-upload'))
    })
    observeEvent(input$lab_abntable_att, {
        if(is.null(n_tfl$table)) n_tfl$table <- 1
        else n_tfl$table <- n_tfl$table + 1
        if(is.null(n_tfl$lab_abnormality_table)) n_tfl$lab_abnormality_table <- 1
        else n_tfl$lab_abnormality_table <- n_tfl$lab_abnormality_table + 1
        
        title_key <- paste0('lab_abnormality_table_', n_tfl$lab_abnormality_table)
        tfln <- paste0('2.', n_tfl$table)
        row_list <- list(
            title_key, 'Table', tfln, 'Abnormality table', input$lab_analyte,
            input$lab_group, NULL, lab_abntable_title$value,
            lab_abntable_footnote$value, NULL, NULL, NULL, NULL, NULL, NULL,
            NULL, NULL, NULL
        )
        row_df <- setNames(
            data.frame(
                lapply(row_list, function(x) replace(x, is.null(x), NA)),
                check.names = FALSE, stringsAsFactors = FALSE
            ),
            lab_cols
        )
        if(is.null(tnf_rows$lab)) {
            tnf_rows$lab <- row_df
        } else {
            tnf_rows$lab <- rbind(tnf_rows$lab, row_df)
        }
    })
    
    # add to TNF button for summary line
    output$lab_summary_lineplot_att <- renderUI({
        req(lab_show$sumline)
        actionButton('lab_summary_lineplot_att', 'Add to TNF',
                     icon = icon('cloud-upload'))
    })
    observeEvent(input$lab_summary_lineplot_att, {
        if(is.null(n_tfl$figure)) n_tfl$figure <- 1
        else n_tfl$figure <- n_tfl$figure + 1
        if(is.null(n_tfl$lab_summary_lineplot)) n_tfl$lab_summary_lineplot <- 1
        else n_tfl$lab_summary_lineplot <- n_tfl$lab_summary_lineplot + 1
        
        title_key <- paste0('lab_summary_line_', n_tfl$lab_summary_lineplot)
        tfln <- paste0('1.', n_tfl$figure)
        row_list <- list(
            title_key, 'Figure', tfln, 'Summary line', input$lab_analyte,
            input$lab_group, NULL, NULL, NULL, input$lab_summary, NULL,
            input$lab_points, lab_refline$value, input$lab_log_y,
            lab_sumplot_xlab$value, lab_sumplot_ylab$value,
            lab_sumplot_main$value, lab_sumplot_footnote$value
        )
        row_df <- setNames(
            data.frame(
                lapply(row_list, function(x) replace(x, is.null(x), NA)),
                check.names = FALSE, stringsAsFactors = FALSE
            ),
            lab_cols
        )
        if(is.null(tnf_rows$lab)) {
            tnf_rows$lab <- row_df
        } else {
            tnf_rows$lab <- rbind(tnf_rows$lab, row_df)
        }
    })
    
    # add to TNF button for individual line
    output$lab_individual_lineplot_att <- renderUI({
        req(lab_show$indline)
        actionButton('lab_individual_lineplot_att', 'Add to TNF',
                     icon = icon('cloud-upload'))
    })
    observeEvent(input$lab_individual_lineplot_att, {
        if(is.null(n_tfl$figure)) n_tfl$figure <- 1
        else n_tfl$figure <- n_tfl$figure + 1
        if(is.null(n_tfl$lab_individual_lineplot))
            n_tfl$lab_individual_lineplot <- 1
        else n_tfl$lab_individual_lineplot <- n_tfl$lab_individual_lineplot + 1
        
        title_key <- paste0('lab_individual_table_',
                            n_tfl$lab_individual_lineplot)
        tfln <- paste0('1.', n_tfl$figure)
        subjid <- paste(input$lab_subjid, collapse = '\\n')
        row_list <- list(
            title_key, 'Figure', tfln, 'Individual line', input$lab_analyte,
            input$lab_group, NULL, NULL, NULL, NULL, subjid,
            NULL, lab_refline$value, input$lab_log_y,
            lab_indplot_xlab$value, lab_indplot_ylab$value,
            lab_indplot_main$value, lab_indplot_footnote$value
        )
        row_df <- setNames(
            data.frame(
                lapply(row_list, function(x) replace(x, is.null(x), NA)),
                check.names = FALSE, stringsAsFactors = FALSE
            ),
            lab_cols
        )
        if(is.null(tnf_rows$lab)) {
            tnf_rows$lab <- row_df
        } else {
            tnf_rows$lab <- rbind(tnf_rows$lab, row_df)
        }
    })
    
    
    
    # a tabset UI for lab and AE safety analysis output
    output$lab_tabpanel <- renderUI({
        req(input$lab_ae_panel)
        if(input$lab_ae_panel == 'Lab data  time profile') {
            req(input$lab_analyte, !is.null(input$lab_group))
            tabsetPanel(
                tabPanel('Summary table', uiOutput('lab_table')),
                tabPanel('Abnormality table', uiOutput('lab_abnorm_table')),
                tabPanel('Summary line', uiOutput('lab_summary_lineplot_ui')),
                tabPanel('Individual line', uiOutput('lab_individual_lineplot_ui')),
                id = 'lab_tabs',
                selected = isolate(input$lab_tabs[input$lab_tabs %in% lab_tabnames])
            )
        } else if(input$lab_ae_panel == 'AE analysis') {
            selected <- isolate(
                if(is.null(input$ae_tabs)) NULL else input$ae_tabs
            )
            tabsetPanel(
                tabPanel('Summary of AE by SOC and PT',
                         uiOutput('ae_summary_barplot_ui')),
                tabPanel('Summary of AE by Subject',
                         uiOutput('ae_gantt_chart_ui')),
                id = 'ae_tabs', selected = selected
            )
        }
    })
    
    # # action button for "add to download list"
    # output$lab_add_to_download <- renderUI({
    #     req(data_import_status$lab, input$lab_analyte, !is.null(input$lab_group))
    #     actionButton('lab_add_to_download', 'Add to download list',
    #                  icon = icon('cloud-upload'))
    # })
    # observeEvent(input$lab_add_to_download, {
    #     owd <- setwd(temp_dir)
    #     on.exit(setwd(owd))
    #     if(input$lab_tabs == 'Summary table') {
    #         req(!is.null(input$lab_decimal))
    #         output_names <- paste(paste(
    #             't', length(download_list$table) + 1, time_stamp, sep = '-'
    #         ), 'pdf', sep = '.')
    #         file_html <- tempfile('table', fileext = '.html')
    #         htmltbl <- lab_table()
    #         htmlpage <- html_page(htmltbl)
    #         cat(htmlpage, file = file_html)
    #         convert_command <- paste0(
    #             'wkhtmltopdf -q ', 
    #             file_html, ' ', output_names
    #         )
    #         system(convert_command)
    #         download_list$table <- c(download_list$table, output_names)
    #     } else if(input$lab_tabs == 'Summary line' ||
    #               input$lab_tabs == 'Individual line'){
    #         if(input$lab_tabs == 'Summary line') {
    #             req(input$lab_summary,
    #                 !is.null(input$lab_log_y), !is.null(input$lab_points))
    #             plot_ <- lab_summary_lineplot()
    #         }
    #         if(input$lab_tabs == 'Individual line') {
    #             req(!is.null(input$lab_log_y), !is.null(input$lab_points))
    #             plot_ <- lab_individual_lineplot()
    #         }
    #         formats <- input$lab_plot_format
    #         output_names <- paste(paste(
    #             'g-lineplot', length(download_list$lineplot) + 1,
    #             time_stamp, sep = '-'
    #         ), formats, sep = '.')
    #         download_list$lineplot <- c(
    #             download_list$lineplot, output_names
    #         )
    #         for(name_ in output_names) {
    #             ggsave(
    #                 name_, plot_, units = 'in', height = input$lab_plot_height,
    #                 width = input$lab_plot_width, dpi = dpi_default
    #             )
    #         }
    #     }
    # })
    
    # # download plot button
    # output$lab_download_button <- renderUI({
    #     req(any(sapply(c(download_list$boxplot, download_list$lineplot,
    #                      download_list$table), length) > 0))
    #     downloadButton('lab_download', "Download saved TFL's")
    # })
    # 
    # # download plot handler
    # output$lab_download <- downloadHandler(
    #     filename = function() {paste('output', 'zip', sep = '.')},
    #     content = function(file) {
    #         owd <- setwd(temp_dir)
    #         on.exit(setwd(owd))
    #         system2(
    #             'zip', args = shQuote(
    #                 c('-r9X', file, download_list$boxplot,
    #                   download_list$lineplot, download_list$table)
    #             ), stdout = FALSE
    #         )
    #     }
    # )
    
    
    lab_show <- reactiveValues(
        sumtable = FALSE, abntable = FALSE, sumline = FALSE, indline = FALSE
    )
    observe({
        if(isTRUE(data_import_status$lab) &&
           !is_blank(input$lab_analyte) &&
           !is.null(input$lab_group) &&
           !is.null(input$lab_tabs)) {
            if(input$lab_tabs == 'Summary table') {
                lab_show$sumline <- FALSE
                lab_show$abntable <- FALSE
                lab_show$indline <- FALSE
                if(!is.null(input$lab_decimal)) {
                    lab_show$sumtable <- TRUE
                }
            } else if(input$lab_tabs == 'Abnormality table') {
                lab_show$sumline <- FALSE
                lab_show$indline <- FALSE
                lab_show$sumtable <- FALSE
                lab_show$abntable <- TRUE
            }else if(input$lab_tabs == 'Summary line') {
                lab_show$sumtable <- FALSE
                lab_show$abntable <- FALSE
                lab_show$indline <- FALSE
                if(!is_blank(input$lab_summary) &&
                   !is.null(input$lab_log_y) &&
                   !is.null(input$lab_points)) {
                    lab_show$sumline <- TRUE
                }
            } else if(input$lab_tabs == 'Individual line') {
                lab_show$sumtable <- FALSE
                lab_show$abntable <- FALSE
                lab_show$sumline <- FALSE
                if(!is.null(input$lab_log_y)) {
                    lab_show$indline <- TRUE
                }
            }
        } else {
            lab_show$sumtable <- FALSE
            lab_show$abntable <- FALSE
            lab_show$sumline <- FALSE
            lab_show$indline <- FALSE
        }
    })
    
    
    #-----------------------------------------------
    # Outputs for lab analysis
    
    # dataset for lab and AE safety analysis
    data_lab <- reactive({
        req(data_import_status$lab, input$lab_analyte, !is.null(input$lab_group))
        data <- data_$lab
        data <- data[data[[lab_tstnam_col]] %in% input$lab_analyte, , drop = F]
        expr <- list(lazyeval::interp(~factor(x), x = as.name(lab_visitnum_col)))
        data <- data %>%
            filter_(paste(lab_visitnum_col, '!=', lab_visit_to_remove)) %>%
            mutate_(.dots = setNames(expr, lab_visitnum_col))
        return(data)
    })
    
    # Summary statistic table  for lab and AE safety analysis
    lab_table <- reactive({
        data <- data_lab()
        req(data, lab_show$sumtable)
        is_categorical <- any(is.na(as.numeric(data[[lab_rptresc_col]])))
        if(is_categorical) {
            count_occurances_wrapper <- function(value) {
                partial(count_occurances, value = value)
            }
            values <- sort(unique_na(data[[lab_rptresc_col]]))
            lab_summary_func <- lapply(values, count_occurances_wrapper)
            names(lab_summary_func) <- values
            lab_summary_func <- c('N' = n_nna, lab_summary_func)
        } else {
            data[[lab_rptresc_col]] <- as.numeric(data[[lab_rptresc_col]])
            dgt <- input$lab_decimal
            lab_summary_func <- c(
                'N' = n_nna,
                'Mean (SD)' = partial(mean_sd_str, digits = dgt),
                '%CV' = partial(coeff_var_str, digits = dgt),
                'Median' = partial(median_str, digits = dgt),
                'Q1, Q3' = partial(q1_q3_str, digits = dgt),
                'Min, Max' = partial(min_max_str, digits = dgt),
                'Geom Mean (%CV)' = partial(geo_mean_cv_str, digits = dgt),
                'Mean (SD) of LN' = partial(mean_sd_ln_str, digits = dgt)
            )
        }
        header <- ifelse(is_categorical, 'Count', 'Value')
        visit_ <- lab_visitnum_col
        if(lab_visit_col %in% names(data)) {
            visit_ <- lab_visit_col
            data[[visit_]] <- factor(
                data[[visit_]],
                levels = unique(data[[visit_]][order(data[[lab_visitnum_col]])])
            )
            row_names <- levels(data[[visit_]])
        } else {
            visit_ <- lab_visitnum_col
            data[[visit_]] <- factor(
                data[[visit_]], levels = sort(unique(data[[visit_]]))
            )
            row_names <- paste('Visit', levels(data[[visit_]]))
        }
        if(is_blank(input$lab_group)) {
            data <- select_(data, lab_rptresc_col, visit_)
            summary_tbl <- summary_table_row(
                data, row_var = visit_, row_names = row_names, rowlabel = '',
                caption = lab_table_title$value,
                footnote = lab_table_footnote$value,
                func_list = lab_summary_func, header = header
            )
        } else {
            expr <- list(lazyeval::interp(~factor(c), c = as.name(input$lab_group)))
            data <- data %>% mutate_(.dots = setNames(expr, input$lab_group))
            data <- select_(data, lab_rptresc_col, visit_, input$lab_group)
            if(isTRUE(lab_table_overall_col$value)) {
                col_totals <- length(unique(data[[input$lab_group]]))
                name_totals <- 'Overall'
            } else {
                col_totals <- NULL
                name_totals <- NULL
            }
            summary_tbl <- summary_table_all(
                data, row_var = visit_, col_var = input$lab_group,
                val_var = lab_rptresc_col, col_totals = col_totals,
                name_totals = name_totals, n_in_header = FALSE,
                subj_col = NULL, func_list = lab_summary_func,
                caption = lab_table_title$value,
                footnote = lab_table_footnote$value,
                rowlabel = '', format = 'html'
            )
        }
        return(summary_tbl)
    })
    output$lab_table <- renderUI({ HTML(lab_table()) })
    
    # download summary statistic table  for lab and AE safety analysis
    output$lab_table_download_button <- renderUI({
        data <- data_lab()
        req(data, lab_show$sumtable)
        downloadButton('lab_table_download', 'Download table')
    })
    output$lab_table_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'lab_summary_table_',
                format(Sys.Date(),format = '%Y%m%d'), '.rtf'
            )
            return(file_name)
        },
        content = function(file) {
            data <- data_lab()
            dgt <- input$lab_decimal
            
            is_categorical <- any(is.na(as.numeric(data[[lab_rptresc_col]])))
            if(is_categorical) {
                count_occurances_wrapper <- function(value) {
                    partial(count_occurances, value = value)
                }
                values <- sort(unique_na(data[[lab_rptresc_col]]))
                lab_summary_func <- lapply(values, count_occurances_wrapper)
                names(lab_summary_func) <- values
                lab_summary_func <- c('N' = n_nna, lab_summary_func)
            } else {
                data[[lab_rptresc_col]] <- as.numeric(data[[lab_rptresc_col]])
                dgt <- input$lab_decimal
                lab_summary_func <- c(
                    'N' = n_nna,
                    'Mean (SD)' = partial(mean_sd_str, digits = dgt),
                    '%CV' = partial(coeff_var_str, digits = dgt),
                    'Median' = partial(median_str, digits = dgt),
                    'Q1, Q3' = partial(q1_q3_str, digits = dgt),
                    'Min, Max' = partial(min_max_str, digits = dgt),
                    'Geom Mean (%CV)' = partial(geo_mean_cv_str, digits = dgt),
                    'Mean (SD) of LN' = partial(mean_sd_ln_str, digits = dgt)
                )
            }
            header <- ifelse(is_categorical, 'Count', 'Value')
            
            if(lab_visit_col %in% names(data)) {
                visit_ <- lab_visit_col
                data[[visit_]] <- factor(
                    data[[visit_]],
                    levels = unique(data[[visit_]][order(data[[lab_visitnum_col]])])
                )
                row_names <- levels(data[[visit_]])
            } else {
                visit_ <- lab_visitnum_col
                data[[visit_]] <- factor(
                    data[[visit_]], levels = sort(unique(data[[visit_]]))
                )
                row_names <- paste('Visit', levels(data[[visit_]]))
            }
            if(is_blank(input$lab_group)) {
                data <- select_(data, lab_rptresc_col, visit_)
            } else {
                data[[input$lab_group]] <- factor(
                    data[[input$lab_group]],
                    levels = sort(unique(data[[input$lab_group]]))
                )
                data <- select_(data, lab_rptresc_col, visit_, input$lab_group)
            }
            
            if(isTRUE(lab_table_overall_col$value)) {
                col_totals <- length(unique(data[[input$lab_group]]))
                name_totals <- 'Overall'
            } else {
                col_totals <- NULL
                name_totals <- NULL
            }
            summary_tbl <- summary_table_all(
                data, row_var = visit_, row_names = row_names,
                col_var = input$lab_group,
                col_names = paste(input$lab_group, '=',
                                  levels(data[[input$lab_group]])),
                val_var = lab_rptresc_col, col_totals = col_totals,
                name_totals = name_totals, n_in_header = FALSE,
                subj_col = NULL, func_list = lab_summary_func,
                caption = lab_table_title$value,
                footnote = lab_table_footnote$value,
                rowlabel = '', format = 'html'
            )
            rtf_table_wrapper(
                file, summary_table, block_break = TRUE,
                nline_block = length(lab_summary_func) + 1,
                caption = lab_table_title$value,
                footnote = lab_table_footnote$value
            )
        }
    )
    
    # abnormality table for listing grades
    lab_abnorm_table <- reactive({
        data <- data_lab()
        req(data, lab_show$abntable)
        values <- sort(unique_na(data[[lab_toxgrg_col]]))
        if(is_blank(values)) {
            return('There are no grade records')
        }
        count_occurances_wrapper <- function(value) {
            partial(count_occurances, value = value)
        }
        lab_summary_func <- lapply(values, count_occurances_wrapper)
        names(lab_summary_func) <- paste('Grade', values)
        lab_summary_func <- c('N' = n_nna, lab_summary_func)
        header <- 'Count'
        visit_ <- lab_visitnum_col
        if(lab_visit_col %in% names(data)) {
            visit_ <- lab_visit_col
            data[[visit_]] <- factor(
                data[[visit_]],
                levels = unique(data[[visit_]][order(data[[lab_visitnum_col]])])
            )
            row_names <- levels(data[[visit_]])
        } else {
            visit_ <- lab_visitnum_col
            data[[visit_]] <- factor(
                data[[visit_]], levels = sort(unique(data[[visit_]]))
            )
            row_names <- paste('Visit', levels(data[[visit_]]))
        }
        if(is_blank(input$lab_group)) {
            data <- select_(data, lab_toxgrg_col, visit_)
            summary_tbl <- summary_table_row(
                data, row_var = visit_, row_names = row_names, rowlabel = '',
                caption = lab_abntable_title$value,
                footnote = lab_abntable_footnote$value,
                func_list = lab_summary_func, header = header
            )
        } else {
            expr <- list(lazyeval::interp(~factor(c), c = as.name(input$lab_group)))
            data <- data %>% mutate_(.dots = setNames(expr, input$lab_group))
            data <- select_(data, lab_toxgrg_col, visit_, input$lab_group)
            summary_tbl <- summary_table_2d(
                data, row_var = visit_, col_var = input$lab_group,
                row_names = row_names, rowlabel = '',
                caption = lab_abntable_title$value,
                footnote = lab_abntable_footnote$value,
                func_list = lab_summary_func
            )
        }
        return(summary_tbl)
    })
    output$lab_abnorm_table <- renderUI({ HTML(lab_abnorm_table()) })
    
    # download abnormality table  for lab and AE safety analysis
    output$lab_abntable_download_button <- renderUI({
        data <- data_lab()
        req(data, lab_show$abntable)
        downloadButton('lab_abntable_download', 'Download table')
    })
    output$lab_abntable_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'lab_abnormality_summary_table_',
                format(Sys.Date(),format = '%Y%m%d'), '.rtf'
            )
            return(file_name)
        },
        content = function(file) {
            data <- data_lab()
            values <- sort(unique_na(data[[lab_toxgrg_col]]))
            if(is_blank(values)) {
                return('')
            }
            count_occurances_wrapper <- function(value) {
                partial(count_occurances, value = value)
            }
            lab_summary_func <- lapply(values, count_occurances_wrapper)
            names(lab_summary_func) <- paste('Grade', values)
            lab_summary_func <- c('N' = n_nna, lab_summary_func)
            header <- 'Count'
            visit_ <- lab_visitnum_col
            if(lab_visit_col %in% names(data)) {
                visit_ <- lab_visit_col
                data[[visit_]] <- factor(
                    data[[visit_]],
                    levels = unique(data[[visit_]][order(data[[lab_visitnum_col]])])
                )
                row_names <- levels(data[[visit_]])
            } else {
                visit_ <- lab_visitnum_col
                data[[visit_]] <- factor(
                    data[[visit_]], levels = sort(unique(data[[visit_]]))
                )
                row_names <- paste('Visit', levels(data[[visit_]]))
            }
            if(is_blank(input$lab_group)) {
                data <- select_(data, lab_toxgrg_col, visit_)
            } else {
                expr <- list(lazyeval::interp(~factor(c), c = as.name(input$lab_group)))
                data <- data %>% mutate_(.dots = setNames(expr, input$lab_group))
                data <- select_(data, lab_toxgrg_col, visit_, input$lab_group)
            }
            summary_table <- summary_table_all(
                data, row_var = visit_, row_names = row_names,
                col_var = input$lab_group,
                col_names = paste(input$lab_group, '=',
                                  levels(data[[input$lab_group]])),
                val_var = lab_toxgrg_col,
                n_in_header = FALSE, func_list = lab_summary_func,
                caption = lab_abntable_title$value,
                footnote = lab_abntable_footnote$value,
                rowlabel = ' ', format = 'rtf'
            )
            rtf_table_wrapper(
                file, summary_table, block_break = TRUE,
                nline_block = length(lab_summary_func) + 1,
                caption = lab_abntable_title$value,
                footnote = lab_abntable_footnote$value
            )
        }
    )

    # Summary Line Plot for lab analysis
    lab_summary_lineplot <- reactive({
        data <- data_lab()
        req(data, lab_show$sumline)
        req(!is.null(lab_sumplot_xlab$value),
            !is.null(lab_sumplot_ylab$value),
            !is.null(lab_sumplot_main$value),
            !is.null(lab_sumplot_footnote$value),
            !is.null(lab_refline$value))
        data <- data[!is.na(data[[lab_visitnum_col]]), , drop = FALSE]
        if(!is_blank(input$lab_group)) {
            expr <- list(lazyeval::interp(~factor(c), c = as.name(input$lab_group)))
            data <- data %>% mutate_(.dots = setNames(expr, input$lab_group))
        }
        plot_ <- gg_lineplot(data, lab_visitnum_col, lab_rptresn_col,
                             group = input$lab_group, log_y = input$lab_log_y,
                             x_lab = lab_sumplot_xlab$value,
                             y_lab = lab_sumplot_ylab$value,
                             title = lab_sumplot_main$value,
                             summary_method = input$lab_summary,
                             with_points = input$lab_points)
        refline <- suppressWarnings(
            as.numeric(trimws(unlist(strsplit(lab_refline$value, ','))))
        )
        if(!is_blank(refline)) {
            plot_ <- plot_ +
                geom_hline(yintercept = refline, linetype = 'dashed')
        }
        # plot_ <- add_footnote(plot_, lab_sumplot_footnote$value)
        return(plot_)
    })
    output$lab_summary_lineplot_ui <- renderUI({
        shiny::tagList(
            plotOutput('lab_summary_lineplot'),
            uiOutput('lab_summary_lineplot_fn_out'),
            tags$head(tags$style(
                "#lab_summary_lineplot_fn_out{font-size: 9px;}"
            ))
        )
    })
    output$lab_summary_lineplot <- renderPlot({
        # grid.draw(lab_summary_lineplot())
        lab_summary_lineplot()
    })
    output$lab_summary_lineplot_fn_out <- renderUI({
        req(lab_sumplot_footnote$value)
        HTML(paste(
            strsplit(lab_sumplot_footnote$value, '\n')[[1]],
            collapse = '<br/>'
        ))
    })
    
    # download summary Line Plot for lab analysis
    output$lab_sumplot_download_button <- renderUI({
        req(data, lab_show$sumline)
        downloadButton('lab_sumplot_download', 'Download plot')
    })
    output$lab_sumplot_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'lab_summary_line_',
                format(Sys.Date(),format = '%Y%m%d'), '.pdf'
            )
            return(file_name)
        },
        content = function(file) {
            plot_ <- lab_summary_lineplot()
            plot_ <- add_footnote(plot_, lab_sumplot_footnote$value)
            height_width_ratio <- 3 / 4
            width <- 8
            height <- width * height_width_ratio
            ggsave(filename = file, plot = plot_, width = width,
                   height = height, dpi = 600)
        }
    )
    
    # Individual line plot for lab analysis
    lab_individual_lineplot <- reactive({
        data <- data_lab()
        req(data, lab_show$indline, input$lab_subjid)
        req(!is.null(lab_indplot_xlab$value),
            !is.null(lab_indplot_ylab$value),
            !is.null(lab_indplot_main$value),
            !is.null(lab_indplot_footnote$value),
            !is.null(lab_refline$value))
        data <- data[!is.na(data[[lab_visitnum_col]]), , drop = FALSE]
        data <- data[data[[lab_subj_col]] %in% input$lab_subjid, , drop = F]
        data[[lab_subj_col]] <- trimws(as.character(data[[lab_subj_col]]))
        if(!is_blank(input$lab_group)) {
            data[[input$lab_group]] <- factor(
                data[[input$lab_group]],
                levels = sort(unique(data[[input$lab_group]]))
            )
            data <- arrange_(data, paste0('`', input$lab_group, '`'),
                             lab_subj_col, lab_visitnum_col)
        } else {
            data <- arrange_(data, lab_subj_col, lab_visitnum_col)
        }
        data[[lab_subj_col]] <- factor(
            data[[lab_subj_col]],
            levels = unique(data[[lab_subj_col]])
        )
        # shapes_ <- all_shapes[seq_along(unique(data[[lab_subj_col]]))]
        nsubj <- length(levels(data[[lab_subj_col]]))
        shape_value <- seq_len(nsubj)
        # shape_value <- seq_along(levels(data[[lab_subj_col]]))
        baseplot <- gg_wrapper(data, aes_string(x = lab_visitnum_col,
                                                y = lab_rptresn_col,
                                                group = lab_subj_col),
                   log_y = input$lab_log_y)
        if(!is_blank(input$lab_group)) {
            nsubj_group <- sapply(by(
                data, data[[input$lab_group]],
                function(df) {unique(df[[lab_subj_col]])}
            ), length)
            shape_value <- unname(sequence(nsubj_group))
            ngroups <- length(nsubj_group)
            all_colors <- gg_color_hue(ngroups)
            color_value <- all_colors[rep.int(1:ngroups, times = nsubj_group)]
            
            baseplot <- baseplot +
                aes_string(colour = lab_subj_col, fill = input$lab_group) +
                scale_colour_manual(
                    name = lab_subj_col,
                    labels = levels(data[[lab_subj_col]]),
                    values = color_value
                ) +
                scale_fill_manual(
                    name = input$lab_group,
                    labels = levels(data[[input$lab_group]]),
                    values = all_colors,
                    guide=guide_legend(
                        override.aes = list(colour = all_colors),
                        order = 2
                    )
                )
        }
        baseplot <- baseplot +
            stat_summary(fun.y = mean_na, geom = 'line') +
            stat_summary(aes_string(shape = lab_subj_col),
                         fun.y = mean_na, geom = 'point') +
            scale_shape_manual(
                name = lab_subj_col,
                labels = levels(data[[lab_subj_col]]),
                values = shape_value
            ) +
            labs(x = lab_indplot_xlab$value,
                 y = lab_indplot_ylab$value,
                 title = lab_indplot_main$value)
        refline <- suppressWarnings(
            as.numeric(trimws(unlist(strsplit(lab_refline$value, ','))))
        )
        if(!is_blank(refline)) {
            baseplot <- baseplot +
                geom_hline(yintercept = refline, linetype = 'dashed')
        }
        # baseplot <- add_footnote(baseplot, lab_indplot_footnote$value)
        return(baseplot)
    })
    output$lab_individual_lineplot_ui <- renderUI({
        shiny::tagList(
            plotOutput('lab_individual_lineplot'),
            uiOutput('lab_individual_lineplot_fn_out'),
            tags$head(tags$style(
                "#lab_individual_lineplot_fn_out{font-size: 9px;}"
            ))
        )
    })
    output$lab_individual_lineplot <- renderPlot({
        # grid.draw(lab_individual_lineplot())
        lab_individual_lineplot()
    })
    output$lab_individual_lineplot_fn_out <- renderUI({
        req(lab_indplot_footnote$value)
        HTML(paste(
            strsplit(lab_indplot_footnote$value, '\n')[[1]],
            collapse = '<br/>'
        ))
    })
    
    # download individual Line Plot for lab analysis
    output$lab_indplot_download_button <- renderUI({
        req(data_lab(), lab_show$indline, input$lab_subjid)
        downloadButton('lab_indplot_download', 'Download plot')
    })
    output$lab_indplot_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'lab_individual_line_',
                format(Sys.Date(),format = '%Y%m%d'), '.pdf'
            )
            return(file_name)
        },
        content = function(file) {
            plot_ <- lab_individual_lineplot()
            plot_ <- add_footnote(plot_, lab_indplot_footnote$value)
            height_width_ratio <- 3 / 4
            width <- 8
            height <- width * height_width_ratio
            ggsave(filename = file, plot = plot_, width = width,
                   height = height, dpi = 600)
        }
    )
    
    
    #-----------------------------------------------
    # UI widgets for AE listing
    
    # Choose SOC
    output$ae_soc <- renderUI({
        req(data_import_status$ae, req(input$ae_tabs) == 'Summary of AE by SOC and PT')
        choices <- c('Choose' = '', unique(data_$ae[[ae_mdrsoc_col]]))
        selected <- isolate(ifelse(is.null(input$ae_soc), '', input$ae_soc))
        selectInput('ae_soc', 'SOC', choices, selected)
    })
    
    # Choose PT
    output$ae_pt <- renderUI({
        req(data_import_status$ae_enr, input$ae_soc,
            req(input$ae_tabs) == 'Summary of AE by SOC and PT')
        cond_soc <- data_$ae[[ae_mdrsoc_col]] %in% input$ae_soc
        data <- data_$ae[cond_soc, , drop = FALSE]
        choices <- c('Choose' = '', unique(data[[ae_mdrpt_col]]))
        if(!is_blank(input$ae_pt) && input$ae_pt %in% choices)
            selected <- input$ae_pt
        else selected <- ''
        selectInput('ae_pt', 'PT', choices, selected)
    })
    
    # Choose a group variable
    output$ae_group <- renderUI({
        req(data_import_status$ae,
            req(input$ae_tabs) == 'Summary of AE by SOC and PT')
        data <- if(isTRUE(data_import_status$ae_enr)) data_$ae_enr else data_$ae
        choices <- c('Choose' = '', gtools::mixedsort(names(data)))
        selected <- isolate(ifelse(is.null(input$ae_group), '', input$ae_group))
        selectInput('ae_group', 'Group', choices, selected)
    })
    
    # Choose how to rank the bars in AE summary barplot
    output$ae_barplot_rank <- renderUI({
        req(data_import_status$ae, req(ae_summary_level$value) != 'Subject',
            req(input$ae_tabs) == 'Summary of AE by SOC and PT')
        choices <- c('Alphabetically', 'By frequency')
        radioButtons('ae_barplot_rank', 'Sort bars', choices, inline = TRUE)
    })
    
    # Actionbutton to go one level up
    output$ae_summary_barplot_goback <- renderUI({
        req(req(ae_summary_level$value) != 'SOC',
            req(input$ae_tabs) == 'Summary of AE by SOC and PT')
        actionButton(
            'ae_summary_barplot_goback', 'Go back', icon = icon('reply')
        )
    })
    observeEvent(input$ae_summary_barplot_goback, {
        if(ae_summary_level$value == 'PT') {
            choices <- c('Choose' = '', unique(data_$ae[[ae_mdrsoc_col]]))
            updateSelectInput(session, 'ae_soc', 'SOC', choices, selected = '')
            ae_summary_level$value == 'SOC'
            ae_summary_barplot_clickloc$x <- NULL
            ae_summary_barplot_clickloc$y <- NULL
        } else if(ae_summary_level$value == 'Subject') {
            cond_soc <- data_$ae[[ae_mdrsoc_col]] %in% input$ae_soc
            data <- data_$ae[cond_soc, , drop = FALSE]
            choices <- c('Choose' = '', unique(data[[ae_mdrpt_col]]))
            updateSelectInput(session, 'ae_pt', 'PT', choices, selected = '')
            ae_summary_level$value == 'PT'
            ae_summary_barplot_clickloc$x <- NULL
            ae_summary_barplot_clickloc$y <- NULL
        }
    })
    
    # Choose subject to display AE duration
    output$ae_subject <- renderUI({
        req(data_import_status$ae_enr,
            req(input$ae_tabs) == 'Summary of AE by Subject')
        data <- data_$ae_enr
        has_soc <- !is.na(data[[ae_mdrsoc_col]])
        has_pt <- !is.na(data[[ae_mdrpt_col]])
        has_start_date <- !is.na(data[[ae_aestdatyy_col]]) &
            !is.na(data[[ae_aestdatmm_col]]) &
            !is.na(data[[ae_aestdatdd_col]])
        has_end_date <- !is.na(data[[ae_aeendatyy_col]]) &
            !is.na(data[[ae_aeendatmm_col]]) &
            !is.na(data[[ae_aeendatdd_col]])
        data <- data[has_soc & has_pt & has_start_date & has_end_date, , drop = F]
        choices <- c('Choose'='', gtools::mixedsort(unique(data[[ae_subj_col]])))
        selected <- isolate(
            if(is.null(input$ae_subject)) '' else input$ae_subject
        )
        selectInput('ae_subject', 'Subject', choices, selected)
    })
    
    
    ae_show <- reactiveValues(
        barplot_soc = FALSE, barplot_pt = FALSE,
        gantt_chart_ae = FALSE, gantt_chart_subj = FALSE
    )
    observe({
        if(!is.null(data_$ae)) {
            if(is.null(input$ae_tabs)) {
                ae_show$barplot_soc <- FALSE
                ae_show$barplot_pt <- FALSE
                ae_show$gantt_chart_ae <- FALSE
                ae_show$gantt_chart_subj <- FALSE
            } else if(input$ae_tabs == 'Summary of AE by SOC and PT') {
                ae_show$gantt_chart_subj <- FALSE
                if(any(
                    is.null(ae_summary_level$value),
                    is.null(ae_summary_barplot_info$levels),
                    is.null(ae_summary_barplot_data$value)
                )) {
                    ae_show$barplot_soc <- FALSE
                    ae_show$barplot_pt <- FALSE
                    ae_show$gantt_chart_ae <- FALSE
                } else {
                    if(ae_summary_level$value == 'SOC') {
                        ae_show$barplot_soc <- TRUE
                        ae_show$barplot_pt <- FALSE
                        ae_show$gantt_chart_ae <- FALSE
                    } else if(ae_summary_level$value == 'PT') {
                        ae_show$barplot_soc <- FALSE
                        ae_show$barplot_pt <- TRUE
                        ae_show$gantt_chart_ae <- FALSE
                    } else if(ae_summary_level$value == 'Subject') {
                        ae_show$barplot_soc <- FALSE
                        ae_show$barplot_pt <- FALSE
                        ae_show$gantt_chart_ae <- TRUE
                    }
                }
            } else if(input$ae_tabs == 'Summary of AE by Subject') {
                ae_show$barplot_soc <- FALSE
                ae_show$barplot_pt <- FALSE
                ae_show$gantt_chart_ae <- FALSE
                if(any(is.null(ae_gantt_chart_data$value))) {
                    ae_show$gantt_chart_subj <- FALSE
                } else {
                    ae_show$gantt_chart_subj <- TRUE
                }
            }
        } else {
            ae_show$barplot_soc <- FALSE
            ae_show$barplot_pt <- FALSE
            ae_show$gantt_chart_ae <- FALSE
            ae_show$gantt_chart_subj <- FALSE
        }
    })
    
    
    #-----------------------------------------------
    # Graph/Table refinement UI widgets AE listing
    
    # X-axis label for SOC level barplot
    output$ae_barplot_soc_xlab <- renderUI({
        req(isTRUE(ae_show$barplot_soc))
        value <- isolate(
            if(is.null(input$ae_barplot_soc_xlab)) 'Number of subjects'
            else input$ae_barplot_soc_xlab
        )
        textareaInput('ae_barplot_soc_xlab', 'X-axis label', value)
    })
    ae_barplot_soc_xlab <- reactiveValues(value = 'Number of subjects')
    observe({ ae_barplot_soc_xlab$value = input$ae_barplot_soc_xlab })
    
    # Y-axis label for SOC level barplot
    output$ae_barplot_soc_ylab <- renderUI({
        req(isTRUE(ae_show$barplot_soc))
        value <- isolate(
            if(is.null(input$ae_barplot_soc_ylab)) 'SOC'
            else input$ae_barplot_soc_ylab
        )
        textareaInput('ae_barplot_soc_ylab', 'Y-axis label', value)
    })
    ae_barplot_soc_ylab <- reactiveValues(value = 'SOC')
    observe({ ae_barplot_soc_ylab$value = input$ae_barplot_soc_ylab })
    
    # Title for SOC level barplot
    output$ae_barplot_soc_main <- renderUI({
        req(isTRUE(ae_show$barplot_soc))
        value <- isolate(
            if(is.null(input$ae_barplot_soc_main)) 'Summary of AE by SOC'
            else input$ae_barplot_soc_main
        )
        textareaInput('ae_barplot_soc_main', 'Plot title', value)
    })
    ae_barplot_soc_main <- reactiveValues(value = 'Summary of AE by SOC')
    observe({ ae_barplot_soc_main$value = input$ae_barplot_soc_main })
    
    # Footnote for SOC level barplot
    output$ae_barplot_soc_footnote <- renderUI({
        req(isTRUE(ae_show$barplot_soc))
        value <- isolate(
            if(is.null(input$ae_barplot_soc_footnote)) default_footnote$ae
            else input$ae_barplot_soc_footnote
        )
        textareaInput('ae_barplot_soc_footnote', 'Plot footnote', value)
    })
    ae_barplot_soc_footnote <- reactiveValues(value = NULL)
    observe({
        if(isTRUE(ae_show$barplot_soc)) {
            value <- default_footnote$ae
            ae_barplot_soc_footnote$value <- value
        } else {
            ae_barplot_soc_footnote$value <- NULL
        }
    })
    observe({ ae_barplot_soc_footnote$value = input$ae_barplot_soc_footnote })
    
    
    # X-axis label for PT level barplot
    output$ae_barplot_pt_xlab <- renderUI({
        req(isTRUE(ae_show$barplot_pt))
        value <- isolate(
            if(is.null(input$ae_barplot_pt_xlab)) 'Number of subjects'
            else input$ae_barplot_pt_xlab
        )
        textareaInput('ae_barplot_pt_xlab', 'X-axis label', value)
    })
    ae_barplot_pt_xlab <- reactiveValues(value = 'Number of subjects')
    observe({ ae_barplot_pt_xlab$value = input$ae_barplot_pt_xlab })
    
    # Y-axis label for PT level barplot
    output$ae_barplot_pt_ylab <- renderUI({
        req(isTRUE(ae_show$barplot_pt))
        value <- isolate(
            if(is.null(input$ae_barplot_pt_ylab)) input$ae_soc
            else input$ae_barplot_pt_ylab
        )
        textareaInput('ae_barplot_pt_ylab', 'Y-axis label', value)
    })
    ae_barplot_pt_ylab <- reactiveValues(value = NULL)
    observe({
        if(isTRUE(ae_show$barplot_pt)) {
            value <- input$ae_soc
            ae_barplot_pt_ylab$value <- value
        } else {
            ae_barplot_pt_ylab$value <- NULL
        }
    })
    observe({ ae_barplot_pt_ylab$value = input$ae_barplot_pt_ylab })
    
    # Title for PT level barplot
    output$ae_barplot_pt_main <- renderUI({
        req(isTRUE(ae_show$barplot_pt))
        value <- isolate(
            if(is.null(input$ae_barplot_pt_main)) 'Summary of AE by PT'
            else input$ae_barplot_pt_main
        )
        textareaInput('ae_barplot_pt_main', 'Plot title', value)
    })
    ae_barplot_pt_main <- reactiveValues(value = 'Summary of AE by PT')
    observe({ ae_barplot_pt_main$value = input$ae_barplot_pt_main })
    
    # Footnote for PT level barplot
    output$ae_barplot_pt_footnote <- renderUI({
        req(isTRUE(ae_show$barplot_pt))
        value <- isolate(
            if(is.null(input$ae_barplot_pt_footnote)) default_footnote$ae
            else input$ae_barplot_pt_footnote
        )
        textareaInput('ae_barplot_pt_footnote', 'Plot footnote', value)
    })
    ae_barplot_pt_footnote <- reactiveValues(value = NULL)
    observe({
        if(isTRUE(ae_show$barplot_pt)) {
            value <- default_footnote$ae
            ae_barplot_pt_footnote$value <- value
        } else {
            ae_barplot_pt_footnote$value <- NULL
        }
    })
    observe({ ae_barplot_pt_footnote$value = input$ae_barplot_pt_footnote })
    
    
    # X-axis label for AE Gantt chart
    output$ae_ganttchart_ae_xlab <- renderUI({
        req(isTRUE(ae_show$gantt_chart_ae))
        value <- isolate(
            if(is.null(input$ae_ganttchart_ae_xlab)) 'Study days'
            else input$ae_ganttchart_ae_xlab
        )
        textareaInput('ae_ganttchart_ae_xlab', 'X-axis label', value)
    })
    ae_ganttchart_ae_xlab <- reactiveValues(value = 'Study days')
    observe({ ae_ganttchart_ae_xlab$value = input$ae_ganttchart_ae_xlab })
    
    # Y-axis label for AE Gantt chart
    output$ae_ganttchart_ae_ylab <- renderUI({
        req(isTRUE(ae_show$gantt_chart_ae))
        value <- isolate(
            if(is.null(input$ae_ganttchart_ae_ylab)) 'Subject'
            else input$ae_ganttchart_ae_ylab
        )
        textareaInput('ae_ganttchart_ae_ylab', 'Y-axis label', value)
    })
    ae_ganttchart_ae_ylab <- reactiveValues(value = 'Subject')
    observe({ ae_ganttchart_ae_ylab$value = input$ae_ganttchart_ae_ylab })
    
    # Title for AE Gantt chart
    output$ae_ganttchart_ae_main <- renderUI({
        req(isTRUE(ae_show$gantt_chart_ae))
        value <- isolate(
            if(is.null(input$ae_ganttchart_ae_main))
                paste(input$ae_soc, input$ae_pt)
            else input$ae_ganttchart_ae_main
        )
        textareaInput('ae_ganttchart_ae_main', 'Plot title', value)
    })
    ae_ganttchart_ae_main <- reactiveValues(value = NULL)
    observe({
        if(isTRUE(ae_show$gantt_chart_ae)) {
            value <- input$ae_ganttchart_ae_main
            ae_ganttchart_ae_main$value <- value
        } else {
            ae_ganttchart_ae_main$value <- NULL
        }
    })
    observe({ ae_ganttchart_ae_main$value = input$ae_ganttchart_ae_main })
    
    # Footnote for AE Gantt chart
    output$ae_ganttchart_ae_footnote <- renderUI({
        req(isTRUE(ae_show$gantt_chart_ae))
        value <- isolate(
            if(is.null(input$ae_ganttchart_ae_footnote)) default_footnote$ae
            else input$ae_ganttchart_ae_footnote
        )
        textareaInput('ae_ganttchart_ae_footnote', 'Plot footnote', value)
    })
    ae_ganttchart_ae_footnote <- reactiveValues(value = NULL)
    observe({
        if(isTRUE(ae_show$gantt_chart_ae)) {
            value <- default_footnote$ae
            ae_ganttchart_ae_footnote$value <- value
        } else {
            ae_ganttchart_ae_footnote$value <- NULL
        }
    })
    observe({ae_ganttchart_ae_footnote$value = input$ae_ganttchart_ae_footnote})
    
    
    # X-axis label for subject Gantt chart
    output$ae_ganttchart_subj_xlab <- renderUI({
        req(isTRUE(ae_show$gantt_chart_subj))
        value <- isolate(
            if(is.null(input$ae_ganttchart_subj_xlab)) 'Study days'
            else input$ae_ganttchart_subj_xlab
        )
        textareaInput('ae_ganttchart_subj_xlab', 'X-axis label', value)
    })
    ae_ganttchart_subj_xlab <- reactiveValues(value = 'Study days')
    observe({ ae_ganttchart_subj_xlab$value = input$ae_ganttchart_subj_xlab })
    
    # Y-axis label for subject Gantt chart
    output$ae_ganttchart_subj_ylab <- renderUI({
        req(isTRUE(ae_show$gantt_chart_subj))
        value <- isolate(
            if(is.null(input$ae_ganttchart_subj_ylab)) ''
            else input$ae_ganttchart_subj_ylab
        )
        textareaInput('ae_ganttchart_subj_ylab', 'Y-axis label', value)
    })
    ae_ganttchart_subj_ylab <- reactiveValues(value = '')
    observe({ ae_ganttchart_subj_ylab$value = input$ae_ganttchart_subj_ylab })
    
    # Title for subject Gantt chart
    output$ae_ganttchart_subj_main <- renderUI({
        req(isTRUE(ae_show$gantt_chart_subj))
        value <- isolate(
            if(is.null(input$ae_ganttchart_subj_main))
                paste('Subject', input$ae_subject)
            else input$ae_ganttchart_subj_main
        )
        textareaInput('ae_ganttchart_subj_main', 'Plot title', value)
    })
    ae_ganttchart_subj_main <- reactiveValues(value = NULL)
    observe({
        if(isTRUE(ae_show$gantt_chart_subj)) {
            value <- paste('Subject', input$ae_subject)
            ae_ganttchart_subj_main$value <- value
        } else {
            ae_ganttchart_subj_main$value <- NULL
        }
    })
    observe({ ae_ganttchart_subj_main$value = input$ae_ganttchart_subj_main })
    
    # Footnote for subject Gantt chart
    output$ae_ganttchart_subj_footnote <- renderUI({
        req(isTRUE(ae_show$gantt_chart_subj))
        value <- isolate(
            if(is.null(input$ae_ganttchart_subj_footnote)) default_footnote$ae
            else input$ae_ganttchart_subj_footnote
        )
        textareaInput('ae_ganttchart_subj_footnote', 'Plot footnote', value)
    })
    ae_ganttchart_subj_footnote <- reactiveValues(value = NULL)
    observe({
        if(isTRUE(ae_show$gantt_chart_subj)) {
            value <- default_footnote$ae
            ae_ganttchart_subj_footnote$value <- value
        } else {
            ae_ganttchart_subj_footnote$value <- NULL
        }
    })
    observe({
        ae_ganttchart_subj_footnote$value = input$ae_ganttchart_subj_footnote
    })
    
    #-----------------------------------------------
    # Output for AE listing
    
    ae_summary_barplot_clickloc <- reactiveValues(x = NULL, y = NULL)
    observe({
        ae_summary_barplot_clickloc$x <- input$ae_summary_barplot_click$x
        ae_summary_barplot_clickloc$y <- input$ae_summary_barplot_click$y
    })
    
    ae_summary_level <- reactiveValues(value = 'SOC')
    observe({
        req(data_$ae)
        if(is_blank(input$ae_soc)) {
            ae_summary_level$value <- 'SOC'
            if(!is.null(ae_summary_barplot_info$levels) &&
               !is.null(ae_summary_barplot_clickloc$x) &&
               !is.null(ae_summary_barplot_clickloc$y)) {
                x_click <- ae_summary_barplot_clickloc$x
                y_click <- ae_summary_barplot_clickloc$y
                bars_y <- rev(seq_along(ae_summary_barplot_info$freq))
                cond_x <- x_click >= 0 & x_click <= ae_summary_barplot_info$freq
                cond_y <- y_click >= bars_y - 0.45 & y_click <= bars_y + 0.45
                idx_click <- which(cond_x & cond_y)
                if(length(idx_click) == 1) {
                    soc_click <- ae_summary_barplot_info$levels[idx_click]
                    choices <- c('Choose' = '', unique(data_$ae[[ae_mdrsoc_col]]))
                    updateSelectInput(
                        session, 'ae_soc', 'SOC', choices, selected = soc_click
                    )
                    ae_summary_level$value == 'PT'
                    ae_summary_barplot_clickloc$x <- NULL
                    ae_summary_barplot_clickloc$y <- NULL
                }
            }
        } else {
            if(is_blank(input$ae_pt)) {
                ae_summary_level$value <- 'PT'
                if(!is.null(ae_summary_barplot_info$levels) &&
                   !is.null(ae_summary_barplot_clickloc$x) &&
                   !is.null(ae_summary_barplot_clickloc$y) &&
                   isTRUE(data_import_status$ae_enr)) {
                    x_click <- ae_summary_barplot_clickloc$x
                    y_click <- ae_summary_barplot_clickloc$y
                    bars_y <- rev(seq_along(ae_summary_barplot_info$freq))
                    cond_x <- (x_click >= 0 &
                                   x_click <= ae_summary_barplot_info$freq)
                    cond_y <- y_click >= bars_y - 0.45 & y_click <= bars_y + 0.45
                    idx_click <- which(cond_x & cond_y)
                    if(length(idx_click) == 1) {
                        pt_click <- ae_summary_barplot_info$levels[idx_click]
                        choices <- c(
                            'Choose' = '', unique(data_$ae[[ae_mdrpt_col]])
                        )
                        updateSelectInput(
                            session, 'ae_pt', 'PT', choices, selected = pt_click
                        )
                        ae_summary_level$value == 'Subject'
                        ae_summary_barplot_clickloc$x <- NULL
                        ae_summary_barplot_clickloc$y <- NULL
                    }
                }
            } else if(isTRUE(data_import_status$ae_enr)) {
                ae_summary_level$value <- 'Subject'
            }
        }
    })
    
    ae_summary_barplot_info <- reactiveValues(levels = NULL, freq = NULL)
    observe({
        req(data_$ae, input$ae_barplot_rank, ae_summary_level$value)
        if(ae_summary_level$value == 'SOC') {
            data <- data_$ae %>%
                group_by_(ae_mdrsoc_col, ae_subj_col) %>%
                filter(row_number() == 1)
            soc_tble <- table(as.character(data[[ae_mdrsoc_col]]))
            soc_tble <- soc_tble[gtools::mixedorder(names(soc_tble))]
            if(input$ae_barplot_rank == 'By frequency') {
                soc_tble <- sort(soc_tble, decreasing = TRUE)
            }
            ae_summary_barplot_info$levels <- names(soc_tble)
            ae_summary_barplot_info$freq <- unname(soc_tble)
        } else if(ae_summary_level$value == 'PT') {
            cond_soc <- data_$ae[[ae_mdrsoc_col]] %in% input$ae_soc
            data <- data_$ae[cond_soc, , drop = FALSE]
            data <- data %>%
                group_by_(ae_mdrpt_col, ae_subj_col) %>%
                filter(row_number() == 1)
            pt_tble <- table(data[[ae_mdrpt_col]])
            pt_tble <- pt_tble[gtools::mixedorder(names(pt_tble))]
            if(input$ae_barplot_rank == 'By frequency') {
                pt_tble <- sort(pt_tble, decreasing = TRUE)
            }
            ae_summary_barplot_info$levels <- names(pt_tble)
            ae_summary_barplot_info$freq <- unname(pt_tble)
        } else if(ae_summary_level$value == 'Subject') {
            cond_soc <- data_$ae[[ae_mdrsoc_col]] %in% input$ae_soc
            cond_pt <- data_$ae[[ae_mdrpt_col]] %in% input$ae_pt
            data <- data_$ae[cond_soc & cond_pt, , drop = FALSE]
            unique_subject <- gtools::mixedsort(unique(data[[ae_subj_col]]))
            ae_summary_barplot_info$levels <- unique_subject
            ae_summary_barplot_info$freq <- NULL
        }
    })
    
    ae_summary_barplot_data <- reactiveValues(value = NULL)
    observe({
        if(all(isTRUE(data_import_status$ae), !is.null(ae_summary_level$value),
               !is.null(ae_summary_barplot_info$levels))) {
            if(ae_summary_level$value == 'SOC') {
                data <- data_$ae %>%
                    group_by_(ae_mdrsoc_col, ae_subj_col) %>%
                    filter(row_number() == 1)
                if(!is_blank(input$ae_group)) {
                    data[[input$ae_group]] <- factor(data[[input$ae_group]])
                    levels(data[[input$ae_group]]) <- paste(
                        input$ae_group, '=', levels(data[[input$ae_group]])
                    )
                }
                ae_summary_barplot_data$value <- data
            } else if(ae_summary_level$value == 'PT') {
                data <- data_$ae[data_$ae[[ae_mdrsoc_col]] %in% input$ae_soc, ]
                data <- data %>%
                    group_by_(ae_mdrpt_col, ae_subj_col) %>%
                    filter(row_number() == 1)
                if(!is_blank(input$ae_group)) {
                    data[[input$ae_group]] <- factor(data[[input$ae_group]])
                    levels(data[[input$ae_group]]) <- paste(
                        input$ae_group, '=', levels(data[[input$ae_group]])
                    )
                }
                ae_summary_barplot_data$value <- data
            } else if(ae_summary_level$value == 'Subject') {
                cond_soc <- data_$ae_enr[[ae_mdrsoc_col]] %in% input$ae_soc
                cond_pt <- data_$ae_enr[[ae_mdrpt_col]] %in% input$ae_pt
                data <- data_$ae_enr[cond_soc & cond_pt, , drop = FALSE]
                start_yy <- data[[ae_aestdatyy_col]]
                start_mm <- data[[ae_aestdatmm_col]]
                start_dd <- data[[ae_aestdatdd_col]]
                end_yy <- data[[ae_aeendatyy_col]]
                end_mm <- data[[ae_aeendatmm_col]]
                end_dd <- data[[ae_aeendatdd_col]]
                data[['ae_start_date']] <- tryCatch(
                    as.Date(paste(start_yy, start_mm, start_dd, sep = '-')),
                    error = function(e) NA
                )
                data[['ae_end_date']] <- tryCatch(
                    as.Date(paste(end_yy, end_mm, end_dd, sep = '-')),
                    error = function(e) NA
                )
                enr_yy <- data[[enr_enroldatyy_col]]
                enr_mm <- data[[enr_enroldatmm_col]]
                enr_dd <- data[[enr_enroldatdd_col]]
                data[['enrollment_date']] <- tryCatch(
                    as.Date(paste(enr_yy, enr_mm, enr_dd, sep = '-')),
                    error = function(e) NA
                )
                data <- data %>%
                    mutate(start = as.integer(ae_start_date -enrollment_date+1),
                           end = as.integer(ae_end_date -enrollment_date + 1))
                col_to_keep <- c(ae_subj_col, 'start', 'end')
                id_vars <- c(ae_subj_col)
                if(!is_blank(input$ae_group)) {
                    col_to_keep <- c(col_to_keep, input$ae_group)
                    id_vars <- c(id_vars, input$ae_group)
                }
                data_long <- reshape2::melt(
                    data[, col_to_keep, drop = FALSE], id.vars = id_vars
                )
                data_long <- rename(data_long, time = value, status = variable)
                end_pos <- data_long[['status']] == 'end'
                missing_end_days <- is.na(data_long[['time']]) & end_pos
                max_days <- max(data_long[['time']], na.rm = TRUE)
                if(any(missing_end_days)) {
                    data_long[missing_end_days, 'status'] <- 'ongoing'
                    data_long[missing_end_days, 'time'] <- max_days
                }
                
                if(!is_blank(input$ae_group)) {
                    data_long[[input$ae_group]] <- 
                        factor(data_long[[input$ae_group]])
                    levels(data_long[[input$ae_group]]) <- paste(
                        input$ae_group, '=', levels(data_long[[input$ae_group]])
                    )
                }
                
                ae_summary_barplot_data$value <- data_long
            } else ae_summary_barplot_data$value <- NULL
        } else ae_summary_barplot_data$value <- NULL
    })
    
    ae_gantt_chart_data <- reactiveValues(value = NULL)
    observe({
        if(all(isTRUE(data_import_status$ae_enr), !is_blank(input$ae_subject))) {
            data <- data_$ae_enr
            has_soc <- !is.na(data[[ae_mdrsoc_col]])
            has_pt <- !is.na(data[[ae_mdrpt_col]])
            selected_subjects <- data[[ae_subj_col]] %in% input$ae_subject
            data <- data[has_soc & has_pt & selected_subjects, , drop = FALSE]
            start_yy <- data[[ae_aestdatyy_col]]
            start_mm <- data[[ae_aestdatmm_col]]
            start_dd <- data[[ae_aestdatdd_col]]
            end_yy <- data[[ae_aeendatyy_col]]
            end_mm <- data[[ae_aeendatmm_col]]
            end_dd <- data[[ae_aeendatdd_col]]
            data[['ae_start_date']] <- tryCatch(
                as.Date(paste(start_yy, start_mm, start_dd, sep = '-')),
                error = function(e) NA
            )
            data[['ae_end_date']] <- tryCatch(
                as.Date(paste(end_yy, end_mm, end_dd, sep = '-')),
                error = function(e) NA
            )
            enr_yy <- data[[enr_enroldatyy_col]]
            enr_mm <- data[[enr_enroldatmm_col]]
            enr_dd <- data[[enr_enroldatdd_col]]
            data[['enrollment_date']] <- tryCatch(
                as.Date(paste(enr_yy, enr_mm, enr_dd, sep = '-')),
                error = function(e) NA
            )
            data <- data %>%
                mutate(start = as.integer(ae_start_date -enrollment_date+1),
                       end = as.integer(ae_end_date -enrollment_date + 1))
            col_to_keep <- c(ae_subj_col, ae_mdrsoc_col, ae_mdrpt_col,
                             'start', 'end')
            id_vars <- c(ae_subj_col, ae_mdrsoc_col, ae_mdrpt_col)
            if(!is_blank(input$ae_group)) {
                col_to_keep <- c(col_to_keep, input$ae_group)
                id_vars <- c(id_vars, input$ae_group)
            }
            data_long <- reshape2::melt(
                data[, col_to_keep, drop = FALSE], id.vars = id_vars
            )
            data_long <- rename(data_long, time = value, status = variable)
            end_pos <- data_long[['status']] == 'end'
            missing_end_days <- is.na(data_long[['time']]) & end_pos
            max_days <- max(data_long[['time']], na.rm = TRUE)
            if(any(missing_end_days)) {
                data_long[missing_end_days, 'status'] <- 'ongoing'
                data_long[missing_end_days, 'time'] <- max_days
            }
            data_long[[ae_subj_col]] <- factor(
                data_long[[ae_subj_col]],
                levels = gtools::mixedsort(unique(data_long[[ae_subj_col]]))
            )
            data_long[[ae_mdrsoc_col]] <- factor(
                data_long[[ae_mdrsoc_col]],
                levels = gtools::mixedsort(unique(data_long[[ae_mdrsoc_col]]))
            )
            data_long[[ae_mdrpt_col]] <- factor(
                data_long[[ae_mdrpt_col]],
                levels = gtools::mixedsort(unique(data_long[[ae_mdrpt_col]]))
            )
            data_long[['soc_pt']] <- interaction(
                data_long[[ae_mdrsoc_col]], data_long[[ae_mdrpt_col]],
                sep = '\n', lex.order = TRUE
            )
            
            if(!is_blank(input$ae_group)) {
                data_long[[input$ae_group]] <- 
                    factor(data_long[[input$ae_group]])
                levels(data_long[[input$ae_group]]) <- paste(
                    input$ae_group, '=', levels(data_long[[input$ae_group]])
                )
            }
            
            ae_gantt_chart_data$value <- data_long
        } else ae_gantt_chart_data$value <- NULL
    })
    
    ae_summary_barplot <- reactive({
        req(ae_summary_level$value, ae_summary_barplot_info$levels,
            ae_summary_barplot_data$value,
            req(input$ae_tabs) == 'Summary of AE by SOC and PT')
        if(ae_summary_level$value == 'SOC') {
            req(!is.null(ae_barplot_soc_xlab$value),
                !is.null(ae_barplot_soc_ylab$value),
                !is.null(ae_barplot_soc_main$value))
            plot_ <- gg_barplot(
                ae_summary_barplot_data$value, var = ae_mdrsoc_col,
                var_levels = ae_summary_barplot_info$levels,
                group = NULL, facet_r = NULL,
                facet_c = ternary(is_blank(input$ae_group), NULL, input$ae_group),
                facet_scale = 'free', facet_space = 'free',
                x_lab = ae_barplot_soc_xlab$value,
                y_lab = ae_barplot_soc_ylab$value,
                title = ae_barplot_soc_main$value,
                add_counts = TRUE, counts_pos = 'inside-bar',
                bar_label_align = 'left', grids = 'on',
                bw_theme = TRUE, horizontal = TRUE
            )
        } else if(ae_summary_level$value == 'PT') {
            req(!is.null(ae_barplot_pt_xlab$value),
                !is.null(ae_barplot_pt_ylab$value),
                !is.null(ae_barplot_pt_main$value))
            plot_ <- gg_barplot(
                ae_summary_barplot_data$value, var = ae_mdrpt_col,
                var_levels = ae_summary_barplot_info$levels,
                group = NULL, facet_r = NULL,
                facet_c = ternary(is_blank(input$ae_group), NULL, input$ae_group),
                facet_scale = 'free', facet_space = 'free',
                x_lab = ae_barplot_pt_xlab$value,
                y_lab = ae_barplot_pt_ylab$value,
                title = ae_barplot_pt_main$value,
                add_counts = TRUE, counts_pos = 'inside-bar',
                bar_label_align = 'left', grids = 'on',
                bw_theme = TRUE, horizontal = TRUE
            )
        } else if(ae_summary_level$value == 'Subject') {
            req(!is.null(ae_ganttchart_ae_xlab$value),
                !is.null(ae_ganttchart_ae_ylab$value),
                !is.null(ae_ganttchart_ae_main$value))
            plot_ <- gg_gantt_chart(
                ae_summary_barplot_data$value, var = ae_subj_col,
                var_levels = ae_summary_barplot_info$levels,
                time = 'time', status = 'status',
                group = NULL, facet_r = NULL,
                facet_c = ternary(is_blank(input$ae_group), NULL, input$ae_group),
                facet_scale = 'free', facet_space = 'free',
                x_lab = ae_ganttchart_ae_xlab$value,
                y_lab = ae_ganttchart_ae_ylab$value,
                title = ae_ganttchart_ae_main$value,
                label_align = 'left', label_angle = NULL,
                grids = 'y', bw_theme = TRUE, horizontal = TRUE,
                point_shape_map = list('start' = '\u25BA', 'end' = '\u25C4',
                                       'ongoing' = NA),
                point_size = 3, point_legend = TRUE
            )
            unique_days <- sort(unique(
                c(1, ae_summary_barplot_data$value[['time']])
            ))
            x_lim <- range(unique_days)
            plot_ <- plot_ + scale_x_continuous(breaks = unique_days,
                                                limits = x_lim)
        }

        return(plot_)
    })
    
    ae_gantt_chart <- reactive({
        req(ae_gantt_chart_data$value,
            req(input$ae_tabs) == 'Summary of AE by Subject',
            !is.null(ae_ganttchart_subj_xlab$value),
            !is.null(ae_ganttchart_subj_ylab$value),
            !is.null(ae_ganttchart_subj_main$value))
        plot_ <- gg_gantt_chart(
            ae_gantt_chart_data$value, var = 'soc_pt', var_levels = NULL,
            time = 'time', status = 'status',
            group = NULL, facet_r = NULL, facet_c = NULL,
            facet_scale = 'free', facet_space = 'free',
            x_lab = ae_ganttchart_subj_xlab$value,
            y_lab = ae_ganttchart_subj_ylab$value,
            title = ae_ganttchart_subj_main$value,
            label_align = 'left', label_angle = NULL,
            grids = 'y', bw_theme = TRUE, horizontal = TRUE,
            point_shape_map = list('start' = '\u25BA', 'end' = '\u25C4',
                                   'ongoing' = NA),
            point_size = 3, point_legend = TRUE
        )
        unique_days <- sort(unique(
            c(1, ae_gantt_chart_data$value[['time']])
        ))
        x_lim <- range(unique_days)
        plot_ <- plot_ + scale_x_continuous(breaks = unique_days,
                                            limits = x_lim)
        return(plot_)
    })
    
    output$ae_summary_barplot_ui <- renderUI({
        req(req(input$ae_tabs) == 'Summary of AE by SOC and PT')
        shiny::tagList(
            plotOutput('ae_summary_barplot',
                       click = 'ae_summary_barplot_click',
                       dblclick = 'ae_summary_barplot_dblclick'),
            uiOutput('ae_summary_barplot_fn_out'),
            uiOutput('ae_summary_barplot_goback'),
            tags$head(tags$style(
                "#ae_summary_barplot_fn_out{font-size: 9px;}"
            ))
        )
    })
    output$ae_summary_barplot <- renderPlot({ ae_summary_barplot() })
    
    # footnotes
    output$ae_summary_barplot_fn_out <- renderUI({
        if(isTRUE(ae_show$barplot_soc)) {
            req(!is.null(ae_barplot_soc_footnote$value))
            HTML(paste(
                strsplit(ae_barplot_soc_footnote$value, '\n')[[1]],
                collapse = '<br/>'
            ))
        } else if(isTRUE(ae_show$barplot_pt)) {
            req(!is.null(ae_barplot_pt_footnote$value))
            HTML(paste(
                strsplit(ae_barplot_pt_footnote$value, '\n')[[1]],
                collapse = '<br/>'
            ))
        } else if(isTRUE(ae_show$gantt_chart_ae)) {
            req(!is.null(ae_ganttchart_ae_footnote$value))
            HTML(paste(
                strsplit(ae_ganttchart_ae_footnote$value, '\n')[[1]],
                collapse = '<br/>'
            ))
        }
    })
    
    output$ae_gantt_chart_ui <- renderUI({
        req(req(input$ae_tabs) == 'Summary of AE by Subject')
        shiny::tagList(
            plotOutput('ae_gantt_chart',
                       click = 'ae_gantt_chart_click',
                       dblclick = 'ae_gantt_chart_dblclick'),
            uiOutput('ae_gantt_chart_fn_out'),
            tags$head(tags$style(
                "#ae_gantt_chart_fn_out{font-size: 9px;}"
            ))
        )
    })
    output$ae_gantt_chart <- renderPlot({ ae_gantt_chart() })
    
    # footnote
    output$ae_gantt_chart_fn_out <- renderUI({
        req(ae_show$gantt_chart_subj)
        req(!is.null(ae_ganttchart_subj_footnote$value))
        HTML(paste(
            strsplit(ae_ganttchart_subj_footnote$value, '\n')[[1]],
            collapse = '<br/>'
        ))
    })
    
    
    #-----------------------------------------------
    # Download for AE listing
    
    # download button for SOC level barplota
    output$ae_barplot_soc_download_button <- renderUI({
        req(ae_summary_barplot(), ae_show$barplot_soc)
        downloadButton('ae_barplot_soc_download', 'Download plot')
    })
    output$ae_barplot_soc_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'ae_summary_soc_',
                format(Sys.Date(),format = '%Y%m%d'), '.pdf'
            )
            return(file_name)
        },
        content = function(file) {
            plot_ <- ae_summary_barplot()
            plot_ <- add_footnote(plot_, ae_barplot_soc_footnote$value)
            height_width_ratio <- 3 / 4
            width <- 8
            height <- width * height_width_ratio
            ggsave(filename = file, plot = plot_, width = width,
                   height = height, dpi = 600)
        }
    )
    
    # download button for PT level barplota
    output$ae_barplot_pt_download_button <- renderUI({
        req(ae_summary_barplot(), ae_show$barplot_pt)
        downloadButton('ae_barplot_pt_download', 'Download plot')
    })
    output$ae_barplot_pt_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'ae_summary_pt_',
                format(Sys.Date(),format = '%Y%m%d'), '.pdf'
            )
            return(file_name)
        },
        content = function(file) {
            plot_ <- ae_summary_barplot()
            plot_ <- add_footnote(plot_, ae_barplot_pt_footnote$value)
            height_width_ratio <- 3 / 4
            width <- 8
            height <- width * height_width_ratio
            ggsave(filename = file, plot = plot_, width = width,
                   height = height, dpi = 600)
        }
    )
    
    # download button for AE Gantt chart
    output$ae_ganttchart_ae_download_button <- renderUI({
        req(ae_summary_barplot(), ae_show$gantt_chart_ae)
        downloadButton('ae_ganttchart_ae_download', 'Download plot')
    })
    output$ae_ganttchart_ae_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'ae_gantt_chart_',
                format(Sys.Date(),format = '%Y%m%d'), '.png'
            )
            return(file_name)
        },
        content = function(file) {
            plot_ <- ae_summary_barplot()
            plot_ <- add_footnote(plot_, ae_ganttchart_ae_footnote$value)
            height_width_ratio <- 3 / 4
            width <- 8
            height <- width * height_width_ratio
            ggsave(filename = file, plot = plot_, width = width,
                   height = height, dpi = 600)
        }
    )
    
    # download button for subject Gantt chart
    output$ae_ganttchart_subj_download_button <- renderUI({
        req(ae_gantt_chart(), ae_show$gantt_chart_subj)
        downloadButton('ae_ganttchart_subj_download', 'Download plot')
    })
    output$ae_ganttchart_subj_download <- downloadHandler(
        filename = function() {
            file_name <- paste0(
                'subject_gantt_chart_',
                format(Sys.Date(),format = '%Y%m%d'), '.png'
            )
            return(file_name)
        },
        content = function(file) {
            plot_ <- ae_gantt_chart()
            plot_ <- add_footnote(plot_, ae_ganttchart_subj_footnote$value)
            height_width_ratio <- 3 / 4
            width <- 8
            height <- width * height_width_ratio
            ggsave(filename = file, plot = plot_, width = width,
                   height = height, dpi = 600)
        }
    )
    
    
    
    #-----------------------------------------------
    # Output TNFs
    #-----------------------------------------------
    
    data_import_status <- reactiveValues(
        subj_pk_param = NULL, subj_pd = NULL, subj_pkpd = NULL,
        sample_pk_param = NULL, sample_pk_con = NULL, sample_pd = NULL,
        sample_pkpd = NULL, lab = NULL
    )
    
    # fileInput for uploading TNF file
    output$out_tnf_file <- renderUI({
        if(any(isTRUE(data_import_status$subj_pk_param),
               isTRUE(data_import_status$subj_pd),
               isTRUE(data_import_status$subj_pkpd),
               isTRUE(data_import_status$sample_pk_param),
               isTRUE(data_import_status$sample_pk_con),
               isTRUE(data_import_status$sample_pd),
               isTRUE(data_import_status$sample_pkpd),
               isTRUE(data_import_status$lab)))
        fileInput('out_tnf_file', tags$p('Choose file to import',
                                     tags$a('(Need input data template?)',
                                            target = '_blank',
                                            href = out_tnf_template_file)),
                  accept = out_tnf_accepted_file_format)
    })
    
    out_tnf_data <- reactive({
        if(is.null(input$out_tnf_file)) return()
        out_file <- fix_uploaded_files_names(input$out_tnf_file)
        file_name <- out_file$name
        if(endswith(tolower(file_name), c('xls', 'xlsx'))) {
            wb <- XLConnect::loadWorkbook(out_file$datapath)
            XLConnect::setMissingValue(wb, value = c('NA', ''))
            result <- c()
            pass <- FALSE
            for(name in names(out_tnf_sheet_names)) {
                read_str <- paste0(
                    name,
                    '<- tryCatch(suppressWarnings(XLConnect::readWorksheet(wb, "',
                    out_tnf_sheet_names[name],
                    '")), error = function(e) {NULL})'
                )
                eval(parse(text = read_str))
                if(!is.null(get(name))) {
                    list_str <- paste0(
                        'result <- c(result, list(', name, ' = ', name, '))'
                    )
                    eval(parse(text = list_str))
                    pass <- TRUE
                }
            }
            if(pass) return(result)
            else {
                message <- paste0(
                    'The uploaded TNF file should contain sheets with the ',
                    'following names: ', '\n',
                    paste(out_tnf_sheet_names, collapse = '\n')
                )
                stop(message)
            }
        } else {
            stop('Please upload TNF file in Excel format')
        }
    })
    
    # download TNF button
    output$out_tnf_download_button <- renderUI({
        req(any(!is.null(n_tfl$figure), !is.null(n_tfl$table)))
        downloadButton('out_tnf_download', 'Download TNF')
    })
    output$out_tnf_download <- downloadHandler(
        filename = function() {
            file_name <- paste0('TNF_', format(Sys.Date(), format = '%Y%m%d'),
                                '.xls')
            return(file_name)
        },
        content = function(file) {
            tnf_list <- setNames(
                list(tnf_rows$subj_pk, tnf_rows$subj_pd, tnf_rows$subj_pkpd,
                     tnf_rows$sample_pk, tnf_rows$sample_pd, tnf_rows$sample_pkpd,
                     tnf_rows$lab),
                out_tnf_sheet_names
            )
            tmpdir <- tempdir()
            tnf_name <- 'TNF.xls'
            owd <- setwd(tmpdir)
            on.exit(setwd(owd))
            tnf_create(tnf_name, tmpdir, tnf_list)
            file.copy(paste(tmpdir, tnf_name, sep = '\\'), file)
        }
    )
    
    # an actionButton for processing uploaded TNF file
    output$out_process_tnf <- renderUI({
        tnf_uploaded <- !is.null(out_tnf_data())
        tnf_saved <- any(!is.null(n_tfl$figure), !is.null(n_tfl$table))
        req(any(tnf_uploaded, tnf_saved))
        actionButton('out_process_tnf', 'Process TNF')
    })
    all_tfls <- reactiveValues(value = NULL, ready = FALSE, dir = NULL)
    observeEvent(input$out_process_tnf, {
        tnf_uploaded <- !is.null(out_tnf_data())
        tnf_saved <- any(!is.null(n_tfl$figure), !is.null(n_tfl$table))
        if(tnf_uploaded) out_tnf_data <- out_tnf_data()
        else if(tnf_saved) out_tnf_data <- tnf_rows
        all_sheets <- names(out_tnf_data)
        tmpdir <- tempdir()
        all_tfls$dir <- tmpdir
        owd <- setwd(tmpdir)
        on.exit(setwd(owd))
        
        num_tfls_vec <- lapply(
            names(out_tnf_sheet_names),
            function(sheet) {nrow(out_tnf_data[[sheet]])}
        )
        num_tfls <- sum(unlist(num_tfls_vec))
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = 'Processing TNF', value = 0)
        
        if(!is.null(out_tnf_data[['subj_pk']]) &&
           !is.null(data_$subj_pk_param)) {
            subj_pk_rows <- out_tnf_data[['subj_pk']]
            for(idx in seq_len(nrow(subj_pk_rows))) {
                irow <- subj_pk_rows[idx, , drop = FALSE]
                
                title_key <- irow[[out_subj_pk_title]]
                tflt <- irow[[out_subj_pk_tflt]]
                tfln <- irow[[out_subj_pk_tfln]]
                output_type <- irow[[out_subj_pk_output]]
                pk_param <- irow[[out_subj_pk_pk_param]]
                x <- irow[[out_subj_pk_x]]
                x_type <- irow[[out_subj_pk_x_type]]
                dose_levels <- as.numeric(unlist(strsplit(gsub(
                    '\\\\n', '\n', as.character(irow[[out_subj_pk_dose]])
                ), '\n')))
                group <- irow[[out_subj_pk_group]]
                dgt <- as.numeric(irow[[out_subj_pk_decimal]])
                table_title <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_subj_pk_table_title]]
                )))
                table_title <- trimws(paste(
                    tflt, tfln, ':', table_title
                ))
                table_footnote <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_subj_pk_table_footnote]]
                )))
                log_y <- ifelse(
                    as.character(irow[[out_subj_pk_log_y]]) %in% string_yes,
                    TRUE, FALSE
                )
                add_points <- ifelse(
                    as.character(irow[[out_subj_pk_add_points]]) %in% string_yes,
                    TRUE, FALSE
                )
                summary_method <- irow[[out_subj_pk_summary]]
                x_lab <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_subj_pk_xlab]]
                )))
                y_lab <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_subj_pk_ylab]]
                )))
                plot_title <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_subj_pk_plot_title]]
                )))
                plot_title <- trimws(paste(
                    tflt, tfln, ':', plot_title
                ))
                plot_footnote <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_subj_pk_plot_footnote]]
                )))
                
                data <- data_$subj_pk_param
                data <- data[!is.na(data[[subj_pk_estm_col]]), , drop = F]
                data <- data[data[[subj_pk_param_col]] %in% pk_param, ]
                if(!is_blank(x) && !is_blank(x_type)) {
                    data <- arrange_(data, x)
                    if(x_type == 'Continuous')
                        data[[x]] <- as.numeric(data[[x]])
                    else if(x_type == 'Categorical')
                        data[[x]] <- as.factor(data[[x]])
                }
                
                if(output_type == 'Summary table') {
                    file <- paste(title_key, 'rtf', sep = '.')
                    subj_pk_summary_func <- c(
                        'N' = n_nna,
                        'Mean (SD)' = partial(mean_sd_str, digits = dgt),
                        '%CV' = partial(coeff_var_str, digits = dgt),
                        'Median' = partial(median_str, digits = dgt),
                        'Q1, Q3' = partial(q1_q3_str, digits = dgt),
                        'Min, Max' = partial(min_max_str, digits = dgt),
                        'Geom Mean (%CV)' = partial(geo_mean_cv_str, digits = dgt),
                        'Mean (SD) of LN' = partial(mean_sd_ln_str, digits = dgt)
                    )
                    if(is_blank(x)) {
                        data <- select_(data, subj_pk_estm_col)
                        summary_tbl <- summary_table(
                            data, collabel = 'Value',
                            caption = table_title, footnote = table_footnote,
                            func_list = subj_pk_summary_func, format = 'rtf'
                        )
                    } else {
                        data <- select_(data, subj_pk_estm_col, x)
                        data[[x]] <- factor(
                            data[[x]], levels = sort(unique(data[[x]]))
                        )
                        summary_tbl <- summary_table_col(
                            data, col_var = x,
                            col_names = paste(x, '=', levels(data[[x]])),
                            caption = table_title, footnote = table_footnote,
                            func_list = subj_pk_summary_func, format = 'rtf'
                        )
                    }
                    rtf_table_wrapper(
                        file, summary_tbl, block_break = TRUE,
                        nline_block = length(subj_pk_summary_func),
                        caption = table_title, footnote = table_footnote
                    )
                    progress$inc(1 / num_tfls, detail = file)
                    all_tfls$value <- c(all_tfls$value, file)
                } else if(output_type == 'Box plot') {
                    file <- paste(title_key, 'pdf', sep = '.')
                    
                    data <- data[!is.na(data[[x]]), , drop = F]
                    plot_ <- gg_boxplot(
                        data, x, subj_pk_estm_col, group = group, log_y = log_y,
                        x_lab = x_lab, y_lab = y_lab, title = plot_title,
                        with_points = add_points
                    )
                    plot_ <- add_footnote(plot_, plot_footnote)
                    
                    height_width_ratio <- 3 / 4
                    width <- 8
                    height <- width * height_width_ratio
                    ggsave(filename = file, plot = plot_, width = width,
                           height = height, dpi = 600)
                    progress$inc(1 / num_tfls, detail = file)
                    all_tfls$value <- c(all_tfls$value, file)
                } else if(output_type == 'Line plot') {
                    file <- paste(title_key, 'pdf', sep = '.')
                    
                    data <- data[!is.na(data[[x]]), , drop = F]
                    plot_ <- gg_lineplot(
                        data, x, subj_pk_estm_col, group = group, log_y = log_y,
                        x_lab = x_lab, y_lab = y_lab, title = plot_title,
                        summary_method = summary_method,with_points = add_points
                    )
                    plot_ <- add_footnote(plot_, plot_footnote)
                    
                    height_width_ratio <- 3 / 4
                    width <- 8
                    height <- width * height_width_ratio
                    ggsave(filename = file, plot = plot_, width = width,
                           height = height, dpi = 600)
                    progress$inc(1 / num_tfls, detail = file)
                    all_tfls$value <- c(all_tfls$value, file)
                } else if(output_type == 'Dose proportionality') {
                    file <- paste(title_key, 'pdf', sep = '.')
                    
                    data <- data[data[[subj_pk_dose_col]] %in% dose_levels, ,
                                 drop = FALSE]
                    data[[subj_pk_estm_col]] <- as.numeric(data[[subj_pk_estm_col]])
                    data[[subj_pk_dose_col]] <- as.numeric(data[[subj_pk_dose_col]])
                    log_pk <- log(data[[subj_pk_estm_col]])
                    log_dose <- log(data[[subj_pk_dose_col]])
                    lm_model <- lm(log_pk ~ log_dose)
                    
                    data <- data[data[[subj_pk_dose_col]] %in% dose_levels, ,
                                 drop = FALSE]
                    if(!is_blank(group)) {
                        data[[group]] <- factor(
                            data[[group]], levels = sort(unique(data[[group]]))
                        )
                    }
                    
                    lm_intercept <- lm_model$coefficients[1]
                    lm_slope <- lm_model$coefficients[2]
                    log_dose_name <- 'log(Dose)'
                    log_pk_name <- paste0('log(', pk_param, ')')
                    data[[log_dose_name]] <- log(data[[subj_pk_dose_col]])
                    data[[log_pk_name]] <- log(data[[subj_pk_estm_col]])
                    data <- data[
                        !is.na(data[[log_dose_name]]) & !is.na(data[[log_pk_name]]),
                        , drop = FALSE
                    ]
                    plot_ <- gg_wrapper(
                        data, aes_string(x = paste0('`', log_dose_name, '`'),
                                         y = paste0('`', log_pk_name, '`'))
                    )
                    if(!is_blank(group)) {
                        plot_ <- plot_ + aes_string(colour = group)
                    }
                    plot_ <- plot_ + geom_point(size = 2) +
                        geom_abline(intercept = lm_intercept, slope = lm_slope) +
                        scale_x_continuous(
                            breaks = log(sort(dose_levels)),
                            labels = paste0('log(', dose_levels, ')')
                        ) +
                        labs(x = x_lab, y = y_lab, title = plot_title)
                    plot_ <- add_footnote(plot_, plot_footnote)
                    
                    height_width_ratio <- 3 / 4
                    width <- 8
                    height <- width * height_width_ratio
                    ggsave(filename = file, plot = plot_, width = width,
                           height = height, dpi = 600)
                    progress$inc(1 / num_tfls, detail = file)
                    all_tfls$value <- c(all_tfls$value, file)
                }
            }
        }
        if(!is.null(out_tnf_data[['subj_pd']]) &&
           !is.null(data_$subj_pd)) {
            subj_pd_rows <- out_tnf_data[['subj_pd']]
            for(idx in seq_len(nrow(subj_pd_rows))) {
                irow <- subj_pd_rows[idx, , drop = FALSE]
                
                title_key <- irow[[out_subj_pd_title]]
                tflt <- irow[[out_subj_pd_tflt]]
                tfln <- irow[[out_subj_pd_tfln]]
                output_type <- irow[[out_subj_pd_output]]
                pd_param1 <- irow[[out_subj_pd_pd_param1]]
                pd_value1 <- irow[[out_subj_pd_pd_value1]]
                pd_param2 <- irow[[out_subj_pd_pd_param2]]
                pd_value2 <- irow[[out_subj_pd_pd_value2]]
                x <- irow[[out_subj_pd_x]]
                x_type <- irow[[out_subj_pd_x_type]]
                group <- irow[[out_subj_pd_group]]
                dgt <- as.numeric(irow[[out_subj_pd_decimal]])
                table_title <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_subj_pd_table_title]]
                )))
                table_title <- trimws(paste(
                    tflt, tfln, ':', table_title
                ))
                table_footnote <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_subj_pd_table_footnote]]
                )))
                log_x <- ifelse(
                    as.character(irow[[out_subj_pd_log_x]]) %in% string_yes,
                    TRUE, FALSE
                )
                log_y <- ifelse(
                    as.character(irow[[out_subj_pd_log_y]]) %in% string_yes,
                    TRUE, FALSE
                )
                add_points <- ifelse(
                    as.character(irow[[out_subj_pd_add_points]]) %in% string_yes,
                    TRUE, FALSE
                )
                refline <- trimws(as.character(irow[[out_subj_pd_refline]]))
                summary_method <- trimws(irow[[out_subj_pd_summary]])
                same_y <- ifelse(
                    as.character(irow[[out_subj_pd_same_y]]) %in% string_yes,
                    TRUE, FALSE
                )
                x_lab <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_subj_pd_xlab]]
                )))
                left_y_lab <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_subj_pd_left_ylab]]
                )))
                right_y_lab <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_subj_pd_right_ylab]]
                )))
                plot_title <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_subj_pd_plot_title]]
                )))
                plot_title <- trimws(paste(
                    tflt, tfln, ':', plot_title
                ))
                plot_footnote <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_subj_pd_plot_footnote]]
                )))
                
                data <- data_$subj_pd
                if(output_type %in% c('Summary table', 'Box plot', 'Line plot')) {
                    data <- data[!is.na(pd_value1), , drop = FALSE]
                    data <- data[data[[subj_pd_param_col]] %in% pd_param1, ]
                    if(!is_blank(x)) {
                        data <- arrange_(data, x)
                        if(x_type == 'Continuous')
                            data[[x]] <- as.numeric(data[[x]])
                        else if(x_type == 'Categorical')
                            data[[x]] <- as.factor(data[[x]])
                    }
                }
                
                if(output_type == 'Summary table') {
                    file <- paste(title_key, 'rtf', sep = '.')
                    subj_pd_dose_summary_func <- c(
                        'N' = n_nna,
                        'Mean (SD)' = partial(mean_sd_str, digits = dgt),
                        '%CV' = partial(coeff_var_str, digits = dgt),
                        'Median' = partial(median_str, digits = dgt),
                        'Q1, Q3' = partial(q1_q3_str, digits = dgt),
                        'Min, Max' = partial(min_max_str, digits = dgt),
                        'Geom Mean' = partial(geo_mean_str, digits = dgt),
                        'Mean (SD) of LN' = partial(mean_sd_ln_str, digits = dgt)
                    )
                    if(is_blank(x)) {
                        data <- select_(data, pd_value1)
                        summary_tbl <- summary_table(
                            data, caption = table_title,
                            footnote = table_footnote,
                            func_list = subj_pd_dose_summary_func, format = 'rtf'
                        )
                    } else {
                        data <- select_(data, pd_value1, x)
                        data[[x]] <- factor(
                            data[[x]], levels = sort(unique(data[[x]]))
                        )
                        summary_tbl <- summary_table_col(
                            data, col_var = x,
                            col_names = paste(x, '=', levels(data[[x]])),
                            caption = table_title, footnote = table_footnote,
                            func_list = subj_pd_dose_summary_func, format = 'rtf'
                        )
                    }
                    rtf_table_wrapper(
                        file, summary_tbl, block_break = TRUE,
                        nline_block = length(subj_pd_dose_summary_func),
                        caption = table_title, footnote = table_footnote
                    )
                    progress$inc(1 / num_tfls, detail = file)
                    all_tfls$value <- c(all_tfls$value, file)
                } else if(output_type == 'Box plot') {
                    file <- paste(title_key, 'pdf', sep = '.')
                    
                    data <- data[!is.na(x), , drop = FALSE]
                    plot_ <- gg_boxplot(
                        data, x, pd_value1, group = group, log_y = log_y,
                        x_lab = x_lab, y_lab = left_y_lab, title = plot_title,
                        with_points = add_points
                    )
                    plot_ <- add_footnote(plot_, plot_footnote)
                    
                    height_width_ratio <- 3 / 4
                    width <- 8
                    height <- width * height_width_ratio
                    ggsave(filename = file, plot = plot_, width = width,
                           height = height, dpi = 600)
                    progress$inc(1 / num_tfls, detail = file)
                    all_tfls$value <- c(all_tfls$value, file)
                } else if(output_type == 'Line plot') {
                    file <- paste(title_key, 'pdf', sep = '.')
                    
                    data <- data[!is.na(x), , drop = FALSE]
                    plot_ <- gg_lineplot(
                        data, x, pd_value1, group = group, log_y = log_y,
                        x_lab = x_lab, y_lab = left_y_lab, title = plot_title,
                        summary_method = summary_method,
                        with_points = add_points
                    )
                    plot_ <- add_footnote(plot_, plot_footnote)
                    
                    height_width_ratio <- 3 / 4
                    width <- 8
                    height <- width * height_width_ratio
                    ggsave(filename = file, plot = plot_, width = width,
                           height = height, dpi = 600)
                    progress$inc(1 / num_tfls, detail = file)
                    all_tfls$value <- c(all_tfls$value, file)
                } else if(output_type == 'Scatter plot') {
                    file <- paste(title_key, 'pdf', sep = '.')
                    
                    if(!is_blank(group)) {
                        formula_ <- paste(
                            group, '+', subj_pd_subj_col, '~', subj_pd_param_col
                        )
                    } else {
                        formula_ <- paste(subj_pd_subj_col, '~', subj_pd_param_col)
                    }
                    data <- data.table::dcast(
                        data.table::as.data.table(data), formula_,
                        fun.aggregate = mean, na.rm = TRUE,
                        value.var = c(pd_value1, pd_value2)
                    )
                    if(pd_value1 == pd_value2) {
                        pd_var_1 <- pd_param1
                        pd_var_2 <- pd_param2
                    } else {
                        pd_var_1 <- paste(pd_value1, 'mean',
                                          pd_param1, sep = '_')
                        pd_var_2 <- paste(pd_value2, 'mean',
                                          pd_param2, sep = '_')
                    }
                    data <- data[!is.na(data[[pd_var_1]]) &
                                     !is.na(data[[pd_var_2]]), ]
                    if(!is_blank(group)) {
                        data[[group]] <- factor(data[[group]])
                    }
                    baseplot <- gg_wrapper(
                        data, aes_string(x = paste0('`', pd_var_1, '`'),
                                         y = paste0('`', pd_var_2, '`')),
                        log_x = log_x, log_y = log_y
                    )
                    if(!is_blank(group)) {
                        baseplot <- baseplot +
                            geom_point(aes_string(colour = group, fill = group),
                                       size = 2)
                    } else baseplot <- baseplot + geom_point(size = 2)
                    if(!is_blank(refline)) {
                        if('Loess' %in% refline)
                            baseplot <- baseplot + geom_smooth()
                        if('Linear regression' %in% refline)
                            baseplot <- baseplot + geom_smooth(method = 'lm')
                    }
                    baseplot <- baseplot +
                        labs(x = x_lab, y = left_y_lab, title = plot_title)
                    baseplot <- add_footnote(baseplot, plot_footnote)
                    
                    height_width_ratio <- 3 / 4
                    width <- 8
                    height <- width * height_width_ratio
                    ggsave(filename = file, plot = baseplot, width = width,
                           height = height, dpi = 600)
                    progress$inc(1 / num_tfls, detail = file)
                    all_tfls$value <- c(all_tfls$value, file)
                } else if(output_type == 'Forest plot') {
                    file <- paste(title_key, 'pdf', sep = '.')
                    height_width_ratio <- 3 / 4
                    width <- 8
                    height <- width * height_width_ratio
                    pdf(file = file, width = width, height = height)
                    formula_ <- paste(
                        x, '+', subj_pd_subj_col, '~', subj_pd_param_col
                    )
                    data <- data.table::dcast(
                        data.table::as.data.table(data), formula_,
                        fun.aggregate = mean, na.rm = TRUE,
                        value.var = c(pd_value1, pd_value2)
                    )
                    if(pd_value1 == pd_value2) {
                        pd_var_1 <- pd_param1
                        pd_var_2 <- pd_param2
                    } else {
                        pd_var_1 <- paste(pd_value1, 'mean',
                                          pd_param1, sep = '_')
                        pd_var_2 <- paste(pd_value2, 'mean',
                                          pd_param2, sep = '_')
                    }
                    if(x_type == 'Continuous') {
                        data[[x]] <- as.numeric(data[[x]])
                    } else if(x_type == 'Categorical') {
                        data[[x]] <- factor(data[[x]])
                    }
                    if(summary_method == 'Mean + SD') {
                        method <- 'mean_sd'
                        method_title <- 'Mean (SD)'
                    } else if(summary_method == 'Mean + SE') {
                        method <- 'mean_se'
                        method_title <- 'Mean (SE)'
                    } else if(summary_method == 'Median + IQR'){
                        method <- 'median_iqr'
                        method_title <- 'Median (Q1, Q3)'
                    }
                    forest_plot <- dual_y_axis_sumline(
                        data, x, pd_var_1, var_y2 = pd_var_2,
                        xlab = x_lab, ylab1 = left_y_lab, ylab2 = right_y_lab,
                        title = subj_pd_corr_forestplot_main$value,
                        footnote = plot_footnote,
                        method = method, type = 'p', same_y_axis = same_y,
                        save_plot = FALSE
                    )
                    dev.off()
                    progress$inc(1 / num_tfls, detail = file)
                    all_tfls$value <- c(all_tfls$value, file)
                } else if(output_type == '2D Forest plot') {
                    file <- paste(title_key, 'pdf', sep = '.')
                    
                    if(!is_blank(group)) {
                        formula_ <- paste(
                            group, '+', subj_pd_subj_col, '~', subj_pd_param_col
                        )
                    } else {
                        formula_ <- paste(subj_pd_subj_col, '~', subj_pd_param_col)
                    }
                    data <- data.table::dcast(
                        data.table::as.data.table(data), formula_,
                        fun.aggregate = mean, na.rm = TRUE,
                        value.var = c(pd_value1, pd_value2)
                    )
                    if(pd_value1 == pd_value2) {
                        pd_var_1 <- pd_param1
                        pd_var_2 <- pd_param2
                    } else {
                        pd_var_1 <- paste(pd_value1, 'mean',
                                          pd_param1, sep = '_')
                        pd_var_2 <- paste(pd_value2, 'mean',
                                          pd_param2, sep = '_')
                    }
                    data <- data[!is.na(data[[pd_var_1]]) &
                                     !is.na(data[[pd_var_2]]), ]
                    if(!is_blank(group)) {
                        data[[group]] <- factor(data[[group]])
                        data <- data %>% group_by_(group)
                    }
                    
                    if(summary_method %in% c('Mean + SD', 'Mean + SE')) {
                        avg_expr <- ~mean_na(var)
                        if(summary_method == 'Mean + SD') {
                            lower_expr <- ~mean_na(var) - sd_na(var)
                            upper_expr <- ~mean_na(var) + sd_na(var)
                        } else {
                            lower_expr <- ~mean_na(var) - std_err(var)
                            upper_expr <- ~mean_na(var) + std_err(var)
                        }
                    } else if(summary_method == 'Median + IQR') {
                        avg_expr <- ~median_na(var)
                        lower_expr <- ~q1_na(var)
                        upper_expr <- ~q3_na(var)
                    }
                    expr <- list(
                        lazyeval::interp(avg_expr,var=as.name(pd_var_1)),
                        lazyeval::interp(lower_expr,var=as.name(pd_var_1)),
                        lazyeval::interp(upper_expr,var=as.name(pd_var_1)),
                        lazyeval::interp(avg_expr,var=as.name(pd_var_2)),
                        lazyeval::interp(lower_expr,var=as.name(pd_var_2)),
                        lazyeval::interp(upper_expr,var=as.name(pd_var_2))
                    )
                    dots <- setNames(expr, c('avg_x', 'lower_x', 'upper_x',
                                             'avg_y', 'lower_y', 'upper_y'))
                    data <- data %>% summarise_(.dots = dots)
                    
                    width_x <- diff(range(data$upper_x, data$lower_x)) / 40
                    width_y <- diff(range(data$upper_y, data$lower_y)) / 40
                    baseplot <- gg_wrapper(data, aes(x = avg_x, y = avg_y))
                    if(!is_blank(group)) {
                        baseplot <- baseplot + aes_string(colour = group)
                    }
                    forest2d_plot <- baseplot + geom_point(size = 2) +
                        geom_errorbarh(aes(xmin = lower_x, xmax = upper_x),
                                       height = width_y) +
                        geom_errorbar(aes(ymin = lower_y, ymax = upper_y),
                                      width = width_x) +
                        labs(x = x_lab, y = left_y_lab, title = plot_title)
                    forest2d_plot <- add_footnote(forest2d_plot, plot_footnote)
                    
                    height_width_ratio <- 3 / 4
                    width <- 8
                    height <- width * height_width_ratio
                    ggsave(filename = file, plot = forest2d_plot, width = width,
                           height = height, dpi = 600)
                    progress$inc(1 / num_tfls, detail = file)
                    all_tfls$value <- c(all_tfls$value, file)
                }
                
            }
        }
        if(!is.null(out_tnf_data[['subj_pkpd']]) &&
           !is.null(data_$subj_pkpd)) {
            subj_pkpd_rows <- out_tnf_data[['subj_pkpd']]
            for(idx in seq_len(nrow(subj_pkpd_rows))) {
                irow <- subj_pkpd_rows[idx, , drop = FALSE]
                
                title_key <- irow[[out_subj_pkpd_title]]
                tflt <- irow[[out_subj_pkpd_tflt]]
                tfln <- irow[[out_subj_pkpd_tfln]]
                output_type <- irow[[out_subj_pkpd_output]]
                pk_param <- irow[[out_subj_pkpd_pk_param]]
                pd_param <- irow[[out_subj_pkpd_pd_param]]
                pd_value <- irow[[out_subj_pkpd_pd_value]]
                x <- irow[[out_subj_pkpd_x]]
                x_type <- irow[[out_subj_pkpd_x_type]]
                group <- irow[[out_subj_pkpd_group]]
                log_pk <- ifelse(
                    as.character(irow[[out_subj_pkpd_log_pk]]) %in% string_yes,
                    TRUE, FALSE
                )
                log_pd <- ifelse(
                    as.character(irow[[out_subj_pkpd_log_pd]]) %in% string_yes,
                    TRUE, FALSE
                )
                add_points <- ifelse(
                    as.character(irow[[out_subj_pkpd_add_points]]) %in% string_yes,
                    TRUE, FALSE
                )
                add_line <- ifelse(
                    as.character(irow[[out_subj_pkpd_add_line]]) %in% string_yes,
                    TRUE, FALSE
                )
                refline <- trimws(as.character(irow[[out_subj_pkpd_refline]]))
                summary_method <- trimws(irow[[out_subj_pkpd_summary]])
                same_y <- ifelse(
                    as.character(irow[[out_subj_pkpd_same_y]]) %in% string_yes,
                    TRUE, FALSE
                )
                x_lab <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_subj_pkpd_xlab]]
                )))
                left_y_lab <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_subj_pkpd_left_ylab]]
                )))
                right_y_lab <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_subj_pkpd_right_ylab]]
                )))
                plot_title <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_subj_pkpd_plot_title]]
                )))
                plot_title <- trimws(paste(tflt, tfln, ':', plot_title))
                plot_footnote <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_subj_pkpd_plot_footnote]]
                )))
                
                data <- data_$subj_pkpd
                
                if(output_type == 'Scatter plot') {
                    file <- paste(title_key, 'pdf', sep = '.')
                    
                    data <- data[
                        !is.na(data[[subj_pk_estm_col]]) &
                            !is.na(data[[pd_value]]), , drop = FALSE
                    ]
                    data <- data[data[[subj_pk_param_col]] %in% pk_param, ]
                    data <- data[data[[subj_pd_param_col]] %in% pd_param, ]
                    if(!is_blank(group)) {
                        data[[group]] <- factor(data[[group]])
                    }
                    baseplot <- gg_wrapper(
                        data, aes_string(x = subj_pk_estm_col, y = pd_value),
                        log_x = log_pk, log_y = log_pd
                    )
                    if(!is_blank(group)) {
                        baseplot <- baseplot +
                            geom_point(aes_string(colour = group, fill = group),
                                       size = 2)
                    } else baseplot <- baseplot + geom_point(size = 2)
                    baseplot <- baseplot +
                        labs(x = x_lab, y = left_y_lab, title = plot_title)
                    if(!is_blank(refline)) {
                        if('Loess' %in% refline)
                            baseplot <- baseplot + geom_smooth()
                        if('Linear regression' %in% refline)
                            baseplot <- baseplot + geom_smooth(method = 'lm')
                    }
                    baseplot <- add_footnote(baseplot, plot_footnote)
                    
                    height_width_ratio <- 3 / 4
                    width <- 8
                    height <- width * height_width_ratio
                    ggsave(filename = file, plot = baseplot, width = width,
                           height = height, dpi = 600)
                    progress$inc(1 / num_tfls, detail = file)
                    all_tfls$value <- c(all_tfls$value, file)
                } else if(output_type == 'Forest plot') {
                    file <- paste(title_key, 'pdf', sep = '.')
                    height_width_ratio <- 3 / 4
                    width <- 8
                    height <- width * height_width_ratio
                    pdf(file = file, width = width, height = height)
                    
                    cond_pk <- data[[subj_pk_param_col]] %in% pk_param
                    cond_pd <- data[[subj_pd_param_col]] %in% pd_param
                    data <- data[cond_pk & cond_pd, ]
                    data[[pk_param]] <- data[[subj_pk_estm_col]]
                    data[[pd_param]] <- data[[pd_value]]
                    if(x_type == 'Continuous') {
                        data[[x]] <- as.numeric(data[[x]])
                    } else if(x_type == 'Categorical') {
                        data[[x]] <- factor(data[[x]])
                    }
                    if(summary_method == 'Mean + SD') {
                        method <- 'mean_sd'
                        method_title <- 'Mean (SD)'
                    } else if(summary_method == 'Mean + SE') {
                        method <- 'mean_se'
                        method_title <- 'Mean (SE)'
                    } else if(summary_method == 'Median + IQR'){
                        method <- 'median_iqr'
                        method_title <- 'Median (Q1, Q3)'
                    }
                    title <- paste(method_title, 'of', pk_param,
                                   'and', pd_param, 'by', x)
                    forest_plot <- dual_y_axis_sumline(
                        data, x, pk_param, var_y2 = pd_param,
                        xlab = x_lab, ylab1 = left_y_lab, ylab2 = right_y_lab,
                        title = plot_title, footnote = plot_footnote,
                        method = method, type = 'p',
                        same_y_axis = same_y, save_plot = FALSE
                    )
                    dev.off()
                    
                    progress$inc(1 / num_tfls, detail = file)
                    all_tfls$value <- c(all_tfls$value, file)
                } else if(output_type == '2D Forest plot') {
                    file <- paste(title_key, 'pdf', sep = '.')
                    
                    data <- data[
                        !is.na(data[[subj_pk_estm_col]]) &
                            !is.na(data[[pd_value]]), , drop = FALSE
                    ]
                    cond_pk <- data[[subj_pk_param_col]] %in% pk_param
                    cond_pd <- data[[subj_pd_param_col]] %in% pd_param
                    data <- data[cond_pk & cond_pd, ]
                    data[[pk_param]] <- data[[subj_pk_estm_col]]
                    data[[pd_param]] <- data[[pd_value]]
                    data <- data[!is.na(data[[pk_param]]) &
                                     !is.na(data[[pd_param]]), , drop = FALSE]
                    if(!is_blank(group)) {
                        data[[group]] <- factor(data[[group]])
                        data <- data %>% group_by_(group)
                    }
                    
                    if(summary_method %in% c('Mean + SD', 'Mean + SE')) {
                        avg_expr <- ~mean_na(var)
                        if(summary_method == 'Mean + SD') {
                            lower_expr <- ~mean_na(var) - sd_na(var)
                            upper_expr <- ~mean_na(var) + sd_na(var)
                        } else {
                            lower_expr <- ~mean_na(var) - std_err(var)
                            upper_expr <- ~mean_na(var) + std_err(var)
                        }
                    } else if(summary_method == 'Median + IQR') {
                        avg_expr <- ~median_na(var)
                        lower_expr <- ~q1_na(var)
                        upper_expr <- ~q3_na(var)
                    }
                    expr <- list(
                        lazyeval::interp(avg_expr,var=as.name(pk_param)),
                        lazyeval::interp(lower_expr,var=as.name(pk_param)),
                        lazyeval::interp(upper_expr,var=as.name(pk_param)),
                        lazyeval::interp(avg_expr,var=as.name(pd_param)),
                        lazyeval::interp(lower_expr,var=as.name(pd_param)),
                        lazyeval::interp(upper_expr,var=as.name(pd_param))
                    )
                    dots <- setNames(expr, c('avg_x', 'lower_x', 'upper_x',
                                             'avg_y', 'lower_y', 'upper_y'))
                    data <- data %>% summarise_(.dots = dots)
                    
                    width_x <- diff(range(data$upper_x, data$lower_x)) / 40
                    width_y <- diff(range(data$upper_y, data$lower_y)) / 40
                    baseplot <- gg_wrapper(data, aes(x = avg_x, y = avg_y))
                    if(!is_blank(group)) {
                        baseplot <- baseplot + aes_string(colour = group)
                    }
                    forest2d_plot <- baseplot + geom_point(size = 2) +
                        geom_errorbarh(aes(xmin = lower_x, xmax = upper_x),
                                       height = width_y) +
                        geom_errorbar(aes(ymin = lower_y, ymax = upper_y),
                                      width = width_x) +
                        labs(x = x_lab,
                             y = left_y_lab,
                             title = plot_title)
                    forest2d_plot <- add_footnote(forest2d_plot, plot_footnote)
                    
                    height_width_ratio <- 3 / 4
                    width <- 8
                    height <- width * height_width_ratio
                    ggsave(filename = file, plot = forest2d_plot, width = width,
                           height = height, dpi = 600)
                    progress$inc(1 / num_tfls, detail = file)
                    all_tfls$value <- c(all_tfls$value, file)
                } else if(output_type == 'Quartile plot') {
                    file <- paste(title_key, 'pdf', sep = '.')
                    
                    data <- data[
                        !is.na(data[[subj_pk_estm_col]]) &
                            !is.na(data[[pd_value]]), , drop = FALSE
                    ]
                    cond_pk <- data[[subj_pk_param_col]] %in% pk_param
                    cond_pd <- data[[subj_pd_param_col]] %in% pd_param
                    data <- data[cond_pk & cond_pd, ]
                    data[[pk_param]] <- data[[subj_pk_estm_col]]
                    data[[pd_param]] <- data[[pd_value]]
                    data <- data[!is.na(data[[pk_param]]) &
                                     !is.na(data[[pd_param]]), ]
                    if(!is_blank(group)) {
                        data[[group]] <- factor(data[[group]])
                        # data <- data %>% group_by_(input$subj_pkpd_group)
                    }
                    pk_quartile_label <- paste0('Q', seq_len(4))
                    data[[pk_param]] <- factor(cut(
                        data[[pk_param]],
                        breaks = c(quantile(data[[pk_param]],
                                            probs = seq(0, 1, by = 0.25))),
                        labels = pk_quartile_label, include.lowest = TRUE
                    ))
                    
                    quartile_plot <- gg_boxplot(
                        data, pk_param, pd_param, group = group, log_y = log_pd,
                        x_lab = x_lab, y_lab = left_y_lab, title = plot_title,
                        with_points = add_points, with_line = add_line
                    )
                    quartile_plot <- add_footnote(quartile_plot, plot_footnote)
                    
                    height_width_ratio <- 3 / 4
                    width <- 8
                    height <- width * height_width_ratio
                    ggsave(filename = file, plot = quartile_plot, width = width,
                           height = height, dpi = 600)
                    progress$inc(1 / num_tfls, detail = file)
                    all_tfls$value <- c(all_tfls$value, file)
                }
            }
        }
        if(!is.null(out_tnf_data[['sample_pk']]) &&
           (!is.null(data_$sample_pk_param) || !is.null(data_$sample_pk_con))) {
            sample_pk_rows <- out_tnf_data[['sample_pk']]
            for(idx in seq_len(nrow(sample_pk_rows))) {
                irow <- sample_pk_rows[idx, , drop = FALSE]
                
                title_key <- irow[[out_sample_pk_title]]
                tflt <- irow[[out_sample_pk_tflt]]
                tfln <- irow[[out_sample_pk_tfln]]
                output_type <- irow[[out_sample_pk_output]]
                pk_type <- irow[[out_sample_pk_pk_type]]
                pk_param <- irow[[out_sample_pk_pk_param]]
                visit <- irow[[out_sample_pk_visit]]
                group <- irow[[out_sample_pk_group]]
                dgt <- as.numeric(as.character(irow[[out_sample_pk_decimal]]))
                table_title <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_sample_pk_table_title]]
                )))
                table_footnote <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_sample_pk_table_footnote]]
                )))
                statistics <- irow[[out_sample_pk_statistics]]
                subjid <- unlist(strsplit(gsub(
                    '\\\\n', '\n', as.character(irow[[out_sample_pk_subjid]])
                ), '\n'))
                log_y <- ifelse(
                    as.character(irow[[out_sample_pk_log_y]]) %in% string_yes,
                    TRUE, FALSE
                )
                x_lab <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_sample_pk_xlab]]
                )))
                y_lab <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_sample_pk_ylab]]
                )))
                plot_title <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_sample_pk_plot_title]]
                )))
                plot_title <- trimws(paste(tflt, tfln, ':', plot_title))
                plot_footnote <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_sample_pk_plot_footnote]]
                )))
                
                if(!is.null(data_$sample_pk_param)) data <- data_$sample_pk_param
                if(!is.null(data_$sample_pk_con)) data <- data_$sample_pk_con
                
                if(output_type == 'Summary table') {
                    file <- paste(title_key, 'rtf', sep = '.')
                    
                    if(pk_type == 'PK concentration') {
                        data <- data[
                            !is.na(data[[sample_pk_con_time_col]]) &
                                !is.na(data[[sample_pk_con_con_col]]), , drop = FALSE
                            ]
                        cond <- data[[sample_pk_param_visit_col]] %in% visit
                        data <- data[cond, , drop = FALSE]
                        if(!is_blank(group)) {
                            data[[group]] <- as.factor(data[[group]])
                        }
                        data[[sample_pk_con_con_col]] <- as.numeric(
                            data[[sample_pk_con_con_col]]
                        )
                        data[[sample_pk_param_visit_col]] <- as.numeric(
                            data[[sample_pk_param_visit_col]]
                        )
                        data[[sample_pk_con_time_col]] <- as.numeric(
                            data[[sample_pk_con_time_col]]
                        )
                        data[['time_']] <- paste0(
                            'Visit ', to_character(data[[sample_pk_param_visit_col]]),
                            ', ', 'Timepoint ',
                            to_character(data[[sample_pk_con_time_col]])
                        )
                        time_order <- order(data[[sample_pk_param_visit_col]],
                                            data[[sample_pk_con_time_col]])
                        data[['time_']] <- factor(
                            data[['time_']],
                            levels = unique(data[['time_']][time_order])
                        )
                        
                        sample_pk_con_summary_func <- c(
                            'N' = n_nna,
                            'Mean (SD)' = partial(mean_sd_str, digits = dgt),
                            '%CV' = partial(coeff_var_str, digits = dgt),
                            'Median' = partial(median_str, digits = dgt),
                            'Q1, Q3' = partial(q1_q3_str, digits = dgt),
                            'Min, Max' = partial(min_max_str, digits = dgt),
                            'Geom Mean' = partial(geo_mean_str, digits = dgt),
                            'Mean (SD) of LN' = partial(mean_sd_ln_str, digits = dgt)
                        )
                        if(!is_blank(group)) {
                            data <- select_(data, sample_pk_con_con_col,
                                            'time_', group)
                        } else {
                            data <- select_(data, sample_pk_con_con_col, 'time_')
                        }
                        summary_table <- summary_table_all(
                            data, row_var = 'time_',
                            col_var = group,
                            col_names = paste(group, '=', levels(data[[group]])),
                            val_var = sample_pk_con_con_col,
                            n_in_header = FALSE, func_list = sample_pk_con_summary_func,
                            caption = table_title, footnote = table_footnote,
                            rowlabel = ' ', format = 'rtf'
                        )
                        rtf_table_wrapper(
                            file, summary_table, block_break = TRUE,
                            nline_block = length(sample_pk_con_summary_func) + 1,
                            caption = table_title, footnote = table_footnote
                        )
                    } else if(pk_type == 'PK parameter') {
                        data <- data[
                            !is.na(data[[sample_pk_param_visit_col]]) &
                                !is.na(data[[sample_pk_param_estm_col]]), ,
                            drop = FALSE
                        ]
                        cond <- data[[sample_pk_param_param_col]] %in% pk_param
                        data <- data[cond, , drop = FALSE]
                        if(!is_blank(group)) {
                            data[[group]] <- factor(
                                data[[group]],
                                levels = sort(unique(data[[group]]))
                            )
                        }
                        data[[sample_pk_param_estm_col]] <- as.numeric(
                            data[[sample_pk_param_estm_col]]
                        )
                        data[[sample_pk_param_visit_col]] <- as.numeric(
                            data[[sample_pk_param_visit_col]]
                        )
                        data[[sample_pk_param_visit_col]] <- factor(
                            data[[sample_pk_param_visit_col]],
                            levels = sort(unique(data[[sample_pk_param_visit_col]]))
                        )
                        
                        sample_pk_param_summary_func <- c(
                            'N' = n_nna,
                            'Mean (SD)' = partial(mean_sd_str, digits = dgt),
                            '%CV' = partial(coeff_var_str, digits = dgt),
                            'Median' = partial(median_str, digits = dgt),
                            'Q1, Q3' = partial(q1_q3_str, digits = dgt),
                            'Min, Max' = partial(min_max_str, digits = dgt),
                            'Geom Mean' = partial(geo_mean_str, digits = dgt),
                            'Mean (SD) of LN' = partial(mean_sd_ln_str, digits = dgt)
                        )
                        if(!is_blank(group)) {
                            data <- select_(data, sample_pk_param_estm_col,
                                            sample_pk_param_visit_col,
                                            group)
                        } else {
                            data <- select_(data, sample_pk_param_estm_col,
                                            sample_pk_param_visit_col)
                        }
                        
                        summary_table <- summary_table_all(
                            data, row_var = sample_pk_param_visit_col,
                            row_names = paste('Visit',
                                              levels(data[[sample_pk_param_visit_col]])),
                            col_var = group,
                            col_names = paste(group, '=', levels(data[[group]])),
                            val_var = sample_pk_param_estm_col, n_in_header = FALSE,
                            func_list = sample_pk_param_summary_func,
                            caption = table_title, footnote = table_footnote,
                            rowlabel = ' ', format = 'rtf'
                        )
                        rtf_table_wrapper(
                            file, summary_table, block_break = TRUE,
                            nline_block = length(sample_pk_con_summary_func) + 1,
                            caption = table_title, footnote = table_footnote
                        )
                    }
                    
                    progress$inc(1 / num_tfls, detail = file)
                    all_tfls$value <- c(all_tfls$value, file)
                } else if(output_type == 'Summary line') {
                    file <- paste(title_key, 'pdf', sep = '.')
                    
                    if(pk_type == 'PK concentration') {
                        data <- data[
                            !is.na(data[[sample_pk_con_time_col]]) &
                                !is.na(data[[sample_pk_con_con_col]]), ,
                            drop = FALSE
                        ]
                        cond <- data[[sample_pk_param_visit_col]]%in%visit
                        data <- data[cond, , drop = FALSE]
                        
                        if(!is_blank(group)) {
                            data[[group]] <- as.factor(data[[group]])
                        }
                        data[[sample_pk_con_con_col]] <- as.numeric(
                            data[[sample_pk_con_con_col]]
                        )
                        if(length(visit) > 1) {
                            data[[sample_pk_param_visit_col]] <- as.factor(
                                data[[sample_pk_param_visit_col]]
                            )
                            levels(data[[sample_pk_param_visit_col]]) <- paste(
                                sample_pk_param_visit_col, '=',
                                levels(data[[sample_pk_param_visit_col]])
                            )
                        }
                            if(!is_blank(group)) {
                            data <- group_by_(data, sample_pk_param_visit_col,
                                              group, sample_pk_con_time_col)
                        } else {
                            data <- group_by_(data, sample_pk_param_visit_col,
                                              sample_pk_con_time_col)
                        }
                        
                        if(statistics %in% c('Mean + SD', 'Mean + SE')) {
                            avg_expr <- ~mean_na(var)
                            if(statistics == 'Mean + SD') {
                                lower_expr <- ~mean_na(var) - sd_na(var)
                                upper_expr <- ~mean_na(var) + sd_na(var)
                            } else {
                                lower_expr <- ~mean_na(var) - std_err(var)
                                upper_expr <- ~mean_na(var) + std_err(var)
                            }
                        } else if(statistics == 'Median + IQR') {
                            avg_expr <- ~median_na(var)
                            lower_expr <- ~q1_na(var)
                            upper_expr <- ~q3_na(var)
                        }
                        expr <- list(
                            lazyeval::interp(
                                avg_expr, var = as.name(sample_pk_con_con_col)
                            ),
                            lazyeval::interp(
                                lower_expr, var = as.name(sample_pk_con_con_col)
                            ),
                            lazyeval::interp(
                                upper_expr, var = as.name(sample_pk_con_con_col)
                            )
                        )
                        dots <- setNames(expr, c('avg', 'lower', 'upper'))
                        data <- data %>% summarise_(.dots = dots)
                        data[[sample_pk_con_time_col]] <- as.numeric(
                            data[[sample_pk_con_time_col]]
                        )
                        
                        timepoint <- sort(unique(data[[sample_pk_con_time_col]]))
                        ngroups <- ifelse(is_blank(group), 1,
                                          length(unique(data[[group]])))
                        dodge_width <- min(
                            diff(range(timepoint)) / 40,
                            min(diff(timepoint)) / (3 * (ngroups - 1))
                        )
                        plt <- gg_wrapper(
                            data, aes_string(x = sample_pk_con_time_col, y = 'avg'),
                            log_y = log_y
                        )
                        if(length(visit) > 1) {
                            plt <- plt +
                                facet_grid(formula(paste(sample_pk_param_visit_col,
                                                         '~ .')))
                        }
                            if(!is_blank(group)) {
                                plt <- plt + aes_string(colour = group)
                        }
                        plt <- plt +
                            geom_line(position = position_dodge(dodge_width)) +
                            geom_point(position = position_dodge(dodge_width),
                                       size = 2) +
                            labs(x = x_lab, y = y_lab, title = plot_title)
                        
                        if(any(!is.na(data$lower) & !is.na(data$upper))) {
                            plt <- plt + geom_errorbar(
                                aes(ymin = lower, ymax = upper),
                                width = dodge_width,
                                position = position_dodge(dodge_width)
                            )
                        }
                        plt <- add_footnote(plt, plot_footnote)
                    } else if(pk_type == 'PK parameter') {
                        data <- data[
                            !is.na(data[[sample_pk_param_visit_col]]) &
                                !is.na(data[[sample_pk_param_estm_col]]), ,
                            drop = FALSE
                        ]
                        data <- data[
                            data[[sample_pk_param_param_col]] %in% pk_param,
                            , drop = F
                        ]
                        data[[sample_pk_param_estm_col]] <- as.numeric(
                            data[[sample_pk_param_estm_col]]
                        )
                        if(!is_blank(group)) {
                            data[[group]] <- as.factor(data[[group]])
                            data <- group_by_(data, group,
                                              sample_pk_param_visit_col)
                        } else {
                            data <- group_by_(data, sample_pk_param_visit_col)
                        }
                        
                        if(statistics %in% c('Mean + SD', 'Mean + SE')) {
                            avg_expr <- ~mean_na(var)
                            if(statistics == 'Mean + SD') {
                                lower_expr <- ~mean_na(var) - sd_na(var)
                                upper_expr <- ~mean_na(var) + sd_na(var)
                            } else {
                                lower_expr <- ~mean_na(var) - std_err(var)
                                upper_expr <- ~mean_na(var) + std_err(var)
                            }
                        } else if(statistics == 'Median + IQR') {
                            avg_expr <- ~median_na(var)
                            lower_expr <- ~q1_na(var)
                            upper_expr <- ~q3_na(var)
                        }
                        expr <- list(
                            lazyeval::interp(
                                avg_expr, var = as.name(sample_pk_param_estm_col)
                            ),
                            lazyeval::interp(
                                lower_expr, var = as.name(sample_pk_param_estm_col)
                            ),
                            lazyeval::interp(
                                upper_expr, var = as.name(sample_pk_param_estm_col)
                            )
                        )
                        dots <- setNames(expr, c('avg', 'lower', 'upper'))
                        data <- data %>% summarise_(.dots = dots)
                        data[[sample_pk_param_visit_col]] <- as.numeric(
                            data[[sample_pk_param_visit_col]]
                        )
                        
                        title_ <- paste(
                            summary_title_dict[[statistics]],
                            'PK parameter time profiles'
                        )
                        visit_day <- sort(unique(data[[sample_pk_param_visit_col]]))
                        ngroups <- ifelse(is_blank(group), 1,
                                          length(unique(data[[group]])))
                        dodge_width <- min(
                            diff(range(visit_day)) / 40,
                            min(diff(visit_day)) / (3 * (ngroups - 1))
                        )
                        plt <- gg_wrapper(
                            data, aes_string(x = sample_pk_param_visit_col, y = 'avg'),
                            log_y = log_y
                        )
                        if(!is_blank(group)) {
                            title_ <- paste(title_, 'by', group)
                            plt <- plt + aes_string(colour = group,shape = group)
                        }
                        plt <- plt +
                            geom_line(position = position_dodge(dodge_width)) +
                            geom_point(position = position_dodge(dodge_width),
                                       size = 2) +
                            scale_x_continuous(breaks = visit_day,
                                               labels = visit_day) +
                            labs(x = x_lab, y = y_lab, title = plot_title)
                        
                        if(any(!is.na(data$lower) & !is.na(data$upper))) {
                            plt <- plt + geom_errorbar(
                                aes(ymin = lower, ymax = upper),
                                width = dodge_width,
                                position = position_dodge(dodge_width)
                            )
                        }
                        plt <- add_footnote(plt, plot_footnote)
                    }
                    
                    height_width_ratio <- 3 / 4
                    width <- 8
                    height <- width * height_width_ratio
                    ggsave(filename = file, plot = plt, width = width,
                           height = height, dpi = 600)
                    progress$inc(1 / num_tfls, detail = file)
                    all_tfls$value <- c(all_tfls$value, file)
                } else if(output_type == 'Individual line') {
                    file <- paste(title_key, 'pdf', sep = '.')
                    
                    if(pk_type == 'PK concentration') {
                        data <- data[
                            !is.na(data[[sample_pk_con_time_col]]) &
                                !is.na(data[[sample_pk_con_con_col]]), ,
                            drop = FALSE
                        ]
                        data <- data[
                            data[[sample_pk_param_visit_col]] %in% visit, ,
                            drop = FALSE
                        ]
                        data <- data[
                            data[[sample_pk_con_subj_col]] %in% subjid, ,
                            drop = F
                        ]
                        data[[sample_pk_con_con_col]] <- as.numeric(
                            data[[sample_pk_con_con_col]]
                        )
                        data[[sample_pk_con_time_col]] <- as.numeric(
                            data[[sample_pk_con_time_col]]
                        )
                        data[[sample_pk_con_subj_col]] <- trimws(as.character(
                            data[[sample_pk_con_subj_col]]
                        ))
                        
                        if(!is_blank(group)) {
                            data[[group]] <- factor(
                                data[[group]],
                                levels = sort(unique(data[[group]]))
                            )
                            data <- arrange_(data, group,
                                             sample_pk_con_subj_col,
                                             sample_pk_param_visit_col)
                        } else {
                            data <- arrange_(data, sample_pk_con_subj_col,
                                             sample_pk_param_visit_col)
                        }
                        
                        data[[sample_pk_con_subj_col]] <- factor(
                            data[[sample_pk_con_subj_col]],
                            levels = unique(data[[sample_pk_con_subj_col]])
                        )
                        if(length(visit) > 1) {
                            data[[sample_pk_param_visit_col]] <- as.factor(
                                data[[sample_pk_param_visit_col]]
                            )
                            levels(data[[sample_pk_param_visit_col]]) <- paste(
                                sample_pk_param_visit_col, '=',
                                levels(data[[sample_pk_param_visit_col]])
                            )
                        }
                        
                        nsubj <- length(levels(data[[sample_pk_con_subj_col]]))
                        shape_value <- seq_len(nsubj)
                        plt <- gg_wrapper(
                            data, aes_string(x = sample_pk_con_time_col,
                                             y = sample_pk_con_con_col,
                                             group = sample_pk_con_subj_col),
                            log_y = log_y
                        )
                        if(!is_blank(group)) {
                            nsubj_group <- sapply(by(
                                data, data[[group]],
                                function(df) {unique(df[[sample_pk_con_subj_col]])}
                            ), length)
                            shape_value <- sequence(nsubj_group)
                            ngroups <- length(nsubj_group)
                            all_colors <- gg_color_hue(ngroups)
                            color_value <- all_colors[rep.int(
                                1:ngroups, times = nsubj_group
                            )]
                            plt <- plt + aes_string(
                                colour = sample_pk_con_subj_col, fill = group
                            ) +
                                scale_colour_manual(
                                    name = sample_pk_con_subj_col,
                                    values = color_value
                                ) +
                                scale_fill_manual(
                                    name = group,
                                    labels = levels(data[[group]]),
                                    values = all_colors,
                                    guide = guide_legend(
                                        override.aes = list(colour = all_colors),
                                        order = 2
                                    )
                                )
                        }
                        if(length(visit) > 1) {
                            plt <- plt + 
                                facet_grid(formula(
                                    paste(sample_pk_param_visit_col, '~ .')
                                ))
                        }
                        plt <- plt +
                            stat_summary(fun.y = mean_na, geom = 'line') +
                            stat_summary(aes_string(shape = sample_pk_con_subj_col),
                                         fun.y = mean_na, geom = 'point') +
                            scale_shape_manual(
                                name = sample_pk_con_subj_col,
                                values = unname(shape_value)
                            ) +
                            labs(x = x_lab, y = y_lab, title = plot_title)
                        plt <- add_footnote(plt, plot_footnote)
                    } else if(pk_type == 'PK parameter') {
                        data <- data[
                            !is.na(data[[sample_pk_param_visit_col]]) &
                                !is.na(data[[sample_pk_param_estm_col]]), ,
                            drop = FALSE
                        ]
                        data <- data[
                            data[[sample_pk_param_param_col]] %in% pk_param,
                            , drop = F
                        ]
                        data <- data[
                            data[[sample_pk_param_subj_col]] %in% subjid, ,
                            drop = F
                        ]
                        data[[sample_pk_param_estm_col]] <- as.numeric(
                            data[[sample_pk_param_estm_col]]
                        )
                        data[[sample_pk_param_visit_col]] <- as.numeric(
                            data[[sample_pk_param_visit_col]]
                        )
                        data[[sample_pk_param_subj_col]] <- trimws(as.character(
                            data[[sample_pk_param_subj_col]]
                        ))
                        
                        if(!is_blank(group)) {
                            data[[group]] <- factor(
                                data[[group]],
                                levels = sort(unique(data[[group]]))
                            )
                            data <- arrange_(data, group,
                                             sample_pk_param_subj_col)
                        } else {
                            data <- arrange_(data, sample_pk_param_subj_col)
                        }
                        
                        data[[sample_pk_param_subj_col]] <- factor(
                            data[[sample_pk_param_subj_col]],
                            levels = unique(data[[sample_pk_param_subj_col]])
                        )
                        
                        visit_day <- sort(unique(data[[sample_pk_param_visit_col]]))
                        nsubj <- length(levels(data[[sample_pk_param_subj_col]]))
                        shape_value <- seq_len(nsubj)
                        plt <- gg_wrapper(
                            data, aes_string(x = sample_pk_param_visit_col,
                                             y = sample_pk_param_estm_col,
                                             group = sample_pk_param_subj_col),
                            log_y = input$sample_pk_param_log
                        )
                        if(!is_blank(group)) {
                            nsubj_group <- sapply(by(
                                data, data[[group]],
                                function(df) {unique(df[[sample_pk_param_subj_col]])}
                            ), length)
                            shape_value <- unname(sequence(nsubj_group))
                            ngroups <- length(nsubj_group)
                            all_colors <- gg_color_hue(ngroups)
                            color_value <- all_colors[rep.int(
                                1:ngroups, times = nsubj_group
                            )]
                            
                            plt <- plt + aes_string(colour = sample_pk_param_subj_col,
                                                    fill = group) +
                                scale_colour_manual(
                                    name = sample_pk_param_subj_col,
                                    labels = levels(data[[sample_pk_param_subj_col]]),
                                    values = color_value
                                ) +
                                scale_fill_manual(
                                    name = group,
                                    labels = levels(data[[group]]),
                                    values = all_colors,
                                    guide = guide_legend(
                                        override.aes = list(colour = all_colors),
                                        order = 2
                                    )
                                )
                        }
                        plt <- plt +
                            stat_summary(fun.y = mean_na, geom = 'line') +
                            stat_summary(aes_string(shape = sample_pk_param_subj_col),
                                         fun.y = mean_na, geom = 'point') +
                            scale_x_continuous(breaks = visit_day,
                                               labels = visit_day) +
                            scale_shape_manual(
                                name = sample_pk_param_subj_col,
                                labels = levels(data[[sample_pk_param_subj_col]]),
                                values = shape_value
                            ) +
                            labs(x = x_lab, y = y_lab, title = plot_title)
                        plt <- add_footnote(plt, plot_footnote)
                    }
                    
                    height_width_ratio <- 3 / 4
                    width <- 8
                    height <- width * height_width_ratio
                    ggsave(filename = file, plot = plt, width = width,
                           height = height, dpi = 600)
                    progress$inc(1 / num_tfls, detail = file)
                    all_tfls$value <- c(all_tfls$value, file)
                }
                
            }
        }
        if(!is.null(out_tnf_data[['sample_pd']]) && !is.null(data_$sample_pd)) {
            sample_pd_rows <- out_tnf_data[['sample_pd']]
            for(idx in seq_len(nrow(sample_pd_rows))) {
                irow <- sample_pd_rows[idx, , drop = FALSE]
                
                title_key <- irow[[out_sample_pd_title]]
                tflt <- irow[[out_sample_pd_tflt]]
                tfln <- irow[[out_sample_pd_tfln]]
                output_type <- irow[[out_sample_pd_output]]
                pd_param1 <- irow[[out_sample_pd_param_1]]
                pd_value1 <- irow[[out_sample_pd_value_1]]
                pd_param2 <- irow[[out_sample_pd_param_2]]
                pd_value2 <- irow[[out_sample_pd_value_2]]
                x <- irow[[out_sample_pd_x]]
                visit <- irow[[out_sample_pd_visit]]
                group <- irow[[out_sample_pd_group]]
                dgt <- as.numeric(as.character(irow[[out_sample_pd_decimal]]))
                table_title <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_sample_pd_table_title]]
                )))
                table_footnote <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_sample_pd_table_footnote]]
                )))
                statistics <- irow[[out_sample_pd_statistics]]
                subjid <- unlist(strsplit(gsub(
                    '\\\\n', '\n', as.character(irow[[out_sample_pd_subjid]])
                ), '\n'))
                refline <- irow[[out_sample_pd_refline]]
                log_pd1 <- ifelse(
                    as.character(irow[[out_sample_pd_log_pd_1]]) %in% string_yes,
                    TRUE, FALSE
                )
                log_pd2 <- ifelse(
                    as.character(irow[[out_sample_pd_log_pd_2]]) %in% string_yes,
                    TRUE, FALSE
                )
                same_y <- ifelse(
                    as.character(irow[[out_sample_pd_same_y]]) %in% string_yes,
                    TRUE, FALSE
                )
                x_lab <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_sample_pd_xlab]]
                )))
                left_y_lab <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_sample_pd_left_y_lab]]
                )))
                right_y_lab <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_sample_pd_right_y_lab]]
                )))
                plot_title <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_sample_pd_plot_title]]
                )))
                plot_title <- trimws(paste(tflt, tfln, ':', plot_title))
                plot_footnote <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_sample_pd_plot_footnote]]
                )))
                
                data <- data_$sample_pd
                
                if(output_type == 'Summary table') {
                    file <- paste(title_key, 'rtf', sep = '.')
                    
                    is_visit <- x == sample_pd_avisitn_col
                    x_var <- ifelse(is_visit, sample_pd_avisitn_col,
                                    sample_pd_atptn_col)
                    data <- data[
                        !is.na(data[[x_var]]) & !is.na(data[[pd_value1]]), ,
                        drop = FALSE
                    ]
                    data <- data[
                        data[[sample_pd_param_col]] %in% pd_param1, , drop = F
                    ]
                    if(!is_visit) {
                        data <- data[
                            data[[sample_pd_avisitn_col]] %in%  visit, , drop = F
                        ]
                    }
                    
                    if(!is_blank(group)) {
                        data[[group]] <- factor(
                            data[[group]], levels = sort(unique(data[[group]]))
                        )
                    }
                    data[[pd_value1]] <- as.numeric(data[[pd_value1]])
                    data[[sample_pd_avisitn_col]] <- as.numeric(
                        data[[sample_pd_avisitn_col]]
                    )
                    sample_pd_time_summary_func <- c(
                        'N' = n_nna,
                        'Mean (SD)' = partial(mean_sd_str, digits = dgt),
                        '%CV' = partial(coeff_var_str, digits = dgt),
                        'Median' = partial(median_str, digits = dgt),
                        'Q1, Q3' = partial(q1_q3_str, digits = dgt),
                        'Min, Max' = partial(min_max_str, digits = dgt),
                        'Geom Mean' = partial(geo_mean_str, digits = dgt),
                        'Mean (SD) of LN' = partial(mean_sd_ln_str, digits = dgt)
                    )
                    
                    if(is_visit) {
                        data[[sample_pd_avisitn_col]] <- factor(
                            data[[sample_pd_avisitn_col]],
                            levels = sort(unique(data[[sample_pd_avisitn_col]]))
                        )
                        if(!is_blank(group)) {
                            data <- select_(data, pd_value1,
                                            sample_pd_avisitn_col, group)
                        } else {
                            data <- select_(data, pd_value1,
                                            sample_pd_avisitn_col)
                        }
                        summary_table <- summary_table_all(
                            data, row_var = sample_pd_avisitn_col,
                            row_names = paste(
                                'Visit', levels(data[[sample_pd_avisitn_col]])
                            ),
                            col_var = group,
                            col_names = paste(group, '=', levels(data[[group]])),
                            val_var = pd_value1, n_in_header = FALSE,
                            func_list = sample_pd_time_summary_func,
                            caption = table_title, footnote = table_footnote,
                            rowlabel = ' ', format = 'rtf'
                        )
                        rtf_table_wrapper(
                            file, summary_table, block_break = TRUE,
                            nline_block = length(sample_pd_time_summary_func) + 1,
                            caption = table_title, footnote = table_footnote
                        )
                    } else {
                        cond <- data[[sample_pd_avisitn_col]] %in% visit
                        data <- data[cond, , drop = FALSE]
                        
                        data[[sample_pd_atptn_col]] <- as.numeric(
                            data[[sample_pd_atptn_col]]
                        )
                        data[['time_']] <- paste0(
                            'Visit ', to_character(data[[sample_pd_avisitn_col]]),
                            ', ',
                            'Timepoint', to_character(data[[sample_pd_atptn_col]])
                        )
                        order_time <- order(data[[sample_pd_avisitn_col]],
                                            data[[sample_pd_atptn_col]])
                        data[['time_']] <- factor(
                            data[['time_']],
                            levels = unique(data[['time_']][order_time])
                        )
                        
                        if(!is_blank(group)) {
                            data <- select_(data, pd_value1, 'time_', group)
                        } else {
                            data <- select_(data, pd_value1, 'time_')
                        }
                        summary_table <- summary_table_all(
                            data, row_var = 'time_',
                            row_names = levels(data[['time_']]), col_var = group,
                            col_names = paste(group, '=', levels(data[[group]])),
                            val_var = pd_value1, n_in_header = FALSE,
                            func_list = sample_pd_time_summary_func,
                            caption = table_title, footnote = table_footnote,
                            rowlabel = ' ', format = 'rtf'
                        )
                        rtf_table_wrapper(
                            file, summary_table, block_break = TRUE,
                            nline_block = length(sample_pd_time_summary_func) + 1,
                            caption = table_title, footnote = table_footnote
                        )
                    }
                    
                    progress$inc(1 / num_tfls, detail = file)
                    all_tfls$value <- c(all_tfls$value, file)
                } else if(output_type == 'Summary line') {
                    file <- paste(title_key, 'pdf', sep = '.')
                    
                    is_visit <- x == sample_pd_avisitn_col
                    x_var <- ifelse(is_visit, sample_pd_avisitn_col,
                                    sample_pd_atptn_col)
                    data <- data[
                        !is.na(data[[x_var]]) & !is.na(data[[pd_value1]]), ,
                        drop = FALSE
                    ]
                    data[[pd_value1]] <- as.numeric(data[[pd_value1]])
                    data <- data[
                        data[[sample_pd_param_col]] %in% pd_param1, , drop = F
                    ]
                    if(!is_visit) {
                        data <- data[
                            data[[sample_pd_avisitn_col]] %in% visit, , drop = F
                        ]
                    }
                    if(!is_blank(group)) {
                        data[[group]] <- as.factor(data[[group]])
                        if(!is_visit) {
                            data <- group_by_(data, sample_pd_avisitn_col,
                                              group, sample_pd_atptn_col)
                        } else {
                            data <- group_by_(data, group,
                                              sample_pd_avisitn_col)
                        }
                    } else {
                        if(!is_visit) {
                            data <- group_by_(data, sample_pd_avisitn_col,
                                              sample_pd_atptn_col)
                        } else {
                            data <- group_by_(data, sample_pd_avisitn_col)
                        }
                    }
                    
                    if(statistics %in% c('Mean + SD', 'Mean + SE')) {
                        avg_expr <- ~mean_na(var)
                        if(statistics == 'Mean + SD') {
                            lower_expr <- ~mean_na(var) - sd_na(var)
                            upper_expr <- ~mean_na(var) + sd_na(var)
                        } else {
                            lower_expr <- ~mean_na(var) - std_err(var)
                            upper_expr <- ~mean_na(var) + std_err(var)
                        }
                    } else if(statistics == 'Median + IQR') {
                        avg_expr <- ~median_na(var)
                        lower_expr <- ~q1_na(var)
                        upper_expr <- ~q3_na(var)
                    }
                    expr <- list(
                        lazyeval::interp(avg_expr, var = as.name(pd_value1)),
                        lazyeval::interp(lower_expr, var = as.name(pd_value1)),
                        lazyeval::interp(upper_expr, var = as.name(pd_value1))
                    )
                    dots <- setNames(expr, c('avg', 'lower', 'upper'))
                    data <- data %>% summarise_(.dots = dots)
                    
                    if(!is_visit) {
                        data[[sample_pd_avisitn_col]] <- as.factor(
                            data[[sample_pd_avisitn_col]]
                        )
                        levels(data[[sample_pd_avisitn_col]]) <- paste(
                            'Visit =', levels(data[[sample_pd_avisitn_col]])
                        )
                        x_var <- sample_pd_atptn_col
                        data[[sample_pd_atptn_col]] <- as.numeric(
                            data[[sample_pd_atptn_col]]
                        )
                        visits <- sort(unique(data[[sample_pd_atptn_col]]))
                    } else {
                        x_var <- sample_pd_avisitn_col
                        data[[sample_pd_avisitn_col]] <- as.numeric(
                            data[[sample_pd_avisitn_col]]
                        )
                        visits <- sort(unique(data[[sample_pd_avisitn_col]]))
                    }
                    ngroups <- ifelse(is_blank(group), 1,
                                      length(unique(data[[group]])))
                    dodge_width <- min(
                        diff(range(visits)) / 40,
                        min(diff(visits)) / (ngroups - 1)
                    )
                    plt <- gg_wrapper(
                        data, aes_string(x = x_var, y = 'avg'), log_y = log_pd1
                    )
                    if(!is_blank(group)) {
                        plt <- plt + aes_string(colour = group, shape = group)
                    }
                    if(!is_visit && length(visit) > 1) {
                        plt <- plt + facet_grid(formula(paste(
                            sample_pd_avisitn_col, '~ .'
                        )))
                    }
                    plt <- plt +
                        geom_line(position = position_dodge(dodge_width)) +
                        geom_point(position = position_dodge(dodge_width),
                                   size = 2) +
                        scale_x_continuous(breaks = visits, labels = visits) +
                        labs(x = x_lab, y = left_y_lab, title = plot_title)
                    if(any(!is.na(data$lower) & !is.na(data$upper))) {
                        plt <- plt + geom_errorbar(
                            aes(ymin = lower, ymax = upper),
                            width = dodge_width,
                            position = position_dodge(dodge_width)
                        )
                    }
                    plt <- add_footnote(plt, plot_footnote)
                    
                    height_width_ratio <- 3 / 4
                    width <- 8
                    height <- width * height_width_ratio
                    ggsave(filename = file, plot = plt, width = width,
                           height = height, dpi = 600)
                    progress$inc(1 / num_tfls, detail = file)
                    all_tfls$value <- c(all_tfls$value, file)
                } else if(output_type == 'Individual line') {
                    file <- paste(title_key, 'pdf', sep = '.')
                    
                    is_visit <- x == sample_pd_avisitn_col
                    x_var <- ifelse(is_visit, sample_pd_avisitn_col,
                                    sample_pd_atptn_col)
                    data <- data[
                        !is.na(data[[x_var]]) & !is.na(data[[pd_value1]]), ,
                        drop = FALSE
                        ]
                    data <- data[
                        data[[sample_pd_param_col]] %in% pd_param1, , drop = F
                    ]
                    if(!is_visit) {
                        data <- data[
                            data[[sample_pd_avisitn_col]] %in%  visit, , drop = F
                        ]
                    }
                    data <- data[
                        data[[sample_pd_subj_col]] %in% subjid, , drop = FALSE
                    ]
                    
                    data[[pd_value1]] <- as.numeric(data[[pd_value1]])
                    data[[sample_pd_avisitn_col]] <- as.numeric(
                        data[[sample_pd_avisitn_col]]
                    )
                    data[[sample_pd_subj_col]] <- trimws(as.character(
                        data[[sample_pd_subj_col]]
                    ))
                    
                    if(!is_blank(group)) {
                        data[[group]] <- factor(
                            data[[group]], levels = sort(unique(data[[group]]))
                        )
                        if(!is_visit) {
                            data <- arrange_(data, group,
                                             sample_pd_subj_col,
                                             sample_pd_avisitn_col,
                                             sample_pd_atptn_col)
                        } else {
                            data <- arrange_(data, group,
                                             sample_pd_subj_col,
                                             sample_pd_avisitn_col)
                        }
                    } else {
                        if(!is_visit) {
                            data <- arrange_(data, sample_pd_subj_col,
                                             sample_pd_avisitn_col,
                                             sample_pd_atptn_col)
                        } else {
                            data <- arrange_(data, sample_pd_subj_col,
                                             sample_pd_avisitn_col)
                        }
                    }
                    
                    data[[sample_pd_subj_col]] <- factor(
                        data[[sample_pd_subj_col]],
                        levels = unique(data[[sample_pd_subj_col]])
                    )
                    
                    if(!is_visit) {
                        data[[sample_pd_avisitn_col]] <- factor(
                            data[[sample_pd_avisitn_col]]
                        )
                        levels(data[[sample_pd_avisitn_col]]) <- paste(
                            'Visit =', levels(data[[sample_pd_avisitn_col]])
                        )
                        data[[sample_pd_atptn_col]] <- as.numeric(
                            data[[sample_pd_atptn_col]]
                        )
                        visits <- sort(unique(data[[sample_pd_atptn_col]]))
                    } else {
                        data[[sample_pd_avisitn_col]] <- as.numeric(
                            data[[sample_pd_avisitn_col]]
                        )
                        visits <- sort(unique(data[[sample_pd_avisitn_col]]))
                    }
                    
                    nsubj <- length(levels(data[[sample_pd_subj_col]]))
                    shape_value <- seq_len(nsubj)
                    plt <- gg_wrapper(
                        data, aes_string(x = x_var, y = pd_value1,
                                         group = sample_pd_subj_col),
                        log_y = log_pd1
                    )
                    if(!is_blank(group)) {
                        nsubj_group <- sapply(by(
                            data, data[[group]],
                            function(df) {unique(df[[sample_pd_subj_col]])}
                        ), length)
                        shape_value <- unname(sequence(nsubj_group))
                        ngroups <- length(nsubj_group)
                        all_colors <- gg_color_hue(ngroups)
                        color_value <- all_colors[rep.int(
                            1:ngroups, times = nsubj_group
                        )]
                        
                        plt <- plt + aes_string(colour = sample_pd_subj_col,
                                                fill = group) +
                            scale_colour_manual(
                                name = sample_pd_subj_col,
                                labels = levels(data[[sample_pd_subj_col]]),
                                values = color_value
                            ) +
                            scale_fill_manual(
                                name = group,
                                labels = levels(data[[group]]),
                                values = all_colors,
                                guide = guide_legend(
                                    override.aes = list(colour = all_colors),
                                    order = 2
                                )
                            )
                    }
                    if((!is_visit) && length(visit) > 1) {
                        plt <- plt + facet_grid(formula(paste(
                            sample_pd_avisitn_col, '~ .'
                        )))
                    }
                    plt <- plt +
                        stat_summary(fun.y = mean_na, geom = 'line') +
                        stat_summary(aes_string(shape = sample_pd_subj_col),
                                     fun.y = mean_na, geom = 'point') +
                        scale_x_continuous(breaks = visits, labels = visits) +
                        scale_shape_manual(
                            name = sample_pd_subj_col,
                            labels = levels(data[[sample_pd_subj_col]]),
                            values = shape_value
                        ) +
                        labs(x = x_lab, y = left_y_lab, title = plot_title)
                    plt <- add_footnote(plt, plot_footnote)
                    
                    height_width_ratio <- 3 / 4
                    width <- 8
                    height <- width * height_width_ratio
                    ggsave(filename = file, plot = plt, width = width,
                           height = height, dpi = 600)
                    progress$inc(1 / num_tfls, detail = file)
                    all_tfls$value <- c(all_tfls$value, file)
                } else if(output_type == 'Forest plot') {
                    file <- paste(title_key, 'pdf', sep = '.')
                    height_width_ratio <- 3 / 4
                    width <- 8
                    height <- width * height_width_ratio
                    pdf(file = file, width = width, height = height)
                    
                    is_visit <- x == sample_pd_avisitn_col
                    x_var <- ifelse(is_visit, sample_pd_avisitn_col,
                                    sample_pd_atptn_col)
                    data <- data[
                        !is.na(data[[x_var]]) &
                            !is.na(data[[pd_value1]]) &
                            !is.na(data[[pd_value2]]), , drop = FALSE
                        ]
                    if(!is_visit) {
                        cond <- data[[sample_pd_avisitn_col]] %in% visit
                        data <- data[cond, , drop = FALSE]
                    }
                    formula_ <- ifelse(is_blank(group), '', paste(group, '+'))
                    formula_ <- paste(formula_, sample_pd_subj_col, '+')
                    if(!is_visit) {
                        formula_ <- paste(
                            formula_, sample_pd_avisitn_col, '+', x
                        )
                    } else formula_ <- paste(formula_, sample_pd_avisitn_col)
                    formula_ <- paste(formula_, '~', subj_pd_param_col)
                    # data <- reshape2::dcast(data, formula_, fun.aggregate = mean,
                    #                         na.rm = TRUE, value.var = sample_pd_aval_col)
                    data <- data.table::dcast(
                        data.table::as.data.table(data), formula_,
                        fun.aggregate = mean, na.rm = TRUE,
                        value.var = c(pd_value1, pd_value2)
                    )
                    if(pd_value1 == pd_value2) {
                        pd_var_1 <- pd_param1
                        pd_var_2 <- pd_param2
                    } else {
                        pd_var_1 <- paste(pd_value1, 'mean',pd_param1, sep = '_')
                        pd_var_2 <- paste(pd_value2, 'mean',pd_param2, sep = '_')
                    }
                    data[[x]] <- as.numeric(data[[x]])
                    if(statistics == 'Mean + SD') {
                        method <- 'mean_sd'
                    } else if(statistics == 'Mean + SE') {
                        method <- 'mean_se'
                    } else if(statistics == 'Median + IQR') {
                        method <- 'median_iqr'
                    }
                    nna_x <- !is.na(data[[x]])
                    cond_na <- (!is.na(data[[pd_var_1]]) & nna_x) ||
                        (!is.na(data[[pd_var_2]]) & nna_x)
                    data <- data[cond_na, , drop = FALSE]
                    group_ <- NULL
                    if(!is_blank(group)) {
                        data[[group]] <- factor(as.character(data[[group]]))
                        group_ <- group
                    }
                    footnote <- trimws(plot_footnote)
                    fnote_lines <- ifelse(is_blank(footnote), 0,
                                          length(strsplit(footnote, '\n')[[1]]))
                    fnote_size <- 0.8
                    fnote_space <- 0.8 + fnote_lines * fnote_size + 0.2
                    if(is_visit) {
                        has_group <- !is.null(group_)
                        if(has_group) {
                            if(!is_blank(footnote)) oma_bottom <- 3 + fnote_space
                            else oma_bottom <- 3
                        } else {
                            if(!is_blank(footnote)) oma_bottom <- fnote_space
                            else oma_bottom <- 0
                        }
                        par_opt <- par(oma = c(oma_bottom, 0, 0, 0))
                        if(has_group) forestplot_footnote <- character(0)
                        else forestplot_footnote <- footnote
                        forest_plot <- dual_y_axis_sumline(
                            data, x, pd_var_1, var_y2 = pd_var_2, group = group_,
                            xlab = x_lab, ylab1 = left_y_lab,ylab2 = right_y_lab,
                            ylog1 = log_pd1, ylog2 = log_pd2, title = plot_title,
                            footnote = forestplot_footnote, method = method,
                            type = 'o', same_y_axis = same_y, save_plot = FALSE
                        )
                        if(has_group) {
                            oma_bottom_legend <- ifelse(is_blank(footnote), 0,
                                                        fnote_space)
                            ngroups <- length(unique(data[[group_]]))
                            all_cols <- colorRampPalette(
                                c('red4', 'white')
                            )(3 * ngroups)
                            leg_cols <- all_cols[seq(1, 3 * ngroups, by = 3)]
                            par(fig = c(0, 1, 0, 1),
                                oma = c(oma_bottom_legend, 0, 0, 0),
                                mar = c(0, 0, 0, 0), new = TRUE)
                            plot(0, 0, type = 'n', bty = 'n', xaxt = 'n',
                                 yaxt = 'n')
                            legend('bottom', levels(data[[group_]]),
                                   xpd = TRUE, horiz = TRUE, inset = c(0, 0),
                                   lty = 1, lwd = 1, bty = 'n', col = leg_cols,
                                   title = group_)
                            if(!is_blank(footnote)) {
                                line_pos <- 0.8 + (fnote_lines - 1) * fnote_size
                                mtext(footnote, side = 1, adj = 0,
                                      cex = fnote_size, outer = TRUE,
                                      line = line_pos)
                            }
                        }
                    } else {
                        n_visits <- length(visit)
                        if(n_visits == 1) {
                            has_group <- !is.null(group_)
                            if(has_group) {
                                if(!is_blank(footnote)) oma_bottom <- 3 + fnote_space
                                else oma_bottom <- 3
                            } else {
                                if(!is_blank(footnote)) oma_bottom <- fnote_space
                                else oma_bottom <- 0
                            }
                            par_opt <- par(oma = c(oma_bottom, 0, 0, 0))
                            if(has_group) forestplot_footnote <- character(0)
                            else forestplot_footnote <- footnote
                            # par_opt <- par(oma = c(3, 0, 0, 0))
                            forest_plot <- dual_y_axis_sumline(
                                data, x, pd_var_1, var_y2 = pd_var_2,
                                group = group_,  xlab = x_lab,
                                ylab1 = left_y_lab, ylab2 = right_y_lab,
                                ylog1 = log_pd1, ylog2 = log_pd2,
                                title = plot_title,
                                footnote = forestplot_footnote, method = method,
                                type = 'o', same_y_axis = same_y, xaxis = TRUE,
                                mar = c(5.1, 4.1, 4.1, 4.1), save_plot = F
                            )
                            if(!is.null(group_)) {
                                oma_bottom_legend <- ifelse(is_blank(footnote), 0,
                                                            fnote_space)
                                ngroups <- length(unique(data[[group_]]))
                                all_cols <- colorRampPalette(
                                    c('red4', 'white')
                                )(3 * ngroups)
                                leg_cols <- all_cols[seq(1, 3 * ngroups, by = 3)]
                                par(fig = c(0, 1, 0, 1),
                                    oma = c(oma_bottom_legend, 0, 0, 0),
                                    mar = c(0, 0, 0, 0), new = TRUE)
                                plot(0, 0, type = 'n', bty = 'n', xaxt = 'n',
                                     yaxt = 'n')
                                legend('bottom', levels(data[[group_]]),
                                       xpd = TRUE, horiz = TRUE, inset = c(0, 0),
                                       lty = 1, lwd = 1, bty = 'n', col = leg_cols,
                                       title = group_)
                                if(!is_blank(footnote)) {
                                    line_pos <- 0.8 + (fnote_lines - 1) * fnote_size
                                    mtext(footnote, side = 1, adj = 0,
                                          cex = fnote_size, outer = TRUE,
                                          line = line_pos)
                                }
                            }
                        } else {
                            xlim <- range_na(data[[sample_pd_atptn_col]])
                            x_tick <- sort(unique(data[[sample_pd_atptn_col]]))
                            layout(matrix(seq_len(n_visits), ncol = 1, byrow = TRUE))
                            
                            has_group <- !is.null(group_)
                            if(has_group) {
                                if(!is_blank(footnote)) oma_bottom <- 7 + fnote_space
                                else oma_bottom <- 7
                            } else {
                                if(!is_blank(footnote)) oma_bottom <- 4 + fnote_space
                                else oma_bottom <- 4
                            }
                            par_opt <- par(oma = c(oma_bottom, 4, 4, 4) + 0.1)
                            # par_opt <- par(oma = c(7, 4, 4, 4) + 0.1)
                            for(idx in seq_along(visit)) {
                                visit_iter <- visit[idx]
                                data_visit <- data[
                                    data[[sample_pd_avisitn_col]] %in% visit_iter, ,
                                    drop = F
                                ]
                                xaxis <- idx == n_visits
                                forest_plot <- dual_y_axis_sumline(
                                    data_visit, x, pd_var_1, var_y2 = pd_var_2,
                                    group = group_, xlab = '', ylab1 = '',
                                    ylab2 = '', ylog1 = log_pd1, ylog2 = log_pd2,
                                    title = '', method = method, type = 'o',
                                    xlim = xlim, same_y_axis = same_y,
                                    xaxis = xaxis, x_tick = x_tick,
                                    x_tick_lab = x_tick, mar = c(0, 0, 0, 0) + 0.1,
                                    legend_txt = paste('Visit =', visit_iter),
                                    save_plot = F
                                )
                            }
                            title(main = plot_title, outer = TRUE)
                            mtext(x_lab, side = 1, line = 2.5, outer = TRUE)
                            mtext(left_y_lab, side = 2, line = 3, outer = TRUE,
                                  col = 'red4')
                            mtext(right_y_lab, side = 4, line = 3, outer = TRUE,
                                  col = 'blue4')
                            if(!is.null(group_)) {
                                oma_bottom_legend <- ifelse(is_blank(footnote), 0,
                                                            fnote_space)
                                ngroups <- length(unique(data[[group_]]))
                                all_cols <- colorRampPalette(
                                    c('red4', 'white')
                                )(3 * ngroups)
                                leg_cols <- all_cols[seq(1, 3 * ngroups, by = 3)]
                                par(fig = c(0, 1, 0, 1),
                                    oma = c(oma_bottom_legend, 0, 0, 0),
                                    mar = c(0, 0, 0, 0), new = TRUE)
                                plot(0, 0, type = 'n', bty = 'n', xaxt = 'n',
                                     yaxt = 'n')
                                legend('bottom', levels(data[[group_]]),
                                       xpd = TRUE, horiz = TRUE, inset = c(0, 0),
                                       lty = 1, lwd = 1, bty = 'n', col = leg_cols,
                                       title = group_)
                                if(!is_blank(footnote)) {
                                    line_pos <- 0.8 + (fnote_lines - 1) * fnote_size
                                    mtext(footnote, side = 1, adj = 0,
                                          cex = fnote_size, outer = TRUE,
                                          line = line_pos)
                                }
                            } else {
                                oma_bottom_legend <- ifelse(is_blank(footnote), 0,
                                                            fnote_space)
                                if(!is_blank(footnote)) {
                                    par(fig = c(0, 1, 0, 1),
                                        oma = c(oma_bottom_legend,0,0,0),
                                        mar = c(0, 0, 0, 0), new = TRUE)
                                    plot(0, 0, type = 'n', bty = 'n', xaxt = 'n',
                                         yaxt = 'n')
                                    line_pos <- 0.8 + (fnote_lines - 1) * fnote_size
                                    mtext(footnote, side = 1, adj = 0,
                                          cex = fnote_size, outer = TRUE,
                                          line = line_pos)
                                }
                            }
                        }
                    }
                    par(par_opt)
                    dev.off()
                    
                    progress$inc(1 / num_tfls, detail = file)
                    all_tfls$value <- c(all_tfls$value, file)
                } else if(output_type == 'Scatter plot') {
                    file <- paste(title_key, 'pdf', sep = '.')
                    
                    is_visit <- !any(!sapply(data[[sample_pd_atptn_col]], is_blank))
                    data <- data[
                        !is.na(data[[pd_value1]]) &
                            !is.na(data[[pd_value2]]), , drop = F
                    ]
                    if(!is_blank(group)) {
                        if(!is_visit) {
                            formula_ <- formula(paste(
                                group, '+', sample_pd_subj_col, '+',
                                sample_pd_avisitn_col, '+', sample_pd_atptn_col,
                                '~', sample_pd_param_col
                            ))
                        } else {
                            formula_ <- formula(paste(
                                group, '+', sample_pd_subj_col, '+',
                                sample_pd_avisitn_col, '~', sample_pd_param_col
                            ))
                        }
                    } else {
                        if(!is_visit) {
                            formula_ <- formula(paste(
                                sample_pd_subj_col, '+',sample_pd_avisitn_col,'+',
                                sample_pd_atptn_col, '~', sample_pd_param_col
                            ))
                        } else {
                            formula_ <- formula(paste(
                                sample_pd_subj_col, '+', sample_pd_avisitn_col,
                                '~', sample_pd_param_col
                            ))
                        }
                    }
                    data <- data.table::dcast(
                        data.table::as.data.table(data), formula_,
                        fun.aggregate = mean, na.rm = TRUE,
                        value.var = c(pd_value1, pd_value2)
                    )
                    if(pd_value1 == pd_value2) {
                        pd_var_1 <- pd_param1
                        pd_var_2 <- pd_param2
                    } else {
                        pd_var_1 <- paste(pd_value1, 'mean',pd_param1, sep = '_')
                        pd_var_2 <- paste(pd_value2, 'mean',pd_param2, sep = '_')
                    }
                    data <- data[!is.na(data[[pd_var_1]]) & !is.na(data[[pd_var_2]]), ]
                    if(!is_blank(group)) {
                        data[[group]] <- factor(as.character(data[[group]]))
                    }
                    baseplot <- gg_wrapper(
                        data, aes_string(x = paste0('`', pd_var_1, '`'),
                                         y = paste0('`', pd_var_2, '`')),
                        log_x = log_pd1, log_y = log_pd2
                    )
                    if(!is_blank(group)) {
                        baseplot <- baseplot +
                            geom_point(aes_string(colour = group, fill = group),
                                       size = 2)
                    } else baseplot <- baseplot + geom_point(size = 2)
                    if(!is_blank(refline)) {
                        if('Loess' %in% refline)
                            baseplot <- baseplot + geom_smooth()
                        if('Linear regression' %in% refline)
                            baseplot <- baseplot + geom_smooth(method = 'lm')
                    }
                    baseplot <- baseplot +
                        labs(x = x_lab, y = left_y_lab, title = plot_title)
                    baseplot <- add_footnote(baseplot, plot_footnote)
                    
                    height_width_ratio <- 3 / 4
                    width <- 8
                    height <- width * height_width_ratio
                    ggsave(filename = file, plot = baseplot, width = width,
                           height = height, dpi = 600)
                    progress$inc(1 / num_tfls, detail = file)
                    all_tfls$value <- c(all_tfls$value, file)
                }
            }
        }
        if(!is.null(out_tnf_data[['sample_pkpd']]) &&
           !is.null(data_$sample_pkpd)) {
            sample_pkpd_rows <- out_tnf_data[['sample_pkpd']]
            for(idx in seq_len(nrow(sample_pkpd_rows))) {
                irow <- sample_pkpd_rows[idx, , drop = FALSE]
                
                title_key <- irow[[out_sample_pkpd_title]]
                tflt <- irow[[out_sample_pkpd_tflt]]
                tfln <- irow[[out_sample_pkpd_tfln]]
                output_type <- irow[[out_sample_pkpd_output]]
                pk_type <- irow[[out_sample_pkpd_pk_type]]
                pk_param <- irow[[out_sample_pkpd_pk_param]]
                pd_param <- irow[[out_sample_pkpd_pd_param]]
                pd_value <- irow[[out_sample_pkpd_pd_value]]
                x <- irow[[out_sample_pkpd_x]]
                visit <- irow[[out_sample_pkpd_visit]]
                group <- irow[[out_sample_pkpd_group]]
                statistics <- irow[[out_sample_pkpd_statistics]]
                refline <- irow[[out_sample_pkpd_refline]]
                add_points <- irow[[out_sample_pkpd_add_points]]
                add_line <- irow[[out_sample_pkpd_add_line]]
                log_pk <- irow[[out_sample_pkpd_log_pk]]
                log_pd <- irow[[out_sample_pkpd_log_pd]]
                same_y <- irow[[out_sample_pkpd_same_y]]
                x_lab <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_sample_pkpd_xlab]]
                )))
                left_y_lab <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_sample_pkpd_left_y_lab]]
                )))
                right_y_lab <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_sample_pkpd_right_y_lab]]
                )))
                plot_title <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_sample_pkpd_plot_title]]
                )))
                plot_title <- trimws(paste(tflt, tfln, ':', plot_title))
                plot_footnote <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_sample_pkpd_plot_footnote]]
                )))
                
                data <- data_$sample_pkpd
                
                if(output_type == 'Forest plot') {
                    file <- paste(title_key, 'pdf', sep = '.')
                    height_width_ratio <- 3 / 4
                    width <- 8
                    height <- width * height_width_ratio
                    pdf(file = file, width = width, height = height)
                    
                    is_pk_con <- pk_type == 'PK concentration'
                    left_y <- ifelse(is_pk_con, sample_pk_con_con_col,
                                     sample_pk_param_estm_col)
                    is_visit <- x == sample_pk_con_visit_col
                    if(!is_visit) {
                        cond <- data[[sample_pk_con_visit_col]] %in% visit
                        data <- data[cond, , drop = FALSE]
                    }
                    cond_param <- data[[sample_pd_param_col]] %in% pd_param
                    if(!is_pk_con) {
                        cond_pk <- data[[sample_pk_param_param_col]] %in% pk_param
                        cond_param <- cond_param & cond_pk
                    }
                    data <- data[cond_param, , drop = FALSE]
                    data[[x]] <- as.numeric(data[[x]])
                    data[[left_y]] <- as.numeric(data[[left_y]])
                    data[[pd_value]] <- as.numeric(data[[pd_value]])
                    
                    if(statistics == 'Mean + SD') {
                        method <- 'mean_sd'
                    } else if(statistics == 'Mean + SE') {
                        method <- 'mean_se'
                    } else if(statistics == 'Median + IQR') {
                        method <- 'median_iqr'
                    }
                    group_ <- NULL
                    if(!is_blank(group)) {
                        data[[group]] <- factor(data[[group]])
                        group_ <- group
                    }
                    footnote <- trimws(plot_footnote)
                    fnote_lines <- ifelse(is_blank(footnote), 0,
                                          length(strsplit(footnote, '\n')[[1]]))
                    fnote_size <- 0.8
                    fnote_space <- 0.8 + fnote_lines * fnote_size + 0.2
                    if(is_visit) {
                        has_group <- !is.null(group_)
                        if(has_group) {
                            if(!is_blank(footnote)) oma_bottom <- 3 + fnote_space
                            else oma_bottom <- 3
                        } else {
                            if(!is_blank(footnote)) oma_bottom <- fnote_space
                            else oma_bottom <- 0
                        }
                        par_opt <- par(oma = c(oma_bottom, 0, 0, 0))
                        if(has_group) forestplot_footnote <- character(0)
                        else forestplot_footnote <- footnote
                        
                        # par_opt <- par(oma = c(3, 0, 0, 0))
                        forest_plot <- dual_y_axis_sumline(
                            data, x, left_y, var_y2 = pd_value, group = group_,
                            xlab = x_lab, ylab1 = left_y_lab,ylab2 = right_y_lab,
                            ylog1 = log_pk, ylog2 = log_pd, title = plot_title,
                            footnote = forestplot_footnote, method = method,
                            type = 'o', same_y_axis = same_y, save_plot = FALSE
                        )
                        if(!is.null(group_)) {
                            oma_bottom_legend <- ifelse(is_blank(footnote), 0,
                                                        fnote_space)
                            ngroups <- length(unique(data[[group_]]))
                            all_cols <- colorRampPalette(
                                c('red4', 'white')
                            )(3 * ngroups)
                            leg_cols <- all_cols[seq(1, 3 * ngroups, by = 3)]
                            par(fig = c(0, 1, 0, 1),
                                oma = c(oma_bottom_legend, 0, 0, 0),
                                mar = c(0, 0, 0, 0), new = TRUE)
                            plot(0, 0, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
                            legend('bottom', levels(data[[group_]]),
                                   xpd = TRUE, horiz = TRUE, inset = c(0, 0),
                                   lty = 1, lwd = 1, bty = 'n', col = leg_cols,
                                   title = group_)
                            if(!is_blank(footnote)) {
                                line_pos <- 0.8 + (fnote_lines - 1) * fnote_size
                                mtext(footnote, side = 1, adj = 0,
                                      cex = fnote_size, outer = TRUE,
                                      line = line_pos)
                            }
                        }
                    } else {
                        n_visits <- length(visit)
                        if(n_visits == 1) {
                            has_group <- !is.null(group_)
                            if(has_group) {
                                if(!is_blank(footnote)) oma_bottom <- 3 + fnote_space
                                else oma_bottom <- 3
                            } else {
                                if(!is_blank(footnote)) oma_bottom <- fnote_space
                                else oma_bottom <- 0
                            }
                            par_opt <- par(oma = c(oma_bottom, 0, 0, 0))
                            if(has_group) forestplot_footnote <- character(0)
                            else forestplot_footnote <- footnote
                            
                            # par_opt <- par(oma = c(3, 0, 0, 0))
                            forest_plot <- dual_y_axis_sumline(
                                data, x, left_y, var_y2 = pd_value,
                                group = group_,  xlab = x_lab,
                                ylab1 = left_y_lab, ylab2 = right_y_lab,
                                ylog1 = log_pk, ylog2 = log_pd,
                                title = plot_title,
                                footnote = forestplot_footnote, method = method,
                                type = 'o', same_y_axis = same_y, xaxis = TRUE,
                                mar = c(5.1, 4.1, 4.1, 4.1), save_plot = F
                            )
                            if(!is.null(group_)) {
                                oma_bottom_legend <- ifelse(is_blank(footnote), 0,
                                                            fnote_space)
                                ngroups <- length(unique(data[[group_]]))
                                all_cols <- colorRampPalette(
                                    c('red4', 'white')
                                )(3 * ngroups)
                                leg_cols <- all_cols[seq(1, 3 * ngroups, by = 3)]
                                par(fig = c(0, 1, 0, 1),
                                    oma = c(oma_bottom_legend, 0, 0, 0),
                                    mar = c(0, 0, 0, 0), new = TRUE)
                                plot(0, 0, type = 'n', bty = 'n', xaxt = 'n',
                                     yaxt = 'n')
                                legend('bottom', levels(data[[group_]]),
                                       xpd = TRUE, horiz = TRUE, inset = c(0, 0),
                                       lty = 1, lwd = 1, bty = 'n', col = leg_cols,
                                       title = group_)
                                if(!is_blank(footnote)) {
                                    line_pos <- 0.8 + (fnote_lines - 1) * fnote_size
                                    mtext(footnote, side = 1, adj = 0,
                                          cex = fnote_size, outer = TRUE,
                                          line = line_pos)
                                }
                            }
                        } else {
                            xlim <- range_na(data[[sample_pk_con_time_col]])
                            x_tick <- sort(unique(data[[sample_pk_con_time_col]]))
                            layout(matrix(seq_len(n_visits), ncol = 1, byrow = TRUE))
                            
                            has_group <- !is.null(group_)
                            if(has_group) {
                                if(!is_blank(footnote)) oma_bottom <- 7 + fnote_space
                                else oma_bottom <- 7
                            } else {
                                if(!is_blank(footnote)) oma_bottom <- 4 + fnote_space
                                else oma_bottom <- 4
                            }
                            par_opt <- par(oma = c(oma_bottom, 4, 4, 4) + 0.1)
                            # par_opt <- par(oma = c(7, 4, 4, 4) + 0.1)
                            for(idx in seq_along(visit)) {
                                visit_iter <- visit[idx]
                                data_visit <- data[
                                    data[[sample_pk_con_visit_col]] %in% visit_iter, ,
                                    drop = F ]
                                xaxis <- idx == n_visits
                                forest_plot <- dual_y_axis_sumline(
                                    data_visit, x, left_y, var_y2 = pd_value,
                                    group = group_, xlab = '', ylab1 = '',
                                    ylab2 = '', ylog1 = log_pk, ylog2 = log_pd,
                                    title = '', method = method, type = 'o',
                                    same_y_axis = same_y, xlim = xlim,
                                    xaxis = xaxis, x_tick = x_tick,
                                    x_tick_lab = x_tick, mar = c(0, 0, 0, 0) + 0.1,
                                    legend_txt = paste('Visit =', visit_iter),
                                    save_plot = F
                                )
                            }
                            title(main = plot_title, outer = TRUE)
                            mtext(x_lab, side = 1, line = 2.5, outer = TRUE)
                            mtext(left_y_lab, side = 2, line = 3,
                                  outer = TRUE, col = 'red4')
                            mtext(right_y_lab, side = 4, line = 3,
                                  outer = TRUE, col = 'blue4')
                            if(!is.null(group_)) {
                                oma_bottom_legend <- ifelse(is_blank(footnote), 0,
                                                            fnote_space)
                                ngroups <- length(unique(data[[group_]]))
                                all_cols <- colorRampPalette(
                                    c('red4', 'white')
                                )(3 * ngroups)
                                leg_cols <- all_cols[seq(1, 3 * ngroups, by = 3)]
                                par(fig = c(0, 1, 0, 1),
                                    oma = c(oma_bottom_legend, 0, 0, 0),
                                    mar = c(0, 0, 0, 0), new = TRUE)
                                plot(0, 0, type = 'n', bty = 'n', xaxt = 'n',
                                     yaxt = 'n')
                                legend('bottom', levels(data[[group_]]),
                                       xpd = TRUE, horiz = TRUE, inset = c(0, 0),
                                       lty = 1, lwd = 1, bty = 'n', col = leg_cols,
                                       title = group_)
                            } else {
                                oma_bottom_legend <- ifelse(is_blank(footnote), 0,
                                                            fnote_space)
                                if(!is_blank(footnote)) {
                                    par(fig = c(0, 1, 0, 1),
                                        oma = c(oma_bottom_legend,0,0,0),
                                        mar = c(0, 0, 0, 0), new = TRUE)
                                    plot(0, 0, type = 'n', bty = 'n', xaxt = 'n',
                                         yaxt = 'n')
                                    line_pos <- 0.8 + (fnote_lines - 1) * fnote_size
                                    mtext(footnote, side = 1, adj = 0,
                                          cex = fnote_size,
                                          outer = TRUE, line = line_pos)
                                }
                            }
                        }
                    }
                    par(par_opt)
                    dev.off()
                    
                    progress$inc(1 / num_tfls, detail = file)
                    all_tfls$value <- c(all_tfls$value, file)
                } else if(output_type == 'Scatter plot') {
                    file <- paste(title_key, 'pdf', sep = '.')
                    
                    is_pk_con <- pk_type == 'PK concentration'
                    x_var <- ifelse(is_pk_con, sample_pk_con_con_col,
                                    sample_pk_param_estm_col)
                    data <- data[
                        !is.na(data[[x_var]]) & !is.na(data[[pd_value]]), ,
                        drop = FALSE
                    ]
                    cond_param <- data[[sample_pd_param_col]] %in% pd_param
                    if(!is_pk_con) {
                        cond_pk <- data[[sample_pk_param_param_col]] %in% pk_param
                        cond_param <- cond_param & cond_pk
                    }
                    data <- data[cond_param, , drop = FALSE]
                    data[[x_var]] <- as.numeric(data[[x_var]])
                    data[[pd_value]] <- as.numeric(data[[pd_value]])
                    data <- data[!is.na(data[[x_var]]) & !is.na(data[[pd_value]]), ]
                    if(!is_blank(group)) {
                        data[[group]] <- factor(as.character(data[[group]]))
                    }
                    
                    baseplot <- gg_wrapper(
                        data, aes_string(x = paste0('`', x_var, '`'),
                                         y = paste0('`', pd_value, '`')),
                        log_x = log_pk, log_y = log_pd
                    )
                    if(!is_blank(group)) {
                        baseplot <- baseplot +
                            geom_point(aes_string(colour = group, fill = group),
                                       size = 2)
                    } else baseplot <- baseplot + geom_point(size = 2)
                    if(!is_blank(refline)) {
                        if('Loess' %in% refline)
                            baseplot <- baseplot + geom_smooth()
                        if('Linear regression' %in% refline)
                            baseplot <- baseplot + geom_smooth(method = 'lm')
                    }
                    baseplot <- baseplot +
                        labs(x = x_lab, y = left_y_lab, title = plot_title)
                    baseplot <- add_footnote(baseplot, plot_footnote)
                    
                    height_width_ratio <- 3 / 4
                    width <- 8
                    height <- width * height_width_ratio
                    ggsave(filename = file, plot = baseplot, width = width,
                           height = height, dpi = 600)
                    progress$inc(1 / num_tfls, detail = file)
                    all_tfls$value <- c(all_tfls$value, file)
                } else if(output_type == 'Quartile plot') {
                    file <- paste(title_key, 'pdf', sep = '.')
                    
                    is_pk_con <- pk_type == 'PK concentration'
                    x_var <- ifelse(is_pk_con, sample_pk_con_con_col,
                                    sample_pk_param_estm_col)
                    data <- data[
                        !is.na(data[[x_var]]) & !is.na(data[[pd_value]]), ,
                        drop = FALSE
                    ]
                    cond_param <- data[[sample_pd_param_col]] %in% pd_param
                    if(!is_pk_con) {
                        cond_pk <- data[[sample_pk_param_param_col]] %in% pk_param
                        cond_param <- cond_param & cond_pk
                    }
                    data <- data[cond_param, , drop = FALSE]
                    data[[x_var]] <- as.numeric(data[[x_var]])
                    data[[pd_value]] <- as.numeric(data[[pd_value]])
                    data <- data[!is.na(data[[x_var]]) & !is.na(data[[pd_value]]), ]
                    if(!is_blank(group)) {
                        data[[group]] <- factor(as.character(data[[group]]))
                    }
                    pk_quartile_label <- paste0('Q', seq_len(4))
                    data[[x_var]] <- factor(cut(
                        data[[x_var]],
                        breaks = c(quantile(data[[x_var]],
                                            probs = seq(0, 1, by = 0.25))),
                        labels = pk_quartile_label, include.lowest = TRUE
                    ))
                    quartile_plot <- gg_boxplot(
                        data, xvar = x_var, yvar = pd_value, group = group,
                        log_y = log_pd, x_lab = x_lab, y_lab = left_y_lab,
                        title = plot_title, with_points = add_points,
                        with_line = add_line
                    )
                    quartile_plot <- add_footnote(
                        quartile_plot, plot_footnote
                    )
                    
                    height_width_ratio <- 3 / 4
                    width <- 8
                    height <- width * height_width_ratio
                    ggsave(filename = file, plot = quartile_plot, width = width,
                           height = height, dpi = 600)
                    progress$inc(1 / num_tfls, detail = file)
                    all_tfls$value <- c(all_tfls$value, file)
                }
            }
        }
        if(!is.null(out_tnf_data[['lab']]) &&
           !is.null(data_$lab)) {
            lab_rows <- out_tnf_data[['lab']]
            for(idx in seq_len(nrow(lab_rows))) {
                irow <- lab_rows[idx, , drop = FALSE]
                
                title_key <- irow[[out_lab_title]]
                tflt <- irow[[out_lab_tflt]]
                tfln <- irow[[out_lab_tfln]]
                output_type <- irow[[out_lab_output]]
                analyte <- irow[[out_lab_analyte]]
                group <- irow[[out_lab_group]]
                dgt <- irow[[out_lab_decimal]]
                table_title <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_lab_table_title]]
                )))
                table_footnote <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_lab_table_footnote]]
                )))
                statistics <- irow[[out_lab_statistics]]
                subjid <- unlist(strsplit(gsub(
                    '\\\\n', '\n', as.character(irow[[out_lab_subjid]])
                ), '\n'))
                add_points <- ifelse(
                    as.character(irow[[out_lab_add_points]]) %in% string_yes,
                    TRUE, FALSE
                )
                refline <- irow[[out_lab_refline]]
                log_y <- ifelse(
                    as.character(irow[[out_lab_log_y]]) %in% string_yes,
                    TRUE, FALSE
                )
                x_lab <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_lab_xlab]]
                )))
                y_lab <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_lab_ylab]]
                )))
                plot_title <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_lab_plot_title]]
                )))
                plot_title <- trimws(paste(tflt, tfln, ':', plot_title))
                plot_footnote <- na_to_blank(trimws(gsub(
                    '\\\\n', '\n', irow[[out_lab_plot_footnote]]
                )))
                
                data <- data_$lab
                data <- data[data[[lab_tstnam_col]] %in% analyte, , drop = F]
                expr <- list(lazyeval::interp(~factor(x),
                                              x = as.name(lab_visitnum_col)))
                data <- data %>%
                    filter_(paste(lab_visitnum_col, '!=', lab_visit_to_remove)) %>%
                    mutate_(.dots = setNames(expr, lab_visitnum_col))
                
                if(output_type == 'Summary table') {
                    file <- paste(title_key, 'rtf', sep = '.')
                    
                    is_categorical <- any(is.na(as.numeric(data[[lab_rptresc_col]])))
                    if(is_categorical) {
                        count_occurances_wrapper <- function(value) {
                            partial(count_occurances, value = value)
                        }
                        values <- sort(unique_na(data[[lab_rptresc_col]]))
                        lab_summary_func <- lapply(values, count_occurances_wrapper)
                        names(lab_summary_func) <- values
                        lab_summary_func <- c('N' = n_nna, lab_summary_func)
                    } else {
                        data[[lab_rptresc_col]] <- as.numeric(data[[lab_rptresc_col]])
                        lab_summary_func <- c(
                            'N' = n_nna,
                            'Mean (SD)' = partial(mean_sd_str, digits = dgt),
                            '%CV' = partial(coeff_var_str, digits = dgt),
                            'Median' = partial(median_str, digits = dgt),
                            'Q1, Q3' = partial(q1_q3_str, digits = dgt),
                            'Min, Max' = partial(min_max_str, digits = dgt),
                            'Geom Mean (%CV)' = partial(geo_mean_cv_str, digits = dgt),
                            'Mean (SD) of LN' = partial(mean_sd_ln_str, digits = dgt)
                        )
                    }
                    header <- ifelse(is_categorical, 'Count', 'Value')
                    if(lab_visit_col %in% names(data)) {
                        visit_ <- lab_visit_col
                        data[[visit_]] <- factor(
                            data[[visit_]],
                            levels = unique(data[[visit_]][order(
                                data[[lab_visitnum_col]]
                            )])
                        )
                        row_names <- levels(data[[visit_]])
                    } else {
                        visit_ <- lab_visitnum_col
                        data[[visit_]] <- factor(
                            data[[visit_]], levels = sort(unique(data[[visit_]]))
                        )
                        row_names <- paste('Visit', levels(data[[visit_]]))
                    }
                    if(is_blank(group)) {
                        data <- select_(data, lab_rptresc_col, visit_)
                    } else {
                        data[[group]] <- factor(
                            data[[group]],
                            levels = sort(unique(data[[group]]))
                        )
                        data <- select_(data, lab_rptresc_col, visit_, group)
                    }
                    summary_table <- summary_table_all(
                        data, row_var = visit_, row_names = row_names,
                        col_var = group,
                        col_names = paste(group, '=', levels(data[[group]])),
                        val_var = lab_rptresc_col,
                        n_in_header = FALSE, func_list = lab_summary_func,
                        caption = table_title, footnote = table_footnote,
                        header = header, rowlabel = ' ', format = 'rtf'
                    )
                    rtf_table_wrapper(
                        file, summary_table, block_break = TRUE,
                        nline_block = length(lab_summary_func) + 1,
                        caption = table_title, footnote = table_footnote
                    )
                    
                    progress$inc(1 / num_tfls, detail = file)
                    all_tfls$value <- c(all_tfls$value, file)
                } else if (output_type == 'Abnormality table') {
                    file <- paste(title_key, 'rtf', sep = '.')
                    
                    values <- sort(unique_na(data[[lab_toxgrg_col]]))
                    if(is_blank(values)) {
                        next
                    }
                    count_occurances_wrapper <- function(value) {
                        partial(count_occurances, value = value)
                    }
                    lab_summary_func <- lapply(values, count_occurances_wrapper)
                    names(lab_summary_func) <- paste('Grade', values)
                    lab_summary_func <- c('N' = n_nna, lab_summary_func)
                    header <- 'Count'
                    visit_ <- lab_visitnum_col
                    if(lab_visit_col %in% names(data)) {
                        visit_ <- lab_visit_col
                        data[[visit_]] <- factor(
                            data[[visit_]],
                            levels = unique(data[[visit_]][order(data[[lab_visitnum_col]])])
                        )
                        row_names <- levels(data[[visit_]])
                    } else {
                        visit_ <- lab_visitnum_col
                        data[[visit_]] <- factor(
                            data[[visit_]], levels = sort(unique(data[[visit_]]))
                        )
                        row_names <- paste('Visit', levels(data[[visit_]]))
                    }
                    if(is_blank(group)) {
                        data <- select_(data, lab_toxgrg_col, visit_)
                    } else {
                        data[[group]] <- factor(
                            data[[group]],
                            levels = sort(unique(data[[group]]))
                        )
                        data <- select_(data, lab_toxgrg_col, visit_, group)
                    }
                    summary_table <- summary_table_all(
                        data, row_var = visit_, row_names = row_names,
                        col_var = group,
                        col_names = paste(group, '=', levels(data[[group]])),
                        val_var = lab_toxgrg_col,
                        n_in_header = FALSE, func_list = lab_summary_func,
                        caption = table_title, footnote = table_footnote,
                        header = header, rowlabel = ' ', format = 'rtf'
                    )
                    rtf_table_wrapper(
                        file, summary_table, block_break = TRUE,
                        nline_block = length(lab_summary_func) + 1,
                        caption = table_title, footnote = table_footnote
                    )
                    
                    progress$inc(1 / num_tfls, detail = file)
                    all_tfls$value <- c(all_tfls$value, file)
                } else if(output_type == 'Summary line') {
                    file <- paste(title_key, 'pdf', sep = '.')
                    
                    data <- data[!is.na(data[[lab_visitnum_col]]), , drop = F]
                    if(!is_blank(group)) {
                        expr <- list(lazyeval::interp(~factor(c),
                                                      c = as.name(group)))
                        data <- data %>% mutate_(.dots = setNames(expr, group))
                    }
                    plot_ <- gg_lineplot(data, lab_visitnum_col, lab_rptresn_col,
                                         group = group, log_y = log_y,
                                         x_lab = x_lab, y_lab = y_lab,
                                         title = plot_title,
                                         summary_method = statistics,
                                         with_points = add_points)
                    refline_vec <- suppressWarnings(as.numeric(trimws(
                        unlist(strsplit(na_to_blank(refline), ','))
                    )))
                    if(!is_blank(refline_vec)) {
                        plot_ <- plot_ +
                            geom_hline(yintercept = refline_vec,
                                       linetype = 'dashed')
                    }
                    plot_ <- add_footnote(plot_, plot_footnote)
                    
                    height_width_ratio <- 3 / 4
                    width <- 8
                    height <- width * height_width_ratio
                    ggsave(filename = file, plot = plot_, width = width,
                           height = height, dpi = 600)
                    progress$inc(1 / num_tfls, detail = file)
                    all_tfls$value <- c(all_tfls$value, file)
                } else if(output_type == 'Individual line') {
                    file <- paste(title_key, 'pdf', sep = '.')
                    
                    data <- data[!is.na(data[[lab_visitnum_col]]), , drop = F]
                    data <- data[data[[lab_subj_col]] %in% subjid, , drop = F]
                    data[[lab_subj_col]] <- trimws(as.character(
                        data[[lab_subj_col]]
                    ))
                    if(!is_blank(group)) {
                        data[[group]] <- factor(
                            data[[group]],
                            levels = sort(unique(data[[group]]))
                        )
                        data <- arrange_(data, paste0('`', group, '`'),
                                         lab_subj_col, lab_visitnum_col)
                    } else {
                        data <- arrange_(data, lab_subj_col, lab_visitnum_col)
                    }
                    data[[lab_subj_col]] <- factor(
                        data[[lab_subj_col]],
                        levels = unique(data[[lab_subj_col]])
                    )
                    nsubj <- length(levels(data[[lab_subj_col]]))
                    shape_value <- seq_len(nsubj)
                    baseplot <- gg_wrapper(data, aes_string(x = lab_visitnum_col,
                                                            y = lab_rptresn_col,
                                                            group = lab_subj_col),
                                           log_y = log_y)
                    if(!is_blank(group)) {
                        nsubj_group <- sapply(by(
                            data, data[[group]],
                            function(df) {unique(df[[lab_subj_col]])}
                        ), length)
                        shape_value <- unname(sequence(nsubj_group))
                        ngroups <- length(nsubj_group)
                        all_colors <- gg_color_hue(ngroups)
                        color_value <- all_colors[rep.int(1:ngroups,
                                                          times = nsubj_group)]
                        
                        baseplot <- baseplot +
                            aes_string(colour = lab_subj_col, fill = group) +
                            scale_colour_manual(
                                name = lab_subj_col,
                                labels = levels(data[[lab_subj_col]]),
                                values = color_value
                            ) +
                            scale_fill_manual(
                                name = group,
                                labels = levels(data[[group]]),
                                values = all_colors,
                                guide=guide_legend(
                                    override.aes = list(colour = all_colors),
                                    order = 2
                                )
                            )
                    }
                    baseplot <- baseplot +
                        stat_summary(fun.y = mean_na, geom = 'line') +
                        stat_summary(aes_string(shape = lab_subj_col),
                                     fun.y = mean_na, geom = 'point') +
                        scale_shape_manual(
                            name = lab_subj_col,
                            labels = levels(data[[lab_subj_col]]),
                            values = shape_value
                        ) +
                        labs(x = x_lab, y = y_lab, title = plot_title)
                    refline_vec <- suppressWarnings(as.numeric(trimws(
                            unlist(strsplit(na_to_blank(refline), ','))
                    )))
                    if(!is_blank(refline_vec)) {
                        baseplot <- baseplot +
                            geom_hline(yintercept = refline_vec,
                                       linetype = 'dashed')
                    }
                    baseplot <- add_footnote(baseplot, lab_indplot_footnote$value)
                    
                    height_width_ratio <- 3 / 4
                    width <- 8
                    height <- width * height_width_ratio
                    ggsave(filename = file, plot = baseplot, width = width,
                           height = height, dpi = 600)
                    progress$inc(1 / num_tfls, detail = file)
                    all_tfls$value <- c(all_tfls$value, file)
                }
            }
        }
        if(length(all_tfls$value) > 0) all_tfls$ready <- TRUE
    })
    
    # an download button for TFLs
    output$out_tfl_download_button <- renderUI({
        req(input$out_process_tnf, all_tfls$ready)
        downloadButton('out_tfl_download', 'Output results')
    })
    output$out_tfl_download <- downloadHandler(
        filename = function() {return('result.zip')},
        content = function(file) {
            owd <- setwd(all_tfls$dir)
            on.exit(setwd(owd))
            system2(
                'zip', args = shQuote(c('-r9X', file, all_tfls$value)),
                stdout = FALSE
            )
        }
    )
    
    tnf_data <- reactive({
        tnf_uploaded <- !is.null(out_tnf_data())
        tnf_saved <- any(!is.null(n_tfl$figure), !is.null(n_tfl$table))
        if(!any(tnf_uploaded, tnf_saved)) return()
        if(tnf_uploaded) out_tnf_data <- out_tnf_data()
        else if(tnf_saved) out_tnf_data <- tnf_rows
        return(out_tnf_data)
    })
    
    # a tabset UI for TNF file
    output$out_tnf_tabpanel <- renderUI({
        tabs <- list(id = 'out_tnf_tabs')
        out_tnf_data <- tnf_data()
        req(out_tnf_data)
        if(!is.null(out_tnf_data$subj_pk)) {
            tabs <- append_alist(tabPanel(
                out_tnf_sheet_names['subj_pk'],
                DT::dataTableOutput('out_tnf_tab_subj_pk')
            ), tabs)
        }
        if(!is.null(out_tnf_data$subj_pd)) {
            tabs <- append_alist(tabPanel(
                out_tnf_sheet_names['subj_pd'],
                DT::dataTableOutput('out_tnf_tab_subj_pd')
            ), tabs)
        }
        if(!is.null(out_tnf_data$subj_pkpd)) {
            tabs <- append_alist(tabPanel(
                out_tnf_sheet_names['subj_pkpd'],
                DT::dataTableOutput('out_tnf_tab_subj_pkpd')
            ), tabs)
        }
        if(!is.null(out_tnf_data$sample_pk)) {
            tabs <- append_alist(tabPanel(
                out_tnf_sheet_names['sample_pk'],
                DT::dataTableOutput('out_tnf_tab_sample_pk')
            ), tabs)
        }
        if(!is.null(out_tnf_data$sample_pd)) {
            tabs <- append_alist(tabPanel(
                out_tnf_sheet_names['sample_pd'],
                DT::dataTableOutput('out_tnf_tab_sample_pd')
            ), tabs)
        }
        if(!is.null(out_tnf_data$sample_pkpd)) {
            tabs <- append_alist(tabPanel(
                out_tnf_sheet_names['sample_pkpd'],
                DT::dataTableOutput('out_tnf_tab_sample_pkpd')
            ), tabs)
        }
        if(!is.null(out_tnf_data$lab)) {
            tabs <- append_alist(tabPanel(
                out_tnf_sheet_names['lab'],
                DT::dataTableOutput('out_tnf_tab_lab')
            ), tabs)
        }
        do.call(tabsetPanel, tabs)
    })
    
    # data table output for TNF file
    output$out_tnf_tab_subj_pk <- DT::renderDataTable({
        out_tnf_data <- tnf_data()
        DT::datatable(out_tnf_data$subj_pk, options = list(dom = 't'))
    })
    output$out_tnf_tab_subj_pd <- DT::renderDataTable({
        out_tnf_data <- tnf_data()
        DT::datatable(out_tnf_data$subj_pd, options = list(dom = 't'))
    })
    output$out_tnf_tab_subj_pkpd <- DT::renderDataTable({
        out_tnf_data <- tnf_data()
        DT::datatable(out_tnf_data$subj_pkpd, options = list(dom = 't'))
    })
    output$out_tnf_tab_sample_pk <- DT::renderDataTable({
        out_tnf_data <- tnf_data()
        DT::datatable(out_tnf_data$sample_pk, options = list(dom = 't'))
    })
    output$out_tnf_tab_sample_pd <- DT::renderDataTable({
        out_tnf_data <- tnf_data()
        DT::datatable(out_tnf_data$sample_pd, options = list(dom = 't'))
    })
    output$out_tnf_tab_sample_pkpd <- DT::renderDataTable({
        out_tnf_data <- tnf_data()
        DT::datatable(out_tnf_data$sample_pkpd, options = list(dom = 't'))
    })
    output$out_tnf_tab_lab <- DT::renderDataTable({
        out_tnf_data <- tnf_data()
        DT::datatable(out_tnf_data$lab, options = list(dom = 't'))
    })
    
    
})


































