# ui.R

shinyUI(navbarPage(
    
    title = 'Safety monitoring',
    
    #-----------------------------------------------
    # Data import page
    #-----------------------------------------------
    tabPanel(
        title = 'Import data',
        fluidRow(
            column(
                width = 3,
                wellPanel(
                    fileInput(
                        'file_adsl',
                        tags$p('ADSL data',
                               tags$a('(Need template?)', target = '_blank',
                                      href = file_adsl_template))
                    ),
                    fileInput(
                        'file_adsl_naming',
                        tags$p('ADSL naming file',
                               tags$a('(Need template?)', target = '_blank',
                                      href = file_adsl_naming_template))
                    ),
                    tags$hr(),
                    fileInput(
                        'file_adae',
                        tags$p('ADAE data',
                               tags$a('(Need template?)', target = '_blank',
                                      href = file_adae_template))
                    ),
                    fileInput(
                        'file_adae_naming',
                        tags$p('ADAE naming file',
                               tags$a('(Need template?)', target = '_blank',
                                      href = file_adae_naming_template))
                    ),
                    uiOutput('file_adslae_merge'),
                    uiOutput('file_adslae_download_button')
                )
                # uiOutput('file_read_into_memory_status')
            ),
            column(
                width = 9,
                fluidRow(
                    column(
                        width = 4,
                        shinyjs::disabled(
                            checkboxInput('file_subset_teae', 'TEAE', value = F)
                        )
                    ),
                    # column(
                    #     width = 3,
                    #     shinyjs::disabled(
                    #         checkboxInput('file_subset_remove', 'Remove AE Prior to First Dose', value = F)
                    #     )
                    # ),
                    column(
                        width = 4,
                        shinyjs::disabled(
                            dateInput('file_subset_snapshot', 'Snapshot Date', width = '50%')
                        )
                    ),
                    column(
                        width = 4,
                        shinyBS::bsButton(
                            'file_subset_button', 'Filter merged data',
                            disabled = TRUE
                        )
                    ),
                    tags$style(type='text/css',
                               "#file_subset_teae_text
                               {margin-top: 10px;}")
                ),
                shinyBS::bsModal(
                    'file_subset_bsmodal', 'Data subsetting', 'file_subset_button',
                    size = 'large',
                    fluidRow(
                        column(5, uiOutput('file_subset_var')),
                        column(5, uiOutput('file_subset_val')),
                        column(2, uiOutput('file_subset_logical'))
                    ),
                    fluidRow(
                        column(2, uiOutput('file_subset_add')),
                        column(2, offset = 3, uiOutput('file_subset_clear')),
                        column(2, offset = 3, uiOutput('file_subset_done'))
                    )
                ),
                tags$hr(),
                uiOutput('file_tabpanel')
            )
        )
    ),
    
    #-----------------------------------------------
    # Safety analysis page
    #-----------------------------------------------
    tabPanel(
        title = 'Safety analysis',
        fluidRow(
            column(
                width = 3,
                wellPanel(
                    uiOutput('treatment_mapping_ui'),
                    uiOutput('ae_structure_ui'),
                    
                    conditionalPanel(
                        condition = 'output.adslae_import_status && output.show_table',
                        tagList(
                            uiOutput('lastdose_plus_ui'),
                            uiOutput('confidence_level_ui'),
                            uiOutput('landmark_km_t_ui'),
                            uiOutput('DFDR_p_ui'),
                            uiOutput('DFDR_ruleof4_ui'),
                            uiOutput('BHMM_control_ui'),
                            tags$br(),
                            actionButton('compute', 'Compute output')
                        )
                    ),
                    # uiOutput('compute_ui'),
                    uiOutput('volcano_x'),
                    uiOutput('volcano_y'),
                    uiOutput('forestplot_x')
                    
                )
            ),
            column(
                width = 7,
                uiOutput('safety_output'),
                uiOutput('output_table_footnote'),
                uiOutput('output_abbrev_footnote')
            ),
            column(
                width = 2,
                
                uiOutput('output_subset_button'),
                shinyBS::bsModal(
                    'output_subset_bsmodal', 'Filter output table', 'output_subset_button',
                    size = 'large',
                    fluidRow(tags$h4('Filter by rows')),
                    fluidRow(
                        column(5, uiOutput('output_subset_var')),
                        column(5, uiOutput('output_subset_val')),
                        column(2, uiOutput('output_subset_logical'))
                    ),
                    fluidRow(
                        column(2, uiOutput('output_subset_add')),
                        column(2, offset = 3, uiOutput('output_subset_clear')),
                        column(2, offset = 3, uiOutput('output_subset_done'))
                    ),
                    tags$hr(),
                    fluidRow(tags$h4('Select columns')),
                    fluidRow(
                        column(
                            width = 4,
                            fluidRow(
                                column(
                                    width = 7,
                                    tags$strong('Summary statistics')
                                ),
                                column(
                                    width = 1,
                                    checkboxInput('output_sumstat', NULL, TRUE)
                                ),
                                column(4)
                            ),
                            fluidRow(
                                column(
                                    width = 12,
                                    checkboxGroupInput(
                                        'output_sumstat_group',
                                        label = NULL,
                                        choices = output_sumstat_group
                                    )
                                )
                            ),
                            fluidRow(
                                column(
                                    width = 7,
                                    tags$strong('Landmark KM')
                                ),
                                column(
                                    width = 1,
                                    checkboxInput('output_landmark', NULL, TRUE)
                                ),
                                column(4)
                            ),
                            fluidRow(
                                column(
                                    width = 12,
                                    checkboxGroupInput(
                                        'output_landmark_group',
                                        label = NULL,
                                        choices = output_landmark_group
                                    )
                                )
                            ),
                            uiOutput('output_bhmm_ui'),
                            uiOutput('output_bhmm_group_ui'),
                            tags$style(
                                type='text/css',
                                "#output_sumstat
                                {margin-top: -5px;
                                margin-left: -50px}"
                            ),
                            tags$style(
                                type='text/css',
                                "#output_sumstat_group
                                {margin-top: -35px;}"
                            ),
                            tags$style(
                                type='text/css',
                                "#output_landmark
                                {margin-top: -5px;
                                margin-left: -50px}"
                            ),
                            tags$style(
                                type='text/css',
                                "#output_landmark_group
                                {margin-top: -35px;}"
                            ),
                            tags$style(
                                type='text/css',
                                "#output_bhmm
                                {margin-top: -5px;
                                margin-left: -50px}"
                            ),
                            tags$style(
                                type='text/css',
                                "#output_bhmm_group
                                {margin-top: -35px;}"
                            )
                        ),
                        column(
                            width = 4,
                            fluidRow(
                                column(
                                    width = 6,
                                    tags$strong('EAIR')
                                ),
                                column(
                                    width = 1,
                                    checkboxInput('output_eair', NULL, TRUE)
                                ),
                                column(5)
                            ),
                            fluidRow(
                                column(
                                    width = 12,
                                    checkboxGroupInput(
                                        'output_eair_group',
                                        label = NULL,
                                        choices = output_eair_group
                                    )
                                )
                            ),
                            fluidRow(
                                column(
                                    width = 6,
                                    tags$strong('Relative risk')
                                ),
                                column(
                                    width = 1,
                                    checkboxInput('output_rr', NULL, TRUE)
                                ),
                                column(5)
                            ),
                            fluidRow(
                                column(
                                    width = 12,
                                    checkboxGroupInput(
                                        'output_rr_group',
                                        label = NULL,
                                        choices = output_rr_group
                                    )
                                )
                            ),
                            fluidRow(
                                column(
                                    width = 6,
                                    tags$strong('Risk difference')
                                ),
                                column(
                                    width = 1,
                                    checkboxInput('output_rd', NULL, TRUE)
                                ),
                                column(5)
                            ),
                            fluidRow(
                                column(
                                    width = 12,
                                    checkboxGroupInput(
                                        'output_rd_group',
                                        label = NULL,
                                        choices = output_rd_group
                                    )
                                )
                            ),
                            tags$style(
                                type='text/css',
                                "#output_eair
                                {margin-top: -5px;
                                margin-left: -50px}"
                            ),
                            tags$style(
                                type='text/css',
                                "#output_eair_group
                                {margin-top: -35px;}"
                            ),
                            tags$style(
                                type='text/css',
                                "#output_rr
                                {margin-top: -5px;
                                margin-left: -50px}"
                            ),
                            tags$style(
                                type='text/css',
                                "#output_rr_group
                                {margin-top: -35px;}"
                            ),
                            tags$style(
                                type='text/css',
                                "#output_rd
                                {margin-top: -5px;
                                margin-left: -50px}"
                            ),
                            tags$style(
                                type='text/css',
                                "#output_rd_group
                                {margin-top: -35px;}"
                            )
                        ),
                        column(
                            width = 4,
                            fluidRow(
                                column(
                                    width = 6,
                                    tags$strong('Odds ratio')
                                ),
                                column(
                                    width = 1,
                                    checkboxInput('output_or', NULL, TRUE)
                                ),
                                column(5)
                            ),
                            fluidRow(
                                column(
                                    width = 12,
                                    checkboxGroupInput(
                                        'output_or_group',
                                        label = NULL,
                                        choices = output_or_group
                                    )
                                )
                            ),
                            fluidRow(
                                column(
                                    width = 6,
                                    tags$strong('Hazard ratio')
                                ),
                                column(
                                    width = 1,
                                    checkboxInput('output_hr', NULL, TRUE)
                                ),
                                column(5)
                            ),
                            fluidRow(
                                column(
                                    width = 12,
                                    checkboxGroupInput(
                                        'output_hr_group',
                                        label = NULL,
                                        choices = output_hr_group
                                    )
                                )
                            ),
                            fluidRow(
                                column(
                                    width = 6,
                                    tags$strong('DFDR')
                                ),
                                column(
                                    width = 1,
                                    checkboxInput('output_dfdr', NULL, TRUE)
                                ),
                                column(5)
                            ),
                            fluidRow(
                                column(
                                    width = 12,
                                    checkboxGroupInput(
                                        'output_dfdr_group',
                                        label = NULL,
                                        choices = output_dfdr_group
                                    )
                                )
                            ),
                            tags$style(
                                type='text/css',
                                "#output_or
                                {margin-top: -5px;
                                margin-left: -50px}"
                            ),
                            tags$style(
                                type='text/css',
                                "#output_or_group
                                {margin-top: -35px;}"
                            ),
                            tags$style(
                                type='text/css',
                                "#output_hr
                                {margin-top: -5px;
                                margin-left: -50px}"
                            ),
                            tags$style(
                                type='text/css',
                                "#output_hr_group
                                {margin-top: -35px;}"
                            ),
                            tags$style(
                                type='text/css',
                                "#output_dfdr
                                {margin-top: -5px;
                                margin-left: -50px}"
                            ),
                            tags$style(
                                type='text/css',
                                "#output_dfdr_group
                                {margin-top: -35px;}"
                            )
                        )
                    )
                ),
                tags$br(),
                uiOutput('output_table_download_button'),
                
                uiOutput('volplot_downloadpage_button'),
                shinyBS::bsModal(
                    'volplot_download_modal', 'Download page',
                    'volplot_downloadpage_button',
                    size = 'large',
                    fluidRow(
                        column(width = 2, uiOutput('volplot_download_format')),
                        column(width = 2, uiOutput('volplot_download_height')),
                        column(width = 2, uiOutput('volplot_download_width')),
                        column(width = 4, uiOutput('volplot_download_resolution')),
                        column(width = 2, uiOutput('volplot_download_button'))
                    ),
                    tags$style(
                        type='text/css',
                        "#volplot_download_button {margin-top: 25px;}"
                    )
                ),
                tags$br(),
                uiOutput('volplot_title'),
                uiOutput('volplot_xlab'),
                uiOutput('volplot_ylab'),
                uiOutput('volplot_hrefline'),
                uiOutput('volplot_vrefline'),
                uiOutput('volplot_label_size'),
                uiOutput('volplot_legend_font_size'),
                uiOutput('volplot_fn_in'),
                
                uiOutput('combplot_downloadpage_button'),
                shinyBS::bsModal(
                    'combplot_download_modal', 'Download page',
                    'combplot_downloadpage_button',
                    size = 'large',
                    fluidRow(
                        column(width = 2, uiOutput('combplot_download_format')),
                        column(width = 2, uiOutput('combplot_download_height')),
                        column(width = 2, uiOutput('combplot_download_width')),
                        column(width = 4, uiOutput('combplot_download_resolution')),
                        column(width = 2, uiOutput('combplot_download_button'))
                    ),
                    tags$style(
                        type='text/css',
                        "#combplot_download_button {margin-top: 25px;}"
                    )
                ),
                tags$br(),
                uiOutput('combplot_title_in'),
                uiOutput('combplot_top_n_ui'),
                uiOutput('combplot_ci_log'),
                uiOutput('combplot_xrange'),
                uiOutput('combplot_fn_in')
            )
        )
    ),
    
    #-----------------------------------------------
    # Document page
    #-----------------------------------------------
    tabPanel(
        title = 'Document',
        navlistPanel(
            widths = c(3, 9),
            tabPanel(
                title = 'Document',
                fluidRow(
                    column(width = 2),
                    column(
                        width = 8,
                        tags$h3('References'),
                        tags$br(),
                        tags$a(
                            paste0('Gilead Biostats Seminar: Safety Signal Detection in Clinical Trials',
                                   ' - by Wei Deng (28SEP17)'),
                            target = '_blank', 
                            href = 'Biostat Safety Signal Detection Stage 1 Report 28SEP17.pptx'
                        ),
                        tags$br(),
                        tags$br(),
                        tags$h3('User manual'),
                        tags$br(),
                        tags$a(
                            paste0('Safety Monitoring Tool',
                                   ' User Manual'),
                            target = '_blank', 
                            href = 'User Manual of Safety Monitoring R Shiny Application.pdf'
                        ),
                        tags$br(),
                        tags$br(),
                        tags$h3('FAQs'),
                        tags$br(),
                        tags$a(
                            paste0('FAQs of Safety Monitoring  Shiny Application'),
                            target = '_blank', 
                            href = 'FAQs of Safety Monitoring  Shiny Application.docx'
                        )
                    ),
                    column(width = 2)
                )
            )
        )
    ),
    
    shinyjs::useShinyjs(),
    windowTitle = 'Safety monitoring'
))


































