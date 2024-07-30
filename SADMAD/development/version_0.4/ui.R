# ui.R

shinyUI(navbarPage(
    
    title = 'PK Exploratory Analysis',
    
    #-----------------------------------------------
    # Import data page
    #-----------------------------------------------
    tabPanel(
        title = 'Import data',
        fluidRow(
            column(
                width = 4,
                shinyBS::bsCollapse(
                    id = 'file_panel', open = 'Subject-level PKPD analysis data',
                    shinyBS::bsCollapsePanel(
                        title = tags$html(
                            'Subject-level PKPD analysis data',
                            tags$style(type = 'text/css',
                                       '#qfile_panel_subj {vertical-align: top;}'),
                            shinyBS::bsButton('qfile_panel_subj', label = '',
                                              icon = icon('question'),
                                              style = 'info',
                                              size = 'extra-small')
                        ),
                        tags$div(
                            id = 'file_subj',
                            fileInput(
                                'file_subj_pk_param',
                                tags$p('PK parameter data',
                                       tags$a('(Need template?)',
                                              target = '_blank',
                                              href = file_pk_param_template))
                            ),
                            tags$hr(),
                            fileInput(
                                'file_subj_pd',
                                tags$p('PD data',
                                       tags$a('(Need template?)',
                                              target = '_blank',
                                              href = file_pd_uni_template))
                            ),
                            tags$hr(),
                            fluidRow(
                                column(4, uiOutput('file_subj_pkpd_merge')),
                                column(8, uiOutput('file_subj_pkpd_download_button'))
                            )
                        ),
                        value = 'Subject-level PKPD analysis data'
                    ),
                    shinyBS::bsCollapsePanel(
                        title = tags$html(
                            'Sample-level PKPD analysis data',
                            tags$style(type = 'text/css',
                                       '#qfile_panel_sample {vertical-align: top;}'),
                            shinyBS::bsButton('qfile_panel_sample', label = '',
                                              icon = icon('question'),
                                              style = 'info',
                                              size = 'extra-small')
                        ),
                        tags$div(
                            id = 'file_sample',
                            radioButtons(
                                'file_sample_pk_type', 'Choose PK data to upload',
                                c('PK parameter', 'PK concentration'),
                                selected = character(0), inline = TRUE
                            ),
                            uiOutput('file_sample_pk_con_ui'),
                            uiOutput('file_sample_pk_param_ui'),
                            tags$hr(),
                            fileInput(
                                'file_sample_pd',
                                tags$p('PD data',
                                       tags$a('(Need template?)',
                                              target = '_blank',
                                              href = file_pd_mul_template))
                            ),
                            tags$hr(),
                            uiOutput('file_sample_pkpd_merge_by'),
                            fluidRow(
                                column(5, uiOutput('file_sample_pkpd_merge')),
                                column(7, uiOutput('file_sample_pkpd_download_button'))
                            )
                        ),
                        value = 'Sample-level PKPD analysis data'
                    ),
                    shinyBS::bsCollapsePanel(
                        title = 'Lab and AE safety analysis data',
                        tags$div(
                            id = 'file_lab',
                            fileInput(
                                'file_lab',
                                tags$p('Lab data',
                                       tags$a('(Need template?)',
                                              target = '_blank',
                                              href = file_lab_template))
                            )
                        ),
                        tags$hr(),
                        tags$div(
                            id = 'file_ae',
                            fileInput(
                                'file_ae',
                                tags$p('AE data',
                                       tags$a('(Need template?)',
                                              target = '_blank',
                                              href = file_ae_template))
                            ),
                            fileInput(
                                'file_enr',
                                tags$p('Enrollment data',
                                       tags$a('(Need template?)',
                                              target = '_blank',
                                              href = file_enr_template))
                            )
                            # uiOutput('file_ae_enr_merge'),
                            # uiOutput('file_ae_enr_download_button')
                        ),
                        value = 'Lab and AE safety analysis data'
                    )
                ),
                shinyBS::bsTooltip(
                    'qfile_panel_subj', file_panel_subj_htext,
                    placement = 'right', trigger = 'hover',
                    options = list(container = 'body')
                ),
                shinyBS::bsTooltip(
                    'qfile_panel_sample', file_panel_sample_htext,
                    placement = 'right', trigger = 'hover',
                    options = list(container = 'body')
                )
            ),
            column(
                width = 8,
                uiOutput('file_tabpanel')
            )
        )
    ),
    
    shinyjs::useShinyjs(),
    
    
    #-----------------------------------------------
    # 1.	Subject-level PKPD analysis
    #-----------------------------------------------
    tabPanel(
        title = 'Subject-level PKPD analysis',
        fluidRow(
            column(
                width = 3,
                shinyBS::bsCollapse(
                    id = 'subj_panel', open = NULL,
                    shinyBS::bsCollapsePanel(
                        title = 'PK analysis',
                        tags$div(
                            id = 'subj_pk',
                            uiOutput('subj_pk_param'),
                            uiOutput('subj_pk_x'),
                            uiOutput('subj_pk_x_type'),
                            uiOutput('subj_pk_dose_level'),
                            uiOutput('subj_pk_group'),
                            uiOutput('subj_pk_summary'),
                            fluidRow(
                                column(width = 6, uiOutput('subj_pk_log_y')),
                                column(width = 6, uiOutput('subj_pk_points'))
                            ),
                            uiOutput('subj_pk_decimal'),
                            uiOutput('subj_pk_projected_dose'),
                            uiOutput('subj_pk_expected_pk_text'),
                            textOutput('subj_pk_expected_pk_value')
                        )
                    ),
                    shinyBS::bsCollapsePanel(
                        title = 'PD analysis',
                        uiOutput('subj_pd_analysis_type'),
                        tags$div(
                            id = 'subj_pd_dose',
                            uiOutput('subj_pd_dose_param'),
                            uiOutput('subj_pd_dose_y'),
                            uiOutput('subj_pd_dose_x'),
                            uiOutput('subj_pd_dose_x_type'),
                            uiOutput('subj_pd_dose_group'),
                            uiOutput('subj_pd_dose_summary'),
                            fluidRow(
                                column(width = 6, uiOutput('subj_pd_dose_log_y')),
                                column(width = 6, uiOutput('subj_pd_dose_points'))
                            ),
                            uiOutput('subj_pd_dose_decimal')
                        ),
                        tags$div(
                            id = 'subj_pd_corr',
                            uiOutput('subj_pd_corr_param1'),
                            uiOutput('subj_pd_corr_y1'),
                            uiOutput('subj_pd_corr_param2'),
                            uiOutput('subj_pd_corr_y2'),
                            uiOutput('subj_pd_corr_x'),
                            uiOutput('subj_pd_corr_x_type'),
                            uiOutput('subj_pd_corr_group'),
                            uiOutput('subj_pd_corr_summary'),
                            uiOutput('subj_pd_corr_refline'),
                            fluidRow(
                                column(width = 6, uiOutput('subj_pd_corr_log_1')),
                                column(width = 6, uiOutput('subj_pd_corr_log_2'))
                            )
                        )
                    ),
                    shinyBS::bsCollapsePanel(
                        title = 'PK - PD analysis',
                        tags$div(
                            id = 'subj_pkpd',
                            uiOutput('subj_pkpd_pk'),
                            uiOutput('subj_pkpd_pd'),
                            uiOutput('subj_pkpd_pd_y'),
                            uiOutput('subj_pkpd_x'),
                            uiOutput('subj_pkpd_x_type'),
                            uiOutput('subj_pkpd_group'),
                            uiOutput('subj_pkpd_summary'),
                            uiOutput('subj_pkpd_refline'),
                            fluidRow(
                                column(width = 6, uiOutput('subj_pkpd_points')),
                                column(width = 6, uiOutput('subj_pkpd_line'))
                            ),
                            fluidRow(
                                column(width = 6, uiOutput('subj_pkpd_log_pk')),
                                column(width = 6, uiOutput('subj_pkpd_log_pd'))
                            )
                        )
                    )
                )
            ),
            column(
                width = 7,
                fluidRow(
                    column(3, uiOutput('subj_add_to_download')),
                    column(3, uiOutput('subj_download_button'), offset = 6)
                ),
                tags$br(),
                uiOutput('subj_tabpanel')
            ),
            column(
                width = 2,
                
                # add to tnf button
                uiOutput('subj_pk_table_add_to_tnf'),
                uiOutput('subj_pk_boxplot_add_to_tnf'),
                uiOutput('subj_pk_lineplot_add_to_tnf'),
                uiOutput('subj_pk_doseprop_add_to_tnf'),
                uiOutput('subj_pd_dose_table_add_to_tnf'),
                uiOutput('subj_pd_dose_boxplot_add_to_tnf'),
                uiOutput('subj_pd_dose_lineplot_add_to_tnf'),
                uiOutput('subj_pd_corr_scatter_add_to_tnf'),
                uiOutput('subj_pd_corr_forest_add_to_tnf'),
                uiOutput('subj_pd_corr_2dforest_add_to_tnf'),
                uiOutput('subj_pkpd_scatter_add_to_tnf'),
                uiOutput('subj_pkpd_forest_add_to_tnf'),
                uiOutput('subj_pkpd_2dforest_add_to_tnf'),
                uiOutput('subj_pkpd_quartile_add_to_tnf'),
                tags$br(),
                
                uiOutput('subj_pk_boxplot_xlab'),
                uiOutput('subj_pk_boxplot_ylab'),
                uiOutput('subj_pk_boxplot_main'),
                uiOutput('subj_pk_boxplot_footnote'),
                uiOutput('subj_pk_lineplot_xlab'),
                uiOutput('subj_pk_lineplot_ylab'),
                uiOutput('subj_pk_lineplot_main'),
                uiOutput('subj_pk_lineplot_footnote'),
                uiOutput('subj_pk_doseprop_xlab'),
                uiOutput('subj_pk_doseprop_ylab'),
                uiOutput('subj_pk_doseprop_main'),
                uiOutput('subj_pk_doseprop_footnote'),
                uiOutput('subj_pd_dose_boxplot_xlab'),
                uiOutput('subj_pd_dose_boxplot_ylab'),
                uiOutput('subj_pd_dose_boxplot_main'),
                uiOutput('subj_pd_dose_boxplot_footnote'),
                uiOutput('subj_pd_dose_lineplot_xlab'),
                uiOutput('subj_pd_dose_lineplot_ylab'),
                uiOutput('subj_pd_dose_lineplot_main'),
                uiOutput('subj_pd_dose_lineplot_footnote'),
                uiOutput('subj_pd_corr_scatterplot_xlab'),
                uiOutput('subj_pd_corr_scatterplot_ylab'),
                uiOutput('subj_pd_corr_scatterplot_main'),
                uiOutput('subj_pd_corr_scatterplot_footnote'),
                uiOutput('subj_pd_corr_forestplot_xlab'),
                uiOutput('subj_pd_corr_forestplot_ylab1'),
                uiOutput('subj_pd_corr_forestplot_ylab2'),
                uiOutput('subj_pd_corr_forestplot_main'),
                uiOutput('subj_pd_corr_forestplot_footnote'),
                uiOutput('subj_pd_corr_forestplot_same_y'),
                uiOutput('subj_pd_corr_forestplot2d_xlab'),
                uiOutput('subj_pd_corr_forestplot2d_ylab'),
                uiOutput('subj_pd_corr_forestplot2d_main'),
                uiOutput('subj_pd_corr_forestplot2d_footnote'),
                uiOutput('subj_pkpd_scatterplot_xlab'),
                uiOutput('subj_pkpd_scatterplot_ylab'),
                uiOutput('subj_pkpd_scatterplot_main'),
                uiOutput('subj_pkpd_scatterplot_footnote'),
                uiOutput('subj_pkpd_forestplot_xlab'),
                uiOutput('subj_pkpd_forestplot_ylab1'),
                uiOutput('subj_pkpd_forestplot_ylab2'),
                uiOutput('subj_pkpd_forestplot_main'),
                uiOutput('subj_pkpd_forestplot_footnote'),
                uiOutput('subj_pkpd_forestplot_same_y'),
                uiOutput('subj_pkpd_forestplot2d_xlab'),
                uiOutput('subj_pkpd_forestplot2d_ylab'),
                uiOutput('subj_pkpd_forestplot2d_main'),
                uiOutput('subj_pkpd_forestplot2d_footnote'),
                uiOutput('subj_pkpd_quartileplot_xlab'),
                uiOutput('subj_pkpd_quartileplot_ylab'),
                uiOutput('subj_pkpd_quartileplot_main'),
                uiOutput('subj_pkpd_quartileplot_footnote'),
                uiOutput('subj_pk_table_title'),
                uiOutput('subj_pk_table_footnote'),
                uiOutput('subj_pd_dose_table_title'),
                uiOutput('subj_pd_dose_table_footnote'),
                
                # download buttons
                uiOutput('subj_pk_table_download_button'),
                uiOutput('subj_pk_boxplot_download_button'),
                uiOutput('subj_pk_lineplot_download_button'),
                uiOutput('subj_pk_doseprop_download_button'),
                uiOutput('subj_pd_dose_table_download_button'),
                uiOutput('subj_pd_dose_boxplot_download_button'),
                uiOutput('subj_pd_dose_lineplot_download_button'),
                uiOutput('subj_pd_corr_scatterplot_download_button'),
                uiOutput('subj_pd_corr_forestplot_download_button'),
                uiOutput('subj_pd_corr_forestplot2d_download_button'),
                uiOutput('subj_pkpd_scatterplot_download_button'),
                uiOutput('subj_pkpd_forestplot_download_button'),
                uiOutput('subj_pkpd_forestplot2d_download_button'),
                uiOutput('subj_pkpd_quartileplot_download_button')
            )
        )
    ),
    
    
    #-----------------------------------------------
    # 2.	Sample-level PKPD analysis
    #-----------------------------------------------
    tabPanel(
        title = 'Sample-level PKPD analysis',
        fluidRow(
            column(
                width = 3,
                shinyBS::bsCollapse(
                    id = 'sample_panel', open = NULL,
                    shinyBS::bsCollapsePanel(
                        title = 'PK analysis',
                        tags$div(
                            id = 'sample_pk_con',
                            uiOutput('sample_pk_con_visit'),
                            uiOutput('sample_pk_con_group'),
                            uiOutput('sample_pk_con_summary'),
                            uiOutput('sample_pk_con_subjid'),
                            uiOutput('sample_pk_con_decimal'),
                            uiOutput('sample_pk_con_log')
                        ),
                        tags$div(
                            id = 'sample_pk_param',
                            uiOutput('sample_pk_param_param'),
                            uiOutput('sample_pk_param_group'),
                            uiOutput('sample_pk_param_summary'),
                            uiOutput('sample_pk_param_subjid'),
                            uiOutput('sample_pk_param_decimal'),
                            uiOutput('sample_pk_param_log')
                        )
                    ),
                    shinyBS::bsCollapsePanel(
                        title = 'PD analysis',
                        uiOutput('sample_pd_analysis_type'),
                        tags$div(
                            id = 'sample_pd_time',
                            uiOutput('sample_pd_time_param'),
                            uiOutput('sample_pd_time_y'),
                            uiOutput('sample_pd_time_x'),
                            uiOutput('sample_pd_time_visit'),
                            uiOutput('sample_pd_time_group'),
                            uiOutput('sample_pd_time_summary'),
                            uiOutput('sample_pd_time_subjid'),
                            uiOutput('sample_pd_time_decimal'),
                            uiOutput('sample_pd_time_log')
                        ),
                        tags$div(
                            id = 'sample_pd_corr',
                            uiOutput('sample_pd_corr_param1'),
                            uiOutput('sample_pd_corr_y1'),
                            uiOutput('sample_pd_corr_param2'),
                            uiOutput('sample_pd_corr_y2'),
                            uiOutput('sample_pd_corr_x'),
                            uiOutput('sample_pd_corr_visit'),
                            uiOutput('sample_pd_corr_group'),
                            uiOutput('sample_pd_corr_summary'),
                            uiOutput('sample_pd_corr_refline'),
                            fluidRow(
                                column(width = 6, uiOutput('sample_pd_corr_log_1')),
                                column(width = 6, uiOutput('sample_pd_corr_log_2'))
                            )
                        )
                    ),
                    shinyBS::bsCollapsePanel(
                        title = 'PK-PD analysis',
                        tags$div(
                            id = 'sample_pkpd',
                            uiOutput('sample_pkpd_pk'),
                            uiOutput('sample_pkpd_pd'),
                            uiOutput('sample_pkpd_pd_y'),
                            uiOutput('sample_pkpd_x'),
                            uiOutput('sample_pkpd_visit'),
                            uiOutput('sample_pkpd_visit_scatter'),
                            uiOutput('sample_pkpd_group'),
                            uiOutput('sample_pkpd_summary'),
                            uiOutput('sample_pkpd_refline'),
                            fluidRow(
                                column(width = 6, uiOutput('sample_pkpd_add_points')),
                                column(width = 6, uiOutput('sample_pkpd_add_line'))
                            ),
                            fluidRow(
                                column(width = 6, uiOutput('sample_pkpd_log_pk')),
                                column(width = 6, uiOutput('sample_pkpd_log_pd'))
                            )
                        )
                    )
                )
            ),
            column(
                width = 7,
                fluidRow(
                    column(3, uiOutput('sample_add_to_download')),
                    column(3, uiOutput('sample_download_button'), offset = 6)
                ),
                tags$br(),
                uiOutput('sample_tabpanel')
            ),
            column(
                width = 2,
                
                # add to 'TNF' button
                uiOutput('sample_pk_con_sumtable_att'),
                uiOutput('sample_pk_con_sumline_att'),
                uiOutput('sample_pk_con_indline_att'),
                uiOutput('sample_pk_param_sumtable_att'),
                uiOutput('sample_pk_param_sumline_att'),
                uiOutput('sample_pk_param_indline_att'),
                uiOutput('sample_pd_time_sumtable_att'),
                uiOutput('sample_pd_time_sumline_att'),
                uiOutput('sample_pd_time_indline_att'),
                uiOutput('sample_pd_corr_sumline_att'),
                uiOutput('sample_pd_corr_scatter_att'),
                uiOutput('sample_pkpd_sumline_att'),
                uiOutput('sample_pkpd_scatter_att'),
                uiOutput('sample_pkpd_quartile_att'),
                tags$br(),
                
                uiOutput('sample_pk_con_table_title'),
                uiOutput('sample_pk_con_table_footnote'),
                uiOutput('sample_pk_con_sumline_xlab'),
                uiOutput('sample_pk_con_sumline_ylab'),
                uiOutput('sample_pk_con_sumline_main'),
                uiOutput('sample_pk_con_sumline_footnote'),
                uiOutput('sample_pk_con_indline_xlab'),
                uiOutput('sample_pk_con_indline_ylab'),
                uiOutput('sample_pk_con_indline_main'),
                uiOutput('sample_pk_con_indline_footnote'),
                uiOutput('sample_pk_param_table_title'),
                uiOutput('sample_pk_param_table_footnote'),
                uiOutput('sample_pk_param_sumline_xlab'),
                uiOutput('sample_pk_param_sumline_ylab'),
                uiOutput('sample_pk_param_sumline_main'),
                uiOutput('sample_pk_param_sumline_footnote'),
                uiOutput('sample_pk_param_indline_xlab'),
                uiOutput('sample_pk_param_indline_ylab'),
                uiOutput('sample_pk_param_indline_main'),
                uiOutput('sample_pk_param_indline_footnote'),
                uiOutput('sample_pd_time_table_title'),
                uiOutput('sample_pd_time_table_footnote'),
                uiOutput('sample_pd_time_sumline_xlab'),
                uiOutput('sample_pd_time_sumline_ylab'),
                uiOutput('sample_pd_time_sumline_main'),
                uiOutput('sample_pd_time_sumline_footnote'),
                uiOutput('sample_pd_time_indline_xlab'),
                uiOutput('sample_pd_time_indline_ylab'),
                uiOutput('sample_pd_time_indline_main'),
                uiOutput('sample_pd_time_indline_footnote'),
                uiOutput('sample_pd_corr_sumline_xlab'),
                uiOutput('sample_pd_corr_sumline_ylabl'),
                uiOutput('sample_pd_corr_sumline_ylabr'),
                uiOutput('sample_pd_corr_sumline_main'),
                uiOutput('sample_pd_corr_sumline_footnote'),
                uiOutput('sample_pd_corr_sumline_same_y'),
                uiOutput('sample_pd_corr_scatter_xlab'),
                uiOutput('sample_pd_corr_scatter_ylab'),
                uiOutput('sample_pd_corr_scatter_main'),
                uiOutput('sample_pd_corr_scatter_footnote'),
                uiOutput('sample_pkpd_sumline_xlab'),
                uiOutput('sample_pkpd_sumline_ylabl'),
                uiOutput('sample_pkpd_sumline_ylabr'),
                uiOutput('sample_pkpd_sumline_main'),
                uiOutput('sample_pkpd_sumline_footnote'),
                uiOutput('sample_pkpd_sumline_same_y'),
                uiOutput('sample_pkpd_scatter_xlab'),
                uiOutput('sample_pkpd_scatter_ylab'),
                uiOutput('sample_pkpd_scatter_main'),
                uiOutput('sample_pkpd_scatter_footnote'),
                uiOutput('sample_pkpd_quartile_xlab'),
                uiOutput('sample_pkpd_quartile_ylab'),
                uiOutput('sample_pkpd_quartile_main'),
                uiOutput('sample_pkpd_quartile_footnote'),
                
                # download buttons
                uiOutput('sample_pk_con_table_download_button'),
                uiOutput('sample_pk_con_sumline_download_button'),
                uiOutput('sample_pk_con_indline_download_button'),
                uiOutput('sample_pk_param_sumline_download_button'),
                uiOutput('sample_pk_param_indline_download_button'),
                uiOutput('sample_pk_param_table_download_button'),
                uiOutput('sample_pd_time_table_download_button'),
                uiOutput('sample_pd_time_sumline_download_button'),
                uiOutput('sample_pd_time_indline_download_button'),
                uiOutput('sample_pd_corr_sumline_download_button'),
                uiOutput('sample_pd_corr_scatter_download_button'),
                uiOutput('sample_pkpd_sumline_download_button'),
                uiOutput('sample_pkpd_scatter_download_button'),
                uiOutput('sample_pkpd_quartile_download_button')
            )
        )
    ),
    
    #-----------------------------------------------
    # 3.	Lab and AE safety analysis
    #-----------------------------------------------
    tabPanel(
        title = 'Lab and AE safety analysis',
        fluidRow(
            column(
                width = 3,
                shinyBS::bsCollapse(
                    id = 'lab_ae_panel', open = NULL,
                    shinyBS::bsCollapsePanel(
                        title = 'Lab data  time profile',
                        tags$div(
                            id = 'lab',
                            uiOutput('lab_analyte'),
                            uiOutput('lab_group'),
                            uiOutput('lab_summary'),
                            uiOutput('lab_subjid'),
                            fluidRow(
                                column(width = 6, uiOutput('lab_log_y')),
                                column(width = 6, uiOutput('lab_points'))
                            ),
                            uiOutput('lab_decimal')
                        )
                    ),
                    shinyBS::bsCollapsePanel(
                        title = 'AE analysis',
                        tags$div(
                            id = 'ae',
                            uiOutput('ae_soc'),
                            uiOutput('ae_pt'),
                            uiOutput('ae_subject'),
                            uiOutput('ae_group'),
                            uiOutput('ae_barplot_rank')
                        )
                    )
                )
            ),
            column(
                width = 7,
                fluidRow(
                    column(3, uiOutput('lab_add_to_download')),
                    column(3, uiOutput('lab_download_button'), offset = 6)
                ),
                tags$br(),
                uiOutput('lab_tabpanel')
            ),
            column(
                width = 2,
                
                # Add to TNF buttons
                uiOutput('lab_table_att'),
                uiOutput('lab_abntable_att'),
                uiOutput('lab_summary_lineplot_att'),
                uiOutput('lab_individual_lineplot_att'),
                
                uiOutput('ae_barplot_soc_att'),
                uiOutput('ae_barplot_pt_att'),
                uiOutput('ae_ganttchart_ae_att'),
                uiOutput('ae_ganttchart_subj_att'),
                
                uiOutput('lab_sumplot_xlab'),
                uiOutput('lab_sumplot_ylab'),
                uiOutput('lab_sumplot_main'),
                uiOutput('lab_sumplot_footnote'),
                uiOutput('lab_indplot_xlab'),
                uiOutput('lab_indplot_ylab'),
                uiOutput('lab_indplot_main'),
                uiOutput('lab_indplot_footnote'),
                uiOutput('lab_refline'),
                uiOutput('lab_table_title'),
                uiOutput('lab_table_footnote'),
                uiOutput('lab_table_overall_col'),
                uiOutput('lab_abntable_title'),
                uiOutput('lab_abntable_footnote'),
                
                uiOutput('ae_barplot_soc_xlab'),
                uiOutput('ae_barplot_soc_ylab'),
                uiOutput('ae_barplot_soc_main'),
                uiOutput('ae_barplot_soc_footnote'),
                uiOutput('ae_barplot_pt_xlab'),
                uiOutput('ae_barplot_pt_ylab'),
                uiOutput('ae_barplot_pt_main'),
                uiOutput('ae_barplot_pt_footnote'),
                uiOutput('ae_ganttchart_ae_xlab'),
                uiOutput('ae_ganttchart_ae_ylab'),
                uiOutput('ae_ganttchart_ae_main'),
                uiOutput('ae_ganttchart_ae_footnote'),
                uiOutput('ae_ganttchart_subj_xlab'),
                uiOutput('ae_ganttchart_subj_ylab'),
                uiOutput('ae_ganttchart_subj_main'),
                uiOutput('ae_ganttchart_subj_footnote'),
                
                # download buttons
                uiOutput('lab_sumplot_download_button'),
                uiOutput('lab_indplot_download_button'),
                uiOutput('lab_table_download_button'),
                uiOutput('lab_abntable_download_button'),
                
                uiOutput('ae_barplot_soc_download_button'),
                uiOutput('ae_barplot_pt_download_button'),
                uiOutput('ae_ganttchart_ae_download_button'),
                uiOutput('ae_ganttchart_subj_download_button'),
                
                # tooltips
                shinyBS::bsTooltip(
                    'lab_refline',
                    paste0('Enter reference line position.',
                           'Multiple line positions separated by comma, ',
                           'e.g. 1, 2')
                )
            )
        )
    ),
    
    
    
    #-----------------------------------------------
    # 4.	Output TFLs
    #-----------------------------------------------
    
    tabPanel(
        title = 'Output TFLs',
        sidebarLayout(
            sidebarPanel(
                tags$h3('Upload TNF file'),
                uiOutput('out_tnf_file')
            ),
            mainPanel(
                fluidRow(
                    column(width = 3, uiOutput('out_process_tnf')),
                    column(width = 6),
                    column(width = 3, uiOutput('out_tfl_download_button'))
                ),
                tags$hr(),
                uiOutput('out_tnf_download_button'),
                tags$br(),
                uiOutput('out_tnf_tabpanel')
            )
        )
    ),
    
    #-----------------------------------------------
    # 5.	Documents
    #-----------------------------------------------
    
    tabPanel(
        title = 'Documents',
        fluidRow(
            column(3),
            column(6,
                   tags$p(style = 'font-size:175%;',
                          tags$br(), tags$br(), tags$br(), tags$br(), tags$br(),
                          'Please refer to ',
                          tags$a(tags$b('user manual'), target = '_blank',
                                 href = user_manual),
                          ' for more details on how to use the app. Thank you!'
                   )
            ),
            column(3)
        )
    )

))






















