#---------------------#
# PK Exploratory tool #
#---------------------#


# global.R

#-----------------------------------------------
# Set up Shiny environment
#-----------------------------------------------

# for debugging warning messages
# options(warning.expression = quote(recover()))

# Set the heap size = 4GB to read possibly large xlsx data
options(java.parameters = '-Xmx4000m')

# By default, the file size limit is 5MB. Here we'll raise limit to 100MB
options(shiny.maxRequestSize = 100 * 1024^2)


#-----------------------------------------------
# Load necessary libraries and files
source('r_scripts/use_package.R')
use_package('shiny')
use_package('shinyjs')
use_package('shinyBS')
use_package('dplyr')
use_package('lazyeval')
use_package('DT')
use_package('ggplot2')
use_package('grid')
use_package('gtable')
use_package('lattice')
use_package('latticeExtra')
use_package('rtf')
install_('sas7bdat')
install_('reshape2')
install_('XLConnect')
install_('htmlTable')
install_('reshape2')
install_('data.table')
install_('gtools')
install_('scales')
# install_('openxlsx')
source('r_scripts/common_statistics.R')
source('r_scripts/shiny_related.R')
source('r_scripts/format_utils.R')
source('r_scripts/file_utils.R')
source('r_scripts/dataframe_utils.R')
source('r_scripts/table_utils.R')
source('r_scripts/plot_utils.R')
source('r_scripts/tnf_utils.R')
source('r_scripts/gg_lineplot.R')
source('r_scripts/gg_boxplot.R')
source('r_scripts/gg_barplot.R')
source('r_scripts/gg_gantt_chart.R')
source('r_scripts/xy_forestplot.R')
source('r_scripts/dual_axis_plot.R')
source('r_scripts/strings.R')
source('r_scripts/RTF2/RTF_functions.R')
source('r_scripts/RTF2/RTF_misc.R')



#-----------------------------------------------
# Define defaults
#-----------------------------------------------

group_number_levels <- 8
time_stamp <- format(Sys.time(), format = '%Y%m%d%H%M%S')
temp_dir <- tempdir()
dpi_default <- 600
all_shapes <- c(0L:25L, 33L:127L)
summary_title_dict <- list(
    'Mean + SD' = 'Mean (SD)',
    'Mean + SE' = 'Mean (SE)',
    'Median + IQR' = 'Median (Q1, Q3)'
)
n_sujects_per_panel <- 15

string_yes <- c('Yes', 'Y', 'yes', 'y', 'TRUE', 'T', 'True', 'true', 't')

default_footnote_lines <- function() {
    paste(
        paste('Date:', format(Sys.time(), format = '%d%b%Y %H:%M')),
        'Produced by PK Exploratory tool Version 1',
        sep = '\n'
    )
}

#-----------------------------------------------
# Import data page
#-----------------------------------------------

file_pk_param_template <- 'PK_Param_template.xls'
file_pd_uni_template <- 'Biom_univariate_template.xls'
file_pk_con_template <- 'PK_Conc_template.xls'
file_pd_mul_template <- 'Biom_multivariate_template.xls'
file_lab_template <- 'Lab_template.xls'
file_ae_template <- 'AE_template.xls'
file_enr_template <- 'enroll_template.xls'

file_import_success_msg <- 'The file is successfully imported:'
file_import_fail_msg <- 'Error! The file importation failed:'


#-----------------------------------------------
# Subject-level PKPD analysis
#-----------------------------------------------

# ----- PK -----
subj_pk_subj_col <- 'Subject'
subj_pk_cohort_col <- 'Cohort'
subj_pk_dose_col <- 'Dose'
subj_pk_visit_col <- 'Visit'
subj_pk_param_col <- 'Parameter'
subj_pk_unit_col <- 'Units'
subj_pk_estm_col <- 'Estimate'
subj_pk_required_cols <- c(
    subj_pk_subj_col, subj_pk_cohort_col, subj_pk_dose_col, subj_pk_visit_col,
    subj_pk_param_col, subj_pk_unit_col, subj_pk_estm_col
)

subj_pk_summary_opts <- c('Mean + SD', 'Mean + SE', 'Median + IQR')
subj_pk_tabnames <- c('Analysis data', 'Summary table', 'Box plot', 'Line plot',
                      'Dose proportionality')

# ----- PD -----
subj_pd_subj_col <- 'SUBJID'
subj_pd_cohort_col <- 'COHORT'
subj_pd_dose_col <- 'DOSE'
subj_pd_param_col <- 'PARAM'
subj_pd_avisitn_col <- 'AVISITN'
subj_pd_atptn_col <- 'ATPTN'
subj_pd_aval_col <- 'AVAL'
subj_pd_chg_col <- 'CHG'
subj_pd_pchg_col <- 'PCHG'
subj_pd_required_cols <- c(
    subj_pd_subj_col, subj_pd_cohort_col, subj_pd_dose_col, subj_pd_param_col,
    subj_pd_avisitn_col, subj_pd_atptn_col, subj_pd_aval_col, subj_pd_chg_col,
    subj_pd_pchg_col
)

subj_pd_analysis_types <- c('1 PD analysis', '2 PD analysis')

subj_pd_dose_y_types <- c(
    'Raw' = subj_pd_aval_col,
    'Change from baseline' = subj_pd_chg_col,
    'Percentage change from baseline' = subj_pd_pchg_col
)
subj_pd_dose_summary_opts <- subj_pk_summary_opts
subj_pd_dose_tabnames <- c('Analysis data', 'Summary table', 'Box plot', 'Line plot')
subj_pd_corr_summary_opts <- subj_pk_summary_opts
subj_pd_corr_tabnames <- c('Scatter plot', 'Forest plot', '2D Forest plot')

# ----- PK-PD -----
subj_pkpd_tabnames <- c('Scatter plot', 'Forest plot', '2D Forest plot',
                        'Quartile plot')
subj_pkpd_summary_opts <- c('Mean + SD', 'Mean + SE', 'Median + IQR')



#-----------------------------------------------
# Sample-level PKPD analysis
#-----------------------------------------------

# ----- PK -----
sample_pk_con_subj_col <- 'Subject'
sample_pk_con_cohort_col <- 'Cohort'
sample_pk_con_visit_col <- 'Visit'
sample_pk_con_con_col <- 'Concentration'
sample_pk_con_time_col <- 'Timepoint'
sample_pk_con_dose_col <- 'Dose'
sample_pk_con_required_cols <- c(
    sample_pk_con_subj_col, sample_pk_con_cohort_col, sample_pk_con_visit_col,
    sample_pk_con_con_col, sample_pk_con_time_col, sample_pk_con_dose_col
)
sample_pk_con_summary_opts <- c('Mean + SD', 'Mean + SE', 'Median + IQR')
sample_pk_con_tabnames <- c('Summary table', 'Summary line', 'Individual line')

sample_pk_param_subj_col <- 'Subject'
sample_pk_param_cohort_col <- 'Cohort'
sample_pk_param_dose_col <- 'Dose'
sample_pk_param_visit_col <- 'Visit'
sample_pk_param_param_col <- 'Parameter'
sample_pk_param_unit_col <- 'Units'
sample_pk_param_estm_col <- 'Estimate'
sample_pk_param_required_cols <- c(
    sample_pk_param_subj_col, sample_pk_param_cohort_col,
    sample_pk_param_dose_col, sample_pk_param_visit_col,
    sample_pk_param_param_col, sample_pk_param_unit_col,
    sample_pk_param_estm_col
)
sample_pk_param_summary_opts <- c('Mean + SD', 'Mean + SE', 'Median + IQR')
sample_pk_param_tabnames <- c('Summary table', 'Summary line', 'Individual line')

# ----- PD -----
sample_pd_subj_col <- 'SUBJID'
sample_pd_cohort_col <- 'COHORT'
sample_pd_dose_col <- 'DOSE'
sample_pd_param_col <- 'PARAM'
sample_pd_avisitn_col <- 'AVISITN'
sample_pd_atptn_col <- 'ATPTN'
sample_pd_aval_col <- 'AVAL'
sample_pd_chg_col <- 'CHG'
sample_pd_pchg_col <- 'PCHG'
sample_pd_required_cols <- c(
    sample_pd_subj_col, sample_pd_cohort_col, sample_pd_dose_col,
    sample_pd_param_col, sample_pd_avisitn_col, sample_pd_atptn_col,
    sample_pd_aval_col, sample_pd_chg_col, sample_pd_pchg_col
)

sample_pd_analysis_types <- c('1 PD analysis', '2 PD analysis')

sample_pd_dose_y_types <- c(
    'Raw' = sample_pd_aval_col,
    'Change from baseline' = sample_pd_chg_col,
    'Percentage change from baseline' = sample_pd_pchg_col
)

sample_pd_time_summary_opts <- c('Mean + SD', 'Mean + SE', 'Median + IQR')
sample_pd_time_tabnames <- c('Summary table', 'Summary line', 'Individual line')

sample_pd_corr_summary_opts <- c('Mean + SD', 'Mean + SE', 'Median + IQR')
sample_pd_corr_tabnames <- c('Summary line', 'Scatter plot')


# ----- PK-PD -----
sample_pkpd_summary_opts <- c('Mean + SD', 'Mean + SE', 'Median + IQR')
sample_pkpd_tabnames <- c('Summary line', 'Scatter plot', 'Quartile plot')

#-----------------------------------------------
# Lab and AE safety analysis
#-----------------------------------------------

lab_subj_col <- 'SUBJID'
lab_visitnum_col <- 'VISITNUM'
lab_testcd_col <- 'LBTESTCD'
lab_tstnam_col <- 'TSTNAM'
lab_rptresn_col <- 'RPTRESN'
lab_rptresc_col <- 'RPTRESC'
lab_toxgrg_col <- 'TOXGRG'
lab_dose_col <- 'Dose'
lab_visit_col <- 'VISIT'
lab_required_cols <- c(
    lab_subj_col, lab_visitnum_col, lab_testcd_col, lab_tstnam_col, lab_rptresn_col
)

lab_tabnames <- c('Summary table', 'Abnormality table', 'Summary line',
                  'Individual line')
lab_summary_opts <- subj_pk_summary_opts
lab_visit_to_remove <- 9999

ae_subj_col <- 'SUBJID'
ae_aeterm_col <- 'AETERM'
ae_aeserstd_col <- 'AESER_STD'
ae_aestdatyy_col <- 'AESTDAT_YY'
ae_aestdatmm_col <- 'AESTDAT_MM'
ae_aestdatdd_col <- 'AESTDAT_DD'
ae_aeendatyy_col <- 'AEENDAT_YY'
ae_aeendatmm_col <- 'AEENDAT_MM'
ae_aeendatdd_col <- 'AEENDAT_DD'
ae_aerelstd_col <- 'AEREL_STD'
ae_aerelprcstd_col<- 'AERELPRC_STD'
ae_aetoxgrstd_col <- 'AETOXGR_STD'
ae_mdrsoc_col <- 'MDRSOC'
ae_mdrpt_col <- 'MDRPT'
ae_required_cols <- c(
    ae_subj_col, ae_aeterm_col, ae_aeserstd_col, ae_aestdatyy_col,
    ae_aestdatmm_col, ae_aestdatdd_col, ae_aeendatyy_col, ae_aeendatmm_col,
    ae_aeendatdd_col, ae_aerelstd_col, ae_aerelprcstd_col, ae_aetoxgrstd_col,
    ae_mdrsoc_col, ae_mdrpt_col
)

enr_subj_col <- 'SUBJID'
enr_enroldatyy_col <- 'ENROLDAT_YY'
enr_enroldatmm_col <- 'ENROLDAT_MM'
enr_enroldatdd_col <- 'ENROLDAT_DD'
enr_required_cols <- c(
    enr_subj_col, enr_enroldatyy_col, enr_enroldatmm_col, enr_enroldatdd_col
)





#-----------------------------------------------
# Output TNF
#-----------------------------------------------

out_tnf_sheet_names <- c(
    'subj_pk' = 'Subject PK', 'subj_pd' = 'Subject PD',
    'subj_pkpd' = 'Subject PKPD',
    'sample_pk' = 'Sample PK', 'sample_pd' = 'Sample PD',
    'sample_pkpd' = 'Sample PKPD',
    'lab' = 'Lab'
)
out_tnf_template_file <- 'TNF_template.xls'
out_tnf_accepted_file_format <- c('.xls', '.xlsx')

#----------------------------
# Subject-level PKPD TNF

out_subj_pk_title <- 'TitleKey'
out_subj_pk_tflt <- 'TFLT'
out_subj_pk_tfln <- 'TFLN'
out_subj_pk_output <- 'Output'
out_subj_pk_pk_param <- 'PKParam'
out_subj_pk_x <- 'X'
out_subj_pk_x_type <- 'XType'
out_subj_pk_dose <- 'DoseLevels'
out_subj_pk_group <- 'Group'
out_subj_pk_decimal <- 'DecimalPlaces'
out_subj_pk_table_title <- 'TableTitle'
out_subj_pk_table_footnote <- 'TableFootnote'
out_subj_pk_log_y <- 'LogY'
out_subj_pk_add_points <- 'AddPoints'
out_subj_pk_summary <- 'SummaryMethod'
out_subj_pk_xlab <- 'Xlab'
out_subj_pk_ylab <- 'Ylab'
out_subj_pk_plot_title <- 'PlotTitle'
out_subj_pk_plot_footnote <- 'PlotFootnote'
out_subj_pk_cols <- c(
    out_subj_pk_title, out_subj_pk_tflt, out_subj_pk_tfln, out_subj_pk_output,
    out_subj_pk_pk_param, out_subj_pk_x, out_subj_pk_x_type, out_subj_pk_dose,
    out_subj_pk_group, out_subj_pk_decimal, out_subj_pk_table_title,
    out_subj_pk_table_footnote, out_subj_pk_log_y, out_subj_pk_add_points,
    out_subj_pk_summary, out_subj_pk_xlab, out_subj_pk_ylab,
    out_subj_pk_plot_title, out_subj_pk_plot_footnote
)

out_subj_pd_title <- 'TitleKey'
out_subj_pd_tflt <- 'TFLT'
out_subj_pd_tfln <- 'TFLN'
out_subj_pd_output <- 'Output'
out_subj_pd_pd_param1 <- 'PDParam1'
out_subj_pd_pd_value1 <- 'PDValue1'
out_subj_pd_pd_param2 <- 'PDParam2'
out_subj_pd_pd_value2 <- 'PDValue2'
out_subj_pd_x <- 'X'
out_subj_pd_x_type <- 'XType'
out_subj_pd_group <- 'Group'
out_subj_pd_decimal <- 'DecimalPlaces'
out_subj_pd_table_title <- 'TableTitle'
out_subj_pd_table_footnote <- 'TableFootnote'
out_subj_pd_log_x <- 'LogX'
out_subj_pd_log_y <- 'LogY'
out_subj_pd_add_points <- 'AddPoints'
out_subj_pd_refline <- 'ReferenceLine'
out_subj_pd_summary <- 'SummaryMethod'
out_subj_pd_same_y <- 'SameYaxis'
out_subj_pd_xlab <- 'Xlab'
out_subj_pd_left_ylab <- 'LeftYlab'
out_subj_pd_right_ylab <- 'RightYlab'
out_subj_pd_plot_title <- 'PlotTitle'
out_subj_pd_plot_footnote <- 'PlotFootnote'
out_subj_pd_cols <- c(
    out_subj_pd_title, out_subj_pd_tflt, out_subj_pd_tfln,
    out_subj_pd_output, out_subj_pd_pd_param1, out_subj_pd_pd_value1,
    out_subj_pd_pd_param2, out_subj_pd_pd_value2, out_subj_pd_x,
    out_subj_pd_x_type, out_subj_pd_group, out_subj_pd_decimal,
    out_subj_pd_table_title, out_subj_pd_table_footnote, out_subj_pd_log_x,
    out_subj_pd_log_y, out_subj_pd_add_points, out_subj_pd_refline,
    out_subj_pd_summary, out_subj_pd_same_y, out_subj_pd_xlab,
    out_subj_pd_left_ylab, out_subj_pd_right_ylab, out_subj_pd_plot_title,
    out_subj_pd_plot_footnote
)

out_subj_pkpd_title <- 'TitleKey'
out_subj_pkpd_tflt <- 'TFLT'
out_subj_pkpd_tfln <- 'TFLN'
out_subj_pkpd_output <- 'Output'
out_subj_pkpd_pk_param <- 'PKParam'
out_subj_pkpd_pd_param <- 'PDParam'
out_subj_pkpd_pd_value <- 'PDValue'
out_subj_pkpd_x <- 'X'
out_subj_pkpd_x_type <- 'XType'
out_subj_pkpd_group <- 'Group'
out_subj_pkpd_log_pk <- 'LogPK'
out_subj_pkpd_log_pd <- 'LogPD'
out_subj_pkpd_add_points <- 'AddPoints'
out_subj_pkpd_add_line <- 'AddLine'
out_subj_pkpd_refline <- 'ReferenceLine'
out_subj_pkpd_summary <- 'SummaryMethod'
out_subj_pkpd_same_y <- 'SameYaxis'
out_subj_pkpd_xlab <- 'Xlab'
out_subj_pkpd_left_ylab <- 'LeftYlab'
out_subj_pkpd_right_ylab <- 'RightYlab'
out_subj_pkpd_plot_title <- 'PlotTitle'
out_subj_pkpd_plot_footnote <- 'PlotFootnote'
out_subj_pkpd_cols <- c(
    out_subj_pkpd_title, out_subj_pkpd_tflt, out_subj_pkpd_tfln,
    out_subj_pkpd_output, out_subj_pkpd_pk_param, out_subj_pkpd_pd_param,
    out_subj_pkpd_pd_value, out_subj_pkpd_x, out_subj_pkpd_x_type,
    out_subj_pkpd_group, out_subj_pkpd_log_pk, out_subj_pkpd_log_pd,
    out_subj_pkpd_add_points, out_subj_pkpd_add_line, out_subj_pkpd_refline,
    out_subj_pkpd_summary, out_subj_pkpd_same_y, out_subj_pkpd_xlab,
    out_subj_pkpd_left_ylab, out_subj_pkpd_right_ylab, out_subj_pkpd_plot_title,
    out_subj_pkpd_plot_footnote
)


#----------------------------
# Sample-level PKPD TNF

out_sample_pk_title <- 'TitleKey'
out_sample_pk_tflt <- 'TFLT'
out_sample_pk_tfln <- 'TFLN'
out_sample_pk_output <- 'Output'
out_sample_pk_pk_type <- 'PKType'
out_sample_pk_pk_param <- 'PKParam'
out_sample_pk_visit <- 'Visit'
out_sample_pk_group <- 'Group'
out_sample_pk_decimal <- 'DecimalPlaces'
out_sample_pk_table_title <- 'TableTitle'
out_sample_pk_table_footnote <- 'TableFootnote'
out_sample_pk_statistics <- 'Statistics'
out_sample_pk_subjid <- 'SubjectID'
out_sample_pk_log_y <- 'LogY'
out_sample_pk_xlab <- 'Xlab'
out_sample_pk_ylab <- 'Ylab'
out_sample_pk_plot_title <- 'PlotTitle'
out_sample_pk_plot_footnote <- 'PlotFootnote'
out_sample_pk_cols <- c(
    out_sample_pk_title, out_sample_pk_tflt, out_sample_pk_tfln,
    out_sample_pk_output, out_sample_pk_pk_type, out_sample_pk_pk_param,
    out_sample_pk_visit, out_sample_pk_group, out_sample_pk_decimal,
    out_sample_pk_table_title, out_sample_pk_table_footnote,
    out_sample_pk_statistics, out_sample_pk_subjid, out_sample_pk_log_y,
    out_sample_pk_xlab, out_sample_pk_ylab, out_sample_pk_plot_title,
    out_sample_pk_plot_footnote
)

out_sample_pd_title <- 'TitleKey'
out_sample_pd_tflt <- 'TFLT'
out_sample_pd_tfln <- 'TFLN'
out_sample_pd_output <- 'Output'
out_sample_pd_param_1 <- 'PDParam1'
out_sample_pd_value_1 <- 'PDValue1'
out_sample_pd_param_2 <- 'PDParam2'
out_sample_pd_value_2 <- 'PDValue2'
out_sample_pd_x <- 'X'
out_sample_pd_visit <- 'Visit'
out_sample_pd_group <- 'Group'
out_sample_pd_decimal <- 'DecimalPlaces'
out_sample_pd_table_title <- 'TableTitle'
out_sample_pd_table_footnote <- 'TableFootnote'
out_sample_pd_statistics <- 'Statistics'
out_sample_pd_subjid <- 'SubjectID'
out_sample_pd_refline <- 'ReferenceLine'
out_sample_pd_log_pd_1 <- 'LogPD1'
out_sample_pd_log_pd_2 <- 'LogPD2'
out_sample_pd_same_y <- 'SameYaxis'
out_sample_pd_xlab <- 'Xlab'
out_sample_pd_left_y_lab <- 'LeftYlab'
out_sample_pd_right_y_lab <- 'RightYlab'
out_sample_pd_plot_title <- 'PlotTitle'
out_sample_pd_plot_footnote <- 'PlotFootnote'
out_sample_pd_cols <- c(
    out_sample_pd_title, out_sample_pd_tflt, out_sample_pd_tfln,
    out_sample_pd_output, out_sample_pd_param_1, out_sample_pd_value_1,
    out_sample_pd_param_2, out_sample_pd_value_2, out_sample_pd_x,
    out_sample_pd_visit, out_sample_pd_group, out_sample_pd_decimal,
    out_sample_pd_table_title, out_sample_pd_table_footnote,
    out_sample_pd_statistics, out_sample_pd_subjid, out_sample_pd_refline,
    out_sample_pd_log_pd_1, out_sample_pd_log_pd_2, out_sample_pd_same_y,
    out_sample_pd_xlab, out_sample_pd_left_y_lab, out_sample_pd_right_y_lab,
    out_sample_pd_plot_title, out_sample_pd_plot_footnote
)

out_sample_pkpd_title <- 'TitleKey'
out_sample_pkpd_tflt <- 'TFLT'
out_sample_pkpd_tfln <- 'TFLN'
out_sample_pkpd_output <- 'Output'
out_sample_pkpd_pk_type <- 'PKType'
out_sample_pkpd_pk_param <- 'PKParam'
out_sample_pkpd_pd_param <- 'PDParam'
out_sample_pkpd_pd_value <- 'PDValue'
out_sample_pkpd_x <- 'X'
out_sample_pkpd_visit <- 'Visit'
out_sample_pkpd_group <- 'Group'
out_sample_pkpd_statistics <- 'Statistics'
out_sample_pkpd_refline <- 'ReferenceLine'
out_sample_pkpd_add_points <- 'AddPoints'
out_sample_pkpd_add_line <- 'AddLine'
out_sample_pkpd_log_pk <- 'LogPK'
out_sample_pkpd_log_pd <- 'logPD'
out_sample_pkpd_same_y <- 'SameYaxis'
out_sample_pkpd_xlab <- 'Xlab'
out_sample_pkpd_left_y_lab <- 'LeftYlab'
out_sample_pkpd_right_y_lab <- 'RightYlab'
out_sample_pkpd_plot_title <- 'PlotTitle'
out_sample_pkpd_plot_footnote <- 'PlotFootnote'
out_sample_pkpd_cols <- c(
    out_sample_pkpd_title, out_sample_pkpd_tflt, out_sample_pkpd_tfln,
    out_sample_pkpd_output, out_sample_pkpd_pk_type, out_sample_pkpd_pk_param,
    out_sample_pkpd_pd_param, out_sample_pkpd_pd_value, out_sample_pkpd_x,
    out_sample_pkpd_visit, out_sample_pkpd_group, out_sample_pkpd_statistics,
    out_sample_pkpd_refline, out_sample_pkpd_add_points,
    out_sample_pkpd_add_line, out_sample_pkpd_log_pk, out_sample_pkpd_log_pd,
    out_sample_pkpd_same_y, out_sample_pkpd_xlab, out_sample_pkpd_left_y_lab,
    out_sample_pkpd_right_y_lab, out_sample_pkpd_plot_title,
    out_sample_pkpd_plot_footnote
)

#----------------------------
# Lab and AE safety TNF

out_lab_title <- 'TitleKey'
out_lab_tflt <- 'TFLT'
out_lab_tfln <- 'TFLN'
out_lab_output <- 'Output'
out_lab_analyte <- 'Analyte'
out_lab_group <- 'Group'
out_lab_decimal <- 'DecimalPlaces'
out_lab_table_title <- 'TableTitle'
out_lab_table_footnote <- 'TableFootnote'
out_lab_statistics <- 'Statistics'
out_lab_subjid <- 'SubjectID'
out_lab_add_points <- 'AddPoints'
out_lab_refline <- 'ReferenceLine'
out_lab_log_y <- 'LogY'
out_lab_xlab <- 'Xlab'
out_lab_ylab <- 'Ylab'
out_lab_plot_title <- 'PlotTitle'
out_lab_plot_footnote <- 'PlotFootnote'
lab_cols <- c(
    out_lab_title, out_lab_tflt, out_lab_tfln, out_lab_output, out_lab_analyte,
    out_lab_group, out_lab_decimal, out_lab_table_title, out_lab_table_footnote,
    out_lab_statistics, out_lab_subjid, out_lab_add_points, out_lab_refline,
    out_lab_log_y, out_lab_xlab, out_lab_ylab, out_lab_plot_title,
    out_lab_plot_footnote
)



#-----------------------------------------------
# Others
#-----------------------------------------------

#----------------------------
# Hint text
file_panel_subj_htext <- 'One subject should have only 1 data point for PK and/or PD.'
file_panel_sample_htext <- paste(
    'One subject should have multiple data points for',
    'PK and/or PD from various visits/timepoints.'
)
file_sample_pkpd_merge_by_htext <- paste(
    'If merge by Visit only is chosen, data for multiple timepoints (if any)',
    'for the same visit would be averaged.'
)
file_sample_pkpd_merge_htext <- paste(
    'It might take a while to complete the merge process.'
)
sample_pd_time_x_htext <- paste(
    'If AVISITN (for visit) is selected, data for the same visit would be',
    'treated equally (ignoring potential timepoint); if ATVTATPTN (for',
    'timepoint) is chosen, further visit information needs to be specified.'
)
sample_pd_time_visit_htext <- paste(
    'If multiple visits are selected, the output would be stacked'
)
subj_pkpd_line_htext <- paste(
    'If checked, mean values for each group would be marked by triangles and',
    'connected by line.'
)
sample_pkpd_line_htext <- subj_pkpd_line_htext

#----------------------------
# Documents
user_manual <- 'UserManual.doc'




















