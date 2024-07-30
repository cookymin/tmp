#-----------------------------#
# Safety monitoring shiny app #
#-----------------------------#

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
use_package('shinyBS')
use_package('dplyr')
use_package('DT')
use_package('ggplot2')
use_package('survival')
# use_package('ggrepel')
# use_package('grid')
# use_package('gridExtra')
use_package('cowplot')
use_package('rjags')
install_('sas7bdat')
install_('XLConnect')
install_('lazyeval')
source('r_scripts/common_statistics.R')
source('r_scripts/shiny_related.R')
source('r_scripts/strings.R')
source('r_scripts/stat_utils.R')
source('r_scripts/file_utils.R')
source('r_scripts/dataframe_utils.R')
source('r_scripts/DFDR.R')
source('r_scripts/run_BHMM.R')
source('r_scripts/plot_utils.R')
source('r_scripts/gg_volcano.R')
source('r_scripts/gg_dotplot.R')
source('r_scripts/gg_forestplot.R')
poisson_model_file <- 'r_scripts/BHMM_poisson_model1.bug'
logistic_model_file <- 'r_scripts/BHMM_logis_model1.bug'

#-----------------------------------------------
# Data import
#-----------------------------------------------

file_import_success_msg <- 'The file is successfully imported:'
file_import_fail_msg <- 'Error! The file importation failed:'
temp_dir <- tempdir()

file_adsl_template <- 'adsl_ex.sas7bdat'
file_adsl_naming_template <- 'adsl_naming_example.xlsx'
file_adae_template <- 'adae_ex.sas7bdat'
file_adae_naming_template <- 'adae_naming_example.xlsx'

adsl_naming_required_cols <- c(name = 'Name', description = 'Description')
adsl_naming_dscrps <- c(
    'Subject ID', 'Actual treatment received',
    'First dose date of study drug', 'Last dose date of study drug',
    'Safety Analysis Set Flag'
)
adae_naming_required_cols <- c(name = 'Name', description = 'Description')
adae_naming_dscrps <- c(
    'Subject ID', 'Actual treatment received', 'SOC', 'PT',
    'AE onset date', 'AE end date', 'Toxicity grade', 'Flag for SAE',
    'High level group term', 'High level term',
    'Treatment Emergent Analysis Flag'
)
# data_adsl_required_cols <- c(
#     'USUBJID', 'SAFFL', 'TRT01A', 'TRT01AN', 'TRTSDT', 'TRTEDT'
# )
# data_adae_required_cols <- c(
#     'USUBJID', 'TRT01A', 'TRT01AN', 'AEBODSYS', 'AEDECOD', 'ASTDT',
#     'AENDT', 'AETOXGRN', 'TRTEMFL', 'AESER'
# )

file_subset_num_cond <- 10L


#-----------------------------------------------
# Safety analysis
#-----------------------------------------------
soc_label_dict <- c(
    'blood and lymphatic system disorders' = 'Blood',
    'cardiac disorders' = 'Heart',
    'congenital, familial and genetic disorders' = 'Congenital',
    'ear and labyrinth disorders' = 'Ear',
    'endocrine disorders' = 'Endocrine',
    'eye disorders' = 'Eye',
    'gastrointestinal disorders' = 'GI',
    'general disorders and administration site conditions' = 'General',
    'hepatobiliary disorders' = 'Liver',
    'immune system disorders' = 'Immune',
    'infections and infestations' = 'Infection',
    'injury, poisoning and procedural complications' = 'Injury',
    'investigations' = 'Investigation',
    'metabolism and nutrition disorders' = 'Metab',
    'musculoskeletal and connective tissue disorders' = 'Bone',
    'neoplasms benign, malignant and unspecified (incl cysts and polyps)' = 'Neoplasms',
    'nervous system disorders' = 'Nervous',
    'pregnancy, puerperium and perinatal conditions' = 'Pregnancy',
    'product issues' = 'Product',
    'psychiatric disorders' = 'Psych',
    'renal and urinary disorders' = 'Renal',
    'reproductive system and breast disorders' = 'Reprod/Breast',
    'respiratory, thoracic and mediastinal disorders' = 'Lung',
    'skin and subcutaneous tissue disorders' = 'Skin',
    'social circumstances' = 'Social',
    'surgical and medical procedures' = 'Procedure',
    'vascular disorders' = 'Vascular'
)


stat_choices <- c(
    'Odds ratio', 'Relative risk', 'Risk difference', 'Hazard ratio'
)
stat_map <- setNames(c('or', 'rr', 'rd', 'hr'), stat_choices)
p_value_choices <- c(
    'Fisher exact test p value', 'EAIR test p value',
    'Logrank test p value', 'Landmark KM test p value'
)
p_value_map <- setNames(
    c('fisher_p', 'EAIR_p', 'logrank_p', 'landmark_km_p'),
    p_value_choices
)

default_footnote_lines <- paste(
    paste('Date:', format(Sys.time(), format = '%d%b%Y %H:%M')),
    'Produced by Safety Monitoring R-Shiny App Version 1',
    sep = '\n'
)

# select output table columns
output_sumstat_group <- c(
    'No. of Subjects w Events (Treatment)',
    'No. of Subjects w Events (Control)',
    'Total No. of Subjects (Treatment)',
    'Total No. of Subjects (Control)',
    'Drug Exposure at Risk (Treatment)',
    'Drug Exposure at Risk (Control)',
    'Incidence Rate (Treatment)',
    'Incidence Rate (Control)'
)
output_landmark_group <- c(
    'Landmark KM timepoint',
    'Landmark KM estimate (Treatment)',
    'Landmark KM estimate (Control)',
    'Landmark KM test p value'
)
output_eair_group <- c(
    'EAIR (Treatment)',
    'EAIR (Treatment) lower',
    'EAIR (Treatment) upper',
    'EAIR (Control)',
    'EAIR (Control) lower',
    'EAIR (Control) upper',
    'EAIR test p value'
)
output_rr_group <- c(
    'Relative risk',
    'Relative risk lower',
    'Relative risk upper'
)
output_rd_group <- c(
    'Risk difference',
    'Risk difference lower',
    'Risk difference upper'
)
output_or_group <- c(
    'Odds ratio',
    'Odds ratio lower',
    'Odds ratio upper',
    'Fisher exact test p value'
)
output_hr_group <- c(
    'Hazard ratio',
    'Hazard ratio lower',
    'Hazard ratio upper',
    'Logrank test p value'
)
output_dfdr_group <- c(
    'DFDR Fisher exact test p value',
    'DFDR EAIR test p value',
    'DFDR Logrank test p value',
    'DFDR Landmark KM test p value'
)
# output_bhmm_group <- c(
#     'BHMM Poisson posterior probability of risk',
#     'BHMM Logistic posterior probability of risk'
# )


abbreviation_footnote <- paste0(
    'KM - Kaplan-Meier method', tags$br(),
    'EAIR - Exposured-adjusted incidence rate', tags$br(),
    'DFDR - Double false discovery rate', tags$br(),
    'BHMM - Bayesian Hierarchical Mixture Model', tags$br(),
    tags$br(),
    'SOC - System organ class ', tags$br(),
    'HLGT - High level group term ', tags$br(),
    'HLT - High level term ', tags$br(),
    'PT - Preferred term'
)

output_table_footnote <- paste0(
    paste0("'NA' - Based on the merged dataset, at least one subject had all",
           " occurrences of the corresponding AE prior to his/her first dosing date"),
    tags$br(),
    "'-' - at least one treatment group had no event occurred"
)


































