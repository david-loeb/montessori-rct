#==============================================================================#
# Data Setup for Montessori Analyses                                           #
#==============================================================================#

if (!require('pak')) install.packages('pak')
pak::pkg_install(c('this.path', 'readr', 'dplyr', 'stringr'))
suppressMessages(library(dplyr)); library(stringr)
setwd(this.path::here())

df <- readr::read_csv(  # Load data
  '../data/analytic_sample_data.csv', show_col_types = F, progress = F
)

outcomes_bl <- c(  # Set up variable vectors
  'wjlw_bl', 'wjap_bl', 'wjpv_bl', 'htks_bl', 'fdigit_bl', 'bdigit_bl',
  'tom_bl', 'sps_bl', 'puzz_bl'
)
outcomes_k <- str_replace(outcomes_bl, 'bl', 'k')
covariates <- c(
  'assessed_in_spanish_k', 'assessed_in_spanish_bl', 'htks_bl', 
  'household_size_log', 'primary_lang_english', 'hispanic', 
  'caregiver_bachelors', 'caregiver_married', 'income_over_75k', 'female', 
  'asian', 'black', 'racemulti', 'raceother'
)
covariates_ctr <- str_c(covariates, '_ctr')  # Centered covariates

df <- df |>  # Create centered covariates & missing indicators
  mutate(
    across(  # Center (grand mean)
      all_of(c(
        outcomes_bl, covariates, 'age_study', 'assessed_in_spanish_pk3',
        'assessed_in_spanish_pk4'
      )),
      ~ .x - mean(.x, na.rm = T),
      .names = '{col}_ctr'
    ),
    across(  # Missing outcome indicators
      all_of(c(
        outcomes_bl, str_replace(outcomes_bl, 'bl', 'pk3'), 
        str_replace(outcomes_bl, 'bl', 'pk4'), outcomes_k
      )),
      ~ ifelse(is.na(.x), 1, 0),
      .names = 'miss_{col}'
    ),
    race_num = case_match(  # Imputation software (Blimp) needs all numeric vars
      race, 
      "white" ~ 1, "asian" ~ 2, "black" ~ 3, "racemulti" ~ 4, "raceother" ~ 5
    )
  )

