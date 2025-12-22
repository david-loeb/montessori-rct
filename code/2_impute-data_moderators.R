#==============================================================================#
# Impute Data & Save Datasets - Moderator Analyses                             #
#==============================================================================#

if (!require('pak')) install.packages('pak')
pak::pkg_install(c('this.path', 'blimp-stats/rblimp'))
setwd(here())
source('0_functions_analysis.R')
source('1_data-setup.R')
df <- select(df, where(is.numeric))
covars <- c(covariates[1:4], 'race_num', covariates[5:10])

# Race =========================================================================

#* Note: the imputation function drops those with missing race vars b/c it will 
#* be too hard for model impute, and centering wont work. 4 treat and 4 control 

res_wjlw <- impute_data(
  'wjlw_k', covars, bl_ord = F, brn = 120000, itr = 40000, sed = 66, 
  mdrtr = 'race_num'
)
saveRDS(res_wjlw[[5]], '../data/imputed/moderators/race/wjlw.rds')

res_wjap <- impute_data(
  'wjap_k', covars, bl_ord = F, brn = 100000, itr = 40000, mdrtr = 'race_num'
)
saveRDS(res_wjap[[5]], '../data/imputed/moderators/race/wjap.rds')

res_wjpv <- impute_data(
  'wjpv_k', covars, bl_ord = F, brn = 100000, itr = 40000, mdrtr = 'race_num'
)
saveRDS(res_wjpv[[5]], '../data/imputed/moderators/race/wjpv.rds')

res_htks <- impute_data(
  'htks_k', covars, brn = 100000, itr = 40000, mdrtr = 'race_num'
)
saveRDS(res_htks[[5]], '../data/imputed/moderators/race/htks.rds')

res_fdigit <- impute_data(
  'fdigit_k', covars, bl_ord = F, brn = 100000, itr = 40000, mdrtr = 'race_num'
)
saveRDS(res_fdigit[[5]], '../data/imputed/moderators/race/fdigit.rds')

res_bdigit <- impute_data(
  'bdigit_k', covars, brn = 100000, itr = 40000, mdrtr = 'race_num'
)
saveRDS(res_bdigit[[5]], '../data/imputed/moderators/race/bdigit.rds')

res_tom <- impute_data(
  'tom_k', covars, brn = 100000, itr = 40000, mdrtr = 'race_num'
)
saveRDS(res_tom[[5]], '../data/imputed/moderators/race/tom.rds')

res_sps <- impute_data(
  'sps_k', covars, brn = 120000, itr = 40000, mdrtr = 'race_num'
)
saveRDS(res_sps[[5]], '../data/imputed/moderators/race/sps.rds')

res_puzz <- impute_data(
  'puzz_k', covars, brn = 100000, itr = 40000, mdrtr = 'race_num'
)
saveRDS(res_puzz[[5]], '../data/imputed/moderators/race/puzz.rds')

# Hispanic =====================================================================

res_wjlw <- impute_data(
  'wjlw_k', covars, bl_ord = F, brn = 120000, itr = 40000, sed = 66, 
  mdrtr = 'hispanic'
)
saveRDS(res_wjlw[[5]], '../data/imputed/moderators/hispanic/wjlw.rds')

res_wjap <- impute_data(
  'wjap_k', covars, bl_ord = F, brn = 100000, itr = 40000, mdrtr = 'hispanic'
)
saveRDS(res_wjap[[5]], '../data/imputed/moderators/hispanic/wjap.rds')

res_wjpv <- impute_data(
  'wjpv_k', covars, bl_ord = F, brn = 100000, itr = 40000, mdrtr = 'hispanic'
)
saveRDS(res_wjpv[[5]], '../data/imputed/moderators/hispanic/wjpv.rds')

res_htks <- impute_data(  # centered household size & higher burn-in to converge
  'htks_k', c(covars[1:3], 'household_size_log_ctr', covars[5:11]), 
  brn = 120000, itr = 40000, mdrtr = 'hispanic'
)
res_htks[[5]]$household_size_log <- res_htks[[5]]$household_size_log_ctr
saveRDS(res_htks[[5]], '../data/imputed/moderators/hispanic/htks.rds')

res_fdigit <- impute_data(
  'fdigit_k', covars, bl_ord = F, brn = 100000, itr = 40000, mdrtr = 'hispanic'
)
saveRDS(res_fdigit[[5]], '../data/imputed/moderators/hispanic/fdigit.rds')

res_bdigit <- impute_data(
  'bdigit_k', covars, brn = 100000, itr = 40000, mdrtr = 'hispanic'
)
saveRDS(res_bdigit[[5]], '../data/imputed/moderators/hispanic/bdigit.rds')

res_tom <- impute_data(
  'tom_k', covars, brn = 100000, itr = 40000, mdrtr = 'hispanic'
)
saveRDS(res_tom[[5]], '../data/imputed/moderators/hispanic/tom.rds')

res_sps <- impute_data(
  'sps_k', covars, brn = 120000, itr = 40000, mdrtr = 'hispanic'
)
saveRDS(res_sps[[5]], '../data/imputed/moderators/hispanic/sps.rds')

res_puzz <- impute_data(
  'puzz_k', covars, brn = 100000, itr = 40000, mdrtr = 'hispanic'
)
saveRDS(res_puzz[[5]], '../data/imputed/moderators/hispanic/puzz.rds')

# Income =======================================================================

res_wjlw <- impute_data(
  'wjlw_k', covars, bl_ord = F, brn = 120000, itr = 40000, 
  sed = 66, mdrtr = 'income_over_75k'
)
saveRDS(res_wjlw[[5]], '../data/imputed/moderators/income/wjlw.rds')

res_wjap <- impute_data(
  'wjap_k', covars, bl_ord = F, brn = 100000, itr = 40000, 
  mdrtr = 'income_over_75k'
)
saveRDS(res_wjap[[5]], '../data/imputed/moderators/income/wjap.rds')

res_wjpv <- impute_data(
  'wjpv_k', covars, bl_ord = F, brn = 100000, itr = 40000,
  mdrtr = 'income_over_75k'
)
saveRDS(res_wjpv[[5]], '../data/imputed/moderators/income/wjpv.rds')

res_htks <- impute_data(
  'htks_k', covars, brn = 100000, itr = 40000, 
  mdrtr = 'income_over_75k'
)
saveRDS(res_htks[[5]], '../data/imputed/moderators/income/htks.rds')

res_fdigit <- impute_data(
  'fdigit_k', covars, bl_ord = F, brn = 100000, itr = 40000,
  mdrtr = 'income_over_75k'
)
saveRDS(res_fdigit[[5]], '../data/imputed/moderators/income/fdigit.rds')

res_bdigit <- impute_data(
  'bdigit_k', covars, brn = 100000, itr = 40000, 
  mdrtr = 'income_over_75k'
)
saveRDS(res_bdigit[[5]], '../data/imputed/moderators/income/bdigit.rds')

res_tom <- impute_data(
  'tom_k', covars, brn = 100000, itr = 40000, 
  mdrtr = 'income_over_75k'
)
saveRDS(res_tom[[5]], '../data/imputed/moderators/income/tom.rds')

res_sps <- impute_data(
  'sps_k', covars, brn = 120000, itr = 40000, 
  mdrtr = 'income_over_75k'
)
saveRDS(res_sps[[5]], '../data/imputed/moderators/income/sps.rds')

res_puzz <- impute_data(
  'puzz_k', covars, brn = 100000, itr = 40000, 
  mdrtr = 'income_over_75k'
)
saveRDS(res_puzz[[5]], '../data/imputed/moderators/income/puzz.rds')

# Education ====================================================================

res_wjlw <- impute_data(
  'wjlw_k', covars, bl_ord = F, brn = 120000, itr = 40000, sed = 66, 
  mdrtr = 'caregiver_bachelors'
)
saveRDS(res_wjlw[[5]], '../data/imputed/moderators/education/wjlw.rds')

res_wjap <- impute_data(
  'wjap_k', covars, bl_ord = F, brn = 100000, itr = 40000, 
  mdrtr = 'caregiver_bachelors'
)
saveRDS(res_wjap[[5]], '../data/imputed/moderators/education/wjap.rds')

res_wjpv <- impute_data(
  'wjpv_k', covars, bl_ord = F, brn = 100000, itr = 40000,
  mdrtr = 'caregiver_bachelors'
)
saveRDS(res_wjpv[[5]], '../data/imputed/moderators/education/wjpv.rds')

res_htks <- impute_data(
  'htks_k', covars, brn = 100000, itr = 40000, 
  mdrtr = 'caregiver_bachelors'
)
saveRDS(res_htks[[5]], '../data/imputed/moderators/education/htks.rds')

res_fdigit <- impute_data(
  'fdigit_k', covars, bl_ord = F, brn = 100000, itr = 40000,
  mdrtr = 'caregiver_bachelors'
)
saveRDS(res_fdigit[[5]], '../data/imputed/moderators/education/fdigit.rds')

res_bdigit <- impute_data(
  'bdigit_k', covars, brn = 100000, itr = 40000, 
  mdrtr = 'caregiver_bachelors'
)
saveRDS(res_bdigit[[5]], '../data/imputed/moderators/education/bdigit.rds')

res_tom <- impute_data(
  'tom_k', covars, brn = 100000, itr = 40000, 
  mdrtr = 'caregiver_bachelors'
)
saveRDS(res_tom[[5]], '../data/imputed/moderators/education/tom.rds')

res_sps <- impute_data(
  'sps_k', covars, brn = 120000, itr = 40000, 
  mdrtr = 'caregiver_bachelors'
)
saveRDS(res_sps[[5]], '../data/imputed/moderators/education/sps.rds')

res_puzz <- impute_data(
  'puzz_k', covars, brn = 100000, itr = 40000, 
  mdrtr = 'caregiver_bachelors'
)
saveRDS(res_puzz[[5]], '../data/imputed/moderators/education/puzz.rds')

# Gender =======================================================================

res_wjlw <- impute_data(
  'wjlw_k', covars, bl_ord = F, brn = 120000, itr = 40000, sed = 66, 
  mdrtr = 'female'
)
saveRDS(res_wjlw[[5]], '../data/imputed/moderators/gender/wjlw.rds')

res_wjap <- impute_data(
  'wjap_k', covars, bl_ord = F, brn = 100000, itr = 40000, mdrtr = 'female'
)
saveRDS(res_wjap[[5]], '../data/imputed/moderators/gender/wjap.rds')

res_wjpv <- impute_data(
  'wjpv_k', covars, bl_ord = F, brn = 100000, itr = 40000, mdrtr = 'female'
)
saveRDS(res_wjpv[[5]], '../data/imputed/moderators/gender/wjpv.rds')

res_htks <- impute_data(
  'htks_k', covars, brn = 100000, itr = 40000, mdrtr = 'female'
)
saveRDS(res_htks[[5]], '../data/imputed/moderators/gender/htks.rds')

res_fdigit <- impute_data(
  'fdigit_k', covars, bl_ord = F, brn = 100000, itr = 40000, mdrtr = 'female'
)
saveRDS(res_fdigit[[5]], '../data/imputed/moderators/gender/fdigit.rds')

res_bdigit <- impute_data(
  'bdigit_k', covars, brn = 100000, itr = 40000, mdrtr = 'female'
)
saveRDS(res_bdigit[[5]], '../data/imputed/moderators/gender/bdigit.rds')

res_tom <- impute_data(
  'tom_k', covars, brn = 100000, itr = 40000, mdrtr = 'female'
)
saveRDS(res_tom[[5]], '../data/imputed/moderators/gender/tom.rds')

res_sps <- impute_data(
  'sps_k', covars, brn = 120000, itr = 40000, mdrtr = 'female'
)
saveRDS(res_sps[[5]], '../data/imputed/moderators/gender/sps.rds')

res_puzz <- impute_data(
  'puzz_k', covars, brn = 100000, itr = 40000, mdrtr = 'female'
)
saveRDS(res_puzz[[5]], '../data/imputed/moderators/gender/puzz.rds')

# Baseline Outcome =============================================================

res_wjlw <- impute_data(
  'wjlw_k', covars, bl_ord = F, brn = 120000, itr = 40000, sed = 66, 
  mdrtr = 'wjlw_bl'
)
saveRDS(res_wjlw[[5]], '../data/imputed/moderators/baseline_outcome/wjlw.rds')

res_wjap <- impute_data(
  'wjap_k', covars, bl_ord = F, brn = 100000, itr = 40000, mdrtr = 'wjap_bl'
)
saveRDS(res_wjap[[5]], '../data/imputed/moderators/baseline_outcome/wjap.rds')

res_wjpv <- impute_data(
  'wjpv_k', covars, bl_ord = F, brn = 100000, itr = 40000, mdrtr = 'wjpv_bl'
)
saveRDS(res_wjpv[[5]], '../data/imputed/moderators/baseline_outcome/wjpv.rds')

res_htks <- impute_data(
  'htks_k', covars, brn = 100000, itr = 40000, mdrtr = 'htks_bl'
)
saveRDS(res_htks[[5]], '../data/imputed/moderators/baseline_outcome/htks.rds')

res_fdigit <- impute_data(
  'fdigit_k', covars, bl_ord = F, brn = 100000, itr = 40000, mdrtr = 'fdigit_bl'
)
saveRDS(res_fdigit[[5]], '../data/imputed/moderators/baseline_outcome/fdigit.rds')

res_bdigit <- impute_data(
  'bdigit_k', covars, brn = 100000, itr = 40000, mdrtr = 'bdigit_bl'
)
saveRDS(res_bdigit[[5]], '../data/imputed/moderators/baseline_outcome/bdigit.rds')

res_tom <- impute_data(
  'tom_k', covars, brn = 100000, itr = 40000, mdrtr = 'tom_bl'
)
saveRDS(res_tom[[5]], '../data/imputed/moderators/baseline_outcome/tom.rds')

res_sps <- impute_data(
  'sps_k', covars, brn = 120000, itr = 40000, mdrtr = 'sps_bl'
)
saveRDS(res_sps[[5]], '../data/imputed/moderators/baseline_outcome/sps.rds')

res_puzz <- impute_data(
  'puzz_k', covars, brn = 100000, itr = 40000, mdrtr = 'puzz_bl'
)
saveRDS(res_puzz[[5]], '../data/imputed/moderators/baseline_outcome/puzz.rds')

# HTKS =========================================================================

res_wjlw <- impute_data(
  'wjlw_k', covars, bl_ord = F, brn = 120000, itr = 40000, sed = 66, 
  mdrtr = 'htks_bl'
)
saveRDS(res_wjlw[[5]], '../data/imputed/moderators/htks/wjlw.rds')

res_wjap <- impute_data(
  'wjap_k', covars, bl_ord = F, brn = 100000, itr = 40000, mdrtr = 'htks_bl'
)
saveRDS(res_wjap[[5]], '../data/imputed/moderators/htks/wjap.rds')

res_wjpv <- impute_data(
  'wjpv_k', covars, bl_ord = F, brn = 100000, itr = 40000, mdrtr = 'htks_bl'
)
saveRDS(res_wjpv[[5]], '../data/imputed/moderators/htks/wjpv.rds')

res_fdigit <- impute_data(
  'fdigit_k', covars, bl_ord = F, brn = 100000, itr = 40000, mdrtr = 'htks_bl'
)
saveRDS(res_fdigit[[5]], '../data/imputed/moderators/htks/fdigit.rds')

res_bdigit <- impute_data(
  'bdigit_k', covars, brn = 100000, itr = 40000, mdrtr = 'htks_bl'
)
saveRDS(res_bdigit[[5]], '../data/imputed/moderators/htks/bdigit.rds')

res_tom <- impute_data(
  'tom_k', covars, brn = 100000, itr = 40000, mdrtr = 'htks_bl'
)
saveRDS(res_tom[[5]], '../data/imputed/moderators/htks/tom.rds')

res_sps <- impute_data(
  'sps_k', covars, brn = 120000, itr = 40000, mdrtr = 'htks_bl'
)
saveRDS(res_sps[[5]], '../data/imputed/moderators/htks/sps.rds')

res_puzz <- impute_data(
  'puzz_k', covars, brn = 100000, itr = 40000, mdrtr = 'htks_bl'
)
saveRDS(res_puzz[[5]], '../data/imputed/moderators/htks/puzz.rds')
