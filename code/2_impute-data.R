#==============================================================================#
# Impute Data & Save Datasets                                                  #
#==============================================================================#

#* This file imputes data and saves the imputed datasets. The external software 
#* Blimp is used to do the imputation, called via the `rblimp` package. Blimp 
#* must be installed for this to work. Blimp is free and can be downloaded here: 
#* https://www.appliedmissingdata.com/blimp. Datasets are saved as RData objects
#* because they are smaller than alternatives like CSV. However you can save 
#* them in any tabular data format you like.

source(here('code/0_functions_analysis.R'))
source(here('code/1_data-setup.R'))
df <- select(df, where(is.numeric))  # Blimp requires all variables be numeric
covars <- c(covariates[1:4], 'race_num', covariates[5:10])

# K ============================================================================

## ITT -------------------------------------------------------------------------

res_wjlw <- impute_data(  # I set seed = 66 for convergence; prob not necessary
  'wjlw_k', covars, bl_ord = F, brn = 120000, itr = 40000, sed = 66
)
saveRDS(res_wjlw[[5]], here('data/imputed/wjlw_itt.rds'))

res_wjap <- impute_data(
  'wjap_k', covars, bl_ord = F, brn = 100000, itr = 40000
)
saveRDS(res_wjap[[5]], here('data/imputed/wjap_itt.rds'))

res_wjpv <- impute_data(  # use centered household size for convergence
  'wjpv_k', c(covars[1:3], 'household_size_log_ctr', covars[5:11]),
  bl_ord = F, brn = 100000, itr = 40000
)
res_wjpv[[5]]$household_size_log <- res_wjpv[[5]]$household_size_log_ctr
saveRDS(res_wjpv[[5]], here('data/imputed/wjpv_itt.rds'))

res_htks <- impute_data(
  'htks_k', covars, brn = 100000, itr = 40000
)
saveRDS(res_htks[[5]], here('data/imputed/htks_itt.rds'))

res_fdigit <- impute_data(
  'fdigit_k', covars, bl_ord = F, brn = 100000, itr = 40000
)
saveRDS(res_fdigit[[5]], here('data/imputed/fdigit_itt.rds'))
res_fdigit[[4]]$y_spec <- 'continuous'

res_bdigit <- impute_data(
  'bdigit_k', covars, brn = 100000, itr = 40000
)
saveRDS(res_bdigit[[5]], here('data/imputed/bdigit_itt.rds'))
res_bdigit[[4]]$bl_spec <- 'ordinal'

res_tom <- impute_data(
  'tom_k', covars, brn = 100000, itr = 40000
)
saveRDS(res_tom[[5]], here('data/imputed/tom_itt.rds'))

res_sps <- impute_data(
  'sps_k', covars, brn = 120000, itr = 40000
)
saveRDS(res_sps[[5]], here('data/imputed/sps_itt.rds'))

res_puzz <- impute_data(
  'puzz_k', covars, brn = 100000, itr = 40000
)
saveRDS(res_puzz[[5]], here('data/imputed/puzz_itt.rds'))

## CACE ------------------------------------------------------------------------

res_wjlw <- impute_data(
  'wjlw_k', covars, bl_ord = F, sed = 66,
  brn = 120000, itr = 40000, cace = T
)
saveRDS(res_wjlw[[5]], here('data/imputed/wjlw_cace.rds'))

res_wjap <- impute_data(
  'wjap_k', covars, bl_ord = F,
  brn = 100000, itr = 40000, cace = T
)
saveRDS(res_wjap[[5]], here('data/imputed/wjap_cace.rds'))

res_wjpv <- impute_data(  # used centered household size for convergence
  'wjpv_k', c(covars[1:3], 'household_size_log_ctr', covars[5:11]), 
  bl_ord = F, brn = 100000, itr = 40000, cace = T
)
res_wjpv[[5]]$household_size_log <- res_wjpv[[5]]$household_size_log_ctr
saveRDS(res_wjpv[[5]], here('data/imputed/wjpv_cace.rds'))

res_htks <- impute_data(
  'htks_k', c(covars[1:3], 'household_size_log_ctr', covars[5:11]), 
  cace = T, brn = 120000, itr = 40000
)
saveRDS(res_htks[[5]], here('data/imputed/htks_cace.rds'))

res_fdigit <- impute_data(
  'fdigit_k', covars, bl_ord = F, sed = 66,
  brn = 120000, itr = 40000, cace = T
)
saveRDS(res_fdigit[[5]], here('data/imputed/fdigit_cace.rds'))

res_bdigit <- impute_data(
  'bdigit_k', covars, brn = 100000, itr = 40000, cace = T
)
saveRDS(res_bdigit[[5]], here('data/imputed/bdigit_cace.rds'))

res_tom <- impute_data(
  'tom_k', covars, brn = 100000, itr = 40000, cace = T
)
saveRDS(res_tom[[5]], here('data/imputed/tom_cace.rds'))

res_sps <- impute_data(
  'sps_k', covars, brn = 120000, itr = 40000, cace = T
)
saveRDS(res_sps[[5]], here('data/imputed/sps_cace.rds'))

res_puzz <- impute_data(
  'puzz_k', c(covars[1:3], 'household_size_log_ctr', covars[5:11]),
  brn = 100000, itr = 40000, cace = T
)
saveRDS(res_puzz[[5]], here('data/imputed/puzz_cace.rds'))

# PK4 ==========================================================================

covars_pk4 <- c('assessed_in_spanish_pk4', covars[2:11])

## ITT -------------------------------------------------------------------------

res_wjlw <- impute_data(
  'wjlw_pk4', covars_pk4, bl_ord = F, brn = 100000, itr = 40000
)
saveRDS(res_wjlw[[5]], here('data/imputed/pk4/wjlw_itt.rds'))

res_wjap <- impute_data(
  'wjap_pk4', covars_pk4, bl_ord = F, brn = 100000, itr = 40000, 
  sed = 66  # <- for convergence; prob unnecessary
)
saveRDS(res_wjap[[5]], here('data/imputed/pk4/wjap_itt.rds'))

res_wjpv <- impute_data(
  'wjpv_pk4', covars_pk4, bl_ord = F, brn = 100000, itr = 40000
)
saveRDS(res_wjpv[[5]], here('data/imputed/pk4/wjpv_itt.rds'))

res_htks <- impute_data(
  'htks_pk4', covars_pk4, brn = 100000, itr = 40000
)
saveRDS(res_htks[[5]], here('data/imputed/pk4/htks_itt.rds'))

res_fdigit <- impute_data(
  'fdigit_pk4', covars_pk4, bl_ord = F, brn = 100000, itr = 40000
)
saveRDS(res_fdigit[[5]], here('data/imputed/pk4/fdigit_itt.rds'))

res_bdigit <- impute_data(
  'bdigit_pk4', covars_pk4, brn = 100000, itr = 40000
)
saveRDS(res_bdigit[[5]], here('data/imputed/pk4/bdigit_itt.rds'))

res_tom <- impute_data(
  'tom_pk4', covars_pk4, brn = 100000, itr = 40000
)
saveRDS(res_tom[[5]], here('data/imputed/pk4/tom_itt.rds'))

res_sps <- impute_data(
  'sps_pk4', covars_pk4, brn = 120000, itr = 40000
)
saveRDS(res_sps[[5]], here('data/imputed/pk4/sps_itt.rds'))

res_puzz <- impute_data(
  'puzz_pk4', covars_pk4, brn = 100000, itr = 40000
)
saveRDS(res_puzz[[5]], here('data/imputed/pk4/puzz_itt.rds'))

## CACE ------------------------------------------------------------------------

res_wjlw <- impute_data(
  'wjlw_pk4', covars_pk4, bl_ord = F,
  brn = 100000, itr = 40000, cace = T
)
saveRDS(res_wjlw[[5]], here('data/imputed/pk4/wjlw_cace.rds'))

res_wjap <- impute_data(
  'wjap_pk4', covars_pk4, bl_ord = F,
  brn = 100000, itr = 40000, cace = T, sed = 66
)
saveRDS(res_wjap[[5]], here('data/imputed/pk4/wjap_cace.rds'))

res_wjpv <- impute_data(
  'wjpv_pk4', covars_pk4, bl_ord = F,
  brn = 100000, itr = 40000, cace = T
)
saveRDS(res_wjpv[[5]], here('data/imputed/pk4/wjpv_cace.rds'))

res_htks <- impute_data(
  'htks_pk4', covars_pk4, 
  brn = 100000, itr = 40000, cace = T
)
saveRDS(res_htks[[5]], here('data/imputed/pk4/htks_cace.rds'))

res_fdigit <- impute_data(
  'fdigit_pk4', covars_pk4, bl_ord = F, sed = 66,
  brn = 120000, itr = 40000, cace = T
)
saveRDS(res_fdigit[[5]], here('data/imputed/pk4/fdigit_cace.rds'))

res_bdigit <- impute_data(
  'bdigit_pk4', covars_pk4,
  brn = 100000, itr = 40000, cace = T
)
saveRDS(res_bdigit[[5]], here('data/imputed/pk4/bdigit_cace.rds'))

res_tom <- impute_data(
  'tom_pk4', covars_pk4, brn = 100000, itr = 40000, 
  cace = T
)
saveRDS(res_tom[[5]], here('data/imputed/pk4/tom_cace.rds'))

res_sps <- impute_data(
  'sps_pk4', covars_pk4, brn = 120000, itr = 40000, 
  cace = T
)
saveRDS(res_sps[[5]], here('data/imputed/pk4/sps_cace.rds'))

res_puzz <- impute_data(
  'puzz_pk4', covars_pk4,
  brn = 100000, itr = 40000, cace = T
)
saveRDS(res_puzz[[5]], here('data/imputed/pk4/puzz_cace.rds'))

# PK3 ==========================================================================

covars_pk3 <- c('assessed_in_spanish_pk3', covars[2:11])

## ITT -------------------------------------------------------------------------

res_wjlw <- impute_data(
  'wjlw_pk3', covars_pk3, bl_ord = F, brn = 100000, itr = 40000
)
saveRDS(res_wjlw[[5]], here('data/imputed/pk3/wjlw_itt.rds'))

res_wjap <- impute_data(
  'wjap_pk3', covars_pk3, bl_ord = F, brn = 100000, itr = 40000
)
saveRDS(res_wjap[[5]], here('data/imputed/pk3/wjap_itt.rds'))

res_wjpv <- impute_data(
  'wjpv_pk3', covars_pk3, bl_ord = F, brn = 100000, itr = 40000
)
saveRDS(res_wjpv[[5]], here('data/imputed/pk3/wjpv_itt.rds'))

res_htks <- impute_data(
  'htks_pk3', covars_pk3, brn = 100000, itr = 40000
)
saveRDS(res_htks[[5]], here('data/imputed/pk3/htks_itt.rds'))

res_fdigit <- impute_data(
  'fdigit_pk3', covars_pk3, bl_ord = F, brn = 100000, itr = 40000
)
saveRDS(res_fdigit[[5]], here('data/imputed/pk3/fdigit_itt.rds'))

res_bdigit <- impute_data(
  'bdigit_pk3', covars_pk3, brn = 100000, itr = 40000
)
saveRDS(res_bdigit[[5]], here('data/imputed/pk3/bdigit_itt.rds'))

res_tom <- impute_data(
  'tom_pk3', covars_pk3, brn = 100000, itr = 40000
)
saveRDS(res_tom[[5]], here('data/imputed/pk3/tom_itt.rds'))

res_sps <- impute_data(
  'sps_pk3', covars_pk3, brn = 120000, itr = 40000
)
saveRDS(res_sps[[5]], here('data/imputed/pk3/sps_itt.rds'))

res_puzz <- impute_data(
  'puzz_pk3', covars_pk3, brn = 100000, itr = 40000
)
saveRDS(res_puzz[[5]], here('data/imputed/pk3/puzz_itt.rds'))

## CACE ------------------------------------------------------------------------

res_wjlw <- impute_data(
  'wjlw_pk3', covars_pk3, bl_ord = F,
  brn = 100000, itr = 40000, cace = T
)
saveRDS(res_wjlw[[5]], here('data/imputed/pk3/wjlw_cace.rds'))

res_wjap <- impute_data(
  'wjap_pk3', covars_pk3, bl_ord = F,
  brn = 100000, itr = 40000, cace = T
)
saveRDS(res_wjap[[5]], here('data/imputed/pk3/wjap_cace.rds'))

res_wjpv <- impute_data(
  'wjpv_pk3', covars_pk3, bl_ord = F,
  brn = 100000, itr = 40000, cace = T
)
saveRDS(res_wjpv[[5]], here('data/imputed/pk3/wjpv_cace.rds'))

res_htks <- impute_data(
  'htks_pk3', covars_pk3, 
  brn = 100000, itr = 40000, cace = T
)
saveRDS(res_htks[[5]], here('data/imputed/pk3/htks_cace.rds'))

res_fdigit <- impute_data(
  'fdigit_pk3', covars_pk3, bl_ord = F, sed = 66,
  brn = 120000, itr = 40000, cace = T
)
saveRDS(res_fdigit[[5]], here('data/imputed/pk3/fdigit_cace.rds'))

res_bdigit <- impute_data(
  'bdigit_pk3', covars_pk3,
  brn = 100000, itr = 40000, cace = T
)
saveRDS(res_bdigit[[5]], here('data/imputed/pk3/bdigit_cace.rds'))

res_tom <- impute_data(
  'tom_pk3', covars_pk3, brn = 100000, itr = 40000, 
  cace = T
)
saveRDS(res_tom[[5]], here('data/imputed/pk3/tom_cace.rds'))

res_sps <- impute_data(
  'sps_pk3', covars_pk3, brn = 120000, itr = 40000, 
  cace = T
)
saveRDS(res_sps[[5]], here('data/imputed/pk3/sps_cace.rds'))

res_puzz <- impute_data(
  'puzz_pk3', covars_pk3,
  brn = 100000, itr = 40000, cace = T
)
saveRDS(res_puzz[[5]], here('data/imputed/pk3/puzz_cace.rds'))

# Alt Covariate Specs ==========================================================

## Baseline outcome only -------------------------------------------------------

covars_bl_y <- c('assessed_in_spanish_k', 'assessed_in_spanish_bl')

res_wjlw <- impute_data(
  'wjlw_k', covars_bl_y, bl_ord = F, brn = 50000, itr = 40000, sed = 66
)
saveRDS(res_wjlw[[5]], here('data/imputed/alt_covars/bl_y_only/wjlw.rds'))

res_wjap <- impute_data(
  'wjap_k', covars_bl_y, bl_ord = F, brn = 50000, itr = 40000
)
saveRDS(res_wjap[[5]], here('data/imputed/alt_covars/bl_y_only/wjap.rds'))

res_wjpv <- impute_data(
  'wjpv_k', covars_bl_y, bl_ord = F, brn = 50000, itr = 40000
)
res_wjpv[[5]]$household_size_log <- res_wjpv[[5]]$household_size_log_ctr
saveRDS(res_wjpv[[5]], here('data/imputed/alt_covars/bl_y_only/wjpv.rds'))

res_htks <- impute_data(
  'htks_k', covars_bl_y, brn = 50000, itr = 40000
)
saveRDS(res_htks[[5]], here('data/imputed/alt_covars/bl_y_only/htks.rds'))

res_fdigit <- impute_data(
  'fdigit_k', covars_bl_y, bl_ord = F, brn = 50000, itr = 40000
)
saveRDS(res_fdigit[[5]], here('data/imputed/alt_covars/bl_y_only/fdigit.rds'))

res_bdigit <- impute_data(
  'bdigit_k', covars_bl_y, brn = 50000, itr = 40000
)
saveRDS(res_bdigit[[5]], here('data/imputed/alt_covars/bl_y_only/bdigit.rds'))

res_tom <- impute_data(
  'tom_k', covars_bl_y, brn = 50000, itr = 40000
)
saveRDS(res_tom[[5]], here('data/imputed/alt_covars/bl_y_only/tom.rds'))

res_sps <- impute_data(
  'sps_k', covars_bl_y, brn = 50000, itr = 40000
)
saveRDS(res_sps[[5]], here('data/imputed/alt_covars/bl_y_only/sps.rds'))

res_puzz <- impute_data(
  'puzz_k', covars_bl_y, brn = 50000, itr = 40000
)
saveRDS(res_puzz[[5]], here('data/imputed/alt_covars/bl_y_only/puzz.rds'))

## All baseline outcomes -------------------------------------------------------

# Use centered household size to help convergence
covars_bl_y <- c(covars[1:2], 'household_size_log_ctr', covars[5:11])

res_wjlw <- impute_data(
  'wjlw_k', covars_bl_y, 
  bl_ord = F, brn = 10000, itr = 5000, all_bl_y = T
)
saveRDS(res_wjlw[[5]], here('data/imputed/alt_covars/bl_y_all/wjlw.rds'))

res_wjap <- impute_data(
  'wjap_k', covars_bl_y, 
  bl_ord = F, brn = 10000, itr = 5000, all_bl_y = T
)
saveRDS(res_wjap[[5]], here('data/imputed/alt_covars/bl_y_all/wjap.rds'))

res_wjpv <- impute_data(
  'wjpv_k', covars_bl_y,
  bl_ord = F, brn = 10000, itr = 5000, all_bl_y = T
)
saveRDS(res_wjpv[[5]], here('data/imputed/alt_covars/bl_y_all/wjpv.rds'))

res_htks <- impute_data(
  'htks_k', covars_bl_y, brn = 10000, itr = 5000, all_bl_y = T
)
saveRDS(res_htks[[5]], here('data/imputed/alt_covars/bl_y_all/htks.rds'))

res_fdigit <- impute_data(
  'fdigit_k', covars_bl_y, bl_ord = F, brn = 10000, itr = 5000,
  all_bl_y = T
)
saveRDS(res_fdigit[[5]], here('data/imputed/alt_covars/bl_y_all/fdigit.rds'))

res_bdigit <- impute_data(
  'bdigit_k', covars_bl_y, brn = 10000, itr = 5000, all_bl_y = T
)
saveRDS(res_bdigit[[5]], here('data/imputed/alt_covars/bl_y_all/bdigit.rds'))

res_tom <- impute_data(
  'tom_k', covars_bl_y, brn = 10000, itr = 5000, all_bl_y = T
)
saveRDS(res_tom[[5]], here('data/imputed/alt_covars/bl_y_all/tom.rds'))

res_sps <- impute_data(
  'sps_k', covars_bl_y, brn = 10000, itr = 5000, all_bl_y = T
)
saveRDS(res_sps[[5]], here('data/imputed/alt_covars/bl_y_all/sps.rds'))

res_puzz <- impute_data(
  'puzz_k', covars_bl_y, brn = 10000, itr = 5000, all_bl_y = T
)
saveRDS(res_puzz[[5]], here('data/imputed/alt_covars/bl_y_all/puzz.rds'))

## Age Control -----------------------------------------------------------------

covars_age <- c(covars, 'age_study')

res_wjlw <- impute_data(
  'wjlw_k', covars_age, bl_ord = F, brn = 120000, itr = 40000, sed = 66
)
saveRDS(res_wjlw[[5]], here('data/imputed/alt_covars/age_ctrl/wjlw.rds'))

res_wjap <- impute_data(
  'wjap_k', covars_age, bl_ord = F, brn = 100000, itr = 40000
)
saveRDS(res_wjap[[5]], here('data/imputed/alt_covars/age_ctrl/wjap.rds'))

res_wjpv <- impute_data(
  'wjpv_k', covars_age, bl_ord = F, brn = 100000, itr = 40000
)
saveRDS(res_wjpv[[5]], here('data/imputed/alt_covars/age_ctrl/wjpv.rds'))

res_htks <- impute_data(
  'htks_k', covars_age, brn = 100000, itr = 40000
)
saveRDS(res_htks[[5]], here('data/imputed/alt_covars/age_ctrl/htks.rds'))

res_fdigit <- impute_data(
  'fdigit_k', covars_age, bl_ord = F, brn = 100000, itr = 40000
)
saveRDS(res_fdigit[[5]], here('data/imputed/alt_covars/age_ctrl/fdigit.rds'))

res_bdigit <- impute_data(
  'bdigit_k', covars_age, brn = 100000, itr = 40000
)
saveRDS(res_bdigit[[5]], here('data/imputed/alt_covars/age_ctrl/bdigit.rds'))

res_tom <- impute_data(
  'tom_k', covars_age, brn = 100000, itr = 40000
)
saveRDS(res_tom[[5]], here('data/imputed/alt_covars/age_ctrl/tom.rds'))

res_sps <- impute_data(
  'sps_k', covars_age, brn = 120000, itr = 40000
)
saveRDS(res_sps[[5]], here('data/imputed/alt_covars/age_ctrl/sps.rds'))

res_puzz <- impute_data(
  'puzz_k', covars_age, brn = 100000, itr = 40000
)
saveRDS(res_puzz[[5]], here('data/imputed/alt_covars/age_ctrl/puzz.rds'))

# Ranked Choice Lotteries Excluded =============================================

df_no_rc <- filter(df, lottery_ranked_choice == 0)

# Use grand mean center household num to help convergence
covars_household_ctr <- c(covars[1:3], 'household_size_log_ctr', covars[5:11])

res_wjlw <- impute_data(
  'wjlw_k', covars_household_ctr, 
  bl_ord = F, brn = 120000, itr = 40000, sed = 66, dat = df_no_rc
)
saveRDS(res_wjlw[[5]], here('data/imputed/no_rank_choice/wjlw.rds'))

res_wjap <- impute_data(
  'wjap_k', covars_household_ctr, 
  bl_ord = F, brn = 100000, itr = 40000, dat = df_no_rc
)
saveRDS(res_wjap[[5]], here('data/imputed/no_rank_choice/wjap.rds'))

res_wjpv <- impute_data(
  'wjpv_k', covars_household_ctr, 
  bl_ord = F, brn = 100000, itr = 40000, dat = df_no_rc
)
saveRDS(res_wjpv[[5]], here('data/imputed/no_rank_choice/wjpv.rds'))

res_htks <- impute_data(
  'htks_k', covars_household_ctr, 
  brn = 100000, itr = 40000, dat = df_no_rc
)
saveRDS(res_htks[[5]], here('data/imputed/no_rank_choice/htks.rds'))

res_fdigit <- impute_data(
  'fdigit_k', covars_household_ctr, 
  bl_ord = F, brn = 100000, itr = 40000, dat = df_no_rc
)
saveRDS(res_fdigit[[5]], here('data/imputed/no_rank_choice/fdigit.rds'))

res_bdigit <- impute_data(
  'bdigit_k', covars_household_ctr, 
  brn = 100000, itr = 40000, dat = df_no_rc
)
saveRDS(res_bdigit[[5]], here('data/imputed/no_rank_choice/bdigit.rds'))

res_tom <- impute_data(
  'tom_k', covars_household_ctr, brn = 100000, itr = 40000, 
  dat = df_no_rc
)
saveRDS(res_tom[[5]], here('data/imputed/no_rank_choice/tom.rds'))

res_sps <- impute_data(
  'sps_k', covars_household_ctr, brn = 120000, itr = 40000, 
  dat = df_no_rc
)
saveRDS(res_sps[[5]], here('data/imputed/no_rank_choice/sps.rds'))

res_puzz <- impute_data(
  'puzz_k', covars_household_ctr, brn = 120000, itr = 40000, 
  dat = df_no_rc
)
saveRDS(res_puzz[[5]], here('data/imputed/no_rank_choice/puzz.rds'))

# Propensity Scores ============================================================

#* This section estimates propensity scores in the context of each outcome model 
#* and adds them (along with pscore weights and a prognostic score if 
#* applicable) to each imputed dataset. The new datasets are then saved to use 
#* for pscore-based sensitivity analyses.

## Data setup ------------------------------------------------------------------

library(purrr)

# Load imputed datasets
df_list <- list.files(here('data/imputed'), '_itt', full.names = T) |> 
  set_names(~ str_remove(basename(.x), '_itt.rds')) |> 
  map(readRDS)

df_list <- df_list |>  # Data prep
  map(~ mutate(
    .x,  # create race dummies in all imputed datasets
    white = ifelse(race_num == 1, 1, 0),
    asian = ifelse(race_num == 2, 1, 0),
    black = ifelse(race_num == 3, 1, 0),
    racemulti = ifelse(race_num == 4, 1, 0),
    raceother = ifelse(race_num == 5, 1, 0),
    racemultiother = ifelse(race_num %in% c(4, 5), 1, 0),
    household_size = round(exp(household_size_log), 0),
    across(  # observed data indicators for missingness pscore estimation
      any_of(outcomes_k),
      ~ ifelse(get(str_c('miss_', cur_column())) == 0, 1, 0),
      .names = 'obs_{col}'
    )
  )) |> 
  map(~ mutate(  # create group mean centered vars in all imputed datasets
    .x,
    across(  
      any_of(c(
        covariates, outcomes_bl, 'mont_offer', 'racemultiother', 
        'age_study'
      )),
      ~ .x - mean(.x), 
      .names = '{col}_ctr_group'
    ),
    .by = c(lottery_id, imp_num)
  ))

df_list <- list(  # rearrange order
  df_list[['wjlw']], df_list[['wjap']], df_list[['wjpv']], df_list[['htks']],
  df_list[['fdigit']], df_list[['bdigit']], df_list[['tom']], df_list[['sps']],
  df_list[['puzz']]
)

## Treatment -------------------------------------------------------------------

preds_pscore_mod <- c(  # Preds for pscore mod (function adds bl outcome)
  'age_study', covariates[!grepl("assess|race", covariates)], 
  'racemultiother'
)
df_list <- rep(df_list, 2)  # Vectors of function arguments
y <- rep(outcomes_k, 2)
both_tx <- c(rep(T,9), rep(F,9))
file_name <- rep(  # Vec for file naming
  c('wjlw', 'wjap', 'wjpv', 'htks', 'fdigit', 'bdigit', 'tom', 'sps', 'puzz'),
  times = length(y) / 9
)
file_name <- ifelse(
  both_tx == T,
  str_c(file_name, '_tx_both-tx.rds'),
  str_c(file_name, '_tx_all-obs.rds')
)

for (i in 1:length(y)) {  # Loop through pscore estimation & save data
  saveRDS(
    get_pscores_tx(df_list[[i]], y[i], both_tx[i]),
    str_c(here('data/imputed/propensity_scores/'), file_name[i])
  )
}

## Missingness -----------------------------------------------------------------

preds_pscore_mod <- c(  # Preds for pscore mod (function adds bl outcome)
  covariates[!grepl("assess|race", covariates)], "racemultiother", "mont_offer"
)
file_name <- c(  # Vec for file naming
  'wjlw', 'wjap', 'wjpv', 'htks', 'fdigit', 'bdigit', 'tom', 'sps', 'puzz'
)

for (i in 1:length(outcomes_k)) {  # Loop through pscore estimation & save data
  saveRDS(
    get_pscores_miss(df_list[[i]], outcomes_k[[i]]),
    str_c(
      here('data/imputed/propensity_scores/'), 
      file_name[[i]], '_miss.rds'
    )
  )
}
