#==============================================================================#
# Sensitivity Analyses                                                         #
#==============================================================================#

if (!require('pak')) install.packages('pak')
pak::pkg_install(c('this.path', 'tibble', 'fixest', 'broom'))
library(purrr); library(tibble)
setwd(this.path::here())
source('0_functions_analysis.R')
source('1_data-setup.R')

# ==== No Ranked Choice Lotteries ==============================================

#* This section drops schools that enroll students using ranked choice / 
#* deferred acceptance algorithms and re-estimates the ITT models with just the
#* fully random lotteries.

## --- Imp ---------------------------------------------------------------------

# Load imputed datasets
df_list <- list.files('../data/imputed/no_rank_choice', full.names = T) |> 
  set_names(~ str_remove(basename(.x), '\\.rds')) |> 
  map(readRDS)
df_list <- list(  # reorder
  df_list[['wjlw']], df_list[['wjap']], df_list[['wjpv']], df_list[['htks']],
  df_list[['fdigit']], df_list[['bdigit']], df_list[['tom']], df_list[['sps']],
  df_list[['puzz']]
)
covars_hh_ctr <- c(  # centered hh size used in imputations for convergence
  covariates[1:3], 'household_size_log_ctr', covariates[5:14]
)

res <- map2(  # Run models
  df_list, outcomes_k, 
  ~ run_mod_imp_itt(.x, .y, covars = covars_hh_ctr, full_stats = T)
) |> list_rbind()
write.csv(  # Save
  res, '../output/k/sensitivity/no-rank-choice_imp_itt.csv', 
  na = '', row.names = F
)

## --- CC ----------------------------------------------------------------------

df_no_rc <- filter(df, lottery_ranked_choice == 0)  # Drop ranked choice
func_params <- tibble(y = outcomes_k, covars = list(covariates_ctr))  # Func args
res <- pmap(func_params, ~ run_mod_cc(..1, ..2, dat = df_no_rc))  # Run mod
treat_fx <- map(res, `[[`, 2) |>  # Combine into results table
  list_rbind() |> 
  select(-statistic) |> 
  relocate(y) |> relocate(p, .after = se) |> 
  relocate(g, .after = p) |> relocate(g_se, .after = g)
write.csv(  # Save
  treat_fx, 
  '../output/k/sensitivity/no-rank-choice_cc_itt.csv', na = '', row.names = F
)

# ==== Consent =================================================================

#* This section performs sensitivity analyses related to differential consent.

df_app <- readr::read_csv(  # All lottery applicants dataset
  '../data/all_lottery_applicants.csv', show_col_types = F, progress = F
)  # Drop schools w/o tx info for non-consenters
df_app_tx <- filter(df_app, !is.na(mont_offer))
df_cnsnt <- df_app_tx |>  # Consent rates by treatment status by lottery
  summarise(
    n = n(),
    n_treat = sum(mont_offer),
    n_ctrl = sum(mont_offer == 0),
    n_cnsnt = sum(consented),
    n_cnsnt_treat = sum(consented[mont_offer == 1]),
    n_cnsnt_ctrl = sum(consented[mont_offer == 0]),
    .by = lottery_id
  ) |> 
  mutate(
    cnsnt = n_cnsnt / n,
    cnsnt_treat = n_cnsnt_treat / n_treat,
    cnsnt_ctrl = n_cnsnt_ctrl / n_ctrl
  )

# Consent rates
mean(df_app$consented)  # Overall: 21.0%
mean(df_app$consented[!is.na(df_app$lottery_id)])  # Schools in study: 20.9%
mean(df_app_tx$consented)  # Schools w/ treatment status: 20.1%
mean(df_app_tx$consented[df_app_tx$mont_offer == 1])  # Treatment: 30.5%
mean(df_app_tx$consented[df_app_tx$mont_offer == 0])  # Control: 16.8%

# Rates by income & treatment status at schools with low-income lotteries
mean(df_cnsnt$cnsnt_treat[df_cnsnt$lottery_id %in% c(30,7,22)])  # lo inc: 40.7%
mean(df_cnsnt$cnsnt_ctrl[df_cnsnt$lottery_id %in% c(30,7,22)])   # lo inc: 27.9%
mean(df_cnsnt$cnsnt_treat[df_cnsnt$lottery_id %in% c(29,6,21)])  # hi inc: 56.9%
mean(df_cnsnt$cnsnt_ctrl[df_cnsnt$lottery_id %in% c(29,6,21)])   # hi inc: 30.6%

## --- Lee Bounds --------------------------------------------------------------

df_list <- list.files('../data/imputed', 'itt', full.names = T) |>
  set_names(~ str_remove(basename(.x), '_itt.rds')) |> 
  map(readRDS)
df_list <- list(
  df_list[['wjlw']], df_list[['wjap']], df_list[['wjpv']], df_list[['htks']],
  df_list[['fdigit']], df_list[['bdigit']], df_list[['tom']], df_list[['sps']],
  df_list[['puzz']]
) |>  # Drop lotteries w/o both treat & ctrl
  map(~ filter(.x, !(lottery_id %in% c(3,14,12,15,13,24,34,25))))

set.seed(6)  # For random tie-breaks
res <- map2(df_list, outcomes_k, ~ get_lee_bounds(.x, .y, imp = T, cnsnt = T)) |> 
  map(`[[`, 2) |> list_rbind()
write.csv(
  res, '../output/k/sensitivity/consent_lee-bounds.csv',
  na = '', row.names = F
)

## --- Principal Stratification ------------------------------------------------

# Compute consent rates within treat and control groups
pct_ctrl_cons <- mean(df_app_tx$consented[df_app_tx$mont_offer == 0])
pct_ctrl_non_cons <- 1 - pct_ctrl_cons
pct_treat_cons <- mean(df_app_tx$consented[df_app_tx$mont_offer == 1])
pct_treat_non_cons <- 1 - pct_treat_cons
wt_always <- pct_ctrl_cons / pct_treat_cons

res <- readr::read_csv(  # Load multiple imputation ITT results
  '../output/k/imp_itt.csv', show_col_types = F, progress = F
) |> 
  mutate(  # Compute "treat-only consenter" mean required for spurious result
    comply_mean = (treat_mean - ctrl_mean * wt_always) / (1 - wt_always),
    comply_always_diff = comply_mean - ctrl_mean,  # diff v "always consenter"
    diff_g = g / coef * comply_always_diff  # effect size diff
  ) |> 
  select(
    y, ctrl_mean, treat_mean, comply_mean, comply_always_diff, diff_g,
    coef, se, p, g
  )
write.csv(
  res, '../output/k/sensitivity/consent_principal-strat.csv',
  na = '', row.names = F
)

# ==== Missing Data ============================================================

#* This section performs sensitivity analyses related to missing data.

## --- Propensity --------------------------------------------------------------

#* This analysis estimates participants' propensity for having observed data
#* and uses that to estimate inverse probability weighted models.

preds_pscore_mod <- c(  # Preds for pscore mod
  covariates[!grepl("assess|race", covariates)], "racemultiother", "mont_offer"
)

### --- Imp --------------------------------------------------------------------

#* This version uses imputed baseline covariates and reweights the sample with
#* observed K assessments.

df_list <- list.files(
  '../data/imputed/propensity_scores', 'miss', full.names = T
) |>
  set_names(~ str_remove(basename(.x), '_miss.rds')) |> 
  map(readRDS)
df_list <- list(
  df_list[['wjlw']], df_list[['wjap']], df_list[['wjpv']], df_list[['htks']],
  df_list[['fdigit']], df_list[['bdigit']], df_list[['tom']], df_list[['sps']],
  df_list[['puzz']]
)

res <- map2(df_list, outcomes_k, ~ run_mod_pscore_miss(.x, .y))  # Run mod

res_balance <- map(res, `[[`, 1) |>  # Get balance stats
  list_rbind() |> 
  select(
    y, var, matches('ctrl_mean'), matches('treat_mean'), matches('coef'),
    matches('g'), matches('n')
  ) |> 
  rename_with(~ str_replace(.x, 'coef', 'diff')) |> 
  filter(var != 'mont_offer')
write.csv(  # Save balance stats
  res_balance, '../output/k/sensitivity/miss_pscore_imp_balance.csv',
  na = '', row.names = F
)
res_balance_mean <- res_balance |>  # Calc mean balance across outcomes
  summarise(across(!matches('^y$'), mean), .by = var)
write.csv(  # Save mean balance stats
  res_balance_mean,
  '../output/k/sensitivity/miss_pscore_imp_balance_mean-across-outcomes.csv',
  na = '', row.names = F
)

map(res, `[[`, 2) |> list_rbind() |>  # Get ITT model results
  write.csv(  # Save model results
    '../output/k/sensitivity/miss_pscore_imp_itt.csv', na = '', row.names = F
  )

map(1:9, \(i) {  # Save weighted Ns
  df_list[[i]] |> 
    filter(get(str_c('miss_', outcomes_k[i])) == 0) |> 
    summarise(n = sum(wt), .by = c(imp_num, mont_offer)) |> 
    summarise(n = mean(n), .by = mont_offer) |> 
    mutate(y = outcomes_k[i])
}) |> 
  list_rbind() |> 
  tidyr::pivot_wider(names_from = mont_offer, values_from = n) |> 
  rename(n_ctrl = `0`, n_treat = `1`) |> 
  write.csv(
    '../output/k/sensitivity/miss_pscore_n-wtd_imp.csv', na = '', row.names = F
  )

### --- CC ---------------------------------------------------------------------

#* This version reweights the complete case sample by modeling the propensity 
#* for missing on the baseline and/or K assessment, using just the baseline
#* covariates.

df <- df |> 
  mutate(  # Create indicator for observing both baseline & K outcome
    across(
      all_of(outcomes_bl),
      ~ ifelse(
        get(str_c('miss_', cur_column())) == 0 
        & get(str_c('miss_', str_remove(cur_column(), '_bl'), '_k')) == 0, 
        1, 0
      ),
      .names = 'obs_{str_remove(col, "_bl$")}_both'
    ),
    racemultiother = ifelse(race_num %in% c(4, 5), 1, 0)  # For pscore model
  ) |> 
  mutate(
    across(  # group mean centered predictors
      all_of(c(covariates, 'racemultiother', outcomes_bl, 'mont_offer')),
      ~ .x - mean(.x, na.rm = T),
      .names = '{col}_ctr_group'
    ),
    .by = lottery_id
  )
df$miss_preds <- !(complete.cases(  # Drop ppl missing on baseline covars
  select(df, all_of(preds_pscore_mod[!grepl('htks', preds_pscore_mod)]))
))
df_cc <- filter(df, !miss_preds)

pscores <- map(outcomes_k, ~ get_pscores_miss(df_cc, .x, imp = F))  # Get pscores
res <- pscores |>  # Run mod
  set_names(outcomes_k) |> 
  imap(~ run_mod_pscore_miss(.x, .y, imp = F, covars = covariates_ctr))

res_balance <- map(res, `[[`, 1) |>  # Get balance stats
  list_rbind() |> 
  select(
    y, var, matches('ctrl_mean'), matches('treat_mean'), matches('coef'),
    matches('g'), matches('^n_')
  ) |> 
  rename_with(~ str_replace(.x, 'coef', 'diff')) |> 
  filter(var != 'mont_offer')
write.csv(
  res_balance, '../output/k/sensitivity/miss_pscore_cc_balance.csv',
  na = '', row.names = F
)
res_balance_mean <- res_balance |>  # Get mean balance stats across outcomes
  summarise(across(!matches('^y$'), mean), .by = var)
write.csv(
  res_balance_mean,
  '../output/k/sensitivity/miss_pscore_cc_balance_mean-across-outcomes.csv',
  na = '', row.names = F
)

res_itt <- map(res, `[[`, 2) |>  # Get ITT model results
  list_rbind() |> 
  select(
    y, coef, se, p, g, g_se, ctrl_mean, treat_mean, 
    n_ctrl, n_treat, sd_ctrl, sd_treat
  )
write.csv(
  res_itt, '../output/k/sensitivity/miss_pscore_cc_itt.csv',
  na = '', row.names = F
)

map(1:9, \(i) {  # Save weighted Ns
  pscores[[i]] |> 
    filter(get(str_c('obs_', str_remove(outcomes_bl[i], 'bl$'), 'both')) == 1) |> 
    summarise(n = sum(wt), .by = mont_offer) |> 
    mutate(y = outcomes_k[i])
}) |> 
  list_rbind() |> 
  tidyr::pivot_wider(names_from = mont_offer, values_from = n) |> 
  rename(n_ctrl = `0`, n_treat = `1`) |> 
  write.csv(
    '../output/k/sensitivity/miss_pscore_n-wtd_cc.csv', na = '', row.names = F
  )

## --- Lee Bounds --------------------------------------------------------------

pak::pkg_install('data.table')

### --- Imp --------------------------------------------------------------------

#* This version uses imputed predictors and does the sensitivity analysis based
#* on just missing K outcome data.

df_list <- list.files(
  '../data/imputed', 'itt', full.names = T
) |>
  set_names(~ str_remove(basename(.x), '_itt.rds')) |> 
  map(readRDS)
df_list <- list(
  df_list[['wjlw']], df_list[['wjap']], df_list[['wjpv']], df_list[['htks']],
  df_list[['fdigit']], df_list[['bdigit']], df_list[['tom']], df_list[['sps']],
  df_list[['puzz']]
) |>  # Drop those w/o both treat and control
  map(~ filter(.x, !(lottery_id %in% c(3,14,12,15,13,24,34,25))))

set.seed(6)  # For random tie-breaks
res <- map2(df_list, outcomes_k, ~ get_lee_bounds(.x, .y, imp = T))
res |> map(`[[`, 2) |> list_rbind() |> 
  write.csv(
    '../output/k/sensitivity/miss_lee-bounds_imp.csv', na = '', row.names = F
  )

map(1:9, \(i) {  # Save weighted Ns
  res[[i]][[1]] |> 
    filter(drop_low == 0) |> 
    summarise(n = sum(drop_wt_low), .by = c(imp_num, mont_offer)) |> 
    summarise(n = mean(n), .by = mont_offer) |> 
    mutate(y = outcomes_k[i])
}) |> 
  list_rbind() |> 
  tidyr::pivot_wider(names_from = mont_offer, values_from = n) |> 
  rename(n_ctrl = `0`, n_treat = `1`) |> 
  write.csv(
    '../output/k/sensitivity/miss_lee-bounds_n-wtd_imp.csv',
    na = '', row.names = F
  )

### --- CC ---------------------------------------------------------------------

#* This version is based missing any data, using complete cases.

df_trim <- filter(  # Remove lottos w/ 100% miss (not in study at bl) & w/o both tx
  df, !(lottery_id %in% c(24,34,25,2,4,3,8,14,12,9,15,13))
)

set.seed(6)
res <- map(outcomes_k, ~ get_lee_bounds(df_trim, .x))
res |> map(`[[`, 2) |> list_rbind() |> 
  write.csv(
    '../output/k/sensitivity/miss_lee-bounds_cc.csv', na = '', row.names = F
  )

map(1:9, \(i) {  # Save weighted Ns
  res[[i]][[1]] |> 
    filter(drop_low == 0) |> 
    summarise(n = sum(drop_wt_low), .by = mont_offer) |> 
    mutate(y = outcomes_k[i])
}) |> 
  list_rbind() |> 
  tidyr::pivot_wider(names_from = mont_offer, values_from = n) |> 
  rename(n_ctrl = `0`, n_treat = `1`) |> 
  write.csv(
    '../output/k/sensitivity/miss_lee-bounds_n-wtd_cc.csv', 
    na = '', row.names = F
  )

## --- Principal Stratification ------------------------------------------------

### --- Imp --------------------------------------------------------------------

pct_ctrl_obs <- c()  # Compute observed K data rates within tx group by outcome
pct_treat_obs <- c()
for(i in 1:9) {
  pct_ctrl_obs[i] <- mean(
    df[[str_c('miss_', outcomes_k[i])]][df$mont_offer == 0] == 0
  )
  pct_treat_obs[i] <- mean(
    df[[str_c('miss_', outcomes_k[i])]][df$mont_offer == 1] == 0
  )
}

res <- readr::read_csv(  # Load multiple imputation ITT results
  '../output/k/imp_itt.csv', show_col_types = F, progress = F
) |> 
  mutate(  # Compute "treat-only observed" mean needed for spurious result
    pct_ctrl_obs = pct_ctrl_obs,
    pct_treat_obs = pct_treat_obs,
    wt = pct_ctrl_obs / pct_treat_obs,
    comply_mean = (treat_mean - ctrl_mean * wt) / (1 - wt),
    comply_always_diff = comply_mean - ctrl_mean,
    diff_g = g / coef * comply_always_diff
  ) |> 
  select(
    y, ctrl_mean, treat_mean, comply_mean, comply_always_diff, diff_g,
    pct_ctrl_obs, pct_treat_obs, wt, coef, se, p, g
  )
write.csv(
  res, '../output/k/sensitivity/miss_principal-strat_imp.csv',
  na = '', row.names = F
)

### --- CC ---------------------------------------------------------------------

pct_ctrl_obs <- c()  # Same as above but for the complete case models
pct_treat_obs <- c()
for (i in 1:9) {
  pct_ctrl_obs[i] <- mean(complete.cases(
    df |> 
      filter(mont_offer == 0) |> 
      select(all_of(c(covariates, outcomes_k[i], outcomes_bl[i])))
  ))
  pct_treat_obs[i] <- mean(complete.cases(
    df |> 
      filter(mont_offer == 1) |> 
      select(all_of(c(covariates, outcomes_k[i], outcomes_bl[i])))
  ))
}

res <- readr::read_csv(
  '../output/k/cc_itt.csv', show_col_types = F, progress = F
) |> 
  mutate(
    pct_ctrl_obs = pct_ctrl_obs,
    pct_treat_obs = pct_treat_obs,
    wt = pct_ctrl_obs / pct_treat_obs,
    comply_mean = (treat_mean - ctrl_mean * wt) / (1 - wt),
    comply_always_diff = comply_mean - ctrl_mean,
    diff_g = g / coef * comply_always_diff
  ) |> 
  select(
    y, ctrl_mean, treat_mean, comply_mean, comply_always_diff, diff_g,
    pct_ctrl_obs, pct_treat_obs, wt, coef, se, p, g
  )
write.csv(
  res, '../output/k/sensitivity/miss_principal-strat_cc.csv',
  na = '', row.names = F
)

## --- Mixture Model -----------------------------------------------------------

# Note: Blimp must be installed to run these models

covars_hh_ctr <- c(  # Set up function parameters
  covariates[1:3], 'household_size_log_ctr', 'race_num', covariates[5:10]
)
y <- outcomes_k[grepl('^(wj(l|a)|tom|htks|fdig)', outcomes_k)]
func_params <- tidyr::expand_grid(  # each combo of effect size diffs for each Y
  y = factor(y, levels = y),
  ctrl_diff = c(-.5, -.25, -.1, 0, .1, .25, .5),
  treat_diff = c(-.5, -.25, -.1, 0, .1, .25, .5)
) |> 
  arrange(y, treat_diff, ctrl_diff) |> 
  mutate(bl_ord = ifelse(grepl('^htks|^tom', y), T, F))

res <- list()  # Loop through sets of arguments, running model & saving result
for (i in 1:nrow(func_params)) {
  res[[i]] <- run_mod_mixture(
    func_params$y[i], 
    func_params$ctrl_diff[i], func_params$treat_diff[i],
    func_params$bl_ord[i],
    brn = 10000
  )
}
treat_fx <- map(res, `[[`, 4) |> list_rbind()  # Extract treat effect results
write.csv(  # Save
  treat_fx, '../output/k/sensitivity/miss_mixture.csv', na = '', row.names = F
)

# ==== Propensity for Treatment Weighting & Matching ===========================

pak::pkg_install(  # Install if needed & load packages for these analyses
  c(
    "WeightIt", "MatchIt", "MatchThem", "cobalt", "marginaleffects", 
    "mice", "miceadds"
  )
)
library(WeightIt); library(MatchIt); library(MatchThem); library(cobalt)

preds_pscore_mod <- c(  # Preds for pscore mod (the function adds bl outcome)
  'age_study', covariates[!grepl("assess|race", covariates)], 
  'racemultiother'
)

# Version 1: Lotteries w/ both treatment & control participants only
df_list <- list.files(  # Load imp datasets w/ pscores
  '../data/imputed/propensity_scores', 'both-tx', full.names = T
) |>
  set_names(~ str_remove(basename(.x), '_both-tx.rds')) |> 
  map(readRDS)
df_list <- list(
  df_list[['wjlw']], df_list[['wjap']], df_list[['wjpv']], df_list[['htks']],
  df_list[['fdigit']], df_list[['bdigit']], df_list[['tom']], df_list[['sps']],
  df_list[['puzz']]
)

res_both_tx <- map2(df_list, outcomes_k, run_mod_pscore_tx)  # Run models
res_both_out <- map(res_both_tx, `[[`, 1) |>  # Combine ITT results
  list_rbind() |> 
  mutate(lotteries = "has both treat & ctrl")
bal_both_out <- map(res_both_tx, `[[`, 2) |>  # Combine covar balance results
  list_rbind() |> 
  mutate(lotteries = "has both treat & ctrl")

# Version 2: Full sample
df_list <- list.files(
  '../data/imputed/propensity_scores', 'all-obs', full.names = T
) |>
  set_names(~ str_remove(basename(.x), '_all-obs.rds')) |> 
  map(readRDS)
df_list <- list(
  df_list[['wjlw']], df_list[['wjap']], df_list[['wjpv']], df_list[['htks']],
  df_list[['fdigit']], df_list[['bdigit']], df_list[['tom']], df_list[['sps']],
  df_list[['puzz']]
)

res_all_obs <- map2(df_list, outcomes_k, run_mod_pscore_tx)
res_all_out <- map(res_all_obs, `[[`, 1) |> 
  list_rbind() |> 
  mutate(lotteries = "all")
bal_all_out <- map(res_all_obs, `[[`, 2) |> 
  list_rbind() |> 
  mutate(lotteries = "all")

write.csv(  # Combine & save
  bind_rows(res_all_out, res_both_out),
  "../output/k/sensitivity/pscore-treat_itt.csv", na = '', row.names = F
)
write.csv(
  bind_rows(bal_all_out, bal_both_out),
  "../output/k/sensitivity/pscore-treat_balance.csv", na = '', row.names = F
)

# Compute mean balance across all outcomes for each covar & save
bal_means <- bind_rows(bal_all_out, bal_both_out) |> 
  summarise(across(matches('mean$|diff$'), mean), .by = c(var, mod, lotteries))
write.csv(
  bal_means,
  '../output/k/sensitivity/pscore-treat_balance_mean-across-outcomes.csv',
  na = '', row.names = F
)

# ==== Covariates ==============================================================

## --- CC ----------------------------------------------------------------------

func_params_no_covars <- tibble(  # No covars - simple diff in means
  y = outcomes_k, 
  covars = list(''),
  no_bl_y = T,
  version = 'no covars'
)
func_params_only_assess_lang <- tibble(  # Only K assessment lang
  y = outcomes_k, 
  covars = list('assessed_in_spanish_k_ctr'),
  no_bl_y = T,
  version = 'K assess lang only'
)
func_params_only_bl_y <- tibble(  # Only bl outcome (& assess langs)
  y = outcomes_k, 
  covars = list(c('assessed_in_spanish_k_ctr', 'assessed_in_spanish_bl_ctr')),
  no_bl_y = F,
  version = 'bl y & assess langs only'
)
func_params_all_bl_y <- tibble(  # All bl outcomes
  y = outcomes_k,
  covars = list(outcomes_bl),
  no_bl_y = F,
  version = 'all bl outcomes'
)
func_params_all_bl_y_no_bdigit <- tibble(  # All bl outcomes but bdigit
  y = outcomes_k,
  covars = list(outcomes_bl),
  no_bl_y = F,
  version = 'all bl outcomes but bdigit'
)
func_params_all_bl_y_no_bdigit_puzz <- tibble(  # All bl y but bdigit & puzz
  y = outcomes_k,
  covars = list(outcomes_bl),
  no_bl_y = F,
  version = 'all bl outcomes but bdigit & puzz choice'
)
for (i in 1:9) {  # Remove bl_y and bdigit as needed from covariate vec
  func_params_all_bl_y$covars[[i]] <- func_params_all_bl_y$covars[[i]][-i]
  func_params_all_bl_y$covars[[i]] <- c(
    covariates_ctr[!grepl('htks', covariates_ctr)], 
    str_c(func_params_all_bl_y$covars[[i]], '_ctr')
  )
  func_params_all_bl_y_no_bdigit$covars[[i]] <- func_params_all_bl_y$covars[[i]][
    !grepl('bdigit', func_params_all_bl_y$covars[[i]])
  ]
  func_params_all_bl_y_no_bdigit_puzz$covars[[i]] <- func_params_all_bl_y$covars[[i]][
    !grepl('bdigit|puzz', func_params_all_bl_y$covars[[i]])
  ]
}
func_params <- bind_rows(  # Combine into one function parameter df
  func_params_no_covars, func_params_only_assess_lang, func_params_only_bl_y,
  func_params_all_bl_y, func_params_all_bl_y_no_bdigit,
  func_params_all_bl_y_no_bdigit_puzz
)

res <- pmap(func_params, ~ run_mod_cc(..1, ..2, no_bl_y = ..3))  # Run mods
treat_fx <- map(res, `[[`, 2) |> list_rbind() |>  # Combine into results table
  select(-statistic, -term) |> 
  relocate(y) |> relocate(p, .after = se) |> 
  relocate(g, .after = p) |> relocate(g_se, .after = g) |> 
  mutate(covariates = func_params$version)
write.csv(  # Save
  treat_fx, '../output/k/sensitivity/covars_cc_itt.csv', na = '', row.names = F
)

## --- Imp ---------------------------------------------------------------------

# Baseline outcome (& assess langs) only
df_list <- list.files('../data/imputed/alt_covars/bl_y_only', full.names = T) |> 
  set_names(~ str_remove(basename(.x), '\\.rds')) |> 
  map(readRDS)
df_list_func <- list(
  df_list[['wjlw']], df_list[['wjap']], df_list[['wjpv']], df_list[['htks']],
  df_list[['fdigit']], df_list[['bdigit']], df_list[['tom']], df_list[['sps']],
  df_list[['puzz']]
)
res_bl_y_only <- map2(
  df_list_func, outcomes_k, 
  ~ run_mod_imp_itt(
    .x, .y, covars = c('assessed_in_spanish_bl', 'assessed_in_spanish_k'), 
    no_covars = T, full_stats = T
  )
) |> 
  list_rbind() |> 
  mutate(covariates = 'bl y & assess langs only')

# All baseline outcomes
df_list <- list.files('../data/imputed/alt_covars/bl_y_all', full.names = T) |> 
  set_names(~ str_remove(basename(.x), '\\.rds')) |> 
  map(readRDS)
df_list <- list(
  df_list[['wjlw']], df_list[['wjap']], df_list[['wjpv']], df_list[['htks']],
  df_list[['fdigit']], df_list[['bdigit']], df_list[['tom']], df_list[['sps']],
  df_list[['puzz']]
) |> 
  map(\(x) rename_with(  # rename b/c centered in imputation model
    x, ~ str_replace(.x, '^((wj|fdig|house).+)(_ctr)$', '\\1'))
  )
res_bl_y_all <- map2(
  df_list, outcomes_k, 
  ~ run_mod_imp_itt(
    .x, .y, 
    covars = c(outcomes_bl, covariates[!grepl('htks', covariates)]), 
    full_stats = T
  )
) |> 
  list_rbind() |> 
  mutate(covariates = 'all bl outcomes')

write.csv(
  bind_rows(res_bl_y_only, res_bl_y_all),
  '../output/k/sensitivity/covars_imp_itt.csv', na = '', row.names = F
)

# ==== Age Control =============================================================

covars_age <- c(covariates, 'age_study')
covars_age_ctr <- str_c(covars_age, '_ctr')

## --- Imp ---------------------------------------------------------------------

df_list <- list.files('../data/imputed/alt_covars/age_ctrl', full.names = T) |> 
  set_names(~ str_remove(basename(.x), '\\.rds')) |> 
  map(readRDS)

df_list <- list(
  df_list[['wjlw']], df_list[['wjap']], df_list[['wjpv']], df_list[['htks']],
  df_list[['fdigit']], df_list[['bdigit']], df_list[['tom']], df_list[['sps']], 
  df_list[['puzz']]
)

res <- map2(
  df_list, outcomes_k,
  ~ run_mod_imp_itt(.x, .y, covars = covars_age, full_stats = T)
) |> list_rbind()
write.csv(
  res, '../output/k/sensitivity/covars_age-ctrl_imp_itt.csv', 
  na = '', row.names = F
)

## --- CC ----------------------------------------------------------------------

func_params <- tibble(y = outcomes_k, covars = list(covars_age_ctr))
res <- pmap(func_params, ~ run_mod_cc(..1, ..2))
treat_fx <- map(res, `[[`, 2) |> 
  list_rbind() |> 
  select(-statistic) |> 
  relocate(y) |> relocate(p, .after = se) |> 
  relocate(g, .after = p) |> relocate(g_se, .after = g)
write.csv(
  treat_fx, '../output/k/sensitivity/covars_age-ctrl_cc_itt.csv', 
  na = '', row.names = F
)

# ==== Power: CC MDES ==========================================================

pak::pkg_install('PowerUpR')

params <- map(outcomes_sp24_final, \(y) {  # Get params to calc MDES by outcome
  df_cc <- df_mod[
    complete.cases(df_mod[c(covars_dummy, y, str_remove(y, '_sp24'))]), 
  ]
  df_n <- summarise(df_cc, n = n(), .by = itt)
  n <- sum(df_n$n) / 25  # n per randomization block
  p <- df_n$n[df_n$itt == 1] / sum(df_n$n)  # % sample in treatment group
  mod <- fixest::feols(
    as.formula(str_c(
      y, '~', str_remove(y, '_sp24'), '+', str_flatten(covars_dummy, '+'),
      '| lottery_num'
    )),
    data = df_cc, vcov = ~lottery_num_schl
  )
  r2 <- fixest::r2(mod)[[6]]  # % outcome variance explained by covariates
  list(n, p, r2)
})

mdes_cc <- c()  # Calc MDES by outcome
for (i in 1:9) {
  mdes_cc[i] <- PowerUpR::mdes.bira2c1(
    J = 25,  # number randomization blocks
    n = params[[i]][[1]],
    p = params[[i]][[2]],
    g1 = 15,  # number covariates
    r21 = params[[i]][[3]]
  )$mdes[1]
}

res <- data.frame(  # Save results
  y = outcomes_sp24_final,
  mdes = mdes_cc
)
write.csv(res, '../output/k/sensitivity/mdes_cc.xlsx', na = '', row.names = F)
