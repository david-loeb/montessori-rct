#==============================================================================#
# Baseline Characteristics & Equivalence Analysis                              #
#==============================================================================#

if (!require('pak')) install.packages('pak')
pak::pkg_install(
  c('this.path', 'tidyr', 'purrr', 'tibble', 'data.table', 'broom', 'fixest')
)
library(purrr); library(tibble)
setwd(this.path::here())
source('0_functions_analysis.R')  # loads modeling functions
source('1_data-setup.R')  # runs the data setup script

# ==== Baseline Covariate Equivalence ==========================================

#* This section examines baseline covariate equivalence for treatment and 
#* control groups for all participants non-missing on a given covariate, which 
#* is almost the full sample.

covars_eq <- c(  # Set up function params
  'household_size', covariates[!grepl('assess|htks|house', covariates)], 
  'white', 'age_study' 
)
bnry <- c(F, rep(T, 11), F)

res <- map2(  # Run models
  covars_eq, bnry, ~ test_baseline_eq_cc(.x, .y, wave = '', full_stats = T)
  ) |> 
  list_rbind() |> 
  select(
    y, ctrl_mean_mod = ctrl_mean, n_ctrl, treat_mean_mod = treat_mean,
    n_treat, coef, se, p, g, g_se, sd_ctrl, sd_treat
  )
res <- df |>  # Add unadjusted means
  summarise(across(all_of(covars_eq), ~ mean(.x, na.rm = T)), .by = mont_offer) |> 
  data.table::transpose(keep.names = 'y') |> 
  slice(-1) |> 
  rename(treat_mean_unadj = V1, ctrl_mean_unadj = V2) %>%
  left_join(res, ., 'y') |> 
  relocate(ctrl_mean_unadj, .after = y) |> 
  relocate(n_ctrl, .after = ctrl_mean_unadj) |> 
  relocate(treat_mean_unadj, .after = n_ctrl) |> 
  relocate(n_treat, .after = treat_mean_unadj)  # Save
write.csv(res, '../output/equivalence/covariates.csv', na = '', row.names = F)

# ==== Baseline Outcome Equivalence ============================================

#* This section examines baseline outcome equivalence. Different versions are 
#* estimated, that vary as to whether they include covariates (`covrs` arg), and
#* whether they drop participants missing kindergarten data (`wave` arg).

# Set up function parameters
func_params <- tidyr::expand_grid(outcomes_bl, covrs = c(F, T), wave = c('', '_k'))
func_params_imp <- func_params[func_params$wave == '', c(1,2)]

# Complete case results
res_cc <- pmap(func_params, ~ test_baseline_eq_cc(..1, covrs = ..2, wave = ..3)) |>
  list_rbind() |> 
  mutate(wave = func_params$wave, covars = func_params$covrs, mod = 'cc')

# Imputed results
df_list <- list.files('../data/imputed', '_itt', full.names = T) |> 
  map(readRDS)
names(df_list) <- c(
  'bdigit_bl', 'fdigit_bl', 'htks_bl', 'puzz_bl', 'sps_bl', 'tom_bl', 
  'wjap_bl', 'wjlw_bl', 'wjpv_bl'
)
res_imp <- pmap(
  func_params_imp, ~ test_baseline_eq_imp(df_list[[..1]], ..1, covrs = ..2)
) |> 
  list_rbind() |> 
  mutate(covars = func_params_imp$covrs, mod = 'imp')

# Combine results & save
res_all <- bind_rows(res_cc, res_imp) |> 
  relocate(g, .after = y) |> 
  mutate(wave = if_else(wave == '_k', 'k', 'bl', 'k'))
write.csv(res_all, '../output/equivalence/outcomes.csv', na = '', row.names = F)

# Save version with full stats
res_cc_full <- map(outcomes_bl, ~ test_baseline_eq_cc(.x, full_stats = T)) |> 
  list_rbind()
res_imp_full <- map(
  outcomes_bl, ~ test_baseline_eq_imp(df_list[[.x]], .x, full_stats = T)
) |> list_rbind()
res_all_full <- bind_rows(res_cc_full, res_imp_full) |> 
  mutate(mod = ifelse(n_ctrl == 346, 'imp', 'cc'))
write.csv(
  res_all_full, 
  '../output/equivalence/outcomes_k_full-stats.csv', na = '', row.names = F
)

# ==== Baseline All Specs ======================================================

#* This section examines baseline equivalence for both covariates and outcomes
#* subsetting the sample in a number of ways, according to whether specific 
#* combinations of assessment data were observed.

covars_eq <- c(  # Function parameters
  'household_size', covariates[!grepl('assess|htks|house', covariates)], 
  'white', 'age_study' 
)
bnry <- c(F, rep(T, 11), F)

# 1: All with observed covariates (covariates only)
res_all_cov <- map2(
  covars_eq, bnry, ~ test_baseline_eq_cc(.x, binary = .y)
) |> 
  list_rbind() |> 
  mutate(spec = 'all w/ observed covariate')

# 2: Assessed at baseline (covariates & outcomes)
res_bl_assess <- map2(
  c(covars_eq, outcomes_bl), c(bnry, rep(F, 8), T),
  ~ test_baseline_eq_cc(
    .x, binary = .y, keep_miss_k = T, 
    dat = filter(df, assessed_bl == 1)
  )
) |> 
  list_rbind() |> 
  mutate(spec = 'assessed at baseline')

# 3: Assessed at K & baseline (complete cases, basically)
res_bl_k_assess <- map2(
  c(covars_eq, outcomes_bl), c(bnry, rep(F, 8), T),
  ~ test_baseline_eq_cc(
    .x, binary = .y, 
    dat = filter(df, assessed_bl == 1 & assessed_k == 1)
  )
) |> 
  list_rbind() |> 
  mutate(spec = 'assessed at K & baseline')

# 4: Imputed, full sample
df_list <- list.files('../data/imputed', '_itt', full.names = T) |> 
  map(readRDS)
df_list <- list(  # rearrange
  df_list[[8]], df_list[[7]], df_list[[9]], df_list[[3]], df_list[[2]], 
  df_list[[1]], df_list[[6]], df_list[[5]], df_list[[4]]
) |> 
  map(~ mutate(.x, household_size = round(exp(household_size_log), 0)))
bnry_y <- c(rep(F, 8), T)

res_imp_all <- map(1:9, \(i) {
  map2(
    c(covars_eq, outcomes_bl[i]), c(bnry, bnry_y[i]),
    ~ test_baseline_eq_imp(df_list[[i]], .x, binary = .y)
  ) |> 
    list_rbind() |> 
    mutate(y_imp = outcomes_bl[i])
}) |> 
  list_rbind() |> 
  summarise(
    across(where(is.numeric), mean), .by = y
  ) |> 
  mutate(spec = 'imputed, full sample')

# 5: Imputed, assessed at K
res_imp_k <- map(1:9, \(i) {
  map2(
    c(covars_eq, outcomes_bl[i]), c(bnry, bnry_y[i]),
    ~ test_baseline_eq_imp(
      filter(df_list[[i]], assessed_k == 1), .x, binary = .y
    )
  ) |> 
    list_rbind() |> 
    mutate(y_imp = outcomes_bl[i])
}) |> 
  list_rbind() |> 
  summarise(
    across(where(is.numeric), mean), .by = y
  ) |> 
  mutate(spec = 'imputed, assessed at K')

# Combine results & save
write.csv(
  bind_rows(
    res_all_cov, res_bl_assess, res_bl_k_assess, res_imp_all, res_imp_k
  ),
  '../output/equivalence/all-vars_five-specs.csv', na = '', row.names = F
)

# ==== Ranked Choice Lotteries =================================================

## --- No Ranked Choice Lotteries ----------------------------------------------

df_eq <- filter(df, lottery_ranked_choice == 0)

covars_eq <- c(  # Function parameters
  'household_size', covariates[!grepl('assess|htks|house', covariates)], 
  'white', 'age_study' 
)
bnry <- c(F, rep(T, 11), F)

res <- map2(  # Get results
  covars_eq, bnry, 
  ~ test_baseline_eq_cc(.x, .y, wave = '', full_stats = T, dat = df_eq)
) |> 
  list_rbind() |> 
  select(
    y, ctrl_mean_mod = ctrl_mean, n_ctrl, treat_mean_mod = treat_mean,
    n_treat, coef, se, p , g, g_se, sd_ctrl, sd_treat
  )
write.csv(
  res, '../output/equivalence/covariates_no-rank-choice-ltry.csv', 
  na = '', row.names = F
)

## --- Rank Choice Lotteries Only ----------------------------------------------

df_eq <- filter(df, lottery_ranked_choice == 1)

covars_eq <- c(  # Function parameters
  'household_size', covariates[!grepl('assess|htks|house', covariates)], 
  'white', 'age_study' 
)
bnry <- c(F, rep(T, 11), F)

res <- map2(  # Get results
  covars_eq, bnry, 
  ~ test_baseline_eq_cc(.x, .y, wave = '', full_stats = T, dat = df_eq)
) |> 
  list_rbind() |> 
  select(
    y, ctrl_mean_mod = ctrl_mean, n_ctrl, treat_mean_mod = treat_mean,
    n_treat, coef, se, p , g, g_se, sd_ctrl, sd_treat
  )
write.csv(
  res, '../output/equivalence/covariates_rank-choice-ltry-only.csv', 
  na = '', row.names = F
)

# ==== Missingness =============================================================

## --- Rates by Wave -----------------------------------------------------------

#* This section calculates rates of missing assessments by wave, overall and 
#* by treatment condition. The rates are also calculated after excluding schools
#* that joined the study after baseline data collection.

bind_rows(
  df |> 
    summarise(
      bl = sum(assessed_bl == 0) / n(),
      pk3 = sum(assessed_pk3 == 0) / n(),
      pk4 = sum(assessed_pk4 == 0) / n(),
      k = sum(assessed_k == 0) / n()
    ) |> 
    mutate(type = "all"),
  df |> 
    summarise(
      bl = sum(assessed_bl == 0) / n(),
      pk3 = sum(assessed_pk3 == 0) / n(),
      pk4 = sum(assessed_pk4 == 0) / n(),
      k = sum(assessed_k == 0) / n(),
      .by = mont_offer
    ) |> 
    mutate(type = "all"),
  df |> 
    filter(!(df$lottery_school_id %in% c(2, 5, 8))) |> 
    summarise(
      bl = sum(assessed_bl == 0) / n(),
      pk3 = sum(assessed_pk3 == 0) / n(),
      pk4 = sum(assessed_pk4 == 0) / n(),
      k = sum(assessed_k == 0) / n()
    ) |> 
    mutate(type = "in study at baseline"),
  df |> 
    filter(!(df$lottery_school_id %in% c(2, 5, 8))) |> 
    summarise(
      bl = sum(assessed_bl == 0) / n(),
      pk3 = sum(assessed_pk3 == 0) / n(),
      pk4 = sum(assessed_pk4 == 0) / n(),
      k = sum(assessed_k == 0) / n(),
      .by = mont_offer
    ) |> 
    mutate(type = "in study at baseline")
) |> 
  mutate(
    mont_offer = case_match(mont_offer, 0 ~ 'ctrl', 1 ~ 'treat', .default = 'all')
  ) |> 
  arrange(type, mont_offer) |> relocate(type) |> relocate(mont_offer) |> 
  write.csv(
    '../output/equivalence/miss_assess-by-wave.csv', na = '', row.names = F
  )

## --- All Variables Baseline & K ----------------------------------------------

#* This section computes missingness rates for each variable individually.

bind_rows(
  summarise(
    df,
    across(
      all_of(c(covariates[4:11], outcomes_bl, outcomes_k)), 
      ~ mean(is.na(.x))
    )
  ),
  summarise(
    df,
    across(
      all_of(c(covariates[4:11], outcomes_bl, outcomes_k)), 
      ~ mean(is.na(.x))
    ),
    .by = mont_offer
  )
) |> 
  data.table::transpose(keep.names = "y") |> 
  rename(all = V1, treat = V2, ctrl = V3) |> 
  filter(y != 'mont_offer') |> 
  mutate(
    y = factor(  # for ordering the rows
      y, levels = c(
        'income_over_75k', 'caregiver_bachelors', 'caregiver_married',
        'household_size_log', 'primary_lang_english', 'female', 'hispanic', 
        'asian', outcomes_bl, outcomes_k
      )
    ),
    diff = ctrl - treat
  ) |> 
  arrange(y) |> relocate(ctrl, .after = all) |> 
  write.csv('../output/equivalence/miss_all-bl-k.csv', na = '', row.names = F)

## --- Complete Cases ----------------------------------------------------------

#* This section computes the share of participants with observed data on all
#* model variables (complete cases) for each outcome in each follow-up wave.

df$cc_bl_covs <- complete.cases(select(df, all_of(covariates[3:11])))
df |> 
  mutate(
    across(  # Create complete case indicators for each outcome
      all_of(c(outcomes_pk3, outcomes_pk4, outcomes_k)),
      ~ ifelse(
        cc_bl_covs & !is.na(.x) 
        & !is.na(get(str_replace(cur_column(), '_p?k(3|4)?$', '_bl'))),
        1, 0
      ),
      .names = "cc_{col}"
    )
  ) |> 
  summarise(  # Caclulate N and % complete cases for each outcome by tx group
    across(matches('^cc_'), list(n = ~ sum(.x), pct = ~ mean(.x))), 
    .by = mont_offer
  ) |> 
  data.table::transpose(keep.names = 'y') |> 
  slice(-1) |> 
  rename(treat = V1, ctrl = V2) |> 
  mutate(  # Prep dataset for later table creation
    wave = case_when(
      grepl('_pk3', y) ~ 'pk3', grepl('_pk4', y) ~ 'pk4', grepl('_k', y) ~ 'k'
    ),
    measure = ifelse(grepl('_n$', y), 'n', 'pct'),
    y = str_remove_all(y, '^cc_|_p?k(3|4)?_(n|pct)')
  ) |> 
  filter(!grepl('^bl_covs', y)) |> 
  tidyr::pivot_wider(names_from = measure, values_from = c(ctrl, treat)) |> 
  mutate(all_n = ctrl_n + treat_n, all_pct = all_n / 588) |>  # Total N
  relocate(all_n, .after = wave) |> relocate(all_pct, .after = all_n) |> 
  write.csv('../output/equivalence/miss_cc-k.csv', na = '', row.names = F)

## --- Equivalence on Baseline Variables ---------------------------------------

#* These tests replace the treatment indicator with a missing indicator to 
#* "trick" the baseline equivalence test function into comparing those w/ and w/o
#* missing data, looking at their equivalence on baseline covariates & outcomes.

df_miss_bal <- df  # Save copy of dataset to use in function

# 1: Missing on any model variable
df_miss_bal$mont_offer <- !complete.cases(  # Overwrite treat ind w/ miss ind
  select(df_miss_bal, all_of(covariates), assessed_bl, assessed_k)
)
bnry <- c(T,T,F,F,rep(T,11),rep(F,8),T)
res1 <- map2(
  c(covariates[c(1,2,4)], "household_size", covariates[5:14], "white", outcomes_bl),
  bnry,
  ~ test_baseline_eq_cc(.x, dat = df_miss_bal, binary = .y, full_stats = T)
) |>
  list_rbind() |> 
  select(y, coef, se, p, g, n_obsrv = n_ctrl, n_miss = n_treat) |> 
  mutate(spec = "miss any var")

# 2: Missing any assessments
df_miss_bal$mont_offer <- ifelse(
  df$assessed_bl == 0 | df$assessed_k == 0, 1, 0
)
bnry <- c(F,rep(T,11))
res2 <- map2(
  c("household_size", covariates[5:14], "white"), bnry,
  ~ test_baseline_eq_cc(.x, dat = df_miss_bal, binary = .y, full_stats = T)
) |>
  list_rbind() |> 
  select(y, coef, se, p, g, n_obsrv = n_ctrl, n_miss = n_treat) |> 
  mutate(spec = "miss either assess")

# 3: Missing baseline outcome, observed baseline covar
df_miss_bal$mont_offer <- ifelse(df$assessed_bl == 0, 1, 0)
bnry <- c(F,rep(T,11))
res3 <- map2(
  c("household_size", covariates[5:14], "white"), bnry,
  ~ test_baseline_eq_cc(.x, dat = df_miss_bal, binary = .y, full_stats = T)
) |>
  list_rbind() |> 
  select(y, coef, se, p, g, n_obsrv = n_ctrl, n_miss = n_treat) |> 
  mutate(spec = "miss bl assess")

# 4: Missing K outcome, observed baseline covar / outcome
df_miss_bal$mont_offer <- ifelse(df$assessed_k == 0, 1, 0)
bnry <- c(F,rep(T,11),rep(F,8),T)
res4 <- map2(
  c("household_size", covariates[5:14], "white", outcomes_bl), 
  bnry,
  ~ test_baseline_eq_cc(
    .x, dat = df_miss_bal, binary = .y, full_stats = T, keep_miss_k = T
  )
) |>
  list_rbind() |> 
  select(y, coef, se, p, g, n_obsrv = n_ctrl, n_miss = n_treat) |> 
  mutate(spec = "miss k, observe bl")

# 5: Missing K outcome, imputed baseline outcomes
df_list <- list.files('../data/imputed', 'itt', full.names = T) |>
  map(readRDS)
df_list <- list(
  df_list[[8]], df_list[[7]], df_list[[9]], df_list[[3]], df_list[[2]], 
  df_list[[1]], df_list[[6]], df_list[[5]], df_list[[4]]
)
df_list <- map(
  df_list, ~ mutate(.x, mont_offer = ifelse(assessed_k == 0, 1, 0))
)
bnry <- c(rep(F,8),T)
res5 <- pmap(
  list(df_list, outcomes_bl, bnry),
  ~ test_baseline_eq_imp(..1, ..2, binary = ..3, full_stats = T)
) |> 
  list_rbind() |> 
  select(y, coef, se, p, g, n_obsrv = n_ctrl, n_miss = n_treat) |> 
  mutate(spec = "miss k, imputed bl")

write.csv(  # Save
  bind_rows(res1, res2, res3, res4, res5),
  '../output/equivalence/miss_equivalence.csv', na = '', row.names = F
)

