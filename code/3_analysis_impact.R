#==============================================================================#
# Impact Analyses                                                              #
#==============================================================================#

library(purrr); library(tibble)
source(here('code/0_functions_analysis.R'))  # loads analysis functions
source(here('code/1_data-setup.R'))  # runs the data setup script

# Imputed ======================================================================

## ITT -------------------------------------------------------------------------

# Load imputed datasets
df_list <- list.files(here('data/imputed'), '_itt', full.names = T) |> 
  set_names(~ str_remove(basename(.x), '_itt.rds')) |> 
  map(readRDS)
df_list <- list(  # Arrange in proper order
  df_list[['wjlw']], df_list[['wjap']], df_list[['wjpv']], df_list[['htks']],
  df_list[['fdigit']], df_list[['bdigit']], df_list[['tom']], df_list[['sps']],
  df_list[['puzz']]
)  
# Run models
res <- map2(df_list, outcomes_k, ~ run_mod_imp_itt(.x, .y, full_stats = T)) |> 
  list_rbind()
write.csv(res, here('output/k/imp_itt.csv'), na = '', row.names = F)  # Save

## CACE ------------------------------------------------------------------------

df_list <- list.files(here('data/imputed'), '_cace', full.names = T) |> 
  set_names(~ str_remove(basename(.x), '_cace.rds')) |> 
  map(readRDS)
df_list <- list(
  df_list[['wjlw']], df_list[['wjap']], df_list[['wjpv']], df_list[['htks']],
  df_list[['fdigit']], df_list[['bdigit']], df_list[['tom']], df_list[['sps']], 
  df_list[['puzz']]
)
res <- map2(df_list, outcomes_k, ~ run_mod_imp_cace(.x, .y)) |> list_rbind()
write.csv(res, here('output/k/imp_cace.csv'), na = '', row.names = F)

# Complete Case ================================================================

## ITT -------------------------------------------------------------------------

# Create dataframe of function parameters
func_params <- tibble(y = outcomes_k, covars = list(covariates_ctr))
res <- pmap(func_params, ~ run_mod_cc(..1, ..2))  # Run models
treat_fx <- map(res, `[[`, 2) |> list_rbind() |>  # Extract results
  select(-statistic) |> 
  relocate(y) |> relocate(p, .after = se) |> 
  relocate(g, .after = p) |> relocate(g_se, .after = g)
write.csv(treat_fx, here('output/k/cc_itt.csv'), na = '', row.names = F)  # Save

## CACE ------------------------------------------------------------------------

func_params <- tibble(y = outcomes_k, covars = list(covariates_ctr))
res <- pmap(func_params, ~ run_mod_cc(..1, ..2, cace = T))
treat_fx <- map(res, `[[`, 2) |> list_rbind() |> 
  select(-c(statistic)) |> 
  relocate(y) |> relocate(p, .after = se)
write.csv(treat_fx, here('output/k/cc_cace.csv'), na = '', row.names = F)

# Domain Average Effect Sizes ==================================================

# Get correlations between each variable
cor_fdig_bdig <- cor(df$fdigit_k, df$bdigit_k, use = "complete.obs")
cor_fdig_tom <- cor(df$fdigit_k, df$tom_k, use = "complete.obs")
cor_bdig_tom <- cor(df$bdigit_k, df$tom_k, use = "complete.obs")
cor_fdig_htks <- cor(df$fdigit_k, df$htks_k, use = "complete.obs")
cor_bdig_htks <- cor(df$bdigit_k, df$htks_k, use = "complete.obs")
cor_tom_htks <- cor(df$tom_k, df$htks_k, use = "complete.obs")
corr <- mean(c(  # Get mean correlation among all variables
  cor_fdig_bdig, cor_fdig_tom, cor_bdig_tom, cor_fdig_htks, cor_bdig_htks, cor_tom_htks
))

## Imp -------------------------------------------------------------------------

# Load imputed datasets & prep for modeling function
df_list <- list.files(here('data/imputed'), '_itt', full.names = T) |> 
  set_names(~ str_remove(basename(.x), '_itt.rds')) |> 
  map(readRDS)
df_fdigit <- df_list[['fdigit']] |> mutate(outcome = 'fdigit_k', y_std = fdigit_k)
df_bdigit <- df_list[['bdigit']] |> mutate(outcome = 'bdigit_k', y_std = bdigit_k)
df_tom <- df_list[['tom']] |> mutate(outcome = 'tom_k', y_std = tom_k)
df_htks <- df_list[['htks']] |> mutate(outcome = 'htks_k', y_std = htks_k)
df_g <- list_rbind(list(df_fdigit, df_bdigit, df_tom, df_htks))  # df for func
res_imp <- get_domain_effect_size() |> mutate(miss = 'imp')  # Run model

## CC --------------------------------------------------------------------------

# Load complete case ITT results
df_g <- readr::read_csv(here('output/k/cc_itt.csv'), show_col_types = F, progress = F)
gbar <- mean(df_g$g[4:7])  # Get mean Hedges' g across the four variables
gbar_se <- sqrt(  # Compute standard error for mean g
  df_g$g_se[5]^2 + df_g$g_se[6]^2 + df_g$g_se[7]^2 + df_g$g_se[4]^2 + corr * (
    df_g$g_se[5] * df_g$g_se[6] + df_g$g_se[5] * df_g$g_se[7] + df_g$g_se[5] * df_g$g_se[4]
    + df_g$g_se[6] * df_g$g_se[5] + df_g$g_se[6] * df_g$g_se[7] + df_g$g_se[6] * df_g$g_se[4]
    + df_g$g_se[7] * df_g$g_se[5] + df_g$g_se[7] * df_g$g_se[6] + df_g$g_se[7] * df_g$g_se[4]
    + df_g$g_se[4] * df_g$g_se[5] + df_g$g_se[4] * df_g$g_se[6] + df_g$g_se[4] * df_g$g_se[7]
  )
) / 4
deg_frdm <- df_g$n_ctrl[df_g$y == 'bdigit_k'] + df_g$n_treat[df_g$y == 'bdigit_k']
t <- gbar / gbar_se  # Compute t & p, and put results in a df
p <- 2 * pt(-abs(t), deg_frdm)
res_cc <- tibble(gbar = gbar, gbar_se = gbar_se, p = p, t = t, df = deg_frdm, miss = 'cc')

## Combine & Save --------------------------------------------------------------

bind_rows(res_imp, res_cc) |> relocate(miss) |>
  write.csv(here('output/k/domain_avg_effect_size.csv'), na = '', row.names = F)

# Moderators ===================================================================

## Imputed ---------------------------------------------------------------------

# Load imputed datasets & build function parameter dataframe
df_list <- list.files(here('data/imputed/moderators/race'), full.names = T) |>
  set_names(~ str_remove(basename(.x), '\\.rds')) |> 
  map(readRDS)
func_params <- tibble(
  df = list(
    df_list[['wjlw']], df_list[['wjap']], df_list[['wjpv']], df_list[['htks']],
    df_list[['fdigit']], df_list[['bdigit']], df_list[['tom']], df_list[['sps']],
    df_list[['puzz']]
  ),
  y = outcomes_k, covars = list(covariates), 
  mdrtr = list(c('asian', 'black', 'racemulti', 'raceother'))
)

df_list <- list.files(here('data/imputed/moderators/hispanic'), full.names = T) |>
  set_names(~ str_remove(basename(.x), '\\.rds')) |> 
  map(readRDS)
func_params <- bind_rows(
  func_params,
  tibble(
    df = list(
      df_list[['wjlw']], df_list[['wjap']], df_list[['wjpv']], df_list[['htks']],
      df_list[['fdigit']], df_list[['bdigit']], df_list[['tom']], df_list[['sps']],
      df_list[['puzz']]
    ),
    y = outcomes_k, covars = list(covariates),
    mdrtr = list('hispanic')
  )
)

df_list <- list.files(here('data/imputed/moderators/income'), full.names = T) |>
  set_names(~ str_remove(basename(.x), '\\.rds')) |> 
  map(readRDS)
ns <- map(1:9, \(i) {  # Ns for results table
  df_list[[i]] |> 
    summarise(n = sum(income_over_75k), .by = c(imp_num, mont_offer)) |> 
    summarise(n_high = mean(n), .by = mont_offer) |> 
    mutate(id = i)
}) |> 
  list_rbind() |> 
  tidyr::pivot_wider(names_from = mont_offer, values_from = n_high) |> 
  summarise(ctrl = mean(`0`), treat = mean(`1`))
func_params <- bind_rows(
  func_params,
  tibble(
    df = list(
      df_list[['wjlw']], df_list[['wjap']], df_list[['wjpv']], df_list[['htks']],
      df_list[['fdigit']], df_list[['bdigit']], df_list[['tom']], df_list[['sps']],
      df_list[['puzz']]
    ),
    y = outcomes_k, covars = list(covariates), 
    mdrtr = list('income_over_75k')
  )
)

df_list <- list.files(here('data/imputed/moderators/education'), full.names = T) |>
  set_names(~ str_remove(basename(.x), '\\.rds')) |> 
  map(readRDS)
func_params <- bind_rows(
  func_params,
  tibble(
    df = list(
      df_list[['wjlw']], df_list[['wjap']], df_list[['wjpv']], df_list[['htks']],
      df_list[['fdigit']], df_list[['bdigit']], df_list[['tom']], df_list[['sps']],
      df_list[['puzz']]
    ),
    y = outcomes_k, covars = list(covariates), 
    mdrtr = list('caregiver_bachelors')
  )
)

df_list <- list.files(here('data/imputed/moderators/gender'), full.names = T) |>
  set_names(~ str_remove(basename(.x), '\\.rds')) |> 
  map(readRDS)
func_params <- bind_rows(
  func_params,
  tibble(
    df = list(
      df_list[['wjlw']], df_list[['wjap']], df_list[['wjpv']], df_list[['htks']],
      df_list[['fdigit']], df_list[['bdigit']], df_list[['tom']], df_list[['sps']],
      df_list[['puzz']]
    ),
    y = outcomes_k, covars = list(covariates), 
    mdrtr = list('female')
  )
)

df_list <- list.files(here('data/imputed/moderators/htks'), full.names = T) |>
  set_names(~ str_remove(basename(.x), '\\.rds')) |> 
  map(readRDS)
func_params <- bind_rows(
  func_params,
  tibble(
    df = list(
      df_list[['wjlw']], df_list[['wjap']], df_list[['wjpv']],
      df_list[['fdigit']], df_list[['bdigit']], df_list[['tom']], df_list[['sps']],
      df_list[['puzz']]
    ),
    y = outcomes_k[-4], covars = list(covariates),
    mdrtr = list('htks_bl')
  )
)

df_list <- list.files(
  here('data/imputed/moderators/baseline_outcome'), full.names = T
) |>
  set_names(~ str_remove(basename(.x), '\\.rds')) |> 
  map(readRDS)
func_params <- bind_rows(
  func_params,
  tibble(
    df = list(
      df_list[['wjlw']], df_list[['wjap']], df_list[['wjpv']], df_list[['htks']],
      df_list[['fdigit']], df_list[['bdigit']], df_list[['tom']], df_list[['sps']],
      df_list[['puzz']]
    ),
    y = outcomes_k, covars = list(covariates), 
    mdrtr = list(
      list('wjlw_bl'), list('wjap_bl'), list('wjpv_bl'), list('htks_bl'), 
      list('fdigit_bl'), list('bdigit_bl'), list('tom_bl'), list('sps_bl'), 
      list('puzz_bl')
    )
  )
)

res <- pmap(  # Run model
  func_params, ~ run_mod_imp_itt(..1, ..2, ..3, ..4, full_stats = T)
) |> list_rbind()
res |> 
  mutate(
    y = factor(y, levels = outcomes_k),
    moderator = factor(moderator, levels = unique(res$moderator))
  ) |> 
  arrange(moderator, y, term) |> 
  write.csv(here('output/k/moderators/imp_itt.csv'), na = '', row.names = F)

## Complete Case ---------------------------------------------------------------

func_params <- bind_rows(  # Create function argument dataframe
  tibble(
    y = outcomes_k, covars = list(covariates_ctr),
    mdrtr = list(c('asian', 'black', 'racemulti', 'raceother')),
  ),
  tibble(
    y = outcomes_k, covars = list(covariates_ctr), 
    mdrtr = list('hispanic'),
  ),
  tibble(
    y = outcomes_k, covars = list(covariates_ctr),
    mdrtr = list('income_over_75k'),
  ),
  tibble(
    y = outcomes_k, covars = list(covariates_ctr),
    mdrtr = list('caregiver_bachelors'),
  ),
  tibble(
    y = outcomes_k, covars = list(covariates_ctr),
    mdrtr = list('female'),
  ),
  tibble(
    y = outcomes_k, covars = list(covariates_ctr),
    mdrtr = list('htks_bl'),
  ),
  tibble(
    y = outcomes_k, covars = list(covariates_ctr),
    mdrtr = c(
      list('wjlw_bl'), list('wjap_bl'), list('wjpv_bl'), list('htks_bl'), 
      list('fdigit_bl'), list('bdigit_bl'), list('tom_bl'), list('sps_bl'), 
      list('puzz_bl')
    )
  )
)
res <- pmap(func_params, ~ run_mod_cc(..1, ..2, ..3))
treat_fx <- map(res, `[[`, 2) |> 
  list_rbind() |> 
  select(-statistic) |> 
  relocate(moderator) |> relocate(y) |> relocate(p, .after = se)
write.csv(treat_fx, here('output/k/moderators/cc_itt.csv'), na = '', row.names = F)

# PK ===========================================================================

outcomes_pk4 <- str_replace(outcomes_bl, 'bl', 'pk4')  # Create Y & covar vecs
outcomes_pk3 <- str_replace(outcomes_bl, 'bl', 'pk3')
covariates_pk4 <- c('assessed_in_spanish_pk4', covariates[2:14])
covariates_pk3 <- c('assessed_in_spanish_pk3', covariates[2:14])
covariates_pk4_ctr <- str_c(covariates_pk4, '_ctr')
covariates_pk3_ctr <- str_c(covariates_pk3, '_ctr')

## Imputed ---------------------------------------------------------------------

### ITT ------------------------------------------------------------------------

# Load imputed datasets
df_list_pk4 <- list.files(here('data/imputed/pk4'), '_itt', full.names = T) |> 
  set_names(~ str_remove(basename(.x), '_itt.rds')) |> 
  map(readRDS)
df_list_pk3 <- list.files(here('data/imputed/pk3'), '_itt', full.names = T) |> 
  set_names(~ str_remove(basename(.x), '_itt.rds')) |> 
  map(readRDS)
df_list <- list(  # Arrange in proper order
  df_list_pk4[['wjlw']], df_list_pk4[['wjap']], df_list_pk4[['wjpv']], 
  df_list_pk4[['htks']], df_list_pk4[['fdigit']], df_list_pk4[['bdigit']], 
  df_list_pk4[['tom']], df_list_pk4[['sps']], df_list_pk4[['puzz']],
  df_list_pk3[['wjlw']], df_list_pk3[['wjap']], df_list_pk3[['wjpv']], 
  df_list_pk3[['htks']], df_list_pk3[['fdigit']], df_list_pk3[['bdigit']], 
  df_list_pk3[['tom']], df_list_pk3[['sps']], df_list_pk3[['puzz']]
)

func_params <- tibble(  # Set up function parameters
  df = df_list,
  y = c(outcomes_pk4, outcomes_pk3),
  covars = ifelse(
    grepl('_pk4', y), list(covariates_pk4), list(covariates_pk3)
  ),
  wave = c(rep('pk4', 9), rep('pk3', 9))
)

res <- pmap(  # Run models & make results table
  func_params, ~ run_mod_imp_itt(..1, ..2, ..3, full_stats = T)
) |> 
  list_rbind() |> 
  mutate(wave = func_params$wave) |> 
  relocate(wave) |> relocate(y)
write.csv(res, here('output/pk/imp_itt.csv'), na = '', row.names = F)  # Save

### CACE -----------------------------------------------------------------------

df_list_pk4 <- list.files(here('data/imputed/pk4'), '_cace', full.names = T) |> 
  set_names(~ str_remove(basename(.x), '_cace.rds')) |> 
  map(readRDS)
df_list_pk3 <- list.files(here('data/imputed/pk3'), '_cace', full.names = T) |> 
  set_names(~ str_remove(basename(.x), '_cace.rds')) |> 
  map(readRDS)
df_list <- list(
  df_list_pk4[['wjlw']], df_list_pk4[['wjap']], df_list_pk4[['wjpv']], 
  df_list_pk4[['htks']], df_list_pk4[['fdigit']], df_list_pk4[['bdigit']], 
  df_list_pk4[['tom']], df_list_pk4[['sps']], df_list_pk4[['puzz']],
  df_list_pk3[['wjlw']], df_list_pk3[['wjap']], df_list_pk3[['wjpv']], 
  df_list_pk3[['htks']], df_list_pk3[['fdigit']], df_list_pk3[['bdigit']], 
  df_list_pk3[['tom']], df_list_pk3[['sps']], df_list_pk3[['puzz']]
)

func_params <- tibble(
  df = df_list,
  y = c(outcomes_pk4, outcomes_pk3),
  covars = ifelse(
    grepl('_pk4', y), list(covariates_pk4), list(covariates_pk3)
  ),
  wave = c(rep('pk4', 9), rep('pk3', 9))
)

res <- pmap(
  func_params, ~ run_mod_imp_cace(..1, ..2, ..3)
) |> 
  list_rbind() |> 
  mutate(wave = func_params$wave) |> 
  relocate(wave) |> relocate(y)
write.csv(res, here('output/pk/imp_cace.csv'), na = '', row.names = F)

## Complete Case ---------------------------------------------------------------

func_params <- tibble(  # Set up function parameters
  y = c(outcomes_pk4, outcomes_pk3), 
  covars = ifelse(
    grepl('_pk4', y), list(covariates_pk4_ctr), list(covariates_pk3_ctr)
  ),
  wave = c(rep('pk4', 9), rep('pk3', 9))
)

### ITT ------------------------------------------------------------------------

res <- pmap(func_params, ~ run_mod_cc(..1, ..2))  # Run models
treat_fx <- map(res, `[[`, 2) |>  # Make results table
  list_rbind() |> 
  select(-statistic, -term) |> 
  mutate(wave = func_params$wave) |> 
  relocate(wave) |> relocate(y) |> relocate(p, .after = se) |> 
  relocate(g, .after = p) |> relocate(g_se, .after = g)
write.csv(treat_fx, here('output/pk/cc_itt.csv'), na = '', row.names = F)  # Save

### CACE -----------------------------------------------------------------------

res <- pmap(func_params, ~ run_mod_cc(..1, ..2, cace = T))
treat_fx <- map(res, `[[`, 2) |> 
  list_rbind() |> 
  select(-statistic, -term) |> 
  mutate(wave = func_params$wave) |> 
  relocate(wave) |> relocate(y) |> relocate(p, .after = se) |> 
  relocate(g, .after = p) |> relocate(g_se, .after = g)
write.csv(treat_fx, here('output/pk/cc_cace.csv'), na = '', row.names = F)
