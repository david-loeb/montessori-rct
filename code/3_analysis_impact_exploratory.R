#==============================================================================#
# Exploratory Analyses to Further Investigate Impacts                          #
#==============================================================================#

#* This script explores school enrollment patterns and their potential influence
#* on the treatment effects.

library(purrr); library(tibble); library(tidyr)
source(here::here('code/0_functions_analysis.R'))
source(here::here('code/1_data-setup.R'))

# School Settings ==============================================================

#* This section investigates the school types and curricula for treatment and 
#* control children.

## Descriptives ----------------------------------------------------------------

bind_rows(  # School Type/Sector
  df |> 
    summarise(n = n(), .by = c(schl_type_pk3, mont_offer)) |> 
    mutate(wave = 'pk3') |> 
    rename(schl_type = schl_type_pk3),
  df |> 
    summarise(n = n(), .by = c(schl_type_pk4, mont_offer)) |> 
    mutate(wave = 'pk4') |> 
    rename(schl_type = schl_type_pk4),
  df |> 
    summarise(n = n(), .by = c(schl_type_k, mont_offer)) |> 
    mutate(wave = 'k') |> 
    rename(schl_type = schl_type_k)
) |> 
  pivot_wider(names_from = mont_offer, values_from = n) |> 
  rename(ctrl = `0`, treat = `1`) |> 
  mutate(
    ctrl = ifelse(is.na(ctrl), 0, ctrl), treat = ifelse(is.na(treat), 0, treat)
  ) |> 
  mutate(
    ctrl_pct = ctrl / sum(ctrl), treat_pct = treat / sum(treat), .by = wave
  ) |>
  pivot_wider(
    id_cols = schl_type, 
    names_from = wave, 
    values_from = c(ctrl_pct, treat_pct)
  ) |> 
  mutate(
    across(ends_with('pk3'), ~ ifelse(is.na(.x), 0, .x)),
    schl_type = factor(
      schl_type, levels = c(
        'mont_study', 'mont_nonstudy_pub', 'mont_nonstudy_priv', 'public_reg',
        'public_charter', 'public_magnet', 'private', 'not_in_school',
        'out_of_us', 'unknown'
      )
    )
  ) |> 
  arrange(schl_type) |> 
  select(schl_type, matches('pk3$'), matches('pk4$'), matches('k$')) |> 
  write.csv(
    here('output/k/exploratory/school-settings_descriptives_type.csv'),
    na = '', row.names = F
  )

bind_rows(  # Curriculum - Aggregated
  df |> 
    summarise(n = n(), .by = c(schl_curric_pk3, mont_offer)) |> 
    mutate(wave = 'pk3') |> 
    rename(schl_curric = schl_curric_pk3),
  df |> 
    summarise(n = n(), .by = c(schl_curric_pk4, mont_offer)) |> 
    mutate(wave = 'pk4') |> 
    rename(schl_curric = schl_curric_pk4),
  df |> 
    summarise(n = n(), .by = c(schl_curric_k, mont_offer)) |> 
    mutate(wave = 'k') |> 
    rename(schl_curric = schl_curric_k)
) |> 
  pivot_wider(names_from = mont_offer, values_from = n) |> 
  rename(ctrl = `0`, treat = `1`) |> 
  mutate(
    ctrl_pct = ctrl / sum(ctrl), treat_pct = treat / sum(treat), .by = wave
  ) |>
  pivot_wider(
    id_cols = schl_curric, 
    names_from = wave, 
    values_from = c(ctrl_pct, treat_pct)
  ) |> 
  mutate(
    schl_curric = factor(
      schl_curric, 
      levels = c('mont', 'play_proj', 'academic', 'other_unsure', 'unknown')
    )
  ) |> 
  arrange(schl_curric) |> 
  select(schl_curric, matches('pk3$'), matches('pk4$'), matches('k$')) |> 
  write.csv(
    here('output/k/exploratory/school-settings_descriptives_curriculum_agg.csv'),
    na = '', row.names = F
  )

bind_rows(  # Curriculum - Detailed
  df |> 
    summarise(n = n(), .by = c(schl_curric_detail_pk3, mont_offer)) |> 
    mutate(wave = 'pk3') |> 
    rename(schl_curric_detail = schl_curric_detail_pk3),
  df |> 
    summarise(n = n(), .by = c(schl_curric_detail_pk4, mont_offer)) |> 
    mutate(wave = 'pk4') |> 
    rename(schl_curric_detail = schl_curric_detail_pk4),
  df |> 
    summarise(n = n(), .by = c(schl_curric_detail_k, mont_offer)) |> 
    mutate(wave = 'k') |> 
    rename(schl_curric_detail = schl_curric_detail_k)
) |> 
  pivot_wider(names_from = mont_offer, values_from = n) |> 
  rename(ctrl = `0`, treat = `1`) |> 
  mutate(treat = ifelse(is.na(treat), 0, treat)) |> 
  mutate(
    ctrl_pct = ctrl / sum(ctrl), treat_pct = treat / sum(treat), .by = wave
  ) |>
  pivot_wider(
    id_cols = schl_curric_detail, 
    names_from = wave, 
    values_from = c(ctrl_pct, treat_pct)
  ) |> 
  mutate(
    across(ends_with('k'), ~ ifelse(is.na(.x), 0, .x)),
    schl_curric_detail = factor(
      schl_curric_detail, levels = c(
        'Montessori', 'Creative Curriculum', 'Play-Based (Unspecified)', 
        'Academic Focus', 'IB', 'Reggio', 'Project-Based (Unspecified)',
        'Responsive Classroom', 'Expeditionary Learning', 'Every Child Ready',
        'Tools of the Mind', 'Nature-Based', 'HighScope', 'Unclear', 'Unknown'
      )
    )
  ) |> 
  arrange(schl_curric_detail) |> 
  select(schl_curric_detail, matches('pk3$'), matches('pk4$'), matches('k$')) |> 
  write.csv(
    here('output/k/exploratory/school-settings_descriptives_curriculum_detail.csv'),
    na = '', row.names = F
  )

bind_rows(  # Ages in Classroom
  df |> 
    summarise(n = n(), .by = c(schl_class_age_mix_pk3, mont_offer)) |> 
    mutate(wave = 'pk3') |> 
    rename(schl_class_age_mix = schl_class_age_mix_pk3),
  df |> 
    summarise(n = n(), .by = c(schl_class_age_mix_pk4, mont_offer)) |> 
    mutate(wave = 'pk4') |> 
    rename(schl_class_age_mix = schl_class_age_mix_pk4),
  df |> 
    summarise(n = n(), .by = c(schl_class_age_mix_k, mont_offer)) |> 
    mutate(wave = 'k') |> 
    rename(schl_class_age_mix = schl_class_age_mix_k)
) |> 
  pivot_wider(names_from = mont_offer, values_from = n) |> 
  rename(ctrl = `0`, treat = `1`) |> 
  mutate(
    ctrl_pct = ctrl / sum(ctrl, na.rm = T), 
    treat_pct = treat / sum(treat, na.rm = T), 
    .by = wave
  ) |>
  pivot_wider(
    id_cols = schl_class_age_mix, 
    names_from = wave, 
    values_from = c(ctrl_pct, treat_pct)
  ) |> 
  select(schl_class_age_mix, matches('pk3$'), matches('pk4$'), matches('k$')) %>%
  mutate(
    schl_class_age_mix = factor(
      schl_class_age_mix, levels = c("1", "2", "3", "Unclear", "Not in School", "Unknown")
    ),
    across(2:ncol(.), ~ ifelse(is.na(.x), 0, .x))
  ) |> 
  arrange(schl_class_age_mix) |> 
  write.csv(
    here('output/k/exploratory/school-settings_descriptives_class-age-mix.csv'),
    na = '', row.names = F
  )

bind_rows(  # School Span (ie childcare, prek, K+)
  df |> 
    summarise(n = n(), .by = c(schl_span_pk3, mont_offer)) |> 
    mutate(wave = 'pk3') |> 
    rename(schl_span = schl_span_pk3),
  df |> 
    summarise(n = n(), .by = c(schl_span_pk4, mont_offer)) |> 
    mutate(wave = 'pk4') |> 
    rename(schl_span = schl_span_pk4),
  df |> 
    summarise(n = n(), .by = c(schl_span_k, mont_offer)) |> 
    mutate(wave = 'k') |> 
    rename(schl_span = schl_span_k)
) |> 
  pivot_wider(names_from = mont_offer, values_from = n) |> 
  rename(ctrl = `0`, treat = `1`) |> 
  mutate(
    ctrl_pct = ctrl / sum(ctrl), treat_pct = treat / sum(treat, na.rm = T), 
    .by = wave
  ) |>
  pivot_wider(
    id_cols = schl_span, 
    names_from = wave, 
    values_from = c(ctrl_pct, treat_pct)
  ) |> 
  select(schl_span, matches('pk3$'), matches('pk4$'), matches('k$')) |> 
  mutate(
    across(everything(), ~ ifelse(is.na(.x), 0, .x)),
    schl_span = factor(
      schl_span, levels = c(
        "elem", "prek", "childcare", "unclear", "not_in_school", "unknown"
      )
    )
  ) |> 
  arrange(schl_span) |> 
  write.csv(
    here('output/k/exploratory/school-settings_descriptives_span.csv'),
    na = '', row.names = F
  )

## Principal Stratification ----------------------------------------------------

### Setup ----------------------------------------------------------------------

df <- df |>  # Create variables for use in models
  mutate(
    schl_type_pstrat = case_match(
      schl_type_pk3,
      'private' ~ 'priv_nonmont',
      c('public_reg', 'public_charter', 'public_magnet') ~ 'pub_nonmont',
      .default = schl_type_pk3
    ),
    schl_curric_pstrat = case_match(
      schl_curric_pk3, 
      'other_unsure' ~ 'unsure', 'play_proj' ~ 'play', 
      .default = schl_curric_pk3
    )
  ) |> 
  fastDummies::dummy_cols('schl_type_pstrat') |> 
  fastDummies::dummy_cols('schl_curric_pstrat') |> 
  mutate(  # create condensed versions of school type & curriculum
    schl_type_pstrat_mont_pub_all = ifelse(
      schl_type_pstrat %in% c('mont_study', 'mont_nonstudy_pub'), 1, 0
    ),
    schl_type_pstrat_mont_nonstudy_all = ifelse(
      schl_type_pstrat %in% c('mont_nonstudy_pub', 'mont_nonstudy_priv'), 1, 0
    ),
    schl_type_pstrat_mont_all = ifelse(
      grepl('mont', schl_type_pstrat), 1, 0
    ),
    schl_type_pstrat_pub_nonstudy_all = ifelse(
      schl_type_pstrat %in% c('mont_nonstudy_pub', 'pub_nonmont'), 1, 0
    ),
    schl_type_pstrat_priv_all = ifelse(
      schl_type_pstrat %in% c('mont_nonstudy_priv', 'priv_nonmont'), 1, 0
    ),
    schl_type_pstrat_pub_reg_or_none = ifelse(
      schl_type_pstrat %in% c('pub_nonmont', 'not_in_school'), 1, 0
    ),
    schl_type_pstrat_nonmont_all = ifelse(
      !grepl('mont', schl_type_pstrat), 1, 0
    ),
    schl_curric_pstrat_not_play_proj = ifelse(
      schl_curric_pstrat %in% c('academic', 'unsure'), 1, 0
    )
  ) |>  
  mutate(  # Use group-mean centered y to remove cluster counfounding
    across(
      all_of(outcomes_k), 
      ~ .x - mean(.x, na.rm = T), 
      .names = '{col}_ctr_group'
    ),
    .by = lottery_id
  )

margin_vals <- df |>  # Compute vals from "margins" of a compliance/strata table
  filter(
    assessed_k == 1, 
    !(schl_type_pk3 == 'unknown' & schl_type_pk4 == 'unknown')  # 2 unknown pk
  ) |> 
  summarise(
    dplyover::across2x(  # Sample mean Y in each school type/curric by tx group
      all_of(c(outcomes_k, str_c(outcomes_k, '_ctr_group'))), 
      matches('schl_.+_pstrat_'),
      ~ mean(.x[.y == 1], na.rm = T),
      .names = '{xcol}.{ycol}'
    ),
    across(  # Sample N and % in each school type/curric by tx group
      matches('^schl_.+_pstrat_'), 
      list(pct = mean, n = sum), 
      .names = '{fn}.{col}'
    ),
    .by = mont_offer
  ) %>%   # Reshape & clean for use in model
  pivot_longer(2:ncol(.), names_to = c('.values', 'group'), names_sep = '\\.') |> 
  pivot_wider(names_from = mont_offer, values_from = value) |> 
  mutate(group = str_remove(group, 'schl_.+_pstrat_')) |> 
  rename(var = `.values`)

### Model ----------------------------------------------------------------------

groups <- list(  # Sets of school type & curricula that define strata
  c('mont_study', 'pub_nonstudy_all', 'priv_all', 'not_in_school'),
  c(
    'mont_study', 'mont_nonstudy_all', 'pub_nonmont', 'priv_nonmont', 
    'not_in_school'
  ),
  c(
    'mont_study', 'mont_nonstudy_pub', 'mont_nonstudy_priv', 
    'pub_nonmont', 'priv_nonmont', 'not_in_school'
  ),
  c(
    'mont_pub_all', 'mont_nonstudy_priv', 'pub_nonmont', 'priv_nonmont', 
    'not_in_school'
  ),
  c('mont_all', 'pub_nonmont', 'priv_nonmont', 'not_in_school'),
  c('mont', 'play', 'academic', 'unsure')
)

func_params <- tibble(  # Each outcome w/ each strata set, 3 min/max shrink levels
  groups = rep(rep(groups, 9), 3),
  y = rep(rep(str_c(outcomes_k, '_ctr_group'), each = 6), 3),
  shrink = rep(c(1, .75, .5), each = 54)
)

res <- pmap(  # Run mod
  func_params, ~ get_pstrat_bounds(groups = ..1, y = ..2, shrink = ..3)
)
res_bounds <- map(res, `[[`, 2) |> list_rbind() |>  # Get bounds
  mutate(groups = str_remove_all(as.character(groups), '"|c\\(|\\)'))
write.csv(  # Save
  res_bounds, here('output/k/exploratory/principal-strat_bounds_min-max.csv'),
  na = '', row.names = F
)

# Years in Montessori ==========================================================

#* This section creates a descriptive table showing study Montessori 
#* enrollment patterns by treatment group.

df |> 
  summarise(
    None = sum(study_mont_yrs_enrolled == 'never') / n(),
    `PK3 Only` = sum(study_mont_yrs_enrolled == 'pk3') / n(),
    `PK4 Only` = sum(study_mont_yrs_enrolled == 'pk4') / n(),
    `PK3 & PK4` = sum(study_mont_yrs_enrolled == 'pk_both') / n(),
    `PK3 & K` = sum(study_mont_yrs_enrolled == 'pk3_k') / n(),
    `PK4 & K` = sum(study_mont_yrs_enrolled == 'pk4_k') / n(),
    `All 3 Years` = sum(study_mont_yrs_enrolled == 'all') / n(),
    .by = mont_offer
  ) |> 
  data.table::transpose(keep.names = "enroll") |> 
  slice(-1) |> 
  rename(treat = V1, ctrl = V2) |> 
  relocate(ctrl, .before = treat) |> 
  write.csv(
    here('output/k/exploratory/yrs-in-mont_descriptives.csv'),
    na = '', row.names = F
  )

# School Changes ===============================================================

#* Explores the potential mediating influence of school stability. A 
#* variable that controls for school changes is added to the model to see if 
#* it reduces the magnitude of the treatment effect. There are 2 versions: 
#* 1) total school changes; 2) binary indicator for ever changed schools.

## Descriptives ----------------------------------------------------------------

# Descriptive table, share of treat & ctrl that changed schools (complete cases)
df$covars_cc <- complete.cases(select(df, all_of(covariates)))
df |> 
  filter(
    covars_cc,
    assessed_bl == 1, assessed_k == 1,
    schl_type_pk3 != 'unknown' & schl_type_pk4 != 'unknown'
  ) |> 
  summarise(
    c0 = sum(schl_changes_total == 0) / n(),
    c1 = sum(schl_changes_total == 1) / n(),
    c2 = sum(schl_changes_total == 2) / n(),
    c3 = sum(schl_changes_total == 3) / n(),
    avg = mean(schl_changes_total),
    .by = mont_offer
  ) |> 
  data.table::transpose(keep.names = "var") |> 
  slice(-1) |> 
  select(var, ctrl = V1, treat = V2) |> 
  write.csv(
    here('output/k/exploratory/school-changes_descriptives_cc.csv'),
    na = '', row.names = F
  )

## Analysis --------------------------------------------------------------------

c1 <- c(str_c(covariates, '_ctr'), 'schl_changes_total')
c2 <- c(str_c(covariates, '_ctr'), 'schl_changes_ever')
func_params <- tibble(  # Func params - each of the two versions per outcome
  y = rep(outcomes_k, 2),
  covars = list(
    c1,c1,c1,c1,c1,c1,c1,c1,c1,
    c2,c2,c2,c2,c2,c2,c2,c2,c2
  )
)
df_cc <- filter(  # Ensure participants w/ unknown schools are excluded
  df,
  assessed_bl == 1, assessed_k == 1,
  schl_type_pk3 != 'unknown' & schl_type_pk4 != 'unknown'
)

res <- pmap(func_params, ~ run_mod_cc(..1, ..2, dat = df_cc))  # Run mod
treat_fx <- map(res, `[[`, 1) |>  # Get treat effect param
  list_rbind() |> 
  filter(grepl('mont_offer|schl_change', term)) |> 
  select(-statistic) |> 
  relocate(y) |> relocate(p.value, .after = std.error) |> 
  mutate(version = c(rep('total', 18), rep('ever', 18))) |> 
  left_join(  # Add school change param
    map(res, `[[`, 2) |> list_rbind() |>  
      select(-c(coef, se, statistic, p)) |> 
      mutate(version = c(rep('total', 9), rep('ever', 9))), 
    c('y', 'term', 'version')
  ) |> 
  relocate(g, .after = p.value) |> relocate(g_se, .after = g) |> 
  rename(coef = estimate, se = std.error, p = p.value)
write.csv(  # Save
  treat_fx, here('output/k/exploratory/school-changes_cc_itt.csv'),
  na = '', row.names = F
)
