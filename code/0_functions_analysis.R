#==============================================================================#
# Montessori Analysis Functions                                                #
#==============================================================================#

# ==== Baseline Equivalence ====================================================

## --- Complete Case -----------------------------------------------------------

test_baseline_eq_cc <- function(var_bl,           # variable
                                binary = F,       # is outcome binary?
                                covrs = F,        # include covars in mod?
                                wave = '_k',      # wave suffix for sample excl
                                keep_miss_k = F,  # keep obs w/ miss K data?
                                wt = NULL,        # weight var for wtd reg
                                full_stats = F,   # output all calc'd nums?
                                dat = df) {       # dataset
  if (grepl('puzz', var_bl)) binary <- T
  covars <- ''  # Prep covariates
  if (covrs) {  # removes follow-up spanish assessment indicator
    covars_bl_eq <- covariates[!grepl('_p?k(3|4)?$', covariates)]
    if (grepl('htks', var_bl)) {  # if var is HTKS, prevent duplicate bl control
      covars_bl_eq <- covars_bl_eq[!grepl('htks', covars_bl_eq)]
    }
    covars <- str_c('+', str_flatten(covars_bl_eq, '+'))  # prep for reg formula
  }
  
  dat <- dat |> filter(!is.na(.data[[var_bl]]))  # Drop students w/ missing data
  if (var_bl %in% outcomes_bl & !keep_miss_k & wave == '_k') {
    dat <- dat |> filter(!is.na(.data[[str_replace(var_bl, '_bl', wave)]]))
  }  # ^drops those w/ missing K data, to check equiv among those remaining
  if (covrs) dat <- filter_at(  # if covars included, drop anyone w/ missing
    dat, vars(all_of(covars_bl_eq)), all_vars(!is.na(.))
  )
  
  if (!is.null(wt)) {  # Run models
    mod <- fixest::feols(
      as.formula(str_c(var_bl, '~ mont_offer', covars, '| lottery_id')),
      data = dat, vcov = ~lottery_school_id, weights = as.formula(str_c("~", wt))
    )
  } else {
    mod <- fixest::feols(
      as.formula(str_c(var_bl, '~ mont_offer', covars, '| lottery_id')),
      data = dat, vcov = ~lottery_school_id
    )
  }
  
  out <- dat |>  # Calc effect size & return results
    summarise(
      y = var_bl,
      n0 = sum(mont_offer == 0),
      n1 = sum(mont_offer == 1),
      coef = mod$coefficients[[1]],
      se = mod$coeftable[1,2],
      p = mod$coeftable[1,4],
      ctrl_mean = mean(mod$sumFE),  # mean of fixed effects, akin to intercept
      treat_mean = ctrl_mean + coef
    )
  if (binary) {
    out <- out |> 
      mutate(  # Cox index
        odds_treat = treat_mean / (1 - treat_mean),
        odds_ctrl = ctrl_mean / (1 - ctrl_mean),
        g = log(odds_treat / odds_ctrl) / 1.65,
        g_se = 1/1.65 * sqrt(
          1/(treat_mean * n1) + 1/((1-treat_mean) * n1)
          + 1/(ctrl_mean * n0) + 1/((1-ctrl_mean) * n0)
        )
      )
  } else {
    out <- out |> 
      mutate(  # Hedges' g
        sd0 = sd(dat[[var_bl]][dat$mont_offer == 0]),
        sd1 = sd(dat[[var_bl]][dat$mont_offer == 1]),
        sp = sqrt(((n0 - 1) * sd0^2 + (n1 - 1) * sd1^2) / (n0 + n1 - 2)),
        omega = 1 - 3 / (4 * ((n0 + n1) - 2) - 1),
        g = omega * coef / sp,
        g_se = sqrt((n0 + n1) / (n0 * n1) + .5 * g^2 / (n0 + n1)),
      )
  }
  if (!full_stats) {
    if (binary) {
      out <- out |> 
        select(
          -c(n0, n1, odds_treat, odds_ctrl, ctrl_mean, treat_mean)
        )
    } else {
      out <- out |> 
        select(-c(n0, n1, sd0, sd1, sp, omega, ctrl_mean, treat_mean))
    }
  }
  out |> 
    rename_with(~ str_replace(.x, '0', '_ctrl')) |> 
    rename_with(~ str_replace(.x, '1', '_treat'))
}

## --- Imputed -----------------------------------------------------------------

test_baseline_eq_imp <- function(dat,  # Imputed dataset
                                 var_bl, 
                                 binary = F,
                                 covrs = F, 
                                 wt = NULL,
                                 full_stats = F,
                                 keep_miss_k = T) {
  if (grepl('puzz', var_bl)) binary <- T
  covars <- ''  # Prep covariates
  if (covrs) {
    covars_bl_eq <- covariates[!grepl('_k$', covariates)]
    if (grepl('htks', var_bl)) {
      covars_bl_eq <- covars_bl_eq[!grepl('htks', covars_bl_eq)]
    }
    covars <- str_c("+", str_flatten(str_c(covars_bl_eq, "_ctr"), '+'))
  }
  
  # Prep data
  if (!keep_miss_k) dat <- dat[dat[[str_c('miss_', var_bl, '_k')]] == 0, ]
  dat <- dat |> 
    mutate(  # create race dummies
      white = ifelse(race_num == 1, 1, 0),
      asian = ifelse(race_num == 2, 1, 0),
      black = ifelse(race_num == 3, 1, 0),
      racemulti = ifelse(race_num == 4, 1, 0),
      raceother = ifelse(race_num == 5, 1, 0)
    )
  if (covrs) {
    dat <- dat |>  # grand mean center covariates if included
      mutate(
        across(  
          all_of(covars_bl_eq),
          ~ .x - mean(.x), 
          .names = '{col}_ctr'
        ),
        .by = imp_num
      )
  }
  
  # Run models
  nimps <- 20  # number of imputations
  mod_res <- list()
  ctrl_mean <- c()
  for (i in 1:nimps) {
    if (!is.null(wt)) {
      mod <- fixest::feols(
        as.formula(str_c(var_bl, '~ mont_offer', covars, '| lottery_id')),
        data = filter(dat, imp_num == i), vcov = ~lottery_school_id, 
        weights = as.formula(str_c("~", wt))
      )
    } else {
      mod <- fixest::feols(
        as.formula(str_c(var_bl, '~ mont_offer', covars, '| lottery_id')),
        data = filter(dat, imp_num == i), vcov = ~lottery_school_id
      )
    }
    mod_res[[i]] <- broom::tidy(mod)[1,c(2,3,5)]  # extract model results
    ctrl_mean[i] <- mean(mod$sumFE)
  }
  mod_res <- list_rbind(mod_res) |> 
    rename(coef = estimate, se = std.error, p = p.value) |> 
    bind_cols(ctrl_mean = ctrl_mean)
  deg_frdm_cmplt <- fixest::degrees_freedom(mod, 'resid', vcov = 'iid')
  
  out <- dat |>  # Calculate & output pooled results
    summarise(
      n0 = sum(mont_offer == 0),
      n1 = sum(mont_offer == 1),
      sd0 = sd(.data[[var_bl]][mont_offer == 0]),
      sd1 = sd(.data[[var_bl]][mont_offer == 1]),
      .by = imp_num
    ) |>
    bind_cols(mod_res) |> 
    mutate(
      var = se^2, treat_mean = ctrl_mean + coef, var1 = sd0^2, var2 = sd1^2
    )
  if (binary) {  # Effect size calculations
    out <- out |> 
      mutate(
        odds_treat = treat_mean / (1 - treat_mean),
        odds_ctrl = ctrl_mean / (1 - ctrl_mean),
        g = log(odds_treat / odds_ctrl) / 1.65,
        g_var = 1/(treat_mean * n1) + 1/((1-treat_mean) * n1) 
          + 1/(ctrl_mean * n0) + 1/((1-ctrl_mean) * n0)
      )
  } else {
    out <- out |> 
      mutate(
        sp = sqrt(((n0 - 1) * sd0^2 + (n1 - 1) * sd1^2) / (n0 + n1 - 2)),
        omega = 1 - 3 / (4 * ((n0 + n1) - 2) - 1),
        g = omega * coef / sp,
        g_var = (n0 + n1) / (n0 * n1) + .5 * g^2 / (n0 + n1)
      )
  }
  out <- out |>  # Pool results across imputations
    summarise(
      y = var_bl,
      coef_final = mean(coef),  # final pooled parameter value
      Uhat = mean(var),  # within imputation variance
      B = (sum((coef - mean(coef))^2)) / (nimps-1),  # btwn imp variance
      Tv = Uhat + (1 + 1/nimps) * B,  # total variance
      se_final = sqrt(Tv),  # standard error for final parameter
      lambda = (B + B / nimps) / Tv,  # pct variance attributed to missing data
      r = (B + B / nimps) / Uhat,  # relative incrs in var due to nonresponse
      v_old = (nimps - 1) / lambda^2,  # old formula for degrees of freedom
      v_com = deg_frdm_cmplt, # (n - k), normal model df if all data non-missing
      # observed data df that accounts for the missing info
      v_obs = (v_com + 1) / (v_com + 3) * v_com * (1 - lambda),
      v = (v_old * v_obs) / (v_old + v_obs),  # final df for hypothesis test
      gamma = (r + 2 / (v + 3)) / (1 + r),  # % info coef unknown due to missing
      p = (1 - pt(abs(coef_final / se_final), v)) * 2,
      g_final = mean(g),  # final effect size
      g_se = sqrt(  # standard error for final effect size
        mean(g_var) + (1 + 1/nimps) * (sum((g - mean(g))^2) / (nimps-1))
      ),
      g_se = ifelse(grepl("puzz", y), g_se / 1.65, g_se),
      treat_mean = mean(treat_mean),
      ctrl_mean = mean(ctrl_mean),
      sd0 = sqrt(
        mean(var1) + (1 + 1/nimps) * sum(
          (ctrl_mean - mean(ctrl_mean))^2 / (nimps-1)
        )
      ),
      sd1 = sqrt(
        mean(var2) + (1 + 1/nimps) * sum(
          (treat_mean - mean(treat_mean))^2 / (nimps-1)
        )
      ),
      n0 = mean(n0),
      n1 = mean(n1)
    ) |> 
    select(
      y, coef = coef_final, se = se_final, p, g = g_final, g_se, treat_mean, 
      ctrl_mean, sd_ctrl = sd0, sd_treat = sd1, n_ctrl = n0, n_treat = n1
    )
  if (!full_stats) {
    out <- out |> select(y, coef, se, p, g, g_se)
  }
  out
}

# ==== Impute Data =============================================================

impute_data <- function(y,              # outcome
                        covars,         # covariate vector
                        bl_ord = T,     # is baseline y ordinal?
                        cace = F,       # complier avg causal effect?
                        mdrtr = '',     # moderator var (empty if none)
                        brn = 15000,    # num burn-in iterations
                        itr = 10000,    # num analysis iterations
                        sed = 6,        # set seed
                        num_imps = 20,  # number of imputations to save
                        chns = 8,       # number of chains to run
                        save_mod = F,   # save full blimp model object?
                        all_bl_y = F,   # impute using all baseline y?
                        dat = df) {     # dataset
  # Create char strings & vecs to serve as blimp modeling function elements
  wv <- str_extract(y, '_p?k(3|4)?$')  # wave suffix
  if (grepl('htks', y)) covars <- covars[!grepl('htks', covars)]
  
  ordnl_vars <- str_c(  # ordinal variables (includes binary)
    'mont_offer ', str_flatten(covars[!grepl('household|race', covars)], ' ')
  )
  if (bl_ord) ordnl_vars <- str_c(ordnl_vars, ' ', str_replace(y, wv, '_bl'))
  
  ctr_vars <- str_c(  # variables to group mean center (model is multilevel)
    'groupmean = mont_offer ', str_flatten(covars[covars != 'race_num'], ' '), 
    ' ', str_replace(y, wv, '_bl')
  )
  if (mdrtr == 'race_num') ctr_vars <- str_c(ctr_vars, ' race_num')
  
  fxd_vars <- 'female mont_offer lottery_id age_study'  # vars w/ no missing
  if (mdrtr == 'race_num') fxd_vars <- str_c(fxd_vars, ' race_num')
  
  bl <- str_replace(y, wv, '_bl')  # baseline outcome
  
  miss_preds <- covars[!(covars %in% c('female', 'age_study'))]  # preds w/ missing
  no_miss_preds <- covars[covars %in% c('female', 'age_study')]  # preds w/o miss
  
  if (mdrtr != '') mdrtr <- str_c(mdrtr, '*mont_offer ')  # interact mdrtr w/ treat
  
  # If race moderator, drop those w/ missing race, too hard to impute
  if (mdrtr == 'race_num') dat <- filter(dat, !is.na(race_num))
  
  # Create model formulas
  mod_focal <- str_c(  # focal model
    'focal.model: ', y, ' ~ mont_offer ', str_flatten(covars, ' '), ' ',
    bl, ' ', mdrtr, ' ', ' ; '
  )
  
  mod_pred <- str_c(  # predictor imputation models
    'predictor.models: ', str_replace(y, wv, '_bl'), ' ',
    str_flatten(miss_preds, ' '), ' ~ mont_offer ', str_flatten(no_miss_preds, ' '), 
    ' ; '
  )
  if (grepl('race_num', mdrtr)) {  # moves race to non-missing part of pred mod
    mod_pred <- str_remove(mod_pred, ' race_num')
    mod_pred <- str_replace(mod_pred, 'female', 'female race_num')
  }
  
  mod_cace_fs <- ''  # set up CACE model; blank if just doing ITT
  latnt <- ''
  if (cace) {
    mod_rhs <- str_remove(mod_focal, str_c('focal.model: ', y, ' ~ mont_offer'))
    mod_focal <- str_c(
      'level2.model: rand_focal_int ~ 1; rand_iv_int ~ 1; ',
      'rand_focal_int ~~ rand_iv_int; ',
      'level1.model: ', y, ' ~ 1@rand_focal_int mont_enroll ',
      mod_rhs, '; ',
      'mont_enroll ~ 1@rand_iv_int mont_offer ', mod_rhs, '; ',
      y, ' ~~ mont_enroll; '
    )
    latnt <- 'lottery_id = rand_focal_int rand_iv_int'
  }
  
  if (all_bl_y) {  # Alternative set up for all bl outcome control mod
    bl_y <- str_c(
      'bdigit_bl wjpv_bl_ctr wjap_bl_ctr wjlw_bl_ctr fdigit_bl_ctr htks_bl', 
      'tom_bl sps_bl puzz_bl'
    )
    ctr_vars <- str_c(
      'groupmean = mont_offer ', str_flatten(covars[covars != 'race_num'], ' '), 
      ' ', bl_y
    )
    ordnl_vars <- str_c(
      'mont_offer ', str_flatten(covars[!grepl('household|race', covars)], ' '),
      ' bdigit_bl htks_bl tom_bl sps_bl puzz_bl'
    )
    mod_focal <- str_c(
      'focal.model: ', y, ' ~ mont_offer ', str_flatten(covars, ' '), ' ',
      bl_y, ' ', mdrtr, ' ', ' ; '
    )
    mod_pred <- str_c(
      'predictor.models: ', bl_y, ' ',
      str_flatten(miss_preds, ' '), ' ~ mont_offer ', str_flatten(no_miss_preds, ' '),
      ' ; '
    )
  }
  
  # Run models
  if (chns > 10) chns <- 10  # prevent num chains from exceeding 10
  mod <- rblimp::rblimp(
    data = as.data.frame(dat),
    ordinal = ordnl_vars,
    nominal = 'race_num lottery_id',
    fixed = fxd_vars,
    clusterid = 'lottery_id',  # specify lottery clusters, makes mod multilevel
    latent = latnt,
    center = ctr_vars,
    model = str_c(mod_focal, mod_cace_fs, mod_pred),
    seed = sed,
    burn = brn,
    iter = itr,
    nimps = num_imps,
    chains = chns,
    options = "labels"  # adds convergence info to output
  )

  # Output results: 1) param estimates, 2) func output, 3) model converge stats
  out <- list(as.data.frame(mod@estimates), mod@output, mod@psr)
  if (cace) term <- '...mont_enroll' else term <- '...mont_offer'
  out[[4]] <- out[[1]][str_c(y, term), ]  # 4) treatment effect param est
  if (!(mdrtr == '' | grepl('race_num', mdrtr))) {
    out[[4]] <- bind_rows(
      out[[4]],
      out[[1]][str_c(y, '...', str_sub(mdrtr, 1, -13), '.mont_offer'), ]
    )
    out[[4]]$term <- c('mont_offer', str_c('mont_offer*', str_sub(mdrtr, 1, -13)))
  }
  out[[4]]$y <- y
  # 5) imputed datasets
  out[[5]] <- map(1:num_imps, ~ mod@imputations[[.x]] |> mutate(imp_num = .x)) |> 
    list_rbind() |> 
    select(where(~ !any(is.na(.x))))
  if (save_mod) out[[6]] <- mod  # 6) full model object (if option turned on)
  out
}

# ==== Impact Models ===========================================================

## --- Imputed ITT -------------------------------------------------------------

run_mod_imp_itt <- function(dat,                  # imputed datasets for outcome
                            y,                    # outcome
                            covars = covariates,  # covariate vector
                            mdrtr = '',           # moderator (blank if none)
                            nimps = 20,           # number imputed datasets
                            full_stats = F,       # return full stats?
                            full_mod_res = F,     # return all mod results?
                            wt = NULL,            # weight var for weighted reg
                            drop_miss_y = F,      # drop obs w/ missing outcome?
                            no_covars = F) {      # run mod with no covariates?
  if (grepl('htks', y)) covars <- covars[!grepl('htks', covars)]
  wv <- str_extract(y, '_p?k(3|4)?$')
  mdrtr_mod <- ''  # set up moderator for use in reg formula
  if (mdrtr[1] != '') {
    mdrtr_mod <- str_c(
      str_flatten(mdrtr, '+'), '+', str_flatten(str_c(mdrtr, ':mont_offer +'))
    )
    covars <- covars[!(covars %in% mdrtr)]
  }
  y_bl <- str_replace(y, wv, '_bl_ctr')  # centered bl Y for use in reg
  if (mdrtr[1] == str_replace(y, wv, '_bl')) y_bl <- ''  # prevents duplicate
  if (full_mod_res) res_filt <- '' else res_filt <- '^mont'  # keeps treat coef only
  
  if (!no_covars) {  # Prep data
    dat <- dat |> 
      mutate(  # create race dummies
        white = ifelse(race_num == 1, 1, 0),
        asian = ifelse(race_num == 2, 1, 0),
        black = ifelse(race_num == 3, 1, 0),
        racemulti = ifelse(race_num == 4, 1, 0),
        raceother = ifelse(race_num == 5, 1, 0)
      )
  }
  if (
    mdrtr[1] != '' 
    & !grepl('htks', mdrtr[1]) 
    & mdrtr[1] != str_replace(y, wv, '_bl')
  ) {  # prep dataset for merging Ns & SDs for moderator subgroups
    if (length(mdrtr) > 1) {  # for race moderator models
      dat <- dat |> 
        mutate(
          race = case_match(
            race_num,
            1 ~ 'white', 2 ~ 'asian', 3 ~ 'black', 4 ~ 'racemulti', 5 ~ 'raceother'
          ),
          race_merge = ifelse(  # for merging Ns onto mod results
            race == 'white', 'mont_offer', str_c('mont_offer:', race)
          )
        )
    } else {  # for other moderator models
      dat <- dat |> 
        mutate(
          mdrtr_merge = ifelse(
            get(mdrtr) == 0, 'mont_offer', str_c('mont_offer:', mdrtr)
          )
        )
    }
  }
  dat <- dat |>  # center predictors (grand mean)
    mutate(
      across(
        all_of(c(covars, str_replace(y, wv, '_bl'))),
        ~ .x - mean(.x),
        .names = '{col}_ctr'
      ),
      .by = imp_num
    )
  if (drop_miss_y) dat <- filter(dat, get(str_c('miss_', y)) == 0)
  
  mod_res <- list()  # Run models
  for (i in 1:nimps) {
    if (!is.null(wt)) {
      mod <- fixest::feols(
        as.formula(str_c(
          y, '~ mont_offer +', mdrtr_mod, y_bl, '+',
          str_flatten(str_c(covars, '_ctr'), '+'), '| lottery_id'
        )),
        data = filter(dat, imp_num == i), vcov = ~lottery_school_id,
        weights = as.formula(str_c('~', wt))
      )
    } else {
      mod <- fixest::feols(
        as.formula(str_c(
          y, '~ mont_offer +', mdrtr_mod, y_bl, '+',
          str_flatten(str_c(covars, '_ctr'), '+'), '| lottery_id'
        )),
        data = filter(dat, imp_num == i), vcov = ~lottery_school_id
      )
    }
    mod_res[[i]] <- broom::tidy(mod)  # extracts model results
    if (mdrtr[1] != '') {  # extracts all moderator params
      mdrtr_coef_ctrl <- list()
      for (j in 1:length(mdrtr)) {
        mdrtr_coef_ctrl[[j]] <- mod_res[[i]][mod_res[[i]]$term == mdrtr[j], 1:2]
      }
      mdrtr_coef_ctrl <- purrr::list_rbind(mdrtr_coef_ctrl)
    } else {  # adds placeholder vars if it's not a moderator model
      mdrtr_coef_ctrl <- data.frame(term = 'NA', estimate = 'NA')
    }
    mod_res[[i]] <- mod_res[[i]] |>  # Prep model results
      filter(grepl(res_filt, term)) |> 
      select(c(1:3,5)) |> 
      mutate(
        ctrl_mean = mean(mod$sumFE),
        ctrl_mean = ifelse(  # ctrl group means for obs w/ moderator=1
          str_remove(term, 'mont_offer:') %in% mdrtr_coef_ctrl$term,
          ctrl_mean + mdrtr_coef_ctrl$estimate[
            match(str_remove(term, 'mont_offer:'), mdrtr_coef_ctrl$term)
          ],
          ctrl_mean
        ),
        treat_mean = ctrl_mean + estimate,
        treat_mean = ifelse(
          grepl(':', term), treat_mean + estimate[term == 'mont_offer'], treat_mean
        ),
        imp_num = i
      )
  }
  mod_res <- list_rbind(mod_res) |>  # Combine mod results from all imputations
    rename(coef = estimate, se = std.error, p = p.value)
  deg_frdm_cmplt <- fixest::degrees_freedom(mod, 'resid', vcov = 'iid')
  
  
  if (  # Calculate effect sizes
    mdrtr[1] != '' 
    & !grepl('htks', mdrtr[1]) 
    & mdrtr[1] != str_replace(y, wv, '_bl')
  ) {  # compute Ns & SDs for moderator subgroups
    if (length(mdrtr) > 1) {
      out <- dat |> 
        summarise(
          n0 = sum(mont_offer == 0),
          n1 = sum(mont_offer == 1),
          sd0 = sd(.data[[y]][mont_offer == 0]),
          sd1 = sd(.data[[y]][mont_offer == 1]),
          .by = c(imp_num, race_merge)
        ) |> 
        full_join(mod_res, by = c('imp_num', 'race_merge' = 'term')) |> 
        rename(term = race_merge)
    } else {
      out <- dat |> 
        summarise(
          n0 = sum(mont_offer == 0),
          n1 = sum(mont_offer == 1),
          sd0 = sd(.data[[y]][mont_offer == 0]),
          sd1 = sd(.data[[y]][mont_offer == 1]),
          .by = c(imp_num, mdrtr_merge)
        ) |> 
        full_join(mod_res, by = c('imp_num', 'mdrtr_merge' = 'term')) |> 
        rename(term = mdrtr_merge)
    }
  } else {  # or compute overall Ns and SDs if not a moderator model
    out <- dat |> 
      summarise(
        n0 = sum(mont_offer == 0),
        n1 = sum(mont_offer == 1),
        sd0 = sd(.data[[y]][mont_offer == 0]),
        sd1 = sd(.data[[y]][mont_offer == 1]),
        .by = imp_num
      ) |>
      full_join(mod_res, 'imp_num')
  }
  out <- mutate(out, var = se^2, var1 = sd0^2, var2 = sd1^2)
  if (grepl("puzz", y)) {  # compute effect sizes
    out <- out |> 
      mutate(
        odds_treat = treat_mean / (1 - treat_mean),
        odds_ctrl = ctrl_mean / (1 - ctrl_mean),
        g = log(odds_treat / odds_ctrl) / 1.65,
        g_var = 1/(treat_mean * n1) + 1/((1-treat_mean) * n1) 
        + 1/(ctrl_mean * n0) + 1/((1-ctrl_mean) * n0)
      )
  } else {
    out <- out |> 
      mutate(
        sp = sqrt(((n0 - 1) * sd0^2 + (n1 - 1) * sd1^2) / (n0 + n1 - 2)),
        omega = 1 - 3 / (4 * ((n0 + n1) - 2) - 1),
        g = omega * coef / sp,
        g_var = (n0 + n1) / (n0 * n1) + .5 * g^2 / (n0 + n1)
      )
  }
  
  out <- out |>  # Pool results across imputations
    summarise(
      .by = term,
      y = y,
      coef_final = mean(coef),
      Uhat = mean(var),
      B = (sum((coef - mean(coef))^2)) / (nimps-1),
      Tv = Uhat + (1 + 1/nimps) * B,
      se_final = sqrt(Tv),
      lambda = (B + B / nimps) / Tv,
      r = (B + B / nimps) / Uhat,
      v_old = (nimps - 1) / lambda^2,
      v_com = deg_frdm_cmplt,
      v_obs = (v_com + 1) / (v_com + 3) * v_com * (1 - lambda),
      v = (v_old * v_obs) / (v_old + v_obs),
      gamma = (r + 2 / (v + 3)) / (1 + r),
      p = (1 - pt(abs(coef_final / se_final), v)) * 2,
      g_final = mean(g),
      g_se = sqrt(
        mean(g_var) + (1 + 1/nimps) * (sum((g - mean(g))^2) / (nimps-1))
      ),
      g_se = ifelse(grepl("puzz", y), g_se / 1.65, g_se),
      treat_mean = mean(treat_mean),
      ctrl_mean = mean(ctrl_mean),
      sd0 = sqrt(
        mean(var1) + (1 + 1/nimps) * sum(
          (ctrl_mean - mean(ctrl_mean))^2 / (nimps-1)
        )
      ),
      sd1 = sqrt(
        mean(var2) + (1 + 1/nimps) * sum(
          (treat_mean - mean(treat_mean))^2 / (nimps-1)
        )
      ),
      n0 = mean(n0),
      n1 = mean(n1)
    ) |> 
    select(
      y, term, coef = coef_final, se = se_final, p, g = g_final, g_se, 
      ctrl_mean, treat_mean, sd_ctrl = sd0, sd_treat = sd1, 
      n_ctrl = n0, n_treat = n1
    )
  if (grepl('puzz', y) & mdrtr[1] != '') {
    out <- out |>  # Recompute Cox index for Asian, issue w/ pooled result
      mutate(
        odds_treat = ifelse(
          grepl('asian', term),
          treat_mean / (1 - treat_mean), NA
        ),
        odds_ctrl = ifelse(
          grepl('asian', term),
          ctrl_mean / (1 - ctrl_mean), NA
        ),
        g = ifelse(
          grepl('asian', term),
          log(odds_treat / odds_ctrl) / 1.65, g
        ),
        g_se = ifelse(
          grepl('asian', term),
          1/(treat_mean * n_treat) + 1/((1-treat_mean) * n_treat) 
          + 1/(ctrl_mean * n_ctrl) + 1/((1-ctrl_mean) * n_ctrl),
          g_se
        ),
        g = ifelse(term != 'mont_offer', g - g[term == 'mont_offer'], g)
      ) |> 
      select(-odds_treat, -odds_ctrl)
  }
  
  if (!full_stats) out <- out |> select(y, term, coef, se, p, g, g_se)
  if (mdrtr[1] == '') out <- select(out, -term)
  if (mdrtr[1] != '') {
    out <- out |> 
      mutate(  # Rename certain moderator labels
        moderator = case_when(
          length(mdrtr) > 1 ~ 'race',
          mdrtr[[1]] == str_replace(y, wv, '_bl') ~ 'baseline_outcome',
          .default = mdrtr[[1]]
        )
      ) |> 
      relocate(moderator, .after = y)
  }
  out
}

## --- Imputed CACE ------------------------------------------------------------

run_mod_imp_cace <- function(dat, 
                             y, 
                             covars = covariates,
                             nimps = 20,
                             full_stats = F,
                             drop_miss_y = F) {
  if (grepl('htks', y)) covars <- covars[!grepl('htks', covars)]
  wv <- str_extract(y, '_p?k(3|4)?$')

  dat <- dat |>  # Prep data
    mutate(
      white = ifelse(race_num == 1, 1, 0),
      asian = ifelse(race_num == 2, 1, 0),
      black = ifelse(race_num == 3, 1, 0),
      racemulti = ifelse(race_num == 4, 1, 0),
      raceother = ifelse(race_num == 5, 1, 0)
    ) |> 
    mutate(
      across(
        any_of(c(covars, str_replace(y, wv, '_bl'))),
        ~ .x - mean(.x),
        .names = '{col}_ctr'
      ),
      .by = c(imp_num)
    )
  if (drop_miss_y) dat <- filter(dat, get(str_c('miss_', y)) == 0)
  
  mod_res <- list()  # Run models
  ctrl_mean <- c()
  for (i in 1:nimps) {
    mod <- fixest::feols(  # 2SLS, `endog treat ~ instrument` at end of fmla
      as.formula(str_c(
        y, '~', str_replace(y, wv, '_bl_ctr'), '+', 
        str_flatten(str_c(covars, '_ctr'), '+'),
        '| lottery_id | mont_enroll ~ mont_offer'
      )),
      data = filter(dat, imp_num == i), vcov = ~lottery_school_id
    )
    mod_res[[i]] <- broom::tidy(mod)[1,c(2,3,5)]
    ctrl_mean[i] <- mean(mod$sumFE)
  }
  mod_res <- list_rbind(mod_res) |> 
    rename(est = estimate, se = std.error, pvalue = p.value) |> 
    bind_cols(ctrl_mean = ctrl_mean)
  deg_frdm_cmplt <- fixest::degrees_freedom(mod, 'resid', vcov = 'iid')
  
  out <- dat |>  # Calculate effect size
    summarise(
      n0 = sum(mont_offer == 0),
      n1 = sum(mont_offer == 1),
      sd0 = sd(.data[[y]][mont_offer == 0]),
      sd1 = sd(.data[[y]][mont_offer == 1]),
      .by = imp_num
    ) |> 
    bind_cols(mod_res) |> 
    rename(coef = est, p = pvalue) |> 
    mutate(
      var = se^2, treat_mean = ctrl_mean + coef, var1 = sd0^2, var2 = sd1^2
    )
  if (grepl("puzz", y)) {
    out <- out |> 
      mutate(
        odds_treat = treat_mean / (1 - treat_mean),
        odds_ctrl = ctrl_mean / (1 - ctrl_mean),
        g = log(odds_treat / odds_ctrl) / 1.65,
        g_var = 1/(treat_mean * n1) + 1/((1-treat_mean) * n1) 
        + 1/(ctrl_mean * n0) + 1/((1-ctrl_mean) * n0)
      )
  } else {
    out <- out |> 
      mutate(
        sp = sqrt(((n0 - 1) * sd0^2 + (n1 - 1) * sd1^2) / (n0 + n1 - 2)),
        omega = 1 - 3 / (4 * ((n0 + n1) - 2) - 1),
        g = omega * coef / sp,
        g_var = (n0 + n1) / (n0 * n1) + .5 * g^2 / (n0 + n1)
      )
  }
  
  out <- out |>  # Pool results across imputations
    summarise(
      y = y,
      coef_final = mean(coef),
      Uhat = mean(var),
      B = (sum((coef - mean(coef))^2)) / (nimps-1),
      Tv = Uhat + (1 + 1/nimps) * B,
      se_final = sqrt(Tv),
      lambda = (B + B / nimps) / Tv,
      r = (B + B / nimps) / Uhat,
      v_old = (nimps - 1) / lambda^2,
      v_com = deg_frdm_cmplt,
      v_obs = (v_com + 1) / (v_com + 3) * v_com * (1 - lambda),
      v = (v_old * v_obs) / (v_old + v_obs),
      gamma = (r + 2 / (v + 3)) / (1 + r),
      p = (1 - pt(abs(coef_final / se_final), v)) * 2,
      g_final = mean(g),
      g_se_final = sqrt(
        mean(g_var) + (1 + 1/nimps) * (sum((g - mean(g))^2) / (nimps-1))
      ),
      treat_mean = mean(treat_mean),
      ctrl_mean = mean(ctrl_mean),
      sd0 = sqrt(
        mean(var1) + (1 + 1/nimps) * sum(
          (ctrl_mean - mean(ctrl_mean))^2 / (nimps-1)
        )
      ),
      sd1 = sqrt(
        mean(var2) + (1 + 1/nimps) * sum(
          (treat_mean - mean(treat_mean))^2 / (nimps-1)
        )
      ),
      n0 = mean(n0),
      n1 = mean(n1)
    )

  if (!full_stats) {
    out <- out |> 
      select(
        y, coef = coef_final, se = se_final, p, g = g_final, g_se = g_se_final,
        ctrl_mean, treat_mean, sd_ctrl = sd0, sd_treat = sd1, 
        n_ctrl = n0, n_treat = n1
      )
  }
  out
}

## --- Complete Case -----------------------------------------------------------

run_mod_cc <- function(y, 
                       covars, 
                       mdrtr = '', 
                       cace = F,       # CACE model?
                       dat = df, 
                       wt = '',
                       no_bl_y = F) {  # no baseline Y control in model?
  if (grepl('htks', y)) covars <- covars[!grepl('htks', covars)]
  wv <- str_extract(y, '_p?k(3|4)?$')
  mdrtr_mod <- ''  # Moderator prep for reg formula
  if (mdrtr[1] != '') {
    mdrtr_mod <- str_flatten(str_c(mdrtr, ':mont_offer +'))
    for (i in 1:length(mdrtr)) {
      covars[covars == str_c(mdrtr[i], '_ctr')] <- str_remove(
        covars[covars == str_c(mdrtr[i], '_ctr')], '_ctr'
      )
    }
  }
  
  dat <- filter(dat, .data[[str_c("miss_", y)]] == 0)  # Drop obs w/ miss data
  if (!no_bl_y) dat <- filter(
    dat, .data[[str_c("miss_", str_replace(y, wv, '_bl'))]] == 0
  )
  if (covars[1] != '') dat <- dat |> 
    filter_at(vars(all_of(covars)), all_vars(!is.na(.)))
  
  # Run models
  if (cace) {  # CACE
    mod_og <- fixest::feols(
      as.formula(str_c(
        y, '~', str_replace(y, wv, '_bl_ctr'), '+', str_flatten(covars, '+'),
        '| lottery_id | mont_enroll ~ mont_offer'
      )),
      data = dat, vcov = ~lottery_school_id
    )
  } else if (wt != '') {  # weighted regression
    mod_og <- fixest::feols(
      as.formula(str_c(
        y, '~ mont_offer +', mdrtr_mod, str_replace(y, wv, '_bl_ctr'), '+',
        str_flatten(covars, '+'), ' | lottery_id'
      )),
      data = dat, vcov = ~lottery_school_id, weights = as.formula(str_c("~", wt))
    )
  } else if (no_bl_y & covars[1] == '') {  # no covariates
    mod_og <- fixest::feols(
      as.formula(str_c(y, '~ mont_offer | lottery_id')),
      data = dat, vcov = ~lottery_school_id
    )
  } else if (no_bl_y) {  # no baseline control
    mod_og <- fixest::feols(
      as.formula(str_c(
        y, '~ mont_offer +', str_flatten(covars, '+'), '| lottery_id'
      )),
      data = dat, vcov = ~lottery_school_id
    )
  } else {  # ITT primary model
    mod_og <- fixest::feols(
      as.formula(str_c(
        y, '~ mont_offer +', mdrtr_mod, str_replace(y, wv, '_bl_ctr'), '+',
        str_flatten(covars, '+'), ' | lottery_id'
      )),
      data = dat, vcov = ~lottery_school_id
    )
  }
  mod <- broom::tidy(mod_og)
  
  n0 <- sum(dat$mont_offer == 0)  # Effect size & moderator group mean params
  n1 <- sum(dat$mont_offer == 1)
  sd0 <- sd(dat[[y]][dat$mont_offer == 0])
  sd1 <- sd(dat[[y]][dat$mont_offer == 1])
  sp <- sqrt(((n0 - 1) * sd0^2 + (n1 - 1) * sd1^2) / (n0 + n1 - 2))
  omega <- 1 - 3 / (4 * ((n0 + n1) - 2) - 1)
  if (mdrtr[1] != '') {
    mdrtr_coef_ctrl <- list()
    for (i in 1:length(mdrtr)) {
      mdrtr_coef_ctrl[[i]] <- mod[mod$term == mdrtr[i], 1:2]
    }
    mdrtr_coef_ctrl <- purrr::list_rbind(mdrtr_coef_ctrl)
  } else {
    mdrtr_coef_ctrl <- data.frame(term = 'NA', estimate = 'NA')
  }
  
  out <- list(mutate(mod, y = y))  # Full model results
  out[[2]] <- filter(mod, grepl('mont_(offer|enroll)', term)) |>  # Treat effect
    mutate(  # prep for effect size calculation
      y = y,
      moderator = case_when(
        length(mdrtr) > 1 ~ 'race',
        mdrtr[1] == str_replace(y, wv, '_bl') ~ 'baseline_outcome',
        .default = mdrtr[1]
      ),
      ctrl_mean = mean(mod_og$sumFE),
      ctrl_mean = ifelse(  # get correct ctrl mean for moderators
        str_remove(term, 'mont_offer:') %in% mdrtr_coef_ctrl$term,
        ctrl_mean + mdrtr_coef_ctrl$estimate[
          match(str_remove(term, 'mont_offer:'), mdrtr_coef_ctrl$term)
        ],
        ctrl_mean
      ),
      treat_mean = ctrl_mean + estimate,
      treat_mean = ifelse(
        grepl(':', term), treat_mean + estimate[term == 'mont_offer'], treat_mean
      ),
      n_ctrl = n0, n_treat = n1, 
      sd_ctrl = sd0, sd_treat = sd1
    )
  if (mdrtr[1] == '') out[[2]] <- select(out[[2]], -moderator)
  
  if (grepl('puzz', y)) {  # Compute effect sizes
    out[[2]] <- out[[2]] |> 
      mutate(
        odds_treat = treat_mean / (1 - treat_mean),
        odds_ctrl = ctrl_mean / (1 - ctrl_mean),
        g = log(odds_treat / odds_ctrl) / 1.65,
        g_se = 1/1.65 * sqrt(
          1/(treat_mean * n1) + 1/((1-treat_mean) * n1)
          + 1/(ctrl_mean * n0) + 1/((1-ctrl_mean) * n0)
        )
      ) |> 
      select(-odds_treat, -odds_ctrl)
  } else {
    out[[2]] <- out[[2]] |> 
      mutate(
        g = omega * estimate / sp,
        g_se = sqrt((n0 + n1) / (n0 * n1) + .5 * g^2 / (n0 + n1)),
      )
  }
  out[[2]] <- rename(out[[2]], coef = estimate, se = std.error, p = p.value)
  out
}

# ==== Domain Average Effect Size ==============================================

get_domain_effect_size <- function(df = df_g,     # Each var's imputed df stacked
                                   cor = corr) {  # Mean correlation among vars
  y <- c('fdigit_k', 'bdigit_k', 'tom_k', 'htks_k')  # vars that share domain
  nimps <- 20
  df <- df |>  # Data prep
    mutate(
      white = ifelse(race_num == 1, 1, 0),
      asian = ifelse(race_num == 2, 1, 0),
      black = ifelse(race_num == 3, 1, 0),
      racemulti = ifelse(race_num == 4, 1, 0),
      raceother = ifelse(race_num == 5, 1, 0)
    )
  
  res_list <- list()  # Run model for each var & imp to compute g
  for (i in 1:20) {
    imp_res_list <- list()
    for (j in 1:length(y)) {
      mod <- fixest::feols(
        as.formula(str_c(
          y[j], '~ mont_offer +', str_replace(y[j], '_k', '_bl'), '+',
          str_flatten(
            covariates[!grepl(str_sub(y[j], 1, 4), covariates)], '+'
          ), '| lottery_id'
        )),
        data = filter(df, imp_num == i, outcome == y[j]), 
        vcov = ~lottery_school_id
      )
      imp_res_list[[j]] <- broom::tidy(mod) |>
        filter(term == 'mont_offer') |>
        mutate(
          y = y[j],
          imp_num = i
        )
      deg_frdm <- fixest::degrees_freedom(mod, 'resid', vcov = 'iid')
    }
    res_list[[i]] <- imp_res_list
  }
  res <- map(res_list, list_rbind) |> list_rbind() |>
    rename(coef = estimate, se = std.error, p = p.value)

  df |>  # Compute g
    summarise(
      n0 = sum(mont_offer == 0),
      n1 = sum(mont_offer == 1),
      sd0 = sd(y_std[mont_offer == 0]),
      sd1 = sd(y_std[mont_offer == 1]),
      .by = c(imp_num, outcome)
    ) |>
    full_join(res, c('imp_num', 'outcome' = 'y')) |>
    mutate(
      var = se^2, var1 = sd0^2, var2 = sd1^2,
      sp = sqrt(((n0 - 1) * sd0^2 + (n1 - 1) * sd1^2) / (n0 + n1 - 2)),
      omega = 1 - 3 / (4 * ((n0 + n1) - 2) - 1),
      g = omega * coef / sp,
      g_se = sqrt((n0 + n1) / (n0 * n1) + .5 * g^2 / (n0 + n1)),
      outcome = case_match(
        outcome, 'fdigit_k' ~ 'fdig', 'bdigit_k' ~ 'bdig',
        'tom_k' ~ 'tom', 'htks_k' ~ 'htks'
      )
    ) |>
    select(imp_num, outcome, g, g_se) |>
    tidyr::pivot_wider(
      names_from = outcome,
      values_from = c(g, g_se)
    ) |>
    rowwise() |>
    mutate(gbar = mean(c_across(matches('^g_(f|b|t|h)')))) |>  # Mean g within imp
    ungroup() |> 
    mutate(
      gbar_se = sqrt(  # g SE within imp
        g_se_fdig^2 + g_se_bdig^2 + g_se_tom^2 + g_se_htks^2 + cor * (
          2 * (g_se_fdig * g_se_bdig) + 2 * (g_se_fdig * g_se_tom)
          + 2 * (g_se_fdig * g_se_htks)
          + 2 * (g_se_bdig * g_se_tom) + 2 * (g_se_bdig * g_se_htks)
          + 2 * (g_se_tom * g_se_htks)
        )
      ) / 4,
      var = gbar_se^2  # g variance within imp
    ) |>
    summarise(  # Pool results across imputations
      gbar_final = mean(gbar),
      Uhat = mean(var),
      B = (sum((gbar - mean(gbar))^2)) / (nimps-1),
      Tv = Uhat + (1 + 1/nimps) * B,
      gbar_se_final = sqrt(Tv),
      lambda = (B + B / nimps) / Tv,
      r = (B + B / nimps) / Uhat,
      v_old = (nimps - 1) / lambda^2,
      v_com = deg_frdm,
      v_obs = (v_com + 1) / (v_com + 3) * v_com * (1 - lambda),
      v = (v_old * v_obs) / (v_old + v_obs),
      gamma = (r + 2 / (v + 3)) / (1 + r),
      t = gbar_final / gbar_se_final,
      p = (pt(-abs(t), v)) * 2
    ) |>
    select(gbar = gbar_final, gbar_se = gbar_se_final, p, t, df = v)
}

# ==== Treatment Propensity Sensitivity Analyses ===============================

## --- Treatment Pscore Estimation ---------------------------------------------

get_pscores_tx <- function(df,               # Mult imp data for given outcome
                           y,                # Outcome
                           req_both_tx = F,  # Drop lottos w/o treat & ctrl obs?
                           preds_pscore = preds_pscore_mod,  # Pscore mod preds
                           covars = covariates) {  # Covars for prog score mod
  y_bl <- str_replace(y, 'k$', 'bl')  # Variable & equation setup
  if (grepl('htks', y)) {
    covars <- covars[!grepl('htks', covars)]
    preds_pscore <- preds_pscore[!grepl('htks', preds_pscore)]
  }
  preds_pscore_eq <- str_flatten(  # Preds equation for pscore mod, rand effects
    str_c(c(preds_pscore, y_bl), "_ctr_group"), "+"
  )
  preds_pscore_eq2 <- str_flatten(  # Preds equation for pscore model, squares
    str_c("poly(", str_c(c(preds_pscore, y_bl), "_ctr_group"), ", 2)"), "+"
  )
  # Drop lotteries w/o both treat & ctrl children if option is turned on
  if (req_both_tx) df <- df[!(df$lottery_id %in% c(3,14,12,15,13,24,34,25)), ]
  
  df <- map(1:20, \(i) {  # For each imputation...
    dat <- df[df$imp_num == i, ]
    if (req_both_tx) {  # Add prognostic score to dataset if possible
      mod <- glm(  
        as.formula(str_c(
          y, "~", y_bl, "+", str_flatten(covars, "+"),
          "+ factor(lottery_id)"
        )),
        data = dat[dat$mont_offer == 0, ]
      )
      dat$prog_score <- predict(mod, dat)
    }
    mod <- lme4::glmer(  # Run pscore model
      as.formula(str_c(
        "mont_offer ~ ", preds_pscore_eq2, 
        "+ (1 | lottery_id) + (0 +", preds_pscore_eq, "| lottery_id)"
      )),
      data = dat, family = binomial, 
      control = lme4::glmerControl(optimizer = 'nloptwrap')
    )
    dat |> mutate(  # Extract pscore & compute weights
      pscore = as.numeric(exp(predict(mod)) / (1 + exp(predict(mod)))),
      wt_att = ifelse(mont_offer == 1, 1, pscore / (1 - pscore)),
      wt_ate = ifelse(mont_offer == 1, 1 / pscore, 1 / (1 - pscore))
    )
  }) |> 
    list_rbind()  # Return as df w/ pscore, wt cols, & prognostic score added
}

## --- Treatment Pscore Weighted & Matched Models ------------------------------

run_mod_pscore_tx <- function(df,              # Mult imp df w/ pscores
                              y,               # Outcome
                              schl_match = T,  # Match by school (not lottery)
                              preds_pscore = preds_pscore_mod,  # Pscore mod preds
                              covars = covariates) {  # Covars for outcome mod
  y_bl <- str_replace(y, '_k$', '_bl')
  if (grepl('htks', y)) {
    covars <- covars[!grepl('htks', covars)]
    preds_pscore <- preds_pscore[!grepl('htks', preds_pscore)]
  }
  exact_match_var <- 'lottery_id'
  if (schl_match) exact_match_var <- 'lottery_school_id'
  covars_wimi <- c(  # Covars for WeightThem (wimids) & MatchThem (mimids) obj
    'age_study', covars[!grepl('_k$|racemulti|raceother', covars)], 
    'racemultiother'
  )
  hh_ind <- match('household_size_log', covars_wimi)  # get hh size index in vec
  covars_wimi <- c(  # add household size on original scale
    covars_wimi[1:hh_ind], 'household_size', 
    covars_wimi[(hh_ind+1):length(covars_wimi)]
  )
  
  mids_out <- miceadds::datalist2mids(split(df, df$imp_num))  # Make mids obj
  wimids_out <- weightthem(  # Create wimids obj for weighting
    as.formula(str_c("mont_offer ~ ", str_flatten(c(y_bl, covars_wimi, "white"), "+"))),
    data = mids_out,
    estimand = "ATT",
    ps = "pscore"
  )
  mimids_out <- matchthem_custom_distance(  # Create mimids obj for matching
    as.formula(str_c("mont_offer ~ ", str_flatten(c(y_bl, covars_wimi, "white"), "+"))),
    data = mids_out,
    method = "full",
    distance = mids_out$imp$pscore,
    exact = as.formula(str_c('~df[df$imp_num == 1, ]$', exact_match_var)),
    caliper = .1
  )
  
  # Check balance
  hh_ind <- match('household_size_log', preds_pscore)  # for adding in og hh size
  bal_test_vars <- c(
    'pscore', y_bl, preds_pscore[1:hh_ind], 'household_size', 
    preds_pscore[(hh_ind+1):length(preds_pscore)], 'white', 'assessed_in_spanish_bl'
  )
  bnry <- c(rep(F,6), rep(T, 11))
  if (grepl('htks', y)) bnry <- bnry[-4]
  if ("prog_score" %in% names(df)) {  # Balance for wtd sample via lottery FE mod
    bal_test_vars <- c(
      "pscore", "prog_score", bal_test_vars[2:length(bal_test_vars)]
    )
    bnry <- c(F,F,bnry[2:length(bnry)])
  }
  balance_wt <- map2(
    bal_test_vars, bnry,
    ~ test_baseline_eq_imp(
      df, .x, binary = .y, full_stats = T, wt = "wt_att"
    )
  ) |> 
    list_rbind() |> 
    select(
      var = y, ctrl_mean, treat_mean, coef, g, p, se, g_se, sd_ctrl, sd_treat, 
      n_ctrl, n_treat
    ) |>
    mutate(mod = 'weight', y = y)
  if ("prog_score" %in% names(df)) {  # Balance for matched sample
    bal_out <- bal.tab(
      mimids_out, disp = c("means", "sds"), binary = "std", 
      distance = "prog_score"
    )
  } else {
    bal_out <- bal.tab(mimids_out, disp = c("means", "sds"), binary = "std")
  }
  ctrl_means <- data.frame(  # Compute treat & ctrl means across imps
    V1 = rep(NA, length(bal_out$Imputation.Balance[[1]]$Balance$M.0.Adj))
  )
  treat_means <- ctrl_means
  for (i in 1:20) {
    ctrl_means[i] <- bal_out$Imputation.Balance[[i]]$Balance$M.0.Adj
    treat_means[i] <- bal_out$Imputation.Balance[[i]]$Balance$M.1.Adj
  }
  ctrl_means <- rowwise(ctrl_means) |> 
    mutate(ctrl_mean = mean(c_across(everything())), .keep = "none")
  treat_means <- rowwise(treat_means) |> 
    mutate(treat_mean = mean(c_across(everything())), .keep = "none")
  match_means <- bind_cols(ctrl_means, treat_means) |> 
    mutate(
      raw_diff = treat_mean - ctrl_mean,
      cox_index = log(  # Cox ind for another way to assess binary var balance
        (treat_mean / (1 - treat_mean)) / (ctrl_mean / (1 - ctrl_mean))
      ) / 1.65,
    )
  # Combine with diffs computed by matchthem into df for output
  balance_match <- bal_out$Balance.Across.Imputations |>
    tibble::rownames_to_column("var") |> 
    bind_cols(match_means) |> 
    select(-ends_with('Un')) |> 
    mutate(mod = 'match', y = y)
  
  # Get effective sample sizes
  ess_wt <- tibble::rownames_to_column(bal.tab(wimids_out)$Observations, 'des')
  ess_match <- tibble::rownames_to_column(bal_out$Observations, 'des')
  
  res_wt <- map(1:20, \(i) {  # Weighting results: for each imputed dataset...
    dat <- complete(wimids_out, i)  # Extract imputed dataset from wimids obj
    weightit_out <- wimids_out$models[[i]]  # Extract individual weightit obj
    lm_weightit(  # Fit outcome model
      as.formula(str_c(
        y, "~ mont_offer * (", y_bl, "+", str_flatten(covars, "+"), ")",
        "+ factor(lottery_id)"
      )),
      data = dat, weightit = weightit_out, cluster = ~lottery_school_id
    )
  }) |> 
    map(~ marginaleffects::avg_comparisons(  # Compute ATT for each imp
      .x, variables = "mont_offer", newdata = subset(mont_offer == 1)
    )) |> 
    mice::pool() |> summary(conf.int = T) |> as.data.frame() |>  # Pool final ATT
    mutate(mod = 'weight', y = y)
  
  res_match <- map(1:20, \(i) {  # Matching results: for each imputed dataset...
    df_match <- match_data(  # Extract matched df
      mimids_out$models[[i]], data = df[df$imp_num == i, ]
    )
    lm(  # Fit outcome model
      as.formula(str_c(
        y, "~ mont_offer * (", y_bl, "+", str_flatten(covars, "+"), ")",
        "+ factor(lottery_id)"
      )),
      data = df_match, weights = weights
    )
  }) |> 
    map(~ marginaleffects::avg_comparisons(  # Compute ATT for each imp
      .x, variables = "mont_offer", newdata = subset(mont_offer == 1), vcov = ~subclass
    )) |> 
    mice::pool() |> summary(conf.int = T) |> as.data.frame() |>  # Pool final ATT
    mutate(mod = 'match', y = y)
  
  # Prep output
  res_wt <- bind_cols(  # add effective sample sizes to results
    res_wt, ess_wt[2,c(2,3)] |> rename(ess_ctrl = `0`, ess_treat = `1`)
  )
  res_match <- ess_match[3:5, ] |>  # prep matching ESS to merge w/ match result
    mutate(
      des = case_match(
        des, 
        "Matched (ESS)" ~ "ess", 
        "Matched (Unweighted)" ~ "n_matched",
        "Unmatched" ~ "n_unmatched"
      )
    ) |> 
    rename(ctrl = `0`, treat = `1`) |> 
    tidyr::pivot_wider(names_from = des, values_from = c(ctrl, treat)) |> 
    rename_with(~ str_replace(.x, "(ctrl|treat)_(.+)", "\\2_\\1")) |> 
    relocate(ess_treat, .after = ess_ctrl) |> 
    relocate(n_matched_treat, .after = n_matched_ctrl) %>%
    bind_cols(res_match, .)  # merge match ESS with result
  res <- bind_rows(res_wt, res_match) |>  # combine match & weight res to one df
    relocate(mod) |> relocate(y) |> 
    select(-term, -conf.low, -conf.high)
  
  bal_both <- balance_match |>  # combine match & weight balance res to one df
    mutate(
      var = ifelse(var == 'distance', 'pscore', var),
      std_diff = ifelse(Type == 'Binary', cox_index, Mean.Diff.Adj)
    ) |> 
    select(y, mod, var, ctrl_mean, treat_mean, raw_diff, std_diff) %>%
    bind_rows(
      balance_wt |> select(
        y, mod, var, ctrl_mean, treat_mean, raw_diff = coef, std_diff = g, p
      ), .
    )
  
  # Return: 1) results, 2) all balance, 3&4) ind balance, 5&6) match & wt objs
  list(res, bal_both, balance_wt, balance_match, wimids_out, mimids_out)
}

## --- `matchthem()` Modified --------------------------------------------------

#* Modification of the `matchthem()` function from the MatchThem package. Allows 
#* the user to specify a different set of pre-estimated propensity scores for 
#* each imputed dataset as the distance metric. Comments indicate modified 
#* lines.

matchthem_custom_distance <- function(formula, 
                                      datasets, 
                                      approach = "within", 
                                      method = "nearest", 
                                      distance = mids_out$imp$pscore,  # pscores
                                      link = "logit", 
                                      distance.options = list(), 
                                      discard = "none", 
                                      reestimate = FALSE, ...) {
  MatchIt::matchit
  mice::complete
  stats::as.formula
  called <- match.call()
  originals <- datasets
  classed <- class(originals)
  if (identical(approach, "pool-then-match")) {
    approach <- "across"
  }
  else if (identical(approach, "match-then-pool")) {
    approach <- "within"
  }
  if (missing(datasets) || length(datasets) == 0) {
    stop("The input for the datasets must be specified.")
  }
  if (!inherits(datasets, "mids") && !inherits(datasets, "amelia")) {
    stop("The input for the datasets must be an object of the 'mids' or 'amelia' class.")
  }
  if (!is.null(datasets$data$distance)) {
    stop("The input for the datasets shouldn't have a variable named 'distance'.")
  }
  if (!is.null(datasets$data$weights)) {
    stop("The input for the datasets shouldn't have a variable named 'weights'.")
  }
  if (!is.null(datasets$data$subclass)) {
    stop("The input for the datasets shouldn't have a variable named 'subclass'.")
  }
  if (!is.null(datasets$data$discarded)) {
    stop("The input for the datasets shouldn't have a variable named 'discarded'.")
  }
  if (!is.null(datasets$data$estimated.distance) && approach == 
      "across") {
    stop("The input for the datasets shouldn't have a variable named 'estimated.distance', when the 'across' matching approch is selected.")
  }
  approach <- match.arg(approach, c("within", "across"))
  if (approach == "across" && (!(method %in% c("nearest", 
                                               "full", "subclass", "optimal", "genetic", "quick")))) {
    stop("The input for the matching method must be 'nearest', 'full', 'subclass', 'optimal', 'genetic', or 'quick' when the 'across' matching approch is selected.")
  }
  if (approach == "across" && distance == "mahalanobis") {
    stop("The input for the distance should not be 'mahalanobis' when the 'across' matching approch is selected.")
  }
  if (!(method %in% c("nearest", "exact", "full", "genetic", 
                      "subclass", "cem", "optimal", "quick", "cardinality"))) {
    stop("The input for the matching method must be either 'nearest', 'exact', 'full', 'genetic', 'subclass', 'cem', 'optimal', 'quick', or 'cardinality'.")
  }
  if (inherits(datasets, "amelia")) {
    imp0 <- datasets$imputations[[1]]
    is.na(imp0) <- datasets$missMatrix
    imp0$.id <- 1:nrow(imp0)
    imp0$.imp <- 0
    implist <- vector("list", datasets$m + 1)
    implist[[1]] <- imp0
    for (i in 1:datasets$m) {
      imp <- datasets$imputations[[i]]
      imp$.id <- 1:nrow(imp0)
      imp$.imp <- i
      implist[[i + 1]] <- imp
    }
    imp.datasets <- do.call(base::rbind, as.list(noquote(implist)))
    datasets <- mice::as.mids(imp.datasets)
    originals <- datasets
  }
  if (approach == "within") {
    modelslist <- vector("list", datasets$m)
    for (i in 1:datasets$m) {
      dataset <- mice::complete(datasets, i)
      if (i == 1) 
        message(paste0("\n", "Matching Observations  | dataset: #", 
                       i), appendLF = FALSE)
      else message(paste0(" #", i), appendLF = FALSE)
      distance_i <- unlist(distance[i])  # <- modification 1
      model <- MatchIt::matchit(formula, dataset, method = method, 
                                distance = distance_i,  # <- modification 2
                                distance.options = distance.options, 
                                discard = discard, reestimate = reestimate, 
                                ...)
      modelslist[[i]] <- model
    }
  }
  if (approach == "across") {
    modelslist <- vector("list", datasets$m)
    distancelist <- vector("list", datasets$m)
    for (i in 1:datasets$m) {
      dataset <- mice::complete(datasets, i)
      if (i == 1) 
        message(paste0("Estimating distances   | dataset: #", 
                       i), appendLF = FALSE)
      else message(paste0(" #", i), appendLF = FALSE)
      model <- MatchIt::matchit(formula, dataset, method = NULL, 
                                distance = distance, distance.options = distance.options, 
                                discard = modelslist[[i]]$discard, reestimate = FALSE, 
                                ...)
      distancelist[[i]] <- model$distance
    }
    d <- rowMeans(as.matrix(do.call(base::cbind, distancelist)))
    for (i in 1:datasets$m) {
      dataset <- mice::complete(datasets, i)
      if (i == 1) 
        message(paste0("\n", "Matching Observations  | dataset: #", 
                       i), appendLF = FALSE)
      else message(paste0(" #", i), appendLF = FALSE)
      model <- MatchIt::matchit(formula, data = dataset, 
                                method = method, distance = d, distance.options = NULL, 
                                discard = discard, reestimate = FALSE, ...)
      modelslist[[i]] <- model
    }
  }
  output <- list(call = called, object = datasets, models = modelslist, 
                 approach = approach)
  class(output) <- "mimids"
  message("\n", appendLF = FALSE)
  return(output)
}

# ==== Missingness Propensity Sensitivity Analyses =============================

## --- Observed Data Pscore Estimation -----------------------------------------

get_pscores_miss <- function(df,  # Dataset
                             y,   # Outcome
                             preds_pscore = preds_pscore_mod,  # Pscore mod preds
                             imp = T) {  # Using multiply-imputed data?
  y_bl <- str_replace(y, 'k$', 'bl')  # Variable & equation setup
  if (grepl('htks', y) | !imp) {
    preds_pscore <- preds_pscore[!grepl('htks', preds_pscore)]
  }
  if (imp) {  # Preds equation for pscore mod
    preds_pscore_eq <- str_flatten(
      str_c(c(preds_pscore, y_bl), "_ctr_group"), "+"
    )
  } else {
    preds_pscore_eq <- str_flatten(  # bl Y not a pred b/c CC models missing blY
      str_c(preds_pscore, "_ctr_group"), "+"
    )
  }
  preds_pscore_eq2 <- str_flatten(  # Squared (only works for mult imp)
    str_c("poly(", str_c(c(preds_pscore, y_bl), "_ctr_group"), ", 2)"), "+"
  )
  preds_pscore_itt_int <- str_c(  # Interact treat with all other vars
    "mont_offer_ctr_group * (", 
    str_remove(preds_pscore_eq, "\\+mont_offer_ctr_group"), 
    ")"
  )
  # Observed data indicators
  if (imp) obs_var <- str_c('obs_', y)  # K
  if (!imp) obs_var <- str_c('obs_', str_replace(y, 'k$', 'both'))  # K & bl
  
  if (imp) {
    df <- map(1:20, \(i) {  # For each imputation...
      dat <- df[df$imp_num == i, ]
      mod <- lme4::glmer(  # Run pscore model
        as.formula(str_c(
          obs_var, "~", preds_pscore_eq2, "+", preds_pscore_itt_int,
          "+ (1 | lottery_id) + (0 +", preds_pscore_eq, "| lottery_id)"
        )),
        data = dat, family = binomial,
        control = lme4::glmerControl(optimizer = 'nloptwrap')
      )
      dat |> mutate(  # Extract pscore & compute weights
        pscore = fitted(mod),
        wt = ifelse(get(obs_var) == 1, 1 / pscore, 1 / (1 - pscore))
      )
    }) |> 
      list_rbind()  # Return as df w/ pscore & weight cols added
  } else {  # Complete case process, same as above but w/o multiple iterations
    mod <- lme4::glmer(
      as.formula(str_c(
        obs_var, "~", preds_pscore_itt_int,
        "+ (1 | lottery_id) + (0 +", preds_pscore_eq, "| lottery_id)"
      )),
      data = df, family = binomial,
      control = lme4::glmerControl(optimizer = 'nloptwrap')
    )
    df |> mutate(  # Extract pscore & compute weights
      pscore = fitted(mod),
      wt = ifelse(get(obs_var) == 1, 1 / pscore, 1 / (1 - pscore))
    )
  }
}

## --- Observed Data Pscore Weighted Models ------------------------------------

run_mod_pscore_miss <- function(df, 
                                y, 
                                preds_pscore = preds_pscore_mod,
                                covars = covariates,  # Covars for outcome mod
                                imp = T) {
  y_bl <- str_replace(y, 'k$', 'bl')  # Variable & data setup
  if (grepl('htks', y)) covars <- covars[!grepl('htks', covars)]
  bal_test_vars <- c(y_bl, preds_pscore, 'white', 'assessed_in_spanish_bl')
  bnry <- c(F,F,F, rep(T, 12))
  if (imp & grepl('htks', y)) {
    bal_test_vars <- bal_test_vars[-1]
    bnry <- bnry[-1]
  }
  if (imp) obs_var <- str_c('obs_', y)
  if (!imp) obs_var <- str_c('obs_', str_replace(y, 'k$', 'both'))
  df_cc <- filter(df, get(obs_var) == 1)  # Complete case dataset
  out <- list()  # Output storage list
  
  # Get means of preds to compare (full sample, observed, weighted observed)
  if (imp) {  # set up functions for computing cluster-adjusted means
    bal_func <- function(df, y_bl, binary, wt = NULL) {
      test_baseline_eq_imp(
        dat = df, var_bl = y_bl, binary = binary, wt = wt, full_stats = T
      )
    }
  } else {
    bal_func <- function(df, y_bl, binary, wt = NULL) {
      test_baseline_eq_cc(
        dat = df, var_bl = y_bl, binary = binary, wt = wt, full_stats = T
      )
    }
  }  # compute means under the 3 scenarios
  balance_full <- map2(bal_test_vars, bnry, ~ bal_func(df, .x, .y)) |> 
    list_rbind() |> 
    select(var = y, ctrl_mean, treat_mean, coef, g, matches('^n_')) |> 
    rename_with(~ str_c(.x, '_full'), .cols = !matches('^var$'))
  balance_cc <- map2(bal_test_vars, bnry, ~ bal_func(df_cc, .x, .y)) |> 
    list_rbind() |> 
    select(ctrl_mean, treat_mean, coef, g, matches('^n_')) |> 
    rename_with(~ str_c(.x, '_cc'))
  balance_cc_wt <- map2(
    bal_test_vars, bnry, ~ bal_func(df_cc, .x, .y, wt = 'wt')
  ) |>
    list_rbind() |> 
    select(ctrl_mean, treat_mean, coef, g, matches('^n_')) |> 
    rename_with(~ str_c(.x, '_cc_wt'))
  out[[1]] <- bind_cols(balance_full, balance_cc_wt, balance_cc) |> 
    mutate(y = y)
  
  if (imp) {  # Run models
    out[[2]] <- run_mod_imp_itt(df_cc, y, covars, wt = 'wt')
  } else {
    out[[2]] <- run_mod_cc(y, covars, dat = df_cc, wt = 'wt')[[2]]
  }
  out
}

# ==== Lee Bounds ==============================================================

get_lee_bounds <- function(dat = df,             # Data, default = main df
                           y,                    # Outcome
                           covars = covariates,  # Covariates
                           imp = F,              # Multiply-imputed data?
                           cnsnt = F,            # Consent? (vs. missingness)
                           drop_miss_y = T) {    # Drop obs w/ missing outcome?
  y_bl <- str_replace(y, 'k$', 'bl')
  if (grepl('htks', y)) covars <- covars[!grepl('htks', covars)]
  if (!imp) dat$obs_all <- complete.cases(select(dat, all_of(c(covars, y, y_bl))))
  if (imp) dat$obs_all <- dat[[str_c("miss_", y)]] == 0
  if (cnsnt) {  # Create indicators for who to drop & weights for partial drops
    dat <- df_cnsnt |>  # Need to hard code the `df_cnsnt` into environment
      select(lottery_id, treat = cnsnt_treat, ctrl = cnsnt_ctrl) |> 
      mutate(  # treatment group with higher share "observed" ie consented
        tx_group_more_obs = case_when(
          treat > ctrl ~ "treat", ctrl > treat ~ "ctrl", ctrl == treat ~ "equal"
        ),
        amount = case_when(  # diff consent as pct of higher tx group's consent rt
          tx_group_more_obs == "treat" ~ (treat - ctrl) / treat,
          tx_group_more_obs == "ctrl" ~ (ctrl - treat) / ctrl,
          tx_group_more_obs == "equal" ~ 0
        )
      ) %>%
      left_join(dat, ., 'lottery_id')  # merge above vars onto main dataset
    if (drop_miss_y) dat <- filter(dat, obs_all == 1)
    dat <- dat |> 
      mutate(
        num_drop = ifelse(  # number observations to drop in each lottery
          tx_group_more_obs == "treat",
          amount * sum(mont_offer),
          amount * sum(mont_offer == 0)
        ),
        .by = lottery_id
      )
  } else {
    dat <- dat |>  # same as above but for missingness rather than consent
      summarise(
        miss = sum(obs_all) / n(),
        .by = c(lottery_id, mont_offer)
      ) |> 
      tidyr::pivot_wider(names_from = mont_offer, values_from = miss) |> 
      rename(treat = `1`, ctrl = `0`) |> 
      mutate(
        tx_group_more_obs = case_when(
          treat > ctrl ~ "treat", ctrl > treat ~ "ctrl", ctrl == treat ~ "equal"
        ),
        amount = case_when(
          tx_group_more_obs == "treat" ~ (treat - ctrl) / treat,
          tx_group_more_obs == "ctrl" ~ (ctrl - treat) / ctrl,
          tx_group_more_obs == "equal" ~ 0
        )
      ) |> 
      select(lottery_id, tx_group_more_obs, amount, ctrl, treat) %>%
      left_join(
        dat, ., "lottery_id"
      ) |> 
      filter(obs_all == 1) |> 
      mutate(
        num_drop = ifelse(
          tx_group_more_obs == "treat",
          amount * sum(obs_all[mont_offer == 1]),
          amount * sum(obs_all[mont_offer == 0])
        ),
        .by = lottery_id
      )
  }
  dat <- dat |> 
    mutate(  # Rank ppl within each lottery & tx group
      rank_low = data.table::frank(  # tie break on 1) baseline Y, 2) random
        list(get(y), desc(get(y_bl))), ties.method = "random"
      ),
      rank_up = data.table::frank(
        list(desc(get(y)), get(y_bl)), ties.method = "random"
      ),
      .by = c(lottery_id, mont_offer)
    ) |> 
    mutate(  # Assign weight for dropping "fraction of a person"
      partial_num_drop = num_drop %% 1,
      drop_wt_low = ifelse(
        rank_low - num_drop < 1 & rank_low - num_drop > 0 
        & (
          tx_group_more_obs == "treat" & mont_offer == 1 
          | tx_group_more_obs == "ctrl" & mont_offer == 0
        ),
        (1 - partial_num_drop), 1
      ),
      drop_wt_up = ifelse(
        rank_up - num_drop < 1 & rank_up - num_drop > 0 
        & (
          tx_group_more_obs == "treat" & mont_offer == 1 
          | tx_group_more_obs == "ctrl" & mont_offer == 0
        ),
        (1 - partial_num_drop), 1
      ),  # Assign drop indicators
      drop_low = ifelse(
        rank_low <= num_drop & (
          tx_group_more_obs == "treat" & mont_offer == 1
          | tx_group_more_obs == "ctrl" & mont_offer == 0
        ),
        1, 0
      ),
      drop_up = ifelse(
        rank_up <= num_drop & (
          tx_group_more_obs == "treat" & mont_offer == 1
          | tx_group_more_obs == "ctrl" & mont_offer == 0
        ),
        1, 0
      )
    )
  
  out <- list()
  out[[1]] <- dat |>  # Return df showing who gets dropped
    arrange(lottery_id, mont_offer, get(y)) |> 
    select(
      lottery_id, mont_offer, all_of(y), all_of(y_bl), amount, 
      tx_group_more_obs, ctrl_obs = ctrl, treat_obs = treat, num_drop, 
      partial_num_drop, rank_low, drop_low, drop_wt_low, 
      rank_up, drop_up, drop_wt_up, any_of('imp_num')
    )
  if (imp) {  # Run models to get bounds
    res_up <- run_mod_imp_itt(
      y = y, covars = covars, dat = filter(dat, drop_up == 0), wt = 'drop_wt_up',
      full_stats = T
    ) |> 
      mutate(bound = "lower") |> 
      select(y, bound, coef, se, p, g, n_ctrl, n_treat)
    res_low <- run_mod_imp_itt(
      y = y, covars = covars, dat = filter(dat, drop_low == 0), wt = 'drop_wt_low',
      full_stats = T
    ) |> 
      mutate(bound = "upper") |> 
      select(y, bound, coef, se, p, g, n_ctrl, n_treat)
    out[[2]] <- bind_rows(res_up, res_low)
  } else {
    res_up <- run_mod_cc(
      y = y, covars = covars, dat = filter(dat, drop_up == 0), wt = 'drop_wt_up'
    )[[2]] |> 
      mutate(bound = "lower")
    res_low <- run_mod_cc(
      y = y, covars = covars, dat = filter(dat, drop_low == 0), wt = 'drop_wt_low'
    )[[2]] |> 
      mutate(bound = "upper")
    out[[2]] <- bind_rows(res_up, res_low) |> relocate(bound) |> relocate(y) |>
      select(-term)
  }
  out
}

# ==== Mixture Model ===========================================================

run_mod_mixture <- function(y,            # Outcome
                            ctrl_diff,    # Eff size diff, ctrl group, miss v obs
                            treat_diff,   # Eff size diff, treat fx, miss v obs
                            bl_ord = T,   # Baseline Y ordinal (or binary)?
                            covars = covars_hh_ctr,  # Covrs, dfault = ctr hh size
                            brn = 20000,  # Burn-in iterations
                            itr = 5000,   # Analysis iterations
                            dat = df) {   # Dataset
  wv <- str_extract(y, '_p?k(3|4)?$')  # Variable prep for Blimp model
  if (grepl('htks', y)) covars <- covars[!grepl('htks', covars)]
  ordnl_vars <- str_c(  # binary & ordinal variables
    'mont_offer ', str_flatten(covars[!grepl('household|race', covars)], ' '),
    ' miss_', y
  )  # add baseline Y if ordinal/binary
  if (bl_ord) ordnl_vars <- str_c(ordnl_vars, ' ', str_replace(y, wv, '_bl'))
  
  # Compute proportions missing & observed within each tx group (round for Blimp)
  pct_ctrl_miss <- round(mean(dat[[str_c('miss_', y)]][dat$mont_offer == 0]), 9)
  pct_ctrl_obs <- 1 - pct_ctrl_miss
  pct_treat_miss <- round(mean(dat[[str_c('miss_', y)]][dat$mont_offer == 1]), 9)
  pct_treat_obs <- 1 - pct_treat_miss
  
  mod <- rblimp::rblimp(  # Run model
    data = as.data.frame(dat),
    ordinal = ordnl_vars,
    nominal = "race_num",
    fixed = str_c('female mont_offer miss_', y),
    clusterid = "lottery_id",
    center = str_c(
      'groupmean = mont_offer ', str_flatten(covars[covars != 'race_num'], ' '), 
      ' ', str_replace(y, wv, '_bl'), ' miss_', y
    ),
    model = str_c(  # focal model
      "focal.model: ", y, " ~ 1@int_obs miss_", y, "@int_diff ",
      "mont_offer@ate_obs mont_offer*miss_", y, "@ate_diff ",
      str_flatten(covars, " "), " ", str_replace(y, wv, '_bl'), "; ",
      y, " ~~ ", y, "@resid_var; ",  # resid var, to convert eff size -> reg param
      "predictor.models: ", str_replace(y, wv, '_bl'), " ", 
      str_flatten(covars[covars != "female"], " "),
      " ~ mont_offer female miss_", y, "; "
    ),
    parameters = str_c(
      "effect_size_diff_ctrl = ", ctrl_diff, "; ",  # Cohen's D effect size diffs
      "effect_size_diff_treat = ", treat_diff, "; ",
      "int_diff = effect_size_diff_ctrl * sqrt(resid_var); ",  # fixed reg coefs
      "ate_diff = effect_size_diff_treat * sqrt(resid_var); ",
      
      "mean_ctrl_miss = int_obs + int_diff; ",  # missing ctrl group mean
      "mean_treat_obs = int_obs + ate_obs; ",  # observed treat group mean
      "mean_treat_miss = mean_ctrl_miss + ate_obs + ate_diff; ",  # miss treat mean
      
      "mean_ctrl = int_obs *", pct_ctrl_obs,  # control group
      "+ mean_ctrl_miss *", pct_ctrl_miss, "; ",
      "mean_treat = mean_treat_obs *", pct_treat_obs,  # treatment group
      "+ mean_treat_miss *", pct_treat_miss, "; ",
      
      "ate_sens = mean_treat - mean_ctrl; "  # final sensitivity ATE estimate
    ),
    seed = 6,
    burn = brn,
    iter = itr,
    options = "labels"
  )
  
  # Output: 1) all mod params, 2) mod output, 3) PSR convergence metrics
  out <- list(as.data.frame(mod@estimates), mod@output, mod@psr)
  out[[4]] <- out[[1]]['Parameter..ate_sens', ] |>  # 4) ATE estimate
    mutate(y = y, ctrl_diff = ctrl_diff, treat_diff = treat_diff) |> 
    select(
      y, ctrl_diff, treat_diff, est = Estimate, sd = StdDev, p = PValue, 
      ci_low = `2.5%`, ci_up = `97.5%`, n_eff = N_Eff
    )
  out
}

# ==== Principal Stratification ================================================

get_pstrat_bounds <- function(groups,  # Complier counterfactual groups
                              y,       # Outcome
                              takeup_grp = NULL,  # Treat takeup, dfault = groups[1]
                              y_min = NULL,   # Specify outcome min (optional)
                              y_max = NULL,   # Specify outcome max (optional)
                              shrink = NULL,  # Shrink min or max toward 0
                              marg_vals = margin_vals,  # "Margin values" data
                              dat = df) {               # Dataset
  if (is.null(takeup_grp)) takeup_grp <- groups[1]  # Ensures groups[1] = takeup
  if (!is.null(takeup_grp)) groups <- c(takeup_grp, groups[groups != takeup_grp])
  
  if ((is.null(y_min) | is.null(y_max))) {  # Set min & max y vals if not provided
    df_ymin <- dat |> select(all_of(y)) |> arrange(get(y))
    df_ymax <- dat |> select(all_of(y)) |> arrange(desc(get(y)))
    group_n <- marg_vals$`0`[marg_vals$var == 'n'][  # sample size for each group
      match(groups, marg_vals$group[marg_vals$var == 'n'])
    ]
    ymins <- c()
    ymaxs <- c()
    for (i in 1:length(groups)) {  # sample min & max mean for given group size
      ymins[i] <- mean(df_ymin[[y]][1:group_n[i]])
      ymaxs[i] <- mean(df_ymax[[y]][1:group_n[i]])
    }
    marg_vals <- bind_rows(  # add mins & maxs to marg vals dataset
      marg_vals,
      tibble(
        group = rep(groups, 2),
        var = c(rep('ymin', length(groups)), rep('ymax', length(groups))),
        `0` = c(ymins, ymaxs)
      )
    )
  }
  if (!is.null(y_min) & is.null(y_max)) {  # if only one or other provided, insert
    marg_vals <- mutate(marg_vals, `0` = ifelse(var == 'ymin'), y_min, `0`)
  }
  if (is.null(y_min) & !is.null(y_max)) {
    marg_vals <- mutate(marg_vals, `0` = ifelse(var == 'ymax'), y_max, `0`)
  }
  if (!is.null(shrink)) {  # shrink min & max by specified amount
    marg_vals <- mutate(
      marg_vals, `0` = ifelse(var %in% c('ymin', 'ymax'), `0` * shrink, `0`)
    )
  }
  
  out <- list()  # Set up dataset of strata w/ potential post-treat behaviors
  out[[1]] <- expand_grid(group_1 = groups, group_0 = groups) |> 
    filter(group_1 == takeup_grp | group_1 == group_0) |>  # monoton & excl restr
    mutate(
      type = case_when(
        group_1 == takeup_grp & group_0 == takeup_grp ~ 'always',
        group_1 == takeup_grp & group_0 != takeup_grp ~ 'complier',
        group_1 != takeup_grp ~ 'never'
      ),  # Add margin vals to dataset - sample proportions & potential outcomes
      marg_pct_1 = marg_vals$`1`[marg_vals$var == 'pct'][
        match(group_1, marg_vals$group[marg_vals$var == 'pct'])
      ],
      marg_pct_0 = marg_vals$`0`[marg_vals$var == 'pct'][
        match(group_0, marg_vals$group[marg_vals$var == 'pct'])
      ],
      marg_y_1 = marg_vals$`1`[marg_vals$var == y][
        match(group_1, marg_vals$group[marg_vals$var == y])
      ],
      marg_y_0 = marg_vals$`0`[marg_vals$var == y][
        match(group_0, marg_vals$group[marg_vals$var == y])
      ],  # Add strata proportions - always & never observed, complier estimated
      pct = case_when(
        type == 'always' ~ marg_pct_0,
        type == 'never' ~ marg_pct_1,
        type == 'complier' ~ marg_pct_0 - marg_pct_1[match(group_0, group_1)]
      ),
      wt_0 = ifelse(  # weights for calc wtd avg untreated potential outcome
        type == 'complier', pct / (pct + pct[match(group_0, group_1)]), NA
      ),
      wt_1 = ifelse(  # weights for calc wtd avg treated potential outcome
        type %in% c('complier', 'always'), 
        pct / sum(pct[type %in% c('complier', 'always')]), NA
      ),
      complier_share = ifelse(  # Share of compliers in each stratum
        type == 'complier', pct / sum(pct[type == 'complier']), NA
      ),
      y_1 = case_when(  # stratum-specific treated potential outcome
        type == 'never' ~ marg_y_1,
        type == 'always' ~ marg_y_0
      ),
      y_0 = case_when(  # stratum-specific untreated potential outcome
        type == 'always' ~ marg_y_0,
        type == 'never' ~ marg_y_1
      ),
      y_0 = ifelse(  # Compute complier y_0 using margin & never taker row
        type == 'complier',
        (marg_y_0 - y_0[match(group_0, group_1)] * (1 - wt_0)) / wt_0,
        y_0
      ),
      ate = ifelse(type %in% c('always', 'never'), 0, NA),  # ATE=0 by excl restr
      y_min = ifelse(  # Add fixed strata min & max Y to dataset
        type == 'complier',
        marg_vals$`0`[marg_vals$var == 'ymin'][
          match(group_0, marg_vals$group[marg_vals$var == 'ymin'])
        ],
        NA
      ),
      y_max = ifelse(
        type == 'complier',
        marg_vals$`0`[marg_vals$var == 'ymax'][
          match(group_0, marg_vals$group[marg_vals$var == 'ymax'])
        ],
        NA
      ),
      y_min_wtd = y_min * wt_1,  # fixed min/max Y weighted for computations
      y_max_wtd = y_max * wt_1,
      y_1_max = ifelse(  # calc max treated Y for each stratym (others fixed to min)
        type == 'complier',
        marg_y_1 - y_1[type == 'always'] * wt_1[type == 'always']
        - (sum(y_min_wtd[type == 'complier']) - y_min_wtd), 
        NA
      ),
      y_1_min = ifelse(  # calc min treated Y for each stratum (others fixed to max)
        type == 'complier',
        marg_y_1 - y_1[type == 'always'] * wt_1[type == 'always']
        - (sum(y_max_wtd[type == 'complier']) - y_max_wtd),
        NA
      ),  # calc final stratum-specific ATE min & max bounds
      ate_min = ifelse(type == 'complier', y_1_min - y_0, NA),
      ate_max = ifelse(type == 'complier', y_1_max - y_0, NA)
    )
  
  out[[2]] <- out[[1]] |>  # Extract vars of interest for output
    select(
      ctrl_contrast = group_0, ate_min, ate_max, y_0, complier_share, 
      sample_share = pct, y_min_for_others = y_min, y_max_for_others = y_max
    ) |> 
    filter(!is.na(ate_min)) |> 
    mutate(
      y = y, 
      takeup_group = takeup_grp,
      shrink_min_max = shrink,
      groups = list(groups),
      share_bound_pos = case_when(  # what pct of bound is positive?
        ate_min > 0 ~ 1, 
        ate_max < 0 ~ 0, 
        .default = ate_max / (ate_max - ate_min)
      )
    ) |> 
    relocate(takeup_group) |> relocate(y) |> 
    relocate(share_bound_pos, .after = ate_max)
  out
}
