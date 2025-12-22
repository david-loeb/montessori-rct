#==============================================================================#
# Results Table & Figure Creation Functions                                    #
#==============================================================================#

# Tables =======================================================================

make_results_tbl <- function(df,
                             .coef = T,
                             .se = T,
                             .p = T,
                             .g = F,
                             .g_se = F,
                             .means = F,
                             .means_unadj = F,
                             .sd = F,
                             .ns = F,
                             .custom = NULL,
                             pct_col = NULL,
                             spec_col = NULL,
                             group_specs = T,
                             long_spec = F,
                             coef_name = 'Diff',
                             col_names = NULL,
                             col_headers = NULL,
                             foot = NULL,
                             dec = 3,
                             n_dec = 0,
                             get_tbl_df = F) {
  if (!.coef) .p <- F
  if (!.coef) .se <- F
  if (!.g | !.se) .g_se <- F
  if (!.means_unadj) .sd <- F
  if (!is.null(spec_col)) spec_names <- unique(df[[spec_col]])
  df <- rename_with(df, ~ str_replace(.x, "mean_mod$", "mean"))
  
  df <- df |>  # Prep table df
    mutate(
      y = case_when(
        grepl('wjlw', y) ~ "WJ Letter Word",
        grepl('wjap', y) ~ "WJ Applied Prob",
        grepl('wjpv', y) ~ "WJ Pic Vocab",
        grepl('htks', y) ~ "HTKS",
        grepl('fdigit', y) ~ 'Forward Digit',
        grepl('bdigit', y) ~ 'Backward Digit',
        grepl('^tom', y) ~ 'Theory of Mind',
        grepl('^sps', y) ~ "Social Prob-Solve",
        grepl('puzz', y) ~ "Puzzle Choice",
        grepl('house.+size', y) ~ 'Household Size',
        grepl('lang_eng', y) ~ 'Primary Lang English',
        grepl('hispanic', y) ~ 'Hispanic',
        grepl('bachelor', y) ~ 'Caregiver Has B.A.',
        grepl('married', y) ~ 'Caregiver Married',
        grepl('inc.+75k', y) ~ 'Fam Income 75k+',
        grepl('asian', y) ~ 'Asian',
        grepl('black', y) ~ 'Black',
        grepl('white', y) ~ 'White',
        grepl('racemultioth', y) ~ 'Multi or Other Race',
        grepl('racemult', y) ~ 'Multiracial',
        grepl('raceoth', y) ~ 'Other Race',
        grepl('race_num', y) ~ 'Race',
        grepl('female', y) ~ 'Female',
        grepl('^age_', y) ~ 'Age',
        grepl('^ed$', y) ~ 'Econ. Disadv.',
        grepl('reading3', y) ~ 'Grade 3 Read Proficiency',
        .default = y
      )
    )
  if (.means) {
    df <- df |> 
      mutate(
        ctrl_mean_tbl = glue(
          "{format(round(ctrl_mean,", dec, "), nsmall =", dec, ")}"
        ),
        treat_mean_tbl = glue(
          "{format(round(treat_mean,", dec, "), nsmall =", dec, ")}"
        )
      )
  }
  if (.means_unadj) {
    df <- df |> 
      mutate(
        ctrl_unadj_mean_tbl = glue(
          "{format(round(ctrl_mean_unadj,", dec, "), nsmall =", dec, ")}"
        ),
        treat_unadj_mean_tbl = glue(
          "{format(round(treat_mean_unadj,", dec, "), nsmall =", dec, ")}"
        )
      )
  }
  if (.sd) {
    df <- df |> 
      mutate(
        ctrl_sd_tbl = ifelse(
          !is.na(sd_ctrl),
          glue("({format(round(sd_ctrl,", dec, "), nsmall =", dec, ")})"), NA
        ),
        treat_sd_tbl = ifelse(
          !is.na(sd_treat),
          glue("({format(round(sd_treat,", dec, "), nsmall =", dec, ")})"), NA
        ),
        ctrl_sd_tbl = str_replace(ctrl_sd_tbl, "\\(", "{(}"),
        ctrl_sd_tbl = str_replace(ctrl_sd_tbl, "\\)", "{)}"),
        treat_sd_tbl = str_replace(treat_sd_tbl, "\\(", "{(}"),
        treat_sd_tbl = str_replace(treat_sd_tbl, "\\)", "{)}")
      )
  }
  if (.p) {
    df <- df |> 
      mutate(
        sig_stars = case_when(
          p < .001 ~ '{***}',
          p < .01 ~ '{**}',
          p < .05 ~ '{*}',
          p < .1 ~ '{+}',
          .default = ''
        ),
        coef_tbl = glue(
          '{format(round(coef,', dec, '), nsmall =', dec, ')}{sig_stars}'
        )
      )
  } else if (.coef) {
    df <- df |> 
      mutate(
        coef_tbl = glue(
          '{format(round(coef,', dec, '), nsmall =', dec, ')}'
        )
      )
  }
  if (.se) {
    df <- df |> 
      mutate(
        se_tbl = glue("({format(round(se,", dec, "), nsmall =", dec, ")})"),
        se_tbl = str_replace(se_tbl, "\\(", "{(}"),
        se_tbl = str_replace(se_tbl, "\\)", "{)}"),
      )
  }
  if (.g) df <- mutate(
    df, g_tbl = glue("{format(round(g,", dec, "), nsmall =", dec, ")}")
  )
  if (.g_se) {
    df <- df |> 
      mutate(
        g_se_tbl = glue("({format(round(g_se,", dec, "), nsmall =", dec, ")})"),
        g_se_tbl = str_replace(g_se_tbl, "\\(", "{(}"),
        g_se_tbl = str_replace(g_se_tbl, "\\)", "{)}")
      )
  }
  if (!is.null(.custom)) {
    df <- df |> 
      mutate(
        across(
          all_of(.custom),
          ~ glue("{format(round(", cur_column(), ",", dec, "), nsmall =", dec, ")}"),
          .names = '{col}_tbl'
        )
      )
  }
  if (!is.null(pct_col)) {
    df <- df |> 
      mutate(
        across(
          all_of(pct_col),
          ~ str_c(
            glue("{format(round(", cur_column(), "* 100, 1), nsmall = 1)}"), "{\\%}"
          ),
          .names = '{col}_tbl'
        )
      )
  }
  if (.ns) {
    df <- df |> 
      mutate(
        n_ctrl_tbl = glue('{format(round(n_ctrl,', n_dec, '), nsmall =', n_dec, ')}'),
        n_treat_tbl = glue('{format(round(n_treat,', n_dec, '), nsmall =', n_dec, ')}')
      )
  }
  if (is.null(spec_col)) df <- select(df, y, ends_with('_tbl'))
  if (!is.null(spec_col)) {
    if (long_spec) {
      df <- df |> 
        pivot_wider(
          id_cols = c(y, id),
          names_from = all_of(spec_col),
          values_from = ends_with('_tbl')
        ) |> 
        select(y, matches('_tbl'))
    } else {
      df <- df |> 
        select(y, matches('_tbl'), all_of(spec_col)) |> 
        pivot_wider(
          names_from = all_of(spec_col),
          values_from = ends_with('_tbl')
        )
    }
    if (group_specs) {
      temp <- select(df, !matches(str_flatten(spec_names, "|")))
      for (i in 1:length(spec_names)) {  # Reorder so all spec cols together
        temp <- bind_cols(temp, select(df, matches(spec_names[i])))
      }
      df <- temp
    }
  }
  if (.se) {
    if (!is.null(.custom)) {
      df <- df |> 
        rename_with(
          ~ ifelse(
            grepl("(coef|g|mean)_tbl", .x) | .x %in% str_c(.custom, "_tbl"),
            str_c(.x, ".1"), .x
          )
        )
    } else {
      df <- df |> rename_with(
        ~ ifelse(grepl("(coef|g|mean)_tbl", .x), str_c(.x, ".1"), .x)
      )
    }
    df <- df |> 
      rename_with(~ ifelse(grepl("s(e|d)_tbl", .x), str_c(.x, ".2"), .x)) %>%
      pivot_longer(
        matches('\\.\\d$'),
        names_to = c(".value", "param"),
        names_sep = "\\."
      ) |> 
      mutate(
        across(
          matches("coef_tbl"),
          ~ ifelse(is.na(.x), get(str_replace(cur_column(), "coef", "se")), .x)
        )
      )
    if (.g_se) {
      df <- df |> 
        mutate(
          across(
            matches("g_tbl"),
            ~ ifelse(is.na(.x), get(str_replace(cur_column(), "g_", "g_se_")), .x)
          )
        )
    }
    if (.sd) {
      df <- df |> 
        mutate(
          across(
            matches("unadj_mean_tbl"),
            ~ ifelse(is.na(.x), get(str_replace(cur_column(), "unadj_mean", "sd")), .x)
          )
        )
    }
    df <- select(df, -matches("^se|^g_se|^(ctrl|treat)_sd"), -param)
    if (!is.null(spec_col)) {
      df <- df |> 
        rename_with(~ str_remove(.x, "_tbl_")) |> 
        rename_with(~ str_remove_all(.x, " "))
      if (!is.null(.custom)) {
        df <- df |> 
          mutate(
            across(
              c(matches(str_c('^y$|(ctrl|treat)_mean|', str_flatten(.custom, '|')))),
              ~ ifelse(
                grepl(
                  "\\{\\(\\}", 
                  get(str_c('coef', str_remove_all(spec_names[1], " ")))
                ),
                NA, .x
              )
            )
          )
      } else {
        df <- df |> 
          mutate(
            across(
              c(matches('^y$|(ctrl|treat)_mean')),
              ~ ifelse(
                grepl(
                  "\\{\\(\\}", 
                  get(str_c('coef', str_remove_all(spec_names[1], " ")))
                ) | grepl(
                  "\\{\\(\\}", 
                  get(str_c('coef', str_remove_all(spec_names[2], " ")))
                ),
                NA, .x
              )
            )
          )
      }
    } else {
      if (!is.null(.custom)) {
        df <- df |> 
          mutate(
            across(
              matches(str_c('^y$|(ctrl|treat)_mean_tbl|', str_flatten(.custom, '|'))), 
              ~ ifelse(grepl("\\{\\(\\}", coef_tbl), NA, .x)
            )
          )
      } else {
        df <- df |> 
          mutate(
            across(
              matches('^y$|(ctrl|treat)_mean_tbl'), 
              ~ ifelse(grepl("\\{\\(\\}", coef_tbl), NA, .x)
            )
          )
      }
    }
  }
  
  df <- rename_with(df, ~ str_remove(.x, '_tbl$'))
  if (.means_unadj) df <- select(df, y, matches('unadj'), !matches('unadj'))
  
  if (get_tbl_df) {  # Make table
    df
  } else {
    n_cols <- .coef + .g + 2 * .means + 2 * .means_unadj + 2 * .ns
    if (!is.null(.custom)) n_cols <- n_cols + length(.custom)
    if (!is.null(spec_col)) {
      n_cols <- n_cols + (length(spec_names) - 1) * (.coef + .g + 2 * .means + 2 * .ns)
      if (!is.null(.custom)) n_cols <- n_cols + (length(spec_names) - 1) * length(.custom)
    }
    if (is.null(col_names)) {
      col_names <- c(
        " ", "{\\shortstack{Ctrl\\\\Mean}}", "{\\shortstack{Treat\\\\Mean}}", 
        str_c("{", coef_name, "}"), "{Eff. Size}"
      )
      if (.means_unadj) col_names <- c(" ", rep(col_names[2:3], 2), col_names[4:5])
      if (!is.null(spec_col)) col_names <- c(" ", rep(col_names[-1], length(spec_names)))
      if (!.means) col_names <- col_names[!grepl("Ctrl.+Mean|Treat.+Mean", col_names)]
      if (!.g) col_names <- col_names[!grepl("Eff.+Size", col_names)]
      if (!.coef) col_names <- col_names[!grepl("Diff", col_names)]
    }
    if (!is.null(foot) & .p) {
      foot <- str_c('\\\\sffamily\\\\footnotesize{', foot_stars, '. ', foot, '}')
    } else if (.p) {
      foot <- str_c('\\\\sffamily\\\\footnotesize{', foot_stars, '}')
    } else if (!is.null(foot)) {
      foot <- str_c('\\\\sffamily\\\\footnotesize{', foot, '}')
    }
    col_aligns <- rep("d", n_cols)
    if (.ns) {
      if (n_dec == 0) col_aligns[grep("^n_", names(df)) - 1] <- "n"
      if (n_dec == 1) col_aligns[grep("^n_", names(df)) - 1] <- "g"
    }  
    
    tbl <- df |> 
      kbl(
        booktabs = T, escape = F, linesep = "",
        align = c("l", col_aligns), 
        col.names = col_names
      ) |> 
      kable_styling(latex_options = "scale_down")
    if (!is.null(foot) | .p) {
      tbl <- footnote(
        tbl, foot, general_title = "", threeparttable = T, escape = F,
        footnote_as_chunk = T
      )
    } 
    if (!is.null(col_headers)) tbl <- add_header_above(tbl, col_headers, line_sep = 5)
    tbl
  }
}

# Figures ======================================================================

## Effect Size Trends ----------------------------------------------------------

make_plots_eff_size_trend <- function(y_regex, 
                                      title, 
                                      dat = df, 
                                      title_size = 9, 
                                      font_size_same = T,
                                      med_size = F) {
  if (med_size) {
    point_size <- 1.5
    
    y_ann_for <- .61
    y_arr_for <- .5
    yend_arr_for <- .4
    x_arr_for <- 3.95
    xend_arr_for <- 4.05
    
    y_arr_back <- -.12
    yend_arr_back <- -.06
    x_arr_back <- 3.73
    xend_arr_back <- 3.85
  } else {
    point_size <- 2.5
    
    y_ann_for <- .63
    y_arr_for <- .58
    yend_arr_for <- .53
    x_arr_for <- 4.05
    xend_arr_for <- 4.1
    
    y_arr_back <- -.16
    yend_arr_back <- -.11
    x_arr_back <- 3.85
    xend_arr_back <- 3.9
  } 
  plt <- ggplot(filter(dat, grepl(y_regex, y)), aes(wave_time, g, group = y)) +
    geom_hline(yintercept = 0, lty = "dashed") +
    geom_line(color = "deepskyblue2") +
    geom_errorbar(
      aes(ymin = ci_low, ymax = ci_up), width = .1, color = "deepskyblue2"
    ) +
    geom_point(
      aes(wave_time, g, shape = y), 
      show.legend = F, size = point_size, color = "deepskyblue2"
    ) + 
    theme_minimal() +
    ylim(-.48, .63) +
    ylab("Effect Size") +
    xlab("Study Wave") +
    scale_x_continuous(
      breaks = c(1,2,3,4),
      labels = c("Baseline", "PK3", "PK4", "K")
    ) +
    ggtitle(title)
  if (font_size_same) {
    plt <- plt + theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(hjust = .5, size = title_size),
      axis.text.x = element_text(size = title_size),
      axis.text.y = element_text(size = title_size),
      axis.title.x = element_text(size = title_size),
      axis.title.y = element_text(size = title_size),
      text = element_text(family = "Roboto")
    )
    annotate_size <- 3.2 * title_size / 9
  } else {
    plt <- plt + theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(hjust = .5, size = title_size),
      text = element_text(family = "Roboto", size = title_size)
    )
    annotate_size <- 3 * title_size / 10
  }
  if (grepl("dig", y_regex)) {
    plt <- plt +
      annotate(
        "segment", 
        x = x_arr_for, xend = xend_arr_for, y = y_arr_for, yend = yend_arr_for,
        arrow = arrow(type= "closed", length = unit(.02, "npc")), 
        color = "gray30"
      ) +
      annotate(
        "text", x = 3.9, y = y_ann_for, label = "Forward",
        size = annotate_size, color = "gray30", family = "Roboto"
      ) +
      annotate(
        "segment", 
        x = x_arr_back, xend = xend_arr_back, y = y_arr_back, yend = yend_arr_back,
        arrow = arrow(type= "closed", length = unit(.02, "npc")), 
        color = "gray30"
      ) +
      annotate(
        "text", x = 3.8, y = -.2, label = "Backward",
        size = annotate_size, color = "gray30", family = "Roboto"
      ) +
      scale_shape_manual(
        values = c(
          "bdigit" = 16,
          "fdigit" = 15
        )
      )
  }
  plt
}

## Mixture Model Plots ---------------------------------------------------------

make_plots_mix_mod <- function(var, dat = df) {
  dat <- filter(dat, y == var)
  ggplot(dat, aes(treat_diff, est, color = factor(ctrl_diff))) +
    geom_errorbar(
      data = filter(dat, ctrl_diff == .5), 
      aes(ymin = ci_low, ymax = ci_up), 
      width = .03
    ) +
    geom_errorbar(
      data = filter(dat, ctrl_diff == 0), 
      aes(ymin = ci_low, ymax = ci_up), 
      width = .03
    ) +
    geom_errorbar(
      data = filter(dat, ctrl_diff == -.5), 
      aes(ymin = ci_low, ymax = ci_up), 
      width = .03
    ) +
    geom_point(size = 1.5) +
    geom_line() +
    geom_hline(yintercept = 0, lty = "dashed") +
    xlab("ITT Effect Size Diff for Missing vs Observed Treatment Group Members") +
    ylab("ITT Estimate") + 
    guides(
      color = guide_legend(
        "Outcome Effect Size Diff for Missing vs\nObserved Control Group Members"
      )
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = "Roboto", size = 10),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = .5, size = 10),
      axis.title.x = element_text(margin = margin(10, 0, 0, 0))
    ) +
    scale_color_manual(
      values = c(
        "-0.5" = "deepskyblue2",
        "0" = "deeppink2",
        "0.5" = "#37C74A"
      ),
      breaks = c('-0.5', '0', '0.5')
    ) +
    scale_x_continuous(breaks = seq(-5., .5, by = .25)) +
    ggtitle(unique(dat$title))
}

## Love Plots ------------------------------------------------------------------

make_plots_love <- function(wt_or_match, all_lot, dat = df, line = F) {
  if (all_lot) lot_spec <- "all" else lot_spec <- "has both treat & ctrl"
  if (wt_or_match == "weight") mod <- "Weighting" else mod <- "Matching"
  if (all_lot) {
    lot <- "All Lotteries"
  } else {
    lot <- "Both Treat Conditions"
  }
  
  dat <- filter(
    dat, mod == wt_or_match, lotteries == lot_spec, !grepl('^Pro.+Score', y)
  )
  
  plt <- ggplot(dat, aes(abs(g), y, color = spec, group = y)) +
    geom_point(size = 2) +
    geom_vline(xintercept = .1, lty = "dashed") +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      text = element_text(family = "Roboto", size = 10),
      legend.position = "bottom",
      plot.title = element_text(size = 10),
      axis.text.y = element_text(color = "black"),
      axis.title.x = element_text(margin = margin(10, 0, 0, 0))
    ) +
    scale_y_discrete(limits = rev) +
    scale_color_manual(
      values = c(
        "adj" = "deepskyblue2",
        "og" = "deeppink2"
      ),
      labels = c(
        "adj" = "Adjusted",
        "og" = "Unadjusted"
      ),
      breaks = c("adj", "og")
    ) +
    labs(y = "", x = "Absolute Effect Size Differences", color = "") +
    xlim(0, .39) +
    ggtitle(str_c(mod, ", ", lot))
  if (line) plt <- plt + geom_line()
  plt
}
