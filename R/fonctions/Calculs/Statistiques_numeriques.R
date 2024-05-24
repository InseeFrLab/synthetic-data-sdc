calc_stats_mean_num <- function(df) {
  df %>% group_by(index_sim) %>%
    summarise(
      across(where(is.numeric), ~ mean(.),
             .names = "{.col}"), .groups = "drop"
    ) %>% summarise(across(-index_sim, list(
      min = ~ min(.),
      max = ~ max(.),
      mean = ~ mean(.),
      sd = ~ sd(.),
      cv = ~ sd(.) / mean(.),
      median = ~ median(.),
      q1 = ~ quantile(., 0.25),
      q3 = ~ quantile(., 0.75),
      q025 = ~ quantile(., 0.025),
      q975 = ~ quantile(., 0.975))))
}

calc_org_mean_num <- function(df) {
  df %>% summarise(
    across(where(is.numeric), list(
      min = ~ min(.),
      max = ~ max(.),
      mean = ~ mean(.),
      sd = ~ sd(.),
      cv = ~ sd(.) / mean(.),
      median = ~ median(.),
      q1 = ~ quantile(., 0.25),
      q3 = ~ quantile(., 0.75),
      q025 = ~ quantile(., 0.025),
      q975 = ~ quantile(., 0.975)
    )))
}

calc_stats_sd_num <- function(df) {
  df %>% group_by(index_sim) %>%
    summarise(
      across(where(is.numeric), ~ sd(.)
             , .names = "{.col}"), .groups = "drop"
    ) %>% summarise(across(-index_sim, list(
      min = ~ min(.),
      max = ~ max(.),
      mean = ~ mean(.),
      sd = ~ sd(.),
      cv = ~ sd(.) / mean(.),
      median = ~ median(.),
      q1 = ~ quantile(., 0.25),
      q3 = ~ quantile(., 0.75),
      q025 = ~ quantile(., 0.025),
      q975 = ~ quantile(., 0.975))))
}