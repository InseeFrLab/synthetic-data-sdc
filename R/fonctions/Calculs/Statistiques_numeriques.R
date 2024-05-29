calc_stats_mean_num <- function(df) {
  df %>% group_by(index_sim) %>%
    summarise(
      across(where(is.numeric), ~ mean(.),
             .names = "{.col}"), .groups = "drop"
    ) %>% summarise(across(-index_sim, list(
      min = ~ round(min(.), digits = 3),
      max = ~ round(max(.), digits = 3),
      mean = ~ round(mean(.), digits = 3),
      sd = ~ round(sd(.), digits = 3),
      cv = ~ round(sd(.) / mean(.), digits = 3),
      median = ~ round(median(.), digits = 3),
      q1 = ~ round(quantile(., 0.25), digits = 3),
      q3 = ~ round(quantile(., 0.75), digits = 3),
      q025 = ~ round(quantile(., 0.025), digits = 3),
      q975 = ~ round(quantile(., 0.975), digits = 3)
      )))
}

calc_org_mean_num <- function(df) {
  df %>% summarise(
    across(where(is.numeric), list(
      min = ~ round(min(.), digits = 3),
      max = ~ round(max(.), digits = 3),
      mean = ~ round(mean(.), digits = 3),
      sd = ~ round(sd(.), digits = 3),
      cv = ~ round(sd(.) / mean(.), digits = 3),
      median = ~ round(median(.), digits = 3),
      q1 = ~ round(quantile(., 0.25), digits = 3),
      q3 = ~ round(quantile(., 0.75), digits = 3),
      q025 = ~ round(quantile(., 0.025), digits = 3),
      q975 = ~ round(quantile(., 0.975), digits = 3)
    )))
}

calc_stats_sd_num <- function(df) {
  df %>% group_by(index_sim) %>%
    summarise(
      across(where(is.numeric), ~ sd(.)
             , .names = "{.col}"), .groups = "drop"
    ) %>% summarise(across(-index_sim, list(
      min = ~ round(min(.), digits = 3),
      max = ~ round(max(.), digits = 3),
      mean = ~ round(mean(.), digits = 3),
      sd = ~ round(sd(.), digits = 3),
      cv = ~ round(sd(.) / mean(.), digits = 3),
      median = ~ round(median(.), digits = 3),
      q1 = ~ round(quantile(., 0.25), digits = 3),
      q3 = ~ round(quantile(., 0.75), digits = 3),
      q025 = ~ round(quantile(., 0.025), digits = 3),
      q975 = ~ round(quantile(., 0.975), digits = 3)
      )))
}