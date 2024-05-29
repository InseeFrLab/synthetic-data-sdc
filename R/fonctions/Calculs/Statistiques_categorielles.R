calc_stats_cat <- function(df) {
  cat_stats <- df %>%
    select(index_sim, where(is.factor)) %>%
    pivot_longer(cols = -index_sim, names_to = "variable", values_to = "modalites") %>%
    group_by(index_sim, variable, modalites) %>%
    summarise(freq = n(), .groups = "drop") %>%
    group_by(variable, modalites) %>%
    summarise(across(freq, list(
      min = ~ min(.),
      max = ~ max(.),
      mean = ~ round(mean(.), digits = 3),
      sd = ~ round(sd(.), digits = 3),
      cv = ~ round(sd(.) / mean(.), digits = 3),
      median = ~ median(.),
      q1 = ~ quantile(., 0.25),
      q3 = ~ quantile(., 0.75),
      q025 = ~ quantile(., 0.025),
      q975 = ~ quantile(., 0.975))))
}