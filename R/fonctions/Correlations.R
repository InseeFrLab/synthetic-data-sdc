calc_correlation <- function(df) {
  df %>%
    select(where(is.numeric)) %>%
    cor(use = "complete.obs")
}

table_cor <- map(
  data[methodes],
  \(list_df) {
    array_cor <- map(
      list_df,
      \(df) {
        calc_correlation(df)
      }
    ) %>% abind::abind(along = 3)
    
    mean_cor <- array_cor %>% apply(MARGIN = 1:2, FUN = mean)
    sd_cor <- array_cor %>% apply(MARGIN = 1:2, FUN = sd)
    cv_cor <- mean_cor / sd_cor
    
    list(
      mean = mean_cor,
      sd = sd_cor,
      cv = cv_cor
    )
  }
)
