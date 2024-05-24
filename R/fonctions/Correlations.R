calc_correlation <- function(df) {
  df %>%
    select(where(is.numeric)) %>%
    cor(use = "complete.obs")
}

table_cor <- map(
  data[methodes],
  \(list_df){
    array_cor <- map(
      list_df,
      \(df){
        calc_correlation(df)
      }
    ) %>% abind::abind(along = 3) 
    list(mean = array_cor %>% apply(MARGIN = 1:2, FUN = mean),
         sd = array_cor %>% apply(MARGIN = 1:2, FUN = sd))
  }
)