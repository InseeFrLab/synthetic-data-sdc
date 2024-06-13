library(ctgan)

synthesizer <- ctgan()
synthesizer %>% fit(iris, epochs = 20)
synthesizer %>% ctgan_sample() %>%
  dplyr::mutate_if(is.numeric, ~ pmax(.x, 0.1))

model_dir <- tempdir()
synthesizer %>% ctgan_save(model_dir)
ctgan_load(model_dir)