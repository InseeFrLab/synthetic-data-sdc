
library(dplyr)

iris %>% str()

iris %>% 
  mutate(ratio_sepal = Sepal.Length/Sepal.Width) %>% 
  str()

iris %>% 
  group_by(Species) %>% 
  summarise(moy = mean(Sepal.Length))



iris %>% 
  group_by(Species) %>% 
  mutate(
    across(
      Sepal.Length:Petal.Width,
      list(moy = mean, med = median, q1 = ~quantile(., probs = 0.25)),
      .names = "{.col}_{.fn}"
    )
  ) %>% str()



iris %>% 
  group_by(Species) %>% 
  summarise(
    across(
      Sepal.Length:Petal.Width,
      list(moy = mean, med = median, q1 = ~quantile(., probs = 0.25)),
      .names = "{.col}_{.fn}"
    )
  )


iris %>% 
  group_by(Species) %>% 
  summarise(
    across(
      all_of(c("Sepal.Length", "Petal.Length")),
      list(moy = mean, med = median, q1 = ~quantile(., probs = 0.25)),
      .names = "{.col}_{.fn}"
    )
  ) %>% 
  tidyr::pivot_longer(-1, names_to = "indicateur", values_to = "val") %>% 
  tidyr::separate(col = "indicateur", into = c("caract", "stat"), sep = "_") %>% 
  ggplot() +
  geom_point(aes(x = val, y = Species, col = stat)) +
  facet_wrap(~caract)





