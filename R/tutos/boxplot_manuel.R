library(dplyr)
library(ggplot2)

# faire un boxplot à partir des quantiles de la distribution
# boite = intervalle interquartile
# moustaches = intervalle à 95%
# points min et max

df <- data.frame(
  group = c("a","b"),
  min = c(1,3), 
  q2_5 = c(2,5),
  q25 = c(12,16),
  q50 = c(22,28),
  q75 = c(27,35),
  q97_5 = c(34,47),
  max = c(52,53)
)

ggplot(df, aes(x = group)) +
  # boxplot
  geom_boxplot(
    aes(
      ymin = q2_5, lower = q25, middle = q50, upper = q75, ymax = q97_5 
    ),
    stat = "identity"
  ) +
  # pour ajouter les min max comme outliers
  geom_point(aes(y = min)) +
  geom_point(aes(y = max)) +
  labs(y = "") +
  # pour un boxplot à l'horizontal
  coord_flip()
