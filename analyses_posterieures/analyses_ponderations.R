library(dplyr)
library(synthpop)
library(ggplot2)


noms_data_files <- list(
  "orig" = "data/puf65.csv",
  "cart" = "data/puf_cart.csv",
  "ctgan" = "data/puf65_ctgan.csv",
  "tvae" = "data/puf65_tvae.csv"
)


Ldatas <- purrr::map(noms_data_files, read.csv) 

str(Ldatas)

purrr::map(Ldatas, dim)
purrr::map(Ldatas, \(x) length(setdiff(names(x), names(Ldatas$orig))) + length(setdiff( names(Ldatas$orig), names(x))))

names(orig)

keys <- c("AGE6", "DIP7", "METRODOM", "PCS1", "SEXE", "STATUT")
sens <- c("ACTEU", "ANCcHOM")


# Qualité de la variable EXTRIAN
purrr::imap(Ldatas[-3], \(x,n){
  x %>% 
    summarise(
      N = sum(EXTRIAN)
    ) %>% 
    mutate(data = n)
}) %>% 
  purrr::list_rbind() %>% 
  mutate(diff = (N/N[data=="orig"]-1)*100) %>% 
  select(data, N, diff) %>% 
  knitr::kable(format = "latex", caption ="", digits = 3, booktabs=TRUE)


purrr::imap(Ldatas[-3], \(x,n){
  x %>% 
    summarise(
      min = min(EXTRIAN),
      q1 = quantile(EXTRIAN,probs=0.25),
      mean = mean(EXTRIAN),
      q3 = quantile(EXTRIAN,probs=0.75),
      max = max(EXTRIAN)
    ) %>% 
    mutate(data = n)
}) %>% 
  purrr::list_rbind()

extrians <- purrr::imap(Ldatas[-3], \(x,n){
  x %>%
    select(EXTRIAN) %>% 
    mutate(type_data = n)
}) %>%  purrr::list_rbind()

ggplot(extrians) +
  geom_boxplot(aes(x=EXTRIAN, fill = type_data)) +
  scale_y_continuous("", breaks=c(-0.25,0,0.25), labels = c("cart","orig","tvae"), minor_breaks = NULL) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank())


# Reproduction des taux de chômag, activité

acteu_pond <- purrr::imap(Ldatas[-3], \(x,n){
  x %>% 
    group_by(ACTEU) %>% 
    summarise(
      NB = sum(EXTRIAN),
      .groups = "drop"
    ) %>% 
    mutate(TAUX = NB/sum(NB), type_data = n) %>% 
    mutate(CHOM_EMP = ifelse(ACTEU == 3, NA, NB/sum(NB[ACTEU %in% 1:2])))
})%>% 
  purrr::list_rbind()

acteu_pond  %>% 
  ggplot() +
  geom_bar(aes(y=TAUX, x = ACTEU, fill = type_data), stat = "identity", position="dodge") +
  ggtitle("Parts de la population selon leur statut d'activité (en %)", 
          subtitle = "Calculs à partir des pondérations du fichier original ou synthétique") +
  scale_y_continuous("Parts en %", breaks=seq(0,1,0.1), labels = seq(0,1,0.1)*100, minor_breaks = NULL) +
  scale_fill_brewer("data", type = "qual", palette = 7) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank())

# 
# emp_act_labels <- c(
#   "1" = "Taux d'actifs en emploi",
#   "2" = "Taux de chômage"
# )

acteu_pond %>% 
  filter(ACTEU < 3) %>% 
  mutate(ACTEU = factor(ACTEU, levels = 1:2, labels=c("Taux d'actifs en emploi", "Taux de chômage"))) %>% 
  ggplot() +
  geom_bar(aes(y=CHOM_EMP, x = ACTEU,  fill = type_data), stat = "identity", position="dodge") +
  facet_wrap(~ACTEU, scales = "free") +
  ggtitle("Taux d'actifs en emploi et taux de chômage (en %)", 
          subtitle = "Calculs à partir des pondérations du fichier original ou synthétique") +
  scale_y_continuous("Parts en %", breaks=seq(0,1,0.1), labels = seq(0,1,0.1)*100, minor_breaks = NULL) +
  labs(x="") +
  scale_fill_brewer("data", type = "qual", palette = 7) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank())


# Taux de chômage par sexe et par âge

acteu_sexe_age_pond <- purrr::imap(Ldatas[-3], \(x,n){
  x %>% 
    group_by(ACTEU,SEXE,AGE6) %>% 
    summarise(
      NB = sum(EXTRIAN),
      .groups = "drop"
    ) %>% 
    group_by(SEXE,AGE6) %>% 
    mutate(TAUX = NB/sum(NB), type_data = n) %>% 
    mutate(CHOM_EMP = ifelse(ACTEU == 3, NA, NB/sum(NB[ACTEU %in% 1:2])))
})%>% 
  purrr::list_rbind()

acteu_sexe_age_pond %>% 
filter(ACTEU < 3) %>%
  filter(AGE6 < 90) %>% 
  select(type_data,ACTEU,SEXE,AGE6,CHOM_EMP) %>% 
  mutate(ACTEU = factor(ACTEU, levels = 1:2, labels=c("Actifs en emploi", "Chômage"))) %>% 
  mutate(SEXE = factor(SEXE, levels = 1:2, labels=c("Hom.", "Fem."))) %>% 
  ggplot() +
  geom_bar(aes(y=CHOM_EMP, x = as.factor(SEXE),  fill = type_data), stat = "identity", position="dodge") +
  facet_grid(ACTEU~AGE6, scales = "free", labeller = labeller(.cols = label_both, .rows = label_value)) +
  ggtitle("Taux d'actifs en emploi et taux de chômage (en %) par sexe et par âge", 
          subtitle = "Calculs à partir des pondérations du fichier original ou synthétique") +
  scale_y_continuous("Parts en %", breaks=seq(0,1,0.1), labels = seq(0,1,0.1)*100, minor_breaks = NULL) +
  labs(x="") +
  scale_fill_brewer("data", type = "qual", palette = 7) +
  theme_minimal(base_size = 24) +
  theme(panel.grid.major.x = element_blank())


# rename_with(~paste0("EXTRIAN_",n), EXTRIAN)
  


