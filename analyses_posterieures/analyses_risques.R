library(dplyr)
library(synthpop)
library(ggplot2)


noms_data_files <- list(
  "orig" = "data/puf65.csv",
  "cart" = "data/puf_cart.csv",
  "ctgan" = "data/puf65_ctgan.csv",
  "tvae" = "data/puf65_tvae.csv"
)


Ldatas <- purrr::map(
  noms_data_files, 
  \(x) read.csv(x) %>% 
    mutate(
      STATUT = as.integer(ifelse(STATUT == "STATUT", 999, STATUT)),
      PCS1 = as.integer(ifelse(PCS1 == "PCS1", 999, PCS1))
    )
)


str(Ldatas)

purrr::map(Ldatas, dim)
purrr::map(Ldatas, \(x) length(setdiff(names(x), names(Ldatas$orig))) + length(setdiff( names(Ldatas$orig), names(x))))

names(orig)

cles <- c("AGE6", "DIP7", "METRODOM", "PCS1", "SEXE", "STATUT")
sens <- c("ACTEU", "ANCCHOM")


# Uniques on keys in original sample data:

Luniques <- purrr::map(
  Ldatas,
  \(x) x %>% 
    count(across(all_of(cles))) %>%
    filter(n == 1) 
)

purrr::map(Luniques, dim)

Luniques_id_orig <- purrr::map(
  Luniques[-1],
  \(x) x %>% inner_join(Luniques$orig)
) 



str(Luniques_id_orig)

Luniques_id_orig %>% 
  purrr::imap(
    \(x,n) x %>% summarise(nb = n()) %>% 
      mutate(data = n, part = nb/nrow(Ldatas$orig)*100) %>%
      select(data, n_uniques_synth_orig = nb, part_uniques_synth_orig = part)
  ) %>% 
  purrr::list_rbind() %>% 
  bind_rows(tibble(data = "orig", n_uniques_synth_orig = nrow(Luniques$orig), part_uniques_synth_orig = nrow(Luniques$orig)/nrow(Ldatas$orig)*100)) %>% 
  knitr::kable(format = "latex", booktabs = TRUE, digits = 3, caption = "Part d'uniques dans le jeu synthétique qui son aussi uniques dans l'original")



# Risques de divulgation - synthpop
res_disclosure <- purrr::map(
  Ldatas[-1],
  \(x) disclosure(
    object = x %>% select(all_of(c(cles,sens))) %>% 
      mutate(across(all_of(c(cles,sens)), as.character)), 
    data = Ldatas$orig  %>% select(all_of(c(cles,sens))) %>% 
      mutate(across(all_of(c(cles,sens)), as.character)),
    keys = cles, target = "ACTEU",
    denom_lim = 5,
    exclude_ov_denom_lim = TRUE,
    digits = 3,
    usetargetNA = FALSE,
    usekeysNA = FALSE
  )
)

purrr::imap(res_disclosure, \(x,n) x$ident %>% mutate(data=n)) %>% purrr::list_rbind() %>% 
  relocate(data) %>% 
  knitr::kable(row.names = FALSE, format="latex", booktabs=TRUE, digits=3, caption="")

purrr::imap(res_disclosure, \(x,n) x$attrib %>% mutate(data=n)) %>% purrr::list_rbind() %>% 
  relocate(data) %>% 
  select(-max_denom, -mean_denom) %>% 
  knitr::kable(row.names = FALSE, format="latex", booktabs=TRUE, digits=3, caption="")

purrr::imap(res_disclosure, \(x,n) x$allCAPs %>% mutate(data=n)) %>% purrr::list_rbind() %>% 
  relocate(data)

