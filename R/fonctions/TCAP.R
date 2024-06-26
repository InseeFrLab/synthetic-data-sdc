if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr"); library(dplyr)

calc_TCAP <- function(data_org, data_syn, K_s, T_s) {
  WEAP_s <- data_syn %>% 
    group_by(across(all_of(K_s))) %>%
    mutate(nb_ind_by_cle = n()) %>%
    ungroup() %>%
    group_by(across(all_of(c(K_s, T_s)))) %>%
    mutate(num = n()) %>%
    ungroup() %>%
    mutate(WEAP = num / nb_ind_by_cle)
  
  if (sum(WEAP_s$WEAP == 1) == 0) {
    return("TCAP indéfini")
  }
  else {
    TCAP <- merge(
      data_org,
      WEAP_s %>% filter(WEAP == 1) %>% select(all_of(K_s), WEAP) %>% unique(),
      by = K_s,
      all.x = F,
      all.y = F
    ) %>%
      group_by(across(all_of(K_s))) %>%
      mutate(nb_ind_by_cle = n()) %>%
      ungroup() %>%
      group_by(across(all_of(c(K_s, T_s)))) %>%
      mutate(num = n()) %>%
      ungroup() %>%
      mutate(TCAP = num / nb_ind_by_cle)
    
    return(mean(TCAP$TCAP, na.rm = T))
  }
}

# var_quasi_id <- c("sex", "age", "agegr", "placesize", "edu", "socprof", "marital")
# var_sensi <- c("depress", "trust", "trustfam", "trustneigh", "ls", "smoke", "alcabuse", "alcsol")

# calc_TCAP(data$original, data$cart[[1]], c("sex", "age"), "depress")            # 0.1809821
# calc_TCAP(data$original, data$ctree[[1]], c("sex", "age"), "depress")           # 0.1634573
# calc_TCAP(data$original, data$parametric[[1]], c("sex", "age"), "depress")      # 0.2538661
# calc_TCAP(data$original, data$rf[[1]], c("sex", "age"), "depress")              # 0.2195462
# calc_TCAP(data$original, data$bag[[1]], c("sex", "age"), "depress")             # 0.2345012
# calc_TCAP(data$original, data$sample[[1]], c("sex", "age"), "depress")          # 0.1878088
# 
# calc_TCAP(data$original, data$cart[[1]], "sex", var_sensi)                      # 0.005535492
# calc_TCAP(data$original, data$cart[[1]], c("sex", "age"), var_sensi)            # Indéfini



# WEAP_s <- data$cart[[1]] %>% 
#   group_by(across(all_of(K_s))) %>%
#   mutate(nb_ind_by_cle = n()) %>%
#   ungroup() %>%
#   group_by(across(all_of(c(K_s, T_s)))) %>%
#   mutate(num = n()) %>%
#   ungroup() %>%
#   mutate(WEAP = num / denom)
# 
# 
# 
# TCAP <- merge(
#   data$original,
#   WEAP_s %>% filter(WEAP == 1) %>% select(all_of(K_s), WEAP) %>% unique(),
#   by = K_s,
#   all.x = F,
#   all.y = F
# ) %>%
#   group_by(across(all_of(K_s))) %>%
#   mutate(nb_ind_by_cle = n()) %>%
#   ungroup() %>%
#   group_by(across(all_of(c(K_s, T_s)))) %>%
#   mutate(num = n()) %>%
#   ungroup() %>%
#   mutate(TCAP = num / nb_ind_by_cle) %>% str()

# Tests ------------------------------------------------------------------------





