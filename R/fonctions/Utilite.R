moycum_mesures <- function() {
  analyses$utility_measures_all_meth <- analyses$utility_measures_all_meth %>%
    group_by(method) %>% 
    mutate(i = 1:500) %>% 
    mutate(across(pMSE:U, cummean, .names = "{.col}_cummean"))
  
  analyses$utility_measures_all_meth %>% 
    ggplot() +
    geom_line(aes(x = i, y = pMSE_cummean, col = method))
}

distribution_mesures <- function() {
  utility_graph <- map(
    c("pMSE","SPECKS","PO50","U"),
    \(meth){
      analyses$utility_measures_all_meth %>% 
        ggplot( aes(x=method, y=.data[[meth]], fill=method)) +
        geom_violin(width=1.4) +
        geom_boxplot(width=0.1, color="grey", alpha=0.2) +
        scale_fill_viridis(discrete = TRUE) +
        coord_flip() +
        theme_ipsum() +
        theme(
          legend.position="none",
          plot.title = element_text(size=11)
        ) +
        ggtitle(paste0("distribution des ", meth)) +
        ylab(meth) + xlab("méthode")
    }
  )
  names(utility_graph) <- c("pMSE","SPECKS","PO50","U")
  
  return(list(utility_graph$pMSE, utility_graph$SPECKS, utility_graph$PO50, utility_graph$U))
}

resume_mesures <- function() {
  utility_measures_summary <- analyses$utility_measures_all_meth %>% 
    group_by(method) %>% 
    summarise(
      across(
        pMSE:U, 
        list(mean = mean, min = min, max = max, 
             pc025 = ~quantile(., probs = 0.025),
             pc975 = ~quantile(., probs = 0.975),
             sd = sd
        ),
        .names = "{.col}_{.fn}"
      )) %>%
    tidyr::pivot_longer(-1, names_to = "indicateur", values_to = "val") %>% 
    tidyr::separate(indicateur, into = c("utility","indicateur"))
  
  utility_measures_summary %>% 
    filter(indicateur == "mean") %>%
    ggplot() +
    geom_bar(aes(x = method, y = val, fill = utility), stat = "identity")+
    coord_flip() +
    facet_wrap(~utility, scales = "free") +
    theme_ipsum()
}