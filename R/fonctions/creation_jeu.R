jeudedonnees_SD2011 = function() {
  require(synthpop)
  vars_non = c("region", "eduspec", "unempdur", "income", "mmarr",
               "ymarr", "msepdiv", "ysepdiv", "nociga", "wkabdur",
               "wkabintdur", "emcc", "workab")
  jd = SD2011[, !names(SD2011) %in% vars_non, drop = F]
  df = na.omit(jd)
  df = df[-(which(df[, "nofriend"] == -8)), ]
  return(df)
}