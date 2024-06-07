data.afdm <- FAMD(data, ncp = 73, graph = FALSE)
#print(data.afdm)
summary(data.afdm)

eig.val <- get_eigenvalue(data.afdm)
barplot(eig.val[, 2], 
        names.arg = 1:nrow(eig.val), 
        main = "Variance expliquée par dimensions (%)",
        xlab = "Principales dimensions",
        ylab = "Pourcentage de variance",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(eig.val), eig.val[, 2], 
      type = "b", pch = 19, col = "red")

fviz_screeplot(data.afdm)

var <- get_famd_var(data.afdm)

# Coordinates of variables
head(var$coord)
# Cos2: quality of representation on the factore map
head(var$cos2)
# Contributions to the  dimensions
head(var$contrib)

fviz_famd_var(data.afdm, repel = TRUE)
fviz_contrib(data.afdm, "var", axes = 1)
fviz_contrib(data.afdm, "var", axes = 2)

# Variables quantitatives
quanti.var <- get_famd_var(data.afdm, "quanti.var")
fviz_famd_var(data.afdm, "quanti.var", repel = TRUE, col.var = "black") # Premier cercle de corrélation
fviz_famd_var(data.afdm, "quanti.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)
# Color by cos2 values: quality on the factor map
fviz_famd_var(data.afdm, "quanti.var", col.var = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
              repel = TRUE)

# Variables qualitatives
quali.var <- get_famd_var(data.afdm, "quali.var")
fviz_famd_var(data.afdm, "quali.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

# Graphiques des individus
plot(data.afdm, choix = "ind")
plot(data.afdm, choix = "var")

fviz_ellipses(data.afdm, c("sex", "agegr"), geom = "points")