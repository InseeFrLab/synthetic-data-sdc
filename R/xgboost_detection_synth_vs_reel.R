# Utiliser XGBoost pour discriminer entre données réelles et données synthétiques

install.packages("xgboost")
install.packages("Matrix")
install.packages("caret")


# Inspiration: https://openreview.net/pdf?id=tTQzJ6TJGVi 
# Tabular Data Generation: Can We Fool XGBoost ?
# train / test
# L'idée ici est de voir si en présentant un nouvel individu on est en mesure
# de savoir s'il est généré ou bien fait partie de la population originale
# L'indistinction joue en faveur de l'utilité (indistinction = la synthésitation reproduit parfaitement la pop)
# Mais si xgboost ne sait pas dire si un nouvel individu est synth ou orig, 

library(Matrix)
library(xgboost)
library(aws.s3)
library(dplyr)
library(caret)
library(ggplot2)

# data ---------------
# Import des données -----------------------------------------------------------
FILE_KEY_IN_S3_1 <- "20240512_sim_synthpop_sample_cart_ctree_parametric_bag_rf_500_sims.RDS"
BUCKET = "projet-donnees-synthetiques"
BUCKET_SIM_1 = file.path(BUCKET, "simulations")
FILE_KEY_IN_S3_2 <- "resultats_analyses.RDS"
BUCKET_SIM_2 = file.path(BUCKET, "analyses")

sims_synth <- aws.s3::s3read_using(
  FUN = readRDS,
  object = FILE_KEY_IN_S3_1,
  bucket = BUCKET_SIM_1,
  opts = list("region" = "")
)
str(sims_synth, max.level = 1)

num_sim = 1
methode = "bag"

data <- sims_synth$original %>% mutate(is_synth = 0) %>% 
  bind_rows(sims_synth[[methode]][[num_sim]] %>% mutate(is_synth = 1))

str(data)
table(data$is_synth)

# train et test samples -------------------------------------
set.seed(56789)

ratio_train = 0.8
n_train <- floor(nrow(data)*ratio_train)
n_test <- nrow(data) - n_train
rows_train <- sample(1:nrow(data), n_train, replace = FALSE)
rows_test <- sample((1:nrow(data))[-rows_train], n_test, replace = FALSE)

# length(rows_test) + length(rows_train) == nrow(data)

data_train <- data[rows_train,]
data_test <- data[rows_test,]

table(data_train$is_synth)
table(data_test$is_synth)

# tranformation en dgcMatrix (matrices sparses) -----------

matrix_train <- data_train %>% 
  select(-is_synth) %>% 
  mutate(across(everything(), as.numeric)) %>%
  as.matrix() %>% 
  Matrix::Matrix(sparse = TRUE)

str(matrix_train)

matrix_test <- data_test %>%
  select(-is_synth) %>%
  mutate(across(everything(), as.numeric)) %>%
  as.matrix() %>%
  Matrix::Matrix(sparse = TRUE)

str(matrix_test)


# listes tain/test

train <- xgb.DMatrix(data = matrix_train, label = data_train$is_synth)
test <- xgb.DMatrix(data = matrix_test, label = data_test$is_synth)
evals = list(train = train, test = test)

# train model ----------------------------------------------
# détail des paramètres :
# https://xgboost.readthedocs.io/en/latest/parameter.html

bst <- xgboost(
  data = train,
  max.depth = 20, # profondeur max des arbres (plus grand => accroit le risque de surapprentissage)
  eta = 0.3, # taux d'apprentissage  (plus grand => accroit le risque de surapprentissage)
  subsample = 0.6, # taux d'échantillonnage du jeux de donnée - pour limiter le risque de surapprentissage
  sampling_method = "uniform", # méthode d'échantillonnage
  tree_method = "auto", # algorithm de construction de l'arbre (auto, exact, approx, hist)
  nthread = 2, #nb de threads utilisés en parallèle (par défaut tous les threads à dispo sont utilisés)
  # device = "cpu", # type d'outil pour exécuter xgboos: cpu, cuda (gpu)
  nrounds = 100, # nb d'itérations de boosting 
  objective = "binary:logistic",
  eval_metric = "error",
  eval_metric = "logloss",
  verbose = 2
)

str(bst)

plot(bst$evaluation_log[, 1:2], type ="l", col = "blue")
plot(bst$evaluation_log[, c(1,3)], type = "l", col = "red")

# Prédictions
label = getinfo(test, "label")
pred <- predict(bst, test)
err <- mean(as.numeric(pred > 0.5) != label)
pMSE <- mean((pred - 0.5)^2)
print(paste("test-error=", err))
print(paste("pMSE=", pMSE))

# avec xgb.train ---------------

param <- list(
  max.depth=20, 
  eta=0.01, 
  subsample = 0.6, 
  sampling_method = "uniform",
  tree_method = "auto",
  nthread = 5,
  eval.metric = "error", 
  eval.metric = "logloss", 
  objective = "binary:logistic"
)

bst <- xgb.train(
  params = param,
  data=train,
  nrounds=1000, 
  watchlist=evals,
  verbose = FALSE
)


# avec une cross validation ---------------

bst <- xgb.cv(
  params = param,
  data=train,
  nrounds=1000, 
  nfold = 10,
  metrics = "error",
  watchlist=evals,
  showsd = FALSE,
  verbose = FALSE
)

ggplot(data = bst$evaluation_log) +
  geom_line(aes(x= iter, y = train_error_mean, color = "Train", linetype = "Moyenne")) +
  geom_line(aes(x= iter, y = train_error_mean + 2*train_error_std, color = "Train", linetype = "IC")) +
  geom_line(aes(x= iter, y = train_error_mean - 2*train_error_std, color = "Train", linetype = "IC")) +
  geom_line(aes(x= iter, y = test_error_mean, color = "Test", linetype = "Moyenne")) +
  geom_line(aes(x= iter, y = test_error_mean + 2*test_error_std, color = "Test", linetype = "IC")) +
  geom_line(aes(x= iter, y = test_error_mean - 2*test_error_std, color = "Test", linetype = "IC")) +
  geom_hline(yintercept = 0.5, linetype = "dotted", color = "gray5") +
  scale_y_continuous(breaks = seq(0,1,0.1), expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  labs(y = "taux d'erreur de prédiction", x="Itération") +
  scale_color_manual(
    name='',
    breaks=c('Train', 'Test'),
    values=c('Train'='darkred', 'Test'='steelblue')
  ) +
  scale_linetype_manual(
    name='',
    breaks=c('Moyenne', 'IC'),
    values=c('Moyenne'='solid', 'IC'='dashed')
  ) +
  theme_light()



# Comparaison des modèles -------------------------

param <- list(
  max.depth=20, 
  eta=0.01, 
  subsample = 0.6, 
  sampling_method = "uniform",
  tree_method = "auto",
  nthread = 5,
  eval.metric = "error", 
  eval.metric = "logloss", 
  objective = "binary:logistic"
)

source("R/fonctions/synth_class_xgboost.R")

methodes <- names(sims_synth)[names(sims_synth) != "original"]

res_class_bst <- purrr::map(
  methodes,
  function(methode){
    cat("modèle:", methode, "\n")
    num_sim = 1
    data <- sims_synth$original %>% mutate(is_synth = 0) %>% 
      bind_rows(sims_synth[[methode]][[num_sim]] %>% mutate(is_synth = 1))
    synth_class_xgboost(data, param, seed = 40889)
  }
)
names(res_class_bst) <- methodes

res_class_bst %>% 
  purrr::imap(\(l,m) l$evaluation_log %>% mutate(methode = m)) %>% 
  purrr::list_rbind() %>% 
  ggplot() +
  geom_line(aes(x= iter, y = train_error_mean, color = methode, linetype = "Train")) +
  geom_line(aes(x= iter, y = test_error_mean, color = methode, linetype = "Test")) +
  geom_hline(yintercept = 0.5, linetype = "dotted", color = "gray5") +
  scale_y_continuous(breaks = seq(0,1,0.1), expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  labs(y = "taux d'erreur de prédiction", x="Itération") +
  scale_linetype_manual(
    name='',
    breaks=c('Train', 'Test'),
    values=c('Train'='solid', 'Test'='dashed')
  ) +
  theme_light()

