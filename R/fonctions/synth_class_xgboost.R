
synth_class_xgboost <- function(data, param_xgboost, seed){
  require(Matrix)
  require(xgboost)
  
  # train et test samples -------------------------------------
  set.seed(seed)
  
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
  
  # str(matrix_train)
  
  matrix_test <- data_test %>%
    select(-is_synth) %>%
    mutate(across(everything(), as.numeric)) %>%
    as.matrix() %>%
    Matrix::Matrix(sparse = TRUE)
  
  # str(matrix_test)
  
  
  # listes tain/test
  
  train <- xgb.DMatrix(data = matrix_train, label = data_train$is_synth)
  test <- xgb.DMatrix(data = matrix_test, label = data_test$is_synth)
  evals = list(train = train, test = test)
  
  
  bst <- xgb.cv(
    params = param_xgboost,
    data=train,
    nrounds=1000, 
    nfold = 10,
    metrics = "error",
    watchlist=evals,
    showsd = FALSE,
    verbose = FALSE
  )
  
  return(bst)
  
}

