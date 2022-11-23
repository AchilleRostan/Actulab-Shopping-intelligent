library("tidyverse")
library("xgboost")
library("ggplot2")
library("Matrix")
library(data.table)

shopping <- readRDS(file = "C:/Users/ARFOT/Documents/Actulab_final/shopping.rds")
reassureurs <- shopping %>% pull(ReassFinal) %>%  unique %>% as.character %>% sort()
## "None" "Re1"  "Re2"  "Re3"  "Re4"  "Re5"  "Re6"

to_split <- function(dataset = shopping, reas = reassureurs[1]) { 
  
  to_data <- dataset %>% dplyr::filter(ReassFinal == reas)  
  to_data <- droplevels(to_data)
  #to_data <- to_data[, sapply(to_data, nlevels) > 1]
  
  #### Séparer la base de données en entrainement (`60 %`), Validation (`20 %`) et Test (`20 %`)
 
  # Etape 1
  set.seed(62022)   
  train_rows <- sample(1:nrow(to_data), 0.8*nrow(to_data)) 
  to_train <- to_data[train_rows, ] 
  data_test <- to_data[-train_rows, ] 
  
  # Etape 2 
  set.seed(62022) 
  ids_train <- sample(1:nrow(to_train), 0.75*nrow(to_train))  
  data_train <- to_train[ids_train, ] 
  data_valid <- to_train[-ids_train, ] 
  
  data_train$class <- as.numeric(data_train$DecisionFinale) - 1
  data_valid$class <- as.numeric(data_valid$DecisionFinale) - 1
  data_test$class <- as.numeric(data_test$DecisionFinale) - 1
  
  # class <- data.table(DecisionFinale = %>% pull(DecisionFinale) %>%  unique %>% as.character,
  #                     class = )
  
  func <- function(x) {
    !(class(x) == 'factor' & nlevels(x) == 1)
  }

  data_train <- data_train[, sapply(data_train, func)]
  data_valid <- data_valid[, sapply(data_valid, func)]
  data_test <- data_test[, sapply(data_test, func)]
  
  return(list(data_train = data_train,
              data_valid = data_valid,
              data_test = data_test))
}
 
xgb_prepar <- function(dataset2 = shopping, reas2 = reassureurs[1]) {
  
  data1 <- to_split(dataset = dataset2, reas = reas2)
  
  to_train <- data1$data_train
  to_valid <- data1$data_valid 
  
  ## create matrix - One hot encoding 
  fm <- as.formula("class ~. - CaseNumber - DecisionFinale") 
  
  training <- model.matrix(fm, data = to_train) 
  train_matrix <- xgb.DMatrix(data = as.matrix(training), label = to_train$class)
  
  validation <- model.matrix(fm, data = to_valid) 
  valid_matrix <- xgb.DMatrix(data = as.matrix(validation), label = to_valid$class)
  
  return(list(train_matrix = train_matrix,
              valid_matrix = valid_matrix
              ))
}

xgb_model <- function(dataset3 = shopping, reas3 = reassureurs[1]) {
  
  data1 <- to_split(dataset = dataset3, reas = reas3)
  nclass <- length(levels(data1$data_train$DecisionFinale))
  
  d1 <- xgb_prepar(dataset2 = dataset3, reas2 = reas3)
  
  watchlist <- list(train = d1$train_matrix, valid = d1$valid_matrix)
  
  to_params <- list(booster = "gbtree", 
                    objective = "multi:softprob", 
                    eval_metric = "mlogloss", 
                    eta = 0.01, 
                    num_class = nclass, 
                    max_depth = 5, 
                    gamma = 0.01, 
                    subsample = 0.75, 
                    colsample_bytree = 0.75, 
                    min_child_weight = 10)
 
  ## XGBoost Model 
  to_model <- xgb.train(params = to_params, 
                        data = d1$train_matrix, 
                        nrounds = 3e4, # 30 000 
                        verbose = 1, 
                        print_every_n = 1e3, # 1000 
                        early_stopping_rounds = 30,
                        watchlist = watchlist)
  
  preds <- predict(to_model, newdata = d1$valid_matrix, type = "prob") 
  preds <- matrix(preds, nrow = nrow(d1$valid_matrix), ncol = nclass, byrow = T)
  colnames(preds) <- levels(data1$data_train$DecisionFinale)
  
  return(list(to_model = to_model,
              preds = preds))
}


# model_ia <- xgb_model(reas3 = "None")$to_model
# model_re1 <- xgb_model(reas3 = "Re1")$to_model
# model_re2 <- xgb_model(reas3 = "Re2")$to_model
# model_re3 <- xgb_model(reas3 = "Re3")$to_model
# model_re4 <- xgb_model(reas3 = "Re4")$to_model
# model_re5 <- xgb_model(reas3 = "Re5")$to_model
# model_re6 <- xgb_model(reas3 = "Re6")$to_model
# 
# # saves
# saveRDS(model_ia, file = "C:/Users/ARFOT/Documents/Actulab_final/model_ia.rds")
# saveRDS(model_re1, file = "C:/Users/ARFOT/Documents/Actulab_final/model_re1.rds")
# saveRDS(model_re2, file = "C:/Users/ARFOT/Documents/Actulab_final/model_re2.rds")
# saveRDS(model_re3, file = "C:/Users/ARFOT/Documents/Actulab_final/model_re3.rds")
# saveRDS(model_re4, file = "C:/Users/ARFOT/Documents/Actulab_final/model_re4.rds")
# saveRDS(model_re5, file = "C:/Users/ARFOT/Documents/Actulab_final/model_re5.rds")
# saveRDS(model_re6, file = "C:/Users/ARFOT/Documents/Actulab_final/model_re6.rds")

## Exemple d'utilisation du modèle :

# new_data <- shopping
# id1 <- which(colnames(new_data) == "IndRe1")
# new_data[1, id1:ncol(new_data)] <- "0"
# 
# nlev <- nlevels(to_split(reas = "None")$data_train$DecisionFinale)
# v_matrix <- xgb_prepar(dataset2 = new_data, reas2 = "None")$valid_matrix
# preds <- predict(model_ia, newdata = v_matrix, type = "prob")
# 
# final <- NULL
# if (nlev == 2) {
# final <- case_when(which.max(preds[1:nlev]) - 1 == 0 ~ "Surprime",
#                    which.max(preds[1:nlev]) - 1 == 1 ~ "Standard",
#                    TRUE ~ "Error" 
#                   )
# }
# 
# if (nlev == 4) {
#   final <- case_when(which.max(preds[1:nlev]) - 1 == 0 ~ "Refus",
#                      which.max(preds[1:nlev]) - 1 == 1 ~ "Remis",
#                      which.max(preds[1:nlev]) - 1 == 2 ~ "Surprime",
#                      which.max(preds[1:nlev]) - 1 == 1 ~ "Standard",
#                      TRUE ~ "Error" 
#   )
# }

#final



 
 
 
