library(xgboost)
library(data.table)
library(magrittr)

nthread <- 1
setDTthreads(1)
RhpcBLASctl::blas_set_num_threads(1)
RhpcBLASctl::omp_set_num_threads(1)

PrepTrainValidTest <- function(
    data,
    parameters,
    vars_dep,
    vars_ind,
    changes = FALSE,
    changes_column = 'label'
){

  bycols <- c('id_col', 'date')
  dt_tmp <- data %>%
    copy() %>%
    .[order(id_col, date)]

  if(changes){
    dt_tmp <- dt_tmp %>%
      .[, tmp_phase := c(0, abs(diff(get(changes_column)))) > 0,
        by = 'id_col'] %>%
      .[tmp_phase == TRUE] %>%
      .[, tmp_phase := NULL]
  }

  dt_train <- dt_tmp %>%
    .[date >= parameters[['train_start']] & date < parameters[['train_end']]]

  if(nrow(dt_train) > parameters[['max_train_obs']]){
    set.seed(parameters[['seed']])
    dt_train <- dt_train[sample(1:.N, parameters[['max_train_obs']])]
  }

  set.seed(parameters[['seed']])
  idx_validation <- sample(
    1:nrow(dt_train),
    round(nrow(dt_train) * parameters[['validation_share']]))
  idx_train <- setdiff(1:nrow(dt_train), idx_validation)
  dt_validation <- dt_train[idx_validation]
  dt_train <- dt_train[idx_train]
  dt_test <- dt_tmp %>%
    .[date >= parameters[['test_start']] & date < parameters[['test_end']]]


  formula <- paste0(vars_dep, '~', paste0(vars_ind, collapse = '+'))
  return(list(
    train = dt_train[, c(bycols, vars_dep, vars_ind), with = FALSE],
    validation = dt_validation[, c(bycols, vars_dep, vars_ind), with = FALSE],
    test = dt_test[, c(bycols, vars_dep, vars_ind), with = FALSE],
    dep = vars_dep,
    ind = vars_ind,
    formula = formula))
}

n=10000
set.seed(369)
dt_data <- as.data.table(cbind(seq.Date(as.IDate("2000-01-01"), by = '1 day', length.out = n),
                               rnorm(n), sapply(rep(n, 100), rnorm)))

colnames(dt_data) <- c('date', "Y", paste0("X_", 1:100))
dt_data[, id_col := "A"]
dt_data$date <- as.IDate(dt_data$date)
dt_data[, Y := (Y>0)*1]
test_start <- dt_data$date[n*0.8]
vars_ind <-  paste0("X_", 1:100)
# XGBOOST HYPERPARAMETER OPTIMIZATION ------------------------------------------

parameters <- list(
  seed = 2022,
  train_start = dt_data[, min(date)],
  train_end = test_start,
  test_start = test_start,
  test_end = dt_data[, max(date)],
  validation_share = 0.15,
  max_train_obs = nrow(dt_data))

lst_data <- PrepTrainValidTest(
  data = dt_data ,
  parameters = parameters,
  vars_dep = "Y",
  vars_ind = vars_ind)

xgb_train <- xgb.DMatrix(
  as.matrix(lst_data[["train"]][, lst_data[["ind"]], with = FALSE]),
  label = as.matrix(lst_data[["train"]][, get(lst_data[["dep"]])]))
xgb_valid <- xgb.DMatrix(
  as.matrix(lst_data[["validation"]][, lst_data[["ind"]], with = FALSE]),
  label = as.matrix(lst_data[["validation"]][, get(lst_data[["dep"]])]))

set.seed(2022)
dt_params <- lapply(1:100, function(x)
  data.table(
    booster = "gbtree",
    objective = "binary:logistic",
    max_depth = sample(3:10, 1),
    eta = runif(1, .01, .3),
    subsample = runif(1, .7, 1),
    colsample_bytree = runif(1, .7, 1),
    min_child_weight = sample(0:10, 1),
    lambda = runif(1, 0, .7),
    alpha = runif(1, 0, .7))) %>%
  rbindlist()

t1 <- Sys.time()
errors <- sapply(1:100, function(x) {
  set.seed(2022)
  xgb.train(
    data = xgb_train,
    booster = "gbtree",
    objective = "binary:logistic",
    max_depth = dt_params[[x, "max_depth"]],
    eta = dt_params[[x, "eta"]],
    subsample = dt_params[[x, "subsample"]],
    colsample_bytree = dt_params[[x, "colsample_bytree"]],
    min_child_weight = dt_params[[x, "min_child_weight"]],
    lambda = dt_params[[x, "lambda"]],
    alpha = dt_params[[x, "alpha"]],
    nrounds = 5000,
    eval_metric = "logloss",
    early_stopping_rounds = 20,
    scale_pos_weight = table(getinfo(xgb_train, "label"))[[1]] /
      table(getinfo(xgb_train, "label"))[[2]],
    #gpu_id = 0,
    #tree_method = "gpu_hist",
    nthread = nthread,
    verbose = 0,
    watchlist = list(train = xgb_train, val = xgb_valid)
  )$evaluation_log$val_logloss %>%
    min()
})
t2 <- Sys.time()
time_n <- t2-t1

dff <- data.frame(number = time_n, units = units(time_n))
write.csv(dff, "03_benchmark.csv", row.names = FALSE)

print("03_benchmark.R done!")
