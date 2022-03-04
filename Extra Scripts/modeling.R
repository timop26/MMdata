library(pROC)
library(lightgbm)
library(caret)

# Reading in all training data
training <- read.csv("all_training.csv")
sort(cor(training[, 5:ncol(training)])[, "score_diff"])
sort(cor(training[, 5:ncol(training)])[, "home_win"])



evaluate_models <- function(
  train_years,
  test_years,
  columns,
  rounds=0:6,
  max_leaves=6,
  max_depth=3,
  learning_rate=0.1,
  nrounds=100
  ) {

  # Testing data
  test <- training[(training$year %in% test_years) & (training$round %in% rounds), ]
  train <- training[(training$year %in% train_years) & (training$round %in% rounds), ]

  # Parameters for lightgbm
  param_list <- list(
    max_leaves=max_leaves,
    max_depth=max_depth,
    learning_rate=learning_rate,
    objective="binary",
    is_unbalance=TRUE
  )


  # Training lightgbm model
  gbm_mdl <- lightgbm(
    data=as.matrix(train[, columns]),
    params=param_list,
    label=train$home_win,
    nrounds=nrounds,
    verbose=0
  )

  # Feature importance
  lgb.plot.importance(lgb.importance(gbm_mdl), measure="Gain")

  # Predicting on testing data
  preds <- predict(gbm_mdl, as.matrix(test[, columns]))
  return(
    list(
      predictions=preds,
      data.frame(
        accuracy=mean(round(preds) == test$home_win),
        AUC=roc(test$home_win, preds, quiet=TRUE)$auc,
        sensitivity=sensitivity(as.factor(round(preds)), factor(test$home_win, levels=c(1, 0))),
        specificity=specificity(as.factor(round(preds)), factor(test$home_win, levels=c(1, 0)))
      )
    )
  )
}

cv <- function(columns, rounds, nrounds, max_leaves, max_depth, learning_rate) {
  rbind(
    evaluate_models(
      train_years=2011:2013,
      test_years=2014,
      columns=columns,
      rounds=rounds,
      nrounds=nrounds,
      max_leaves=max_leaves,
      max_depth=max_depth,
      learning_rate=learning_rate
    )[[2]],
    evaluate_models(
      train_years=2011:2014,
      test_years=2015,
      columns=columns,
      rounds=rounds,
      nrounds=nrounds,
      max_leaves=max_leaves,
      max_depth=max_depth,
      learning_rate=learning_rate
    )[[2]],
    evaluate_models(
      train_years=2011:2015,
      test_years=2016,
      columns=columns,
      rounds=rounds,
      nrounds=nrounds,
      max_leaves=max_leaves,
      max_depth=max_depth,
      learning_rate=learning_rate
    )[[2]],
    evaluate_models(
      train_years=2011:2016,
      test_years=2017,
      columns=columns,
      rounds=rounds,
      nrounds=nrounds,
      max_leaves=max_leaves,
      max_depth=max_depth,
      learning_rate=learning_rate
    )[[2]],
    evaluate_models(
      train_years=2011:2017,
      test_years=2018,
      columns=columns,
      rounds=rounds,
      nrounds=nrounds,
      max_leaves=max_leaves,
      max_depth=max_depth,
      learning_rate=learning_rate
    )[[2]],
    evaluate_models(
      train_years=2011:2018,
      test_years=2019,
      columns=columns,
      rounds=rounds,
      nrounds=nrounds,
      max_leaves=max_leaves,
      max_depth=max_depth,
      learning_rate=learning_rate
    )[[2]],
    evaluate_models(
      train_years=2011:2019,
      test_years=2021,
      columns=columns,
      rounds=rounds,
      nrounds=nrounds,
      max_leaves=max_leaves,
      max_depth=max_depth,
      learning_rate=learning_rate
    )[[2]]
  )
}

cv(c(2, 5, 6, 9:ncol(training)), 1, 50, 4, 5, 0.1)
cv(sample(c(2, 5, 6, 9:ncol(training)), 10), 1, 50, 4, 5, 0.1)
cv(c("MAS_0", "gini", "MOR_last", "top_100_recruit", "FTr.", "TOV."), 1, 50, 4, 5, 0.1)
cv(c(2, 5, 6, 9:ncol(training)), 0:6, 50, 4, 5, 0.1)

optim_function <- function(hyperparameters){
  apply(
    cv(c("MAS_0", "gini", "MOR_last", "top_100_recruit", "FTr.", "TOV."), 1,
       hyperparameters[1],
       hyperparameters[2],
       hyperparameters[3],
       hyperparameters[4]
    ),
    1,
    mean
    )[1] * -1
}

optim(c(50, 4, 5, 1), fn=optim_function, control=list(maxit=20))

y18 <- training[training$round == 1 & training$year == 2018, ]
y18$preds <- evaluate_models(
  2011:2017, 2018,
  columns=c("MAS_0", "gini", "MOR_last", "top_100_recruit", "FTr.", "TOV."),
  rounds=1,
  nrounds=50,
  max_leaves=4,
  max_depth=5,
  learning_rate=0.1
)[[1]]

y19 <- training[training$round == 1 & training$year == 2019, ]
y19$preds <- evaluate_models(
  2011:2018, 2019,
  columns=c("MAS_0", "gini", "MOR_last", "top_100_recruit", "FTr.", "TOV."),
  rounds=1,
  nrounds=50,
  max_leaves=4,
  max_depth=5,
  learning_rate=0.1
)[[1]]

y21 <- training[training$round == 1 & training$year == 2021, ]
y21$preds <- evaluate_models(
  2011:2019, 2021,
  columns=c("MAS_0", "gini", "MOR_last", "top_100_recruit", "FTr.", "TOV."),
  rounds=1,
  nrounds=50,
  max_leaves=4,
  max_depth=5,
  learning_rate=0.1
)[[1]]
