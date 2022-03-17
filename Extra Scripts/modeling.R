library(pROC)
library(lightgbm)
library(caret)

# Reading in all training data
training <- read.csv("all_training_double.csv")
sort(cor(training[, 5:ncol(training)])[, "score_diff"])
sort(cor(training[, 5:ncol(training)])[, "team_a_win"])

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
    label=train$team_a_win,
    nrounds=nrounds,
    verbose=0
  )

  # Feature importance
  #lgb.plot.importance(lgb.importance(gbm_mdl), measure="Gain")

  # Predicting on testing data
  preds <- predict(gbm_mdl, as.matrix(test[, columns]))
  return(
    list(
      predictions=preds,
      data.frame(
        accuracy=mean(round(preds) == test$team_a_win),
        AUC=roc(test$team_a_win, preds, quiet=TRUE)$auc,
        sensitivity=sensitivity(as.factor(round(preds)), factor(test$team_a_win, levels=c(1, 0))),
        specificity=specificity(as.factor(round(preds)), factor(test$team_a_win, levels=c(1, 0)))
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

# Round 1 model
cv(c(2, 5, 6, 9:ncol(training)), 1, 50, 4, 5, 0.1)
cv(sample(c(2, 5, 6, 9:ncol(training)), 10), 1, 50, 4, 5, 0.1)
apply(cv(c("seed_diff", "MAS_0", "gini", "MOR_last", "top_100_recruit", "FTr.", "TOV."), 0:1, 50, 4, 4, 0.2), 2, mean)
aggregate(team_a_win ~ year, data=training[training$round < 3 & training$year > 2013, ], mean)

possible_features <- colnames(y22)[c(2:30, 35, 37:42, 45:134)][
  colnames(y22)[c(2:30, 35, 37:42, 45:134)] %in% colnames(training) &
    !colnames(y22)[c(2:30, 35, 37:42, 45:134)] %>% endsWith("_l") &
    !colnames(y22)[c(2:30, 35, 37:42, 45:134)] %>% endsWith("_u")]
# Round 2-6 model
start <- Sys.time()
results <- matrix(nrow=81 * 80, ncol=9)
i <- 1
for (feature_combos in 1:80) {
  features <- sample(possible_features, sample(5:12, 1))
  for (nround in c(100, 200, 300)) {
    for (max_leaves in 2:4) {
      for (max_depth in 3:5) {
        for (learning_rate in c(0.1, 0.15, 0.2)) {
          r <- apply(cv(features, 2:6, nround, max_leaves, max_depth, learning_rate), 2, mean)
          print(r)
          results[i, 1:4] <- r
          results[i, 5] <- paste(features, collapse=", ")
          results[i, 6] <- nround
          results[i, 7] <- max_leaves
          results[i, 8] <- max_depth
          results[i, 9] <- learning_rate
          i <- i + 1
        }
      }
    }
  }
}
end <- Sys.time()
end - start

colnames(training)[c(34, 76, 40, 115, 55, 16, 43)]
colnames(training)[c(10, 61, 71, 66, 109, 92, 2, 11)]
cv(c(34, 76, 40, 115, 55, 70, 16, 43), 2:6, 100, 2, 3, 0.1)
cv(c("O_TOV.", "DRB.", "PGH_last", "threePr", "DRTG", "F0_min", "RTP_last", "WLK_last", "MAS_0"), 2:6, 100, 2, 3, 0.1)
apply(cv(c("O_TOV.", "DRB.", "PGH_last", "threePr", "DRTG", "F0_min", "RTP_last", "WLK_last"), 2:6, 100, 2, 3, 0.1), 2, mean)
apply(cv(c("PGH_last", "F0_pts", "O_TPr.", "MAS_0", "POM_last", "F0_min"), 2:6, 300, 2, 3, 0.1), 2, mean)

aggregate(team_a_win ~ year, data=training[training$round > 1 & training$year > 2013, ], mean)


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

y18 <- training[training$round < 2 & training$year == 2018, ]
y18$preds <- evaluate_models(
  2011:2017, 2018,
  columns=c("seed_diff", "MAS_0", "gini", "MOR_last", "top_100_recruit", "FTr.", "TOV."),
  rounds=0:1,
  nrounds=50,
  max_leaves=4,
  max_depth=5,
  learning_rate=0.2
)[[1]]

y19 <- training[training$round < 2 & training$year == 2019, ]
y19$preds <- evaluate_models(
  2011:2018, 2019,
  columns=c("seed_diff", "MAS_0", "gini", "MOR_last", "top_100_recruit", "FTr.", "TOV."),
  rounds=0:1,
  nrounds=50,
  max_leaves=4,
  max_depth=5,
  learning_rate=0.2
)[[1]]
y19[, c("team_a_id", "team_b_id", "team_a_win", "preds")]

y21 <- training[training$round < 2 & training$year == 2021, ]
y21$preds <- evaluate_models(
  2011:2019, 2021,
  columns=c("MAS_0", "gini", "MOR_last", "top_100_recruit", "FTr.", "TOV."),
  rounds=0:1,
  nrounds=50,
  max_leaves=4,
  max_depth=5,
  learning_rate=0.2
)[[1]]

## Final Models ##
# Parameters for lightgbm
param_list <- list(
  max_leaves=4,
  max_depth=4,
  learning_rate=0.2,
  objective="binary",
  is_unbalance=TRUE
)

# Training lightgbm model
gbm_mdl <- lightgbm(
  data=as.matrix(training[training$round < 2, c("seed_diff", "MAS_0", "gini", "MOR_last", "top_100_recruit", "FTr.", "TOV.")]),
  params=param_list,
  label=training$team_a_win[training$round < 2],
  nrounds=50,
  verbose=0
)

cv(c("seed_diff", "MAS_0", "gini", "MOR_last", "top_100_recruit", "FTr.", "TOV."), 0:1, 50, 4, 4, 0.2)
lgb.plot.importance(lgb.importance(gbm_mdl), measure="Gain")
lgb.save(gbm_mdl, "y22_gbm_round1.txt")


param_list <- list(
  max_leaves=2,
  max_depth=3,
  learning_rate=0.1,
  objective="binary",
  is_unbalance=TRUE
)

# Training lightgbm model
gbm_mdl <- lightgbm(
  data=as.matrix(training[training$round > 1, c("O_TOV.", "DRB.", "PGH_last", "threePr", "DRTG", "F0_min", "RTP_last", "WLK_last")]),
  params=param_list,
  label=training$team_a_win[training$round > 1],
  nrounds=100,
  verbose=0
)

cv(c("O_TOV.", "DRB.", "PGH_last", "threePr", "DRTG", "F0_min", "RTP_last", "WLK_last"), 2:6, 100, 2, 3, 0.1)
lgb.plot.importance(lgb.importance(gbm_mdl), measure="Gain")
lgb.save(gbm_mdl, "y22_gbm_round2_6.txt")
