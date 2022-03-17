### 2022 Predictions ###

# 2022 data
y22_last <- fix_this_week("https://masseyratings.com/cb/compare.csv", week=17, 2022, skip=65)
y22_first <- read.csv("y22_preseason.csv")
team_summaries <- all_summaries_by_year(2022)
team_stats <- all_teams_stats(2022)

y22 <- merge(y22_last, y22_first, by="team_id")
y22 <- merge(y22, team_summaries, by="team_id")
y22 <- merge(y22, team_stats, by="team_id")

c("MAS_0", "gini", "MOR_last", "top_100_recruit", "FTr.", "TOV.") %in% colnames(y22)
c("O_TOV.", "DRB.", "PGH_last", "threePr", "DRTG", "F0_min", "RTP_last", "WLK_last") %in% colnames(y22)

# Predict tournament
predict_tournament <- function(teamA, teamB, teamA_seed, teamB_seed, model_obj, data, vars) {
  if("seed_diff" %in% vars) {
    X <- data[data$team_id == teamA, vars[vars != "seed_diff"]] - data[data$team_id == teamB, vars[vars != "seed_diff"]]
    X$seed_diff <- teamA_seed - teamB_seed
  } else {
    X <- data[data$team_id == teamA, vars] - data[data$team_id == teamB, vars]
  }
  prob <- predict(model_obj, as.matrix(X[, vars]))
  return(paste0(teamA, " has a ", round(prob, 3)*100, "% chance to beat ", teamB))
}

#all_data <- read.csv("all_team_data.csv")
#tournaments <- read.csv("all_training.csv")

predict_tournament("virginia", "maryland-baltimore-county", 1, 16, lgb.load("y22_gbm_round1.txt"),
                   data=y22,
                   vars=c("seed_diff", "MAS_0", "gini", "MOR_last", "top_100_recruit", "FTr.", "TOV."))
predict_tournament("cincinnati", "nevada", 1, 16, lgb.load("y22_gbm_round2_6.txt"),
                   data=y22,
                   vars= c("O_TOV.", "DRB.", "PGH_last", "threePr", "DRTG", "F0_min", "RTP_last", "WLK_last"))

y18 <- data.frame(team_a_id=tournaments$home_team_id[tournaments$year == 2018 & tournaments$round < 2],
           team_b_id=tournaments$away_team_id[tournaments$year == 2018 & tournaments$round < 2],
           seed_diff=tournaments$seed_diff[tournaments$year == 2018 & tournaments$round < 2],
           score_diff=tournaments$score_diff[tournaments$year == 2018 & tournaments$round < 2],
           team_a_win=tournaments$home_win[tournaments$year == 2018 & tournaments$round < 2],
           tournaments[tournaments$year == 2018 & tournaments$round < 2, c("MAS_0", "gini", "MOR_last", "top_100_recruit", "FTr.", "TOV.")],
           preds=predict(
             lgb.load("y22_gbm_round1.txt"),
             as.matrix(tournaments[tournaments$year == 2018 & tournaments$round < 2,
                                   c("seed_diff", "MAS_0", "gini", "MOR_last", "top_100_recruit", "FTr.", "TOV.")])))
lgb.interprete(
  lgb.load("y22_gbm_round1.txt"),
  as.matrix(tournaments[tournaments$year == 2018 & training$round < 2,
           c("seed_diff", "MAS_0", "gini", "MOR_last", "top_100_recruit", "FTr.", "TOV.")]),
  idxset=1:36)

c("seed_diff", "MAS_0", "gini", "MOR_last", "top_100_recruit", "FTr.", "TOV.")
c("O_eFG._l", "O_BLK._u", "BLK.", "SEL_last", "threePr_l", "top_100_recruit", "MAS_last", "G1_pts")


predict_tournament("gonzaga", "purdue", 1, 1, lgb.load("y22_gbm_round2_6.txt"))
