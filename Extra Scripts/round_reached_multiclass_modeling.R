library(stringr)
library(lightgbm)

all_years <- read.csv("all_team_data.csv")
tournaments <- do.call("rbind", lapply(c(2011:2019, 2021), scrape_tournament))

team_years <- unique(
  c(paste0(tournaments$home_team_id, tournaments$year),
    paste0(tournaments$away_team_id, tournaments$year)
  )
)

teams_reached <- data.frame(
  team_years,
  reached=sapply(team_years, function(x)
    max(tournaments$round[
      paste0(tournaments$home_team_id, tournaments$year) == x |
        paste0(tournaments$away_team_id, tournaments$year) == x
      ]
      )
    )
)

teams_reached$reached[teams_reached$reached == 6] <- sapply(
  teams_reached$team_years[teams_reached$reached == 6],
  function(x) {
    team_id <- stringr::str_sub(x, 1, -5)
    game_data <- tournaments[
    (paste0(tournaments$home_team_id, tournaments$year) == x |
      paste0(tournaments$away_team_id, tournaments$year) == x) &
      tournaments$round == 6,
    ]
    if(team_id == game_data$home_team_id) {
      return(ifelse(game_data$home_win == 1, 7, 6))
    } else {
      return(ifelse(game_data$home_win == 1, 6, 7))
    }
  }
)

teams_reached$reached[teams_reached$reached == 0] <- 1

all_years$team_years <- paste0(all_years$team_id, all_years$year)
all_years <- merge(all_years, teams_reached, all.x=TRUE)
all_years$reached[is.na(all_years$reached)] <- 0

train <- all_years[all_years$year != 2019, ]
test <- all_years[all_years$year == 2019, ]

params <- list(
  objective="multiclass"
  , metric="multi_error"
  , num_class=8
  , min_data=1
  , learning_rate=0.1
)

params <- list(
  objective="regression"
  , metric="mse"
  , learning_rate=0.1
)

gbm_mdl <- lightgbm(
  data=as.matrix(train[, 3:(ncol(train) - 1)]),
  params=params,
  label=train$reached,
  nrounds=30,
  verbose=0
)

woof <- predict(gbm_mdl, as.matrix(test[, 3:(ncol(test) - 1)]), reshape=TRUE)
woof <- round(woof, 3)
woof <-  t(apply(woof, 1, function(x) sapply(1:8, function(y) sum(x[y:8]))))

colnames(woof) <- 0:7
woof <- cbind(test[, c("team_id", "reached")], woof)

woof <- cbind(test[, c("team_id", "reached")], predict(gbm_mdl, as.matrix(test[, 3:(ncol(test) - 1)])))
