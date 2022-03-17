
years <- c(2011:2019, 2021)
combine_data <- function(years) {
  # Preseasons rankings
  pre_rankings <- lapply(years, function(x) transform_archived_data(year=x, 0)[[1]])
  # Columns that are in each year
  pre_cols <- Reduce(intersect, lapply(pre_rankings, colnames))
  # Combining preseason ranking data
  pre_rankings <- do.call("rbind", lapply(pre_rankings, function(x) x[, pre_cols]))
  # Removing "Week" column
  pre_rankings <- pre_rankings[, which(colnames(pre_rankings) != "Week")]

  # Selection Sunday weeks
  selection_sunday_weeks <- data.frame(
    selection_sunday=c(18, 18, 19, 19, 18, 18, 18, 18, 19, 18, 16),
    year=c(2011:2021)
  )

  # Selection Sunday rankings
  final_rankings <- lapply(
    years,
    function(x) {
      transform_archived_data(
        year=x,
        selection_sunday_weeks$selection_sunday[selection_sunday_weeks$year == x]
      )[[1]]
    }
  )
  # Columns that are in each year
  final_cols <- Reduce(intersect, lapply(final_rankings, colnames))
  # Combining selection Sunday ranking data
  final_rankings <- do.call("rbind", lapply(final_rankings, function(x) x[, final_cols]))
  # Removing "Week" column
  final_rankings <- final_rankings[, which(colnames(final_rankings) != "Week")]

  # Team Summaries
  team_summaries <- lapply(years, all_summaries_by_year) %>% do.call(what="rbind")

  # Team Stats
  team_stats <- lapply(years, all_teams_stats) %>% do.call(what="rbind")

  # Merging all data together
  all_data <- merge(team_stats, team_summaries, by=c("team_id", "year"))
  all_data <- merge(all_data, pre_rankings, by=c("team_id", "year"))
  all_data <- merge(all_data, final_rankings, by=c("team_id", "year"))

  # Removing any columns with missing data
  all_data <- all_data[, apply(all_data, 2, function(x) mean(is.na(x)) == 0)]
  all_data <- all_data[, !colnames(all_data) %in% c("conf", "coach")]
  return(all_data)
}

system.time(all_data <- combine_data(c(2011:2019, 2021)))

# Reading in tournament data
tournaments <- lapply(c(2011:2019, 2021), scrape_tournament) %>% do.call(what="rbind")
colnames(tournaments) <- str_replace(colnames(tournaments), "home_team", "team_a")
colnames(tournaments) <- str_replace(colnames(tournaments), "away_team", "team_b")
colnames(tournaments) <- str_replace(colnames(tournaments), "home", "team_a")
colnames(tournaments) <- str_replace(colnames(tournaments), "away", "team_b")

tournament_b <- tournaments
tournament_b$team_a_id <- tournaments$team_b_id
tournament_b$team_b_id <- tournaments$team_a_id
tournament_b$team_a_seed <- tournaments$team_b_seed
tournament_b$team_b_seed <- tournaments$team_a_seed
tournament_b$team_a_score <- tournaments$team_b_score
tournament_b$team_b_score <- tournaments$team_a_score
tournament_b$team_a_win <- 1 - tournaments$team_a_win
tournament_b$score_diff <- tournaments$score_diff * -1
tournament_b$seed_diff <- tournament_b$team_a_seed - tournament_b$team_b_seed

tournaments <- rbind(tournaments, tournament_b)

tournaments$row <- 1:nrow(tournaments)
tourney_cols <- c("row", "team_a_id", "team_b_id", "year", "round", "seed_diff", "score_diff", "team_a_win")
team_a_stats <- merge(tournaments[, tourney_cols], all_data, by.x=c("team_a_id", "year"), by.y=c("team_id", "year"))
team_b_stats <- merge(tournaments[, tourney_cols], all_data, by.x=c("team_b_id", "year"), by.y=c("team_id", "year"))
team_a_stats <- team_a_stats[order(team_a_stats$row), ]
team_b_stats <- team_b_stats[order(team_b_stats$row), ]
all_training <- team_a_stats
all_training[, 9:ncol(all_training)] <- team_a_stats[, 9:ncol(all_training)] - team_b_stats[, 9:ncol(all_training)]

write.csv(all_training, "all_training_double.csv", row.names=FALSE)
