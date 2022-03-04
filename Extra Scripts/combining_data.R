
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
tournaments$row <- 1:nrow(tournaments)
tourney_cols <- c("row", "home_team_id", "away_team_id", "year", "round", "seed_diff", "score_diff", "home_win")
home_team_stats <- merge(tournaments[, tourney_cols], all_data, by.x=c("home_team_id", "year"), by.y=c("team_id", "year"))
away_team_stats <- merge(tournaments[, tourney_cols], all_data, by.x=c("away_team_id", "year"), by.y=c("team_id", "year"))
home_team_stats <- home_team_stats[order(home_team_stats$row), ]
away_team_stats <- away_team_stats[order(away_team_stats$row), ]
all_training <- home_team_stats
all_training[, 9:ncol(all_training)] <- home_team_stats[, 9:ncol(all_training)] - away_team_stats[, 9:ncol(all_training)]

write.csv(all_training, "all_training.csv", row.names=FALSE)
