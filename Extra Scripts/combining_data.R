
years <- c(2011:2019, 2021)
## Rankings
pre_rankings <- lapply(years, function(x) transform_archived_data(year=x, 0)[[1]])
# Columns that are in each year
pre_cols <- Reduce(intersect, lapply(pre_rankings, colnames))
pre_rankings <- do.call("rbind", lapply(pre_rankings, function(x) x[, pre_cols]))
pre_rankings <- pre_rankings[, which(colnames(final_rankings) != "Week")]

selection_sunday <- c(18, 18, 19, 19, 18, 18, 18, 18, 19, 16)
final_rankings <- lapply(1:10, function(x)
  transform_archived_data(year=years[x], selection_sunday[x])[[1]])
# Columns that are in each year
final_cols <- Reduce(intersect, lapply(final_rankings, colnames))
final_rankings <- do.call("rbind", lapply(final_rankings, function(x) x[, final_cols]))
final_rankings <- final_rankings[, which(colnames(final_rankings) != "Week")]

pre_id_year <- paste0(pre_rankings$team_id, pre_rankings$year)
final_id_year <- paste0(final_rankings$team_id, final_rankings$year)

## Team Summaries
team_summaries <- lapply(years, all_summaries_by_year) %>% do.call(what="rbind")
#
# ## Team Stats
team_stats <- lapply(years, all_teams_stats) %>% do.call(what="rbind")

all_data <- merge(team_stats, team_summaries, by=c("team_id", "year"))
all_data <- merge(all_data, pre_rankings, by=c("team_id", "year"))
all_data <- merge(all_data, final_rankings, by=c("team_id", "year"))

all_data <- all_data[, apply(all_data, 2, function(x) mean(is.na(x)) == 0)]
all_data <- all_data[, colnames(all_data) != "conf"]
# Reading in tournament data
tournaments <- lapply(years, scrape_tournament) %>% do.call(what="rbind")
tournaments$row <- 1:nrow(tournaments)
tourney_cols <- c("row", "home_team_id", "away_team_id", "year", "round", "seed_diff", "score_diff", "home_win")
home_team_stats <- merge(tournaments[, tourney_cols], all_data, by.x=c("home_team_id", "year"), by.y=c("team_id", "year"))
away_team_stats <- merge(tournaments[, tourney_cols], all_data, by.x=c("away_team_id", "year"), by.y=c("team_id", "year"))
home_team_stats <- home_team_stats[order(home_team_stats$row), ]
away_team_stats <- away_team_stats[order(away_team_stats$row), ]
all_training <- home_team_stats
all_training[, 8:ncol(all_training)] <- home_team_stats[, 8:ncol(all_training)] - away_team_stats[, 8:ncol(all_training)]

write.csv(all_training, "all_training.csv")
