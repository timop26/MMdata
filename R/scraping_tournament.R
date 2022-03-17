# Lookup table for team names
lookup_table <- read.csv("tourney_name_mappings.csv")

# Base URL to scrape NCAA tournament results
base_url_tourney <- "https://basketball.realgm.com/ncaa/tournaments/Post-Season/NCAA-Tournament/1/bracket/"

#' Scraping NCAA tournament results for given year
#'
#' @param year Year for which to scrape NCAA tournament results
#'
#' @return Data frame with NCAA tournament results
#' @export
#'
#' @examples
scrape_tournament <- function(year, table_index=173) {
  # Data frame relating years to code
  year_code <- data.frame(
    year=c(2011:2019, 2021:2022),
    code=c(433, 489, 546, 610, 677, 708, 803, 872, 936, 1029, 1128)
  )
  # Full URL
  url <- paste0(base_url_tourney, year, "/", year_code$code[year_code$year == year])
  tournament_data <- url %>%
    rvest::read_html() %>%
    rvest::html_nodes("table") %>%
    rvest::html_table(fill=TRUE) %>%
    .[[table_index]]
  tournament_data$home_seed <- readr::parse_number(tournament_data$`Home Team`)
  # Extracting away seed
  tournament_data$away_seed <- readr::parse_number(tournament_data$`Away Team`)
  # Seed difference
  tournament_data$seed_diff <- tournament_data$home_seed - tournament_data$away_seed
  # Extracting home team name
  tournament_data$home_team <- tournament_data$`Home Team` %>%
    stringr::str_match(pattern=" (.*?) \\(") %>% .[, 2]
  # Extracting road team name
  tournament_data$away_team <- tournament_data$`Away Team` %>%
    stringr::str_match(pattern=" (.*?) \\(") %>% .[, 2]
  # Extracting home team score
  tournament_data[, c("away_score", "home_score")] <- tournament_data$Result %>%
    stringr::str_split("-") %>%
    lapply(as.numeric) %>%
    do.call(what="rbind")
  # Score differential for home team
  tournament_data$score_diff <- tournament_data$home_score - tournament_data$away_score
  # Indicator for whether or not home team won
  tournament_data$home_win <- (tournament_data$score_diff > 0) * 1
  # Adding round
  tournament_data$round <- c(rep(0, 4), rep(1, 32), rep(2, 16), rep(3, 8), rep(4, 4), 5, 5, 6)
  # Adding year
  tournament_data$year <- year

  # Merging tournament table and lookup table to add the team ID to table
  tournament_data <- merge(tournament_data, lookup_table, by.x="home_team", by.y="team_name")
  colnames(tournament_data)[colnames(tournament_data) == "team_id"] <- "home_team_id"
  tournament_data <- merge(tournament_data, lookup_table, by.x="away_team", by.y="team_name")
  colnames(tournament_data)[colnames(tournament_data) == "team_id"] <- "away_team_id"

  tournament_data <- tournament_data[order(tournament_data$round), ]
  # Columns to keep
  col_keep <- c("Date", "year", "round", "home_team_id", "away_team_id",
                "home_seed", "away_seed", "seed_diff", "home_score",
                "away_score", "home_win", "score_diff", "Venue")
  return(tournament_data[!is.na(tournament_data$home_team), col_keep] %>% as.data.frame())
}

