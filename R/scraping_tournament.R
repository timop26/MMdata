
#' Create lookup table between team names and IDs for a given year
#'
#' @param year Year for which to create lookup table between team names and IDs
#'
#' @return Data frame with team names and corresponding team IDs
#' @export
#'
#' @examples
team_name_id_lookup <- function(year) {
  # URL for NCAA postseason of interest
  url <- paste0("https://www.sports-reference.com/cbb/postseason/", year, "-ncaa.html")
  # Raw html code as text
  html_raw <- readLines(url) %>% paste0(collapse = "&")
  # Pattern to find play-in team IDs and team names
  play_in_match_pattern <- "href='/cbb/schools/(.*?)/'>(.*?)</a>"
  # Creating play-in team lookup table
  play_in_lookup <- stringr::str_match_all(html_raw, pattern=play_in_match_pattern) %>%
    .[[1]] %>%
    .[1:8, 2:3] %>%
    as.data.frame()

  # Fixing error with ampersand
  play_in_lookup[, 2] <- play_in_lookup[, 2] %>%
    stringr::str_replace("&amp;", "&")

  # Pattern to find round of 64 team IDs and names
  f64_match_pattern <- "href=\"/cbb/schools/(.*?)/(.*?).html\">(.*?)</a>"
  # Creating round of 64 lookup table
  team_id_lookup <- stringr::str_match_all(html_raw, pattern=f64_match_pattern) %>%
    .[[1]] %>%
    .[-1, c(2, 4)] %>%
    as.data.frame()

  # Combining tables
  team_id_lookup <- rbind(play_in_lookup, team_id_lookup) %>%
    unique()
  colnames(team_id_lookup) <- c("team_id", "team_name")

  return(team_id_lookup)
}

# Base URL to scrape NCAA tournament results
base_url <- "https://basketball.realgm.com/ncaa/tournaments/Post-Season/NCAA-Tournament/1/bracket/"

#' Scraping NCAA tournament results for given year
#'
#' @param year Year for which to scrape NCAA tournament results
#'
#' @return Data frame with NCAA tournament results
#' @export
#'
#' @examples
scrape_tournament <- function(year) {
  # Data frame relating years to code
  year_code <- data.frame(
    year=c(2011:2019, 2021:2022),
    code=c(433, 489, 546, 610, 677, 708, 803, 872, 936, 1029, 1128)
  )
  # Full URL
  url <- paste0(base_url, year, "/", year_code$code[year_code$year == year])
  tournament_data <- url %>%
    rvest::read_html() %>%
    rvest::html_nodes("table") %>%
    rvest::html_table(fill=TRUE) %>%
    .[[173]]
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

  # Creating lookup table for team names and team IDs
  lookup_table <- team_name_id_lookup(year)
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
