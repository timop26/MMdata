# Base URL for preseason odds/rankings
base_url <- "https://www.sports-reference.com/cbb/seasons/"

#' Scrape preseason odds
#'
#' @param year Year for which to scrape preseason odds
#'
#' @return Data frame with all team's preseason odds for given year
#' @export
#'
#' @examples
scrape_preseason_odds <- function(year) {
  # Scraping raw preseason odds table
  preseason_odds <- rvest::read_html(paste0(base_url, year, "-preseason_odds.html")) %>%
    rvest::html_nodes(css="table") %>%
    rvest::html_table(fill=TRUE) %>% .[[1]]
  team_ids <- rvest::read_html(paste0(base_url, year, "-preseason_odds.html")) %>%
    rvest::html_nodes(css="table") %>%
    as.character() %>%
    stringr::str_match_all(pattern="href=\"(.*?)\">") %>%
    .[[1]] %>%
    .[, 2]
  preseason_odds$team_id <- substr(team_ids, 1, nchar(team_ids) - 9)
  preseason_odds$year <- year
  return(as.data.frame(preseason_odds[, c("team_id", "year", "Odds")]))
}


#' Scraping preseason top 25 rankings
#'
#' @param year Year for which to scrape preseason top 25 rankings
#' @param table Table number to scrape. Should be 1 for all but the current season
#'
#' @return Data frame with top 25 teams
#' @export
#'
#' @examples
scrape_preseason_ranks <- function(year, table=1){
  # Scraping raw preseason ranking table
  preseason_ranks <- rvest::read_html(paste0(base_url, year, "-polls.html")) %>%
    rvest::html_nodes(css="table") %>%
    rvest::html_table(fill=TRUE) %>%
    .[[table]]
  # Fixing column names
  colnames(preseason_ranks) <- preseason_ranks[2, ]
  # Removing extra header rows
  preseason_ranks <- preseason_ranks[!preseason_ranks$School %in% c("", "School"), ]
  # Scraping the websites for teams
  team_ids <- rvest::read_html(paste0(base_url, year, "-polls.html")) %>%
    rvest::html_nodes(css="table") %>%
    as.character() %>%
    stringr::str_match_all(pattern="href=\"(.*?)\">") %>%
    .[[table]] %>%
    .[, 2]
  # Adding the team_id to the table
  preseason_ranks$team_id <- substr(team_ids, 1, nchar(team_ids) - 9)[seq(1, length(team_ids), by=2)]
  # Dropping unnecessary rows
  preseason_ranks <- preseason_ranks[preseason_ranks$Pre != "", ]
  # Fixing data type for rankings
  preseason_ranks$Pre <- preseason_ranks$Pre %>%
    as.numeric()
  preseason_ranks$year <- year
  # Returning data frame with preseason top 25
  return(as.data.frame(preseason_ranks[, c("team_id", "year", "Pre")]))
}



