# Base URL to basketball-reference college basketball pages
base_url <- "https://www.sports-reference.com/cbb/schools/"

#' Scrape Team IDs
#'
#' @return Vector of all Team IDs from the teams table
#' @export
#'
#' @examples
scrape_team_ids <- function() {
  return(
    rvest::read_html(base_url) %>%
      rvest::html_nodes(css="table") %>%
      as.character() %>%
      stringr::str_match_all(pattern="href=\"(.*?)\">") %>%
      .[[1]] %>%
      .[, 2] %>%
      stringr::str_sub(start=14, end=-2)
  )
}


#' Scrape Teams Table from basketball-reference
#'
#' @return Data frame with all Division I NCAA men's basketball teams, their
#' years in Division I, and the team IDs of each team
#' @export
#'
#' @examples
scrape_teams_table <- function() {
  # Scraping table with all teams
  teams_table <- rvest::read_html(base_url) %>%
    rvest::html_nodes(css="table") %>%
    rvest::html_table(fill=TRUE) %>%
    .[[1]]
  # Removing column headers that repeat in the table
  teams_table <- teams_table[!teams_table$Rk == "Rk", ]
  # Fixing column class types
  teams_table[, 4:18] <- apply(teams_table[, 4:18], 2, as.numeric)
  # Adding team websites to table
  teams_table$team_id <- scrape_team_ids()
  return(teams_table)
}


# Function to calculate numeric height from string version
calculate_height <- function(height) {
  inches <- as.numeric(substring(height, 1, 1)) * 12 +
    as.numeric(substring(height, 3, nchar(height)))
  return(inches)
}


#' Scrape Various Statistics and Summaries by Team and Year
#'
#' @param team_id Team ID for team of interest from teams table
#' @param year Year to pull data from
#'
#' @return Data Frame with a variety of statistics and summaries by team and year
#' @export
#'
#' @examples
school_summary_by_year <- function(team_id, year) {
  # URL
  url <- paste0(base_url, team_id, "/", year, ".html")
  # Extracting the html in text form
  text_html <- rvest::read_html(url) %>%
    rvest::html_nodes("div") %>%
    rvest::html_text()
  # Extracting minute continuity
  min_continuity <- paste(text_html, collapse="") %>%
    stringr::str_match(pattern="played(.*?)% of m") %>%
    .[, 2] %>%
    stringr::str_sub(start=-4) %>%
    as.numeric()
  # Extracting scoring continuity
  pts_continuity <- paste(text_html, collapse="") %>%
    stringr::str_match(pattern="played(.*?)% of s") %>%
    .[, 2] %>%
    stringr::str_sub(start=-4) %>%
    as.numeric()
  # Reading in the team summary info
  team_summary <- rvest::read_html(url) %>%
    rvest::html_nodes("p") %>%
    as.character() %>%
    paste(collapse="")
  # Conference
  conf <- stringr::str_match(team_summary, pattern="conferences/(.*?)/")[, 2]
  # Head coach
  coach <- stringr::str_match(team_summary, pattern="coaches/(.*?).html")[, 2]

  # Scraping team roster
  roster <- rvest::read_html(url) %>%
    rvest::html_nodes("#all_roster") %>%
    rvest::html_table() %>%
    .[[1]]
  # Number of top 100 recruits
  top_100_recruit <- sum(!(is.na(roster$`RSCI Top 100`) | roster$`RSCI Top 100` == ""))
  # Player heights numerically
  roster$Height <- calculate_height(roster$Height)
  # Standardizing position by height
  roster$Pos <- cut(
      roster$Height,
      breaks=c(0, 77, 81, 100),
      right=FALSE,
      labels=c("G", "F", "C")
    )

  # Scraping player totals for input team for conference games
  player_totals <- rvest::read_html(url) %>%
    rvest::html_nodes("#all_totals_players") %>%
    rvest::html_table() %>%
    .[[1]]

  # Keeping the rows for all games rather than just conference games
  if(length(which(player_totals$Rk == "")) != 0) {
    player_totals <- player_totals[1:(which(player_totals$Rk == "") - 1), ]
  }
  # Making numeric columns numeric
  player_totals[, -2] <- apply(player_totals[, -2], 2, as.numeric)

  # Calculating scoring Gini index
  gini <- DescTools::Gini(player_totals$PTS, unbiased=FALSE)

  # Merging class and position to the player totals
  player_totals <- merge(
      roster[, c("Player", "Class", "Pos", "Height")],
      player_totals,
      by="Player"
    )
  # Making an upper/lower classmen classification
  player_totals$upperclass <- ifelse(player_totals$Class %in% c("JR", "SR"), 1, 0)
  # Making combination of class and position
  player_totals$class_pos <- paste0(player_totals$Pos, player_totals$upperclass)
  # Average height
  avg_height <- weighted.mean(
      player_totals$Height[!is.na(player_totals$MP)],
      w=player_totals$MP[!is.na(player_totals$MP)],
      na.rm=TRUE
    )
  # Converting class to numeric
  player_totals$class_num <- player_totals$Class %>%
    factor(levels=c("FR", "SO", "JR", "SR")) %>%
    as.numeric() - 1
  # Average experience
  avg_exp <- weighted.mean(
    player_totals$class_num[!is.na(player_totals$MP)],
    w=player_totals$MP[!is.na(player_totals$MP)],
    na.rm=TRUE
    )

  # Total minutes and points
  total_min <- sum(player_totals$MP, na.rm=TRUE)
  total_pts <- sum(player_totals$PTS, na.rm=TRUE)
  # Minutes breakdown by position and class
  min_class_pos <- sapply(c("G0", "G1", "F0", "F1", "C0", "C1"), function(x)
    sum(player_totals$MP[player_totals$class_pos == x], na.rm=TRUE) / total_min)
  pts_class_pos <- sapply(c("G0", "G1", "F0", "F1", "C0", "C1"), function(x)
    sum(player_totals$PTS[player_totals$class_pos == x], na.rm=TRUE) / total_pts)
  # Returning the team, year, average height, average years of experience, percent of minutes back
  ### from previous year, percent of points from previous year returning, number of top 100 recruits,
  ### gini index for scoring, conference, coach, and proportion of points and minutes from each
  ### combination of class and position
  return(
    data.frame(
      team_id,
      year,
      avg_height,
      avg_exp,
      min_continuity,
      pts_continuity,
      top_100_recruit,
      gini,
      conf,
      coach,
      G0_min=min_class_pos[1],
      G1_min=min_class_pos[2],
      F0_min=min_class_pos[3],
      F1_min=min_class_pos[4],
      C0_min=min_class_pos[5],
      C1_min=min_class_pos[6],
      G0_pts=pts_class_pos[1],
      G1_pts=pts_class_pos[2],
      F0_pts=pts_class_pos[3],
      F1_pts=pts_class_pos[4],
      C0_pts=pts_class_pos[5],
      C1_pts=pts_class_pos[6]
      )
    )
}


#' Scrape All School Statistics and Summaries for Given Year
#'
#' @param year Year for which to scrape all school statistics and summaries for
#' schools in Division I
#'
#' @return Data Frame with all school statistics and summaries for given year
#' @export
#'
#' @examples
all_summaries_by_year <- function(year) {
  teams_table <- scrape_teams_table()
  # Keeping only teams that were D1 the year of interest
  team_ids <- teams_table$team_id[teams_table$From <= year & teams_table$To >= year]
  # Scraping some features for all teams in the given year
  all_teams <- lapply(team_ids, function(x)
    tryCatch(school_summary_by_year(x, year), error=function(e) NULL))
  # Collapsing list down to a dataframe
  return(do.call("rbind", all_teams))
}
