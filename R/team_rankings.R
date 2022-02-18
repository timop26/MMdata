# File with mappings from team names to team IDs
name_mappings <- read.csv("name_mappings.csv")

# Function to reshape rankings data
reshape_by_week <- function(long_data, week, year) {
  week_data <- long_data[long_data$Week == week, c("Team", "System", "Rank")]
  reshaped_data <- reshape(week_data, idvar="Team", timevar="System", direction="wide")
  reshaped_data$Week <- week
  reshaped_data$year <- year
  colnames(reshaped_data) <- stringr::str_remove(colnames(reshaped_data), "Rank.")
  reshaped_data <- merge(
    reshaped_data,
    name_mappings,
    by.x="Team",
    by.y="team_names",
    all.x=TRUE
  )
  return(reshaped_data)
}


fix_colnames <- function(data, last=FALSE) {
  data <- data[, -1]
  colnames_to_change <- colnames(data)[!colnames(data) %in% c("Week", "year", "team_ids")]
  if(last) {
    new_names <- paste0(colnames_to_change, "_last")
  } else {
    new_names <- paste(colnames_to_change, sep="_", unique(data$Week))
  }
  colnames(data)[!colnames(data) %in% c("Week", "year", "team_ids")] <- new_names
  return(data)
}

#' Get Team Rankings
#'
#' @param year Year for which to get team rankings
#' @param weeks Week of the season for which to get team rankings
#'
#' @return Data frame with team rankings for available systems
#' @export
#'
#' @examples
transform_archived_data <- function(year, weeks) {
  filepath <- paste0("../../Massey CBB Data/cb", year, ".csv")
  year_data <- read.csv(filepath, header=FALSE)
  year_data <- year_data[, c(3:4, 6:7)]
  colnames(year_data) <- c("Team", "System", "Date", "Rank")
  # Trimming whitespace from character columns
  year_data$Team <- trimws(year_data$Team)
  year_data$System <- trimws(year_data$System)
  # Converting dates into real dates
  year_data$Date <- paste(
    stringr::str_sub(year_data$Date, 1, 4),
    stringr::str_sub(year_data$Date, 5, 6),
    stringr::str_sub(year_data$Date, 7, 8),
    sep="-"
  )
  year_data$Week <- year_data$Date %>% as.factor() %>% as.numeric() - 1
  year_data$System <- as.factor(year_data$System)
  data_list <- lapply(weeks, function(x) reshape_by_week(year_data, x, year))
  data_list <- lapply(1:length(weeks), function(x)
    fix_colnames(data_list[[x]], weeks[x] == (max(year_data$Week) - 1)))
  return(data_list)
}

#' Add Team IDs to Week-Specific Rankings
#'
#' @param filepath File path to the week specific rankings
#' @param week Week of the rankings
#' @param year Year of the rankings
#'
#' @return Data frame with transformed
#' @export
#'
#' @examples
fix_this_week <- function(filepath, week, year) {
  this_week <- read.csv(filepath)
  this_week$Team <- trimws(this_week$Team)
  this_week <- this_week[, -c(2:8)]
  this_week$Week <- week
  this_week$year <- year
  this_week <- merge(
    this_week,
    name_mappings,
    by.x="Team",
    by.y="team_names",
    all.x=TRUE
  )
  return(this_week)
}
