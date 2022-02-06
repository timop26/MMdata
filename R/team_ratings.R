
name_mappings <- read.csv("name_mappings.csv")

reshape_by_week <- function(long_data, week, year) {
  week_data <- long_data[long_data$Week == week, c("Team", "System", "Rank")]
  reshaped_data <- reshape(week_data, idvar="Team", timevar="System", direction="wide")
  reshaped_data$Week <- week
  reshaped_data$year <- year
  colnames(reshaped_data) <- stringr::str_remove(colnames(reshaped_data), "Rank.")
  reshaped_data <- merge(reshaped_data, name_mappings, by.x="Team", by.y="team_names")
  return(reshaped_data)
}

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
  return(lapply(weeks, function(x) reshape_by_week(year_data, x, year)))
}

y11 <- transform_archived_data(2011, c(0, 18))

