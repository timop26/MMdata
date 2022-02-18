# Creating mapping of team names to team IDs
y22 <- read.csv("../../../Downloads/compare.csv")
y22$Team <- trimws(y22$Team)
team_names_lower <- stringr::str_to_lower(y22$Team) %>%
  stringr::str_replace_all(pattern=" ", "-")
name_mappings <- data.frame(
  team_names=y22$Team[team_names_lower %in% scrape_team_ids()],
  team_ids=team_names_lower[team_names_lower %in% scrape_team_ids()]
)
write.csv(name_mappings[order(name_mappings$team_names), ], "names_mappings.csv", row.names=FALSE)
