# Base URL to basketball-reference college basketball pages
base_url <- "https://www.sports-reference.com/cbb/schools/"

# Function to read in game logs by team and year
scrape_game_logs <- function(team_id, year){
  # Game log URL
  url <- paste0(base_url, team_id, "/", year, "-gamelogs.html")
  # Raw game log table
  game_logs <- rvest::read_html(url) %>%
    rvest::html_nodes(css="table") %>%
    rvest::html_table(fill=TRUE) %>%
    .[[1]] %>%
    as.data.frame()
  # Columns with stats for team of interest
  team_stats <- which(colnames(game_logs) == "School")
  # Columns with stats for opponent
  opp_stats <- which(colnames(game_logs) == "Opponent")
  game_logs[1, team_stats] <- paste0("TM_", game_logs[1, team_stats])
  game_logs[1, opp_stats] <- paste0("O_", game_logs[1, opp_stats])
  # Dropping column of NAs
  game_logs <- game_logs[, apply(game_logs, 2, function(x) all(!is.na(x)))]
  # Fixing column names
  colnames(game_logs) <- game_logs[1, ]
  # Removing rows with repeat column headings
  game_logs <- game_logs[!game_logs$G %in% c("G", ""), ]
  # Fixing numeric variables
  game_logs[, c(1, 6:ncol(game_logs))] <- apply(game_logs[, c(1, 6:ncol(game_logs))], 2, as.numeric)
  # Fixing blank column name
  colnames(game_logs)[colnames(game_logs) == ""] <- "Loc"
  # Adding column with number of overtimes
  game_logs$num_OT <- substr(game_logs$`W/L`, 4, 4) %>%
    as.numeric() %>%
    tidyr::replace_na(replace=0)
  # Keeping W/L at just "W" or "L"
  game_logs$`W/L` <- substr(game_logs$`W/L`, 1, 1)
  # Returns cleaned game log data frame

  # Reading in sites from the game logs
  opp_ids <- rvest::read_html(url) %>%
    rvest::html_nodes(css="table") %>%
    as.character() %>%
    stringr::str_match_all(pattern="data-stat=\"opp_id\">(.*?)>") %>%
    .[[1]] %>%
    .[, 2]
  opp_ids <- sapply(opp_ids, function(x){
    if(x %>% stringr::str_sub(start=1, end=1) == "<"){
      opp_id <- x %>% stringr::str_sub(start=10, end=-11)
    } else{
      opp_id <- x %>% stringr::str_sub(start=1, end=-5)
    }
    return(opp_id)
  })
  # Keeping only the links for opposing team
  game_logs$opp_id <- opp_ids
  colnames(game_logs)[7] <- "O_PTS"
  colnames(game_logs)[colnames(game_logs) == "Tm"] <- "TM_PTS"

  # Estimating the number of possessions in each game
  game_logs$poss <- poss(
    game_logs$TM_FGA,
    game_logs$TM_FTA,
    game_logs$TM_ORB,
    game_logs$O_TRB,
    game_logs$O_ORB,
    game_logs$TM_FG,
    game_logs$TM_TOV,
    game_logs$O_FGA,
    game_logs$O_FTA,
    game_logs$TM_TRB,
    game_logs$O_FG, game_logs$O_TOV
  )

  # eFG%, TS%, ORB%, DRB%, TRB%, AST%, STL%, BLK%, TO%, FTr, 3Pr, 2P%,
  game_logs$TM_eFG <- eFG(game_logs$TM_FG, game_logs$TM_3P, game_logs$TM_FGA)
  game_logs$TM_TS <- TS(game_logs$TM_PTS, game_logs$TM_FGA, game_logs$TM_FTA)
  game_logs$TM_ORB. <- ORB(game_logs$TM_ORB, game_logs$O_TRB - game_logs$O_ORB)
  game_logs$TM_DRB. <- DRB(game_logs$TM_TRB - game_logs$TM_ORB, game_logs$O_ORB)
  game_logs$TM_TRB. <- TRB(game_logs$TM_TRB, game_logs$O_TRB)
  game_logs$TM_AST. <- AST(game_logs$TM_AST, game_logs$TM_FG)
  game_logs$TM_STL. <- STL(game_logs$TM_STL, game_logs$poss)
  game_logs$TM_BLK. <- BLK(game_logs$TM_BLK, game_logs$poss)
  game_logs$TM_TO. <- TO(game_logs$TM_TOV, game_logs$poss)
  game_logs$TM_FTr <- FTr(game_logs$TM_FTA, game_logs$TM_FGA)
  game_logs$TM_3Pr <- TPr(game_logs$TM_3P, game_logs$TM_FGA)
  game_logs$`TM_2P%` <- (game_logs$TM_FG - game_logs$TM_3P) /
    (game_logs$TM_FGA - game_logs$TM_3PA)

  # OeFG%, OTS%, OAST%, OSTL%, OBLK%, OFTr, O3Pr, O2P%
  game_logs$O_eFG <- eFG(game_logs$O_FG, game_logs$O_3P, game_logs$O_FGA)
  game_logs$O_TS <- TS(game_logs$O_PTS, game_logs$O_FGA, game_logs$O_FTA)
  game_logs$O_AST. <- AST(game_logs$O_AST, game_logs$O_FG)
  game_logs$O_STL. <- STL(game_logs$O_STL, game_logs$poss)
  game_logs$O_BLK. <- BLK(game_logs$O_BLK, game_logs$poss)
  game_logs$O_FTr <- FTr(game_logs$O_FTA, game_logs$O_FGA)
  game_logs$O_3Pr <- TPr(game_logs$O_3P, game_logs$O_FGA)
  game_logs$`O_2P%` <- (game_logs$O_FG - game_logs$O_3P) /
    (game_logs$O_FGA - game_logs$O_3PA)

  # Adding team site to table
  game_logs$team_id <- team_id
  # Adding year to table
  game_logs$year <- year

  return(game_logs)
}

