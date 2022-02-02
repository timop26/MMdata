# Base URL to basketball-reference college basketball pages
base_url <- "https://www.sports-reference.com/cbb/schools/"


#' Scrape game logs by team and year
#'
#' @param team_id Team ID for which to scrape game logs
#' @param year Year for which to scrape game logs
#'
#' @return Data frame with one row per game. Includes opponent and team stats
#' @export
#'
#' @examples
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
      opp_id <- x %>% stringr::str_sub(start=23, end=-12)
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

team_stats <- function(game_logs, team_id, year) {
  team_df <- game_logs[game_logs$team_id == team_id, ]
  team_df <- team_df[complete.cases(team_df), ]
  stats <- data.frame(
    team_id=team_id,
    year=year,
    # ORTG
    ORTG=sum(team_df$TM_PTS) / sum(team_df$poss),
    ORTG_l=quantile(team_df$TM_PTS / team_df$poss, 0.1, names=FALSE),
    ORTG_u=quantile(team_df$TM_PTS / team_df$poss, 0.9, names=FALSE),
    # DRTG
    DRTG=sum(team_df$O_PTS) / sum(team_df$poss),
    DRTG_l=quantile(team_df$O_PTS / team_df$poss, 0.1, names=FALSE),
    DRTG_u=quantile(team_df$O_PTS / team_df$poss, 0.9, names=FALSE),
    # NTRG
    NRTG = sum(team_df$TM_PTS) / sum(team_df$poss) -
      sum(team_df$O_PTS) / sum(team_df$poss),
    # eFG
    eFG.=eFG(sum(team_df$TM_FG), sum(team_df$TM_3P), sum(team_df$TM_FGA)),
    eFG._l=quantile(eFG(team_df$TM_FG, team_df$TM_3P, team_df$TM_FGA), 0.1, names=FALSE),
    eFG._u=quantile(eFG(team_df$TM_FG, team_df$TM_3P, team_df$TM_FGA), 0.9, names=FALSE),
    # TS
    TS.=TS(sum(team_df$TM_PTS), sum(team_df$TM_FGA), sum(team_df$TM_FTA)),
    TS._l=quantile(TS(team_df$TM_PTS, team_df$TM_FGA, team_df$TM_FTA), 0.1, names=FALSE),
    TS._u=quantile(TS(team_df$TM_PTS, team_df$TM_FGA, team_df$TM_FTA), 0.9, names=FALSE),
    # ORB
    ORB.=ORB(sum(team_df$TM_ORB), sum(team_df$O_TRB - team_df$O_ORB)),
    ORB._l=quantile(ORB(team_df$TM_ORB, team_df$O_TRB - team_df$O_ORB), 0.1, names=FALSE),
    ORB._u=quantile(ORB(team_df$TM_ORB, team_df$O_TRB - team_df$O_ORB), 0.9, names=FALSE),
    # DRB
    DRB.=DRB(sum(team_df$TM_TRB - team_df$TM_ORB), sum(team_df$O_ORB)),
    DRB._l=quantile(DRB(team_df$TM_TRB - team_df$TM_ORB, team_df$O_ORB), 0.1, names=FALSE),
    DRB._u=quantile(DRB(team_df$TM_TRB - team_df$TM_ORB, team_df$O_ORB), 0.9, names=FALSE),
    # TRB
    TRB.=TRB(sum(team_df$TM_TRB), sum(team_df$O_TRB)),
    TRB._l=quantile(TRB(team_df$TM_TRB, team_df$O_TRB), 0.1, names=FALSE),
    TRB._u=quantile(TRB(team_df$TM_TRB, team_df$O_TRB), 0.9, names=FALSE),
    # AST
    AST.=AST(sum(team_df$TM_AST), sum(team_df$TM_FG)),
    AST._l=quantile(AST(team_df$TM_AST, team_df$TM_FG), 0.1, names=FALSE),
    AST._u=quantile(AST(team_df$TM_AST, team_df$TM_FG), 0.9, names=FALSE),
    # STL
    STL.=STL(sum(team_df$TM_STL), sum(team_df$poss)),
    STL._l=quantile(STL(team_df$TM_STL, team_df$poss), 0.1, names=FALSE),
    STL._u=quantile(STL(team_df$TM_STL, team_df$poss), 0.9, names=FALSE),
    # BLK
    BLK.=BLK(sum(team_df$TM_BLK), sum(team_df$poss)),
    BLK._l=quantile(BLK(team_df$TM_BLK, team_df$poss), 0.1, names=FALSE),
    BLK._u=quantile(BLK(team_df$TM_BLK, team_df$poss), 0.9, names=FALSE),
    # TOV
    TOV.=TO(sum(team_df$TM_TOV), sum(team_df$poss)),
    TOV_l=quantile(TO(team_df$TM_TOV, team_df$poss), 0.1, names=FALSE),
    TOV_u=quantile(TO(team_df$TM_TOV, team_df$poss), 0.9, names=FALSE),
    # FTr
    FTr.=FTr(sum(team_df$TM_FTA), sum(team_df$TM_FGA)),
    FTr_l=quantile(FTr(team_df$TM_FTA, team_df$TM_FGA), 0.1, names=FALSE),
    FTr_u=quantile(FTr(team_df$TM_FTA, team_df$TM_FGA), 0.9, names=FALSE),
    # 3Pr
    threePr=TPr(sum(team_df$TM_3P), sum(team_df$TM_FGA)),
    threePr_l=quantile(TPr(team_df$TM_3P, team_df$TM_FGA), 0.1, names=FALSE),
    threePr_u=quantile(TPr(team_df$TM_3P, team_df$TM_FGA), 0.9, names=FALSE),
    # 3P%
    threeP.=sum(team_df$TM_3P) / sum(team_df$TM_3PA),
    threeP._l=quantile(team_df$`TM_3P%`, 0.1, names=FALSE),
    threeP._u=quantile(team_df$`TM_3P%`, 0.9, names=FALSE),
    # 2P%
    twoP.=sum(team_df$TM_FG - team_df$TM_3P) / sum(team_df$TM_FGA - team_df$TM_3PA),
    twoP._l=quantile((team_df$TM_FG - team_df$TM_3P) / (team_df$TM_FGA - team_df$TM_3PA), 0.1, names=FALSE),
    twoP._u=quantile((team_df$TM_FG - team_df$TM_3P) / (team_df$TM_FGA - team_df$TM_3PA), 0.9, names=FALSE),
    pace=mean(team_df$poss / (40 + team_df$num_OT * 5)),
    # O_eFG
    O_eFG.=eFG(sum(team_df$O_FG), sum(team_df$O_3P), sum(team_df$O_FGA)),
    O_eFG._l=quantile(eFG(team_df$O_FG, team_df$O_3P, team_df$O_FGA), 0.1, names=FALSE),
    O_eFG._u=quantile(eFG(team_df$O_FG, team_df$O_3P, team_df$O_FGA), 0.9, names=FALSE),
    # O_TS
    O_TS.=TS(sum(team_df$O_PTS), sum(team_df$O_FGA), sum(team_df$O_FTA)),
    O_TS._l=quantile(TS(team_df$O_PTS, team_df$O_FGA, team_df$O_FTA), 0.1, names=FALSE),
    O_TS._u=quantile(TS(team_df$O_PTS, team_df$O_FGA, team_df$O_FTA), 0.9, names=FALSE),
    # O_AST
    O_AST.=AST(sum(team_df$O_AST), sum(team_df$O_FG)),
    O_AST._l=quantile(AST(team_df$O_AST, team_df$O_FG), 0.1, names=FALSE),
    O_AST._u=quantile(AST(team_df$O_AST, team_df$O_FG), 0.9, names=FALSE),
    # O_STL
    O_STL.=STL(sum(team_df$O_STL), sum(team_df$poss)),
    O_STL._l=quantile(STL(team_df$O_STL, team_df$poss), 0.1, names=FALSE),
    O_STL._u=quantile(STL(team_df$O_STL, team_df$poss), 0.9, names=FALSE),
    # O_BLK
    O_BLK.=BLK(sum(team_df$O_BLK), sum(team_df$poss)),
    O_BLK._l=quantile(BLK(team_df$O_BLK, team_df$poss), 0.1, names=FALSE),
    O_BLK._u=quantile(BLK(team_df$O_BLK, team_df$poss), 0.9, names=FALSE),
    # O_TOV
    O_TOV.=TO(sum(team_df$O_TOV), sum(team_df$poss)),
    O_TOV._l=quantile(TO(team_df$O_TOV, team_df$poss), 0.1, names=FALSE),
    O_TOV._u=quantile(TO(team_df$O_TOV, team_df$poss), 0.9, names=FALSE),
    # O_FTr
    O_FTr.=FTr(sum(team_df$O_FTA), sum(team_df$O_FGA)),
    O_FTr._l=quantile(FTr(team_df$O_FTA, team_df$O_FGA), 0.1, names=FALSE),
    O_FTr._u=quantile(FTr(team_df$O_FTA, team_df$O_FGA), 0.9, names=FALSE),
    # O_3Pr
    O_TPr.=TPr(sum(team_df$O_3P), sum(team_df$O_FGA)),
    O_TPr._l=quantile(TPr(team_df$O_3P, team_df$O_FGA), 0.1, names=FALSE),
    O_TPr._u=quantile(TPr(team_df$O_3P, team_df$O_FGA), 0.9, names=FALSE),
    # O_3P%
    O_threeP.=sum(team_df$O_3P) / sum(team_df$O_3PA),
    O_threeP._l=quantile(team_df$`O_3P%`, 0.1, names=FALSE),
    O_threeP._u=quantile(team_df$`O_3P%`, 0.9, names=FALSE),
    # O_2P%
    O_twoP.=sum(team_df$O_FG - team_df$O_3P) / sum(team_df$O_FGA - team_df$O_3PA),
    O_twoP._l=quantile((team_df$O_FG - team_df$O_3P) / (team_df$O_FGA - team_df$O_3PA), 0.1, names=FALSE),
    O_twoP._u=quantile((team_df$O_FG - team_df$O_3P) / (team_df$O_FGA - team_df$O_3PA), 0.9, names=FALSE)
  )
  return(stats)
}


#' Scraping team stats for a given year
#'
#' @param year Year for which to scrape team stats
#'
#' @return Data frame with one row per team and their statistics
#' @export
#'
#' @examples
all_teams_stats <- function(year) {
  # Scraping teams table
  teams_table <- scrape_teams_table()
  # Keeping only teams that were D1 the year of interest
  team_ids <- teams_table$team_id[teams_table$From <= year & teams_table$To >= year]
  # Combining all game logs for games from given season
  all_logs <- lapply(team_ids, function(x)
    scrape_game_logs(x, year) %>%
      tryCatch(error=function(e) NULL)) %>%
    do.call(what="rbind")
  # Removing games against non-D1 teams
  d1_games <- all_logs[all_logs$opp_id %in% all_logs$team_id, ]
  # Removing tournament games
  if(year == 2011){
    selection_sunday <- "2011-03-13"
  } else if(year == 2012) {
    selection_sunday <- "2012-03-11"
  } else if(year == 2013) {
    selection_sunday <- "2013-03-17"
  } else if(year == 2014) {
    selection_sunday <- "2014-03-16"
  } else if(year == 2015) {
    selection_sunday <- "2015-03-15"
  } else if(year == 2016) {
    selection_sunday <- "2016-03-13"
  } else if(year == 2017) {
    selection_sunday <- "2017-03-12"
  } else if(year == 2018) {
    selection_sunday <- "2018-03-11"
  } else if(year == 2019) {
    selection_sunday <- "2019-03-17"
  } else if(year == 2020) {
    selection_sunday <- "2020-03-15"
  } else if(year == 2021) {
    selection_sunday <- "2021-03-14"
  } else {
    selection_sunday <- "2022-03-13"
  }

  # Filtering out games against non-DI schools
  d1_games <- d1_games[d1_games$Date %>% as.Date() <= selection_sunday, ]

  all_stats <- lapply(
    unique(d1_games$team_id),
    function(x) team_stats(d1_games, x, year)
  ) %>%
    do.call(what="rbind")

  stat_cats <- c("ORTG", "DRTG", "NRTG", "eFG.", "TS.", "ORB.", "DRB.", "TRB.",
                 "AST.", "STL.", "BLK.", "TOV.", "FTr.", "threePr", "threeP.",
                 "twoP.", "pace", "O_eFG.", "O_TS.", "O_AST.", "O_STL.",
                 "O_BLK.", "O_TOV.", "O_FTr.", "O_TPr.", "O_threeP.", "O_twoP.")
  all_stats[, stat_cats] <- scale(all_stats[, stat_cats])

  return(all_stats)
}




