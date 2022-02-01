
# poss
#' Estimate possessions in a game
#'
#' @param TM_FGA One team's field goals attempted
#' @param TM_FTA One team's free throws attempted
#' @param TM_ORB One team's offensive rebounds
#' @param TM_FG One team's field goals made
#' @param TM_TOV One team's turnovers
#' @param TM_TRB One team's total rebounds
#' @param O_TRB Opponent's total rebounds
#' @param O_ORB Opponent's offensive rebounds
#' @param O_FGA Opponent's field goals attempted
#' @param O_FTA Opponent's free throws attempted
#' @param O_FG Opponent's field goals made
#' @param O_TOV Opponent's turnovers
#'
#' @return
#' @export
#'
#' @examples
poss <- function(TM_FGA, TM_FTA, TM_ORB, O_TRB, O_ORB, TM_FG, TM_TOV, O_FGA, O_FTA, TM_TRB, O_FG, O_TOV) {
  ((TM_FGA + 0.4 * TM_FTA - 1.07 * (TM_ORB / (TM_ORB + (O_TRB - O_ORB))) * (TM_FGA - TM_FG) + TM_TOV) +
     (O_FGA + 0.4 * O_FTA - 1.07 * (O_ORB / (O_ORB + (TM_TRB - TM_ORB))) * (O_FGA - O_FG) + O_TOV)) / 2
}


#' Calculate Effective Field Goal Percentage
#'
#' @param TM_FG One team's field goals made
#' @param TM_3PM One team's made 3-pointers
#' @param TM_FGA One team's field goals attempted
#'
#' @return
#' @export
#'
#' @examples
eFG <- function(TM_FG, TM_3PM, TM_FGA){
  (TM_FG + 0.5 * TM_3PM) / TM_FGA
}


#' Calculate True Shooting Percentage
#'
#' @param TM_PTS One team's total points scored
#' @param TM_FGA One team's field goals attempted
#' @param TM_FTA One team's free throws attempted
#'
#' @return
#' @export
#'
#' @examples
TS <- function(TM_PTS, TM_FGA, TM_FTA){
  TM_PTS / (2 * (TM_FGA + 0.44 * TM_FTA))
}


#' Calculate Offensive Rebounding Percentage
#'
#' @param TM_ORB One team's offensive rebounds
#' @param O_DRB Opponent's defensive rebounds
#'
#' @return
#' @export
#'
#' @examples
ORB <- function(TM_ORB, O_DRB){
  TM_ORB / (TM_ORB + O_DRB)
}


#' Calculate Defensive Rebounding Percentage
#'
#' @param TM_DRB One team's defensive rebounds
#' @param O_ORB Opponent's offensive rebounds
#'
#' @return
#' @export
#'
#' @examples
DRB <- function(TM_DRB, O_ORB){
  TM_DRB / (TM_DRB + O_ORB)
}


#' Calculate Total Rebounding Percentage
#'
#' @param TM_TRB One team's total rebounds
#' @param O_TRB Opponent's total rebounds
#'
#' @return
#' @export
#'
#' @examples
TRB <- function(TM_TRB, O_TRB){
  TM_TRB / (TM_TRB + O_TRB)
}


#' Calculate Assist Percentage
#'
#' @param TM_AST One team's assists
#' @param TM_FG One team's field goals made
#'
#' @return
#' @export
#'
#' @examples
AST <- function(TM_AST, TM_FG){
  TM_AST / TM_FG
}


#' Calculate Steal Percentage
#'
#' @param TM_STL One team's steals
#' @param poss Number of possessions in game
#'
#' @return
#' @export
#'
#' @examples
STL <- function(TM_STL, poss){
  TM_STL / poss
}


#' Calculate Block Percentage
#'
#' @param TM_BLK One team's blocks
#' @param poss Number of possessions in game
#'
#' @return
#' @export
#'
#' @examples
BLK <- function(TM_BLK, poss){
  TM_BLK / poss
}


#' Calculate Turnover Percentage
#'
#' @param TM_TOV One team's turnovers
#' @param poss Number of possessions in game
#'
#' @return
#' @export
#'
#' @examples
TO <- function(TM_TOV, poss){
  TM_TOV / poss
}


#' Calculate Free Throw Rate
#'
#' @param TM_FTA One team's free throws attempted
#' @param TM_FGA One team's field goals attempted
#'
#' @return
#' @export
#'
#' @examples
FTr <- function(TM_FTA, TM_FGA){
  TM_FTA / TM_FGA
}


#' Calculate Three Point Rate
#'
#' @param TM_3P One team's 3-pointers made
#' @param TM_FGA One team's field goals attempted
#'
#' @return
#' @export
#'
#' @examples
TPr <- function(TM_3P, TM_FGA){
  TM_3P / TM_FGA
}

