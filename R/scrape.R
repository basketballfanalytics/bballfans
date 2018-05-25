# ------------------------------------------------------------------------------

#' Get Play Type Stats
#'
#' Wraps around nbaTools::GetPlayTypeStats to scrape data off stas.nba.com
#'   and then cleans up the data types, values, and names to be consistent
#'   with goals of bballfanalytics
#'
#' @return tibble with data compiled for all play type categories
#' @keywords player team play type
#' @export
#' @examples tbl <- get_play_types(pull_type = "player", seasonType = "post")
#' @examples tbl <- get_play_types(pull_type = "team", names = "defensive")
#' @importFrom nbaTools GetPlayTypeStats
#' @importFrom dplyr %>% bind_rows mutate select rename one_of group_by quos
#' @importFrom purrr map
#' @importFrom tibble as_tibble
#' @importFrom rlang .data UQS
#' @importFrom tidyr complete fill
get_play_types <- function(pull_type = "team", # player
                           season_yr = 2018, # how far back can we pull data?
                           limit = 500, # max results returned by scrape
                           names = "offensive", # defensive
                           seasonType = "reg") { # post

  # the variables we will keep from the raw scrape results
  select_var_list <- c(quos(TeamNameAbbreviation,
                            play_type,
                            GP,
                            Poss,
                            Points,
                            FGA,
                            FGM,
                            PPP,
                            name,
                            season,
                            seasonType,
                            frequency,
                            fg_rate,
                            efg_rate,
                            ft_rate,
                            to_rate,
                            sf_rate,
                            and_one_rate,
                            score_rate))

  # update paramaters for team/player pulls
  if(pull_type == "team"){
    complete_vars <- quos(TeamNameAbbreviation, play_type)
    group_vars <- quos(TeamNameAbbreviation)
    fill_vars <- quos(play_type)
  } else {
    complete_vars <- quos(PlayerIDSID, play_type)
    group_vars <- quos(PlayerIDSID)
    fill_vars <- quos(PlayerFirstName,
                      PlayerLastName,
                      P,
                      play_type,
                      TeamNameAbbreviation)
    select_var_list <- c(quos(PlayerIDSID,
                              PlayerFirstName,
                              PlayerLastName,
                              P),
                         select_var_list)
  }

  raw_data <- map(bball_play_type_constants,
                  function(x) {
                    GetPlayTypeStats(category = x,
                                     type = pull_type,
                                     season = season_yr,
                                     limit = limit,
                                     names = names,
                                     seasonType = seasonType) %>%
                      mutate(play_type = x)}) %>%
    bind_rows() %>%
    as_tibble() %>%
    mutate(frequency = .data$Time / 100,
           fg_rate = .data$FG / 100,
           efg_rate = .data$aFG / 100,
           ft_rate = .data$FT / 100,
           to_rate = .data$TO / 100,
           sf_rate = .data$SF / 100,
           and_one_rate = .data$PlusOne / 100,
           score_rate = .data$Score / 100) %>%
    select(UQS(select_var_list)) %>% # would like to have !!! instead of UQS
    rename(gp = .data$GP,
           poss = .data$Poss,
           points = .data$Points,
           fga = .data$FGA,
           fgm = .data$FGM,
           ppp = .data$PPP,
           season_type = .data$seasonType) %>%
    complete(UQS(complete_vars)) %>%
    group_by(UQS(group_vars)) %>%
    fill(UQS(fill_vars)) %>%
    fill(UQS(fill_vars), .direction = "up")
}
