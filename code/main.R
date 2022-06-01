################################################################################
# Main File to capture daily insights to MLB games
################################################################################

# rm(list=ls())

project_name = 'mlb_odds_trends'

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
repo_path = file.path(substr(getwd(),0,gregexpr(pattern = project_name ,
                                                getwd())[[1]][1]-2),
                      project_name)

source(file.path(repo_path,
                 'code',
                 'utilities',
                 'load_directories.r'),
       local = knitr::knit_global())

p <- c("tidyverse",
       "httr",
       "lubridate",
       "ggplot2",
       "reticulate"
)

load_all_packages(p)

source(file.path(objects_path,'odds_functions.r'))
source(file.path(objects_path,'acquisition_functions.r'))
source_python(file.path(objects_path,'bot_file.py'))

################################################################################
# Daily Data Prep

tidy_data = read.csv(file.path(data_in,'action','tidy.csv'))
res_data = res_data_gen(tidy_data)

################################################################################
# Capture Trends:
#
# Functions:
#   - team_ou, team_ou_f5
#   - team_ats, team_ats_f5
#   - team_runs
#
# Filters:
#   - team_name = NA,
#   - opp_team_name = NA,
#   - last_n_games = NA,
#   - opp_pitcher = NA,
#   - opp_p_hand = NA,
#   - pitcher = NA,
#   - p_hand = NA,
#   - home_away = NA
# Examples:
# Using actual line and price values
#
# res_data %>%
#   team_ats(team_name = "Boston Red Sox", last_n_games = 10)
#
# Using hypothetical line and price values
#
# man_data_gen(tidy_data,
#              total_fg = 7.5,
#              total_o_price_fg = -115,
#              total_u_price_fg = -115,
#              total_f5 = 4.5,
#              total_o_price_f5 = -115,
#              total_u_price_f5 = -115,
#              spread_fg = 0.5,
#              spread_price_fg = -115,
#              spread_f5 = 1.5,
#              spread_price_f5 = -115) %>%
#   team_ats(team_name = "Boston Red Sox", last_n_games = 10)

################################################################################
# Acquire Today's Games' Data
################################################################################

# @params
last_n_games = 10

d = dates = gsub("-", "", Sys.Date())
today_lines = days_results(d)

for(g_num in 1:nrow(today_lines)){

  row = today_lines[g_num,]

  res_data = res_data_gen(tidy_data)
  man_data_home = man_data_gen(tidy_data,
                          row$FG_TOTAL,
                          row$FG_TOTAL_O_PRICE,
                          row$FG_TOTAL_U_PRICE,
                          row$HOME_FG_SPREAD,
                          row$HOME_FG_SPREAD_PRICE,
                          row$F5_TOTAL,
                          row$F5_TOTAL_O_PRICE,
                          row$F5_TOTAL_U_PRICE,
                          row$HOME_F5_SPREAD,
                          row$HOME_F5_SPREAD_PRICE)

  man_data_away = man_data_gen(tidy_data,
                               row$FG_TOTAL,
                               row$FG_TOTAL_O_PRICE,
                               row$FG_TOTAL_U_PRICE,
                               row$AWAY_FG_SPREAD,
                               row$AWAY_FG_SPREAD_PRICE,
                               row$F5_TOTAL,
                               row$F5_TOTAL_O_PRICE,
                               row$F5_TOTAL_U_PRICE,
                               row$AWAY_F5_SPREAD,
                               row$AWAY_F5_SPREAD_PRICE)

  header = paste0(
    row$AWAY_TEAM_ABBR,
    ifelse(!is.na(row$AWAY_PITCHER_NAME),
           paste0(' (',row$AWAY_PITCHER_NAME,')'),'')
    ,' @ ',
    row$HOME_TEAM_ABBR,
    ifelse(!is.na(row$HOME_PITCHER_NAME),
           paste0(' (',row$HOME_PITCHER_NAME,')'),'')
  )

  subheader1 = paste0(
    'FG: ',
    row$AWAY_TEAM_ABBR,' ',row$AWAY_FG_SPREAD,' (',row$AWAY_FG_SPREAD_PRICE,') | ',
    row$HOME_TEAM_ABBR,' ',row$HOME_FG_SPREAD,' (',row$HOME_FG_SPREAD_PRICE,')'
  )
  subheader2 = paste0(
    'F5: ',
    row$AWAY_TEAM_ABBR,' ',row$AWAY_F5_SPREAD,' (',row$AWAY_F5_SPREAD_PRICE,') | ',
    row$HOME_TEAM_ABBR,' ',row$HOME_F5_SPREAD,' (',row$HOME_F5_SPREAD_PRICE,')'
  )
  subheader3 = paste0(
    'Totals: ',
    row$F5_TOTAL,' (',row$F5_TOTAL_O_PRICE,' / ',row$F5_TOTAL_U_PRICE,')',
    ' | ',
    row$FG_TOTAL,' (',row$FG_TOTAL_O_PRICE,' / ',row$FG_TOTAL_U_PRICE,')'
  )

  # Away Team Filter Parameters
  team_name = row$AWAY_TEAM_NAME
  opp_team_name = row$HOME_TEAM_NAME
  last_n_games = last_n_games
  opp_p_hand = row$HOME_PITCHER_THROW
  opp_pitcher = row$HOME_PITCHER_NAME
  pitcher = row$AWAY_PITCHER_NAME
  home_away = "AWAY"

  away_gsr = grid_search_team(res_data,man_data_away,
                   team_name,
                   opp_team_name,
                   last_n_games,
                   opp_p_hand,
                   opp_pitcher,
                   pitcher,
                   home_away)

  away = interpreter(away_gsr) %>%
    unique()

  # Home Team Filter Parameters
  team_name = row$HOME_TEAM_NAME
  opp_team_name = row$AWAY_TEAM_NAME
  last_n_games = last_n_games
  opp_p_hand = row$AWAY_PITCHER_THROW
  opp_pitcher = row$AWAY_PITCHER_NAME
  home_away = "HOME"

  home_gsr = grid_search_team(res_data,
                              man_data_home,
                              team_name,
                              opp_team_name,
                              last_n_games,
                              opp_p_hand,
                              opp_pitcher,
                              home_away)


  home_msgs = interpreter(home_gsr) %>%
    unique()








}

################################################################################
# Send Output to Discord

output_hookurl <- fromJSON(file=file.path(hook_path,'webhooks.json'))[['trends_url']]

msg_elem = c(
  # header,
  # subheader1,
  # subheader2,
  # subheader3,
  # '---------',
  # msgs,
  '---------',
  home_msgs,
  '__________________'
)

for(m in msg_elem){
  print(m)
  send_to_discord(m,output_hookurl)
  Sys.sleep(0.5)
}
