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
       "reticulate",
       'rjson'
)

load_all_packages(p)

source(file.path(objects_path,'odds_functions.r'))
source(file.path(objects_path,'acquisition_functions.r'))
source(file.path(objects_path,'team_functions.r'))
source_python(file.path(objects_path,'bot_file.py'))

################################################################################
# Daily Data Prep

tidy_data = read.csv(file.path(data_in,'action','tidy.csv'))
hour_adj = hours(4)
tidy_data$GAME_TIME = as_datetime(tidy_data$GAME_TIME) - hour_adj
tidy_data$GAME_DATE = as.Date(tidy_data$GAME_TIME)

t_rankings = team_rankings(tidy_data)

res_data = res_data_gen(tidy_data,t_rankings)

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
#   - f5_offense_rank = NA
#   - fg_offense_rank = NA
#   - f5_pitching_rank = NA
#   - fg_pitching_rank = NA
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
opp_f5_pitching_rank = 10
opp_fg_pitching_rank = 10
opp_f5_offense_rank = 10
opp_fg_offense_rank =10

d = gsub("-", "", Sys.Date())
today_lines = days_results(d)

for(g_num in 6:nrow(today_lines)){

  row = today_lines[g_num,]

  man_data_home = man_data_gen(tidy_data,
                               t_rankings,
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
                               t_rankings,
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

  # Get opponent ranking
  t_rank = t_rankings %>%
    select(TEAM_NAME,GAME_DATE,F5_OFFENSE_RANK,F5_PITCHING_RANK,FG_OFFENSE_RANK,FG_PITCHING_RANK) %>%
    filter(TEAM_NAME == opp_team_name) %>%
    ungroup() %>%
    filter(GAME_DATE == max(GAME_DATE))

  away_gsr = grid_search_team(res_data,
                   man_data_away,
                   team_name,
                   opp_team_name,
                   last_n_games,
                   opp_p_hand,
                   opp_pitcher,
                   pitcher,
                   home_away,
                   ifelse(t_rank$F5_PITCHING_RANK <= opp_f5_pitching_rank,opp_f5_pitching_rank,NA),
                   ifelse(t_rank$FG_PITCHING_RANK <= opp_fg_pitching_rank,opp_fg_pitching_rank,NA),
                   ifelse(t_rank$F5_OFFENSE_RANK <= opp_f5_offense_rank,opp_f5_offense_rank,NA),
                   ifelse(t_rank$FG_OFFENSE_RANK <= opp_fg_offense_rank,opp_fg_offense_rank,NA))

  away = interpreter(away_gsr) %>%
    unique()

  # Home Team Filter Parameters
  team_name = row$HOME_TEAM_NAME
  opp_team_name = row$AWAY_TEAM_NAME
  last_n_games = last_n_games
  opp_p_hand = row$AWAY_PITCHER_THROW
  opp_pitcher = row$AWAY_PITCHER_NAME
  home_away = "HOME"

  # Get opponent ranking
  t_rank = t_rankings %>%
    select(TEAM_NAME,GAME_DATE,F5_OFFENSE_RANK,F5_PITCHING_RANK,FG_OFFENSE_RANK,FG_PITCHING_RANK) %>%
    filter(TEAM_NAME == opp_team_name) %>%
    ungroup() %>%
    filter(GAME_DATE == max(GAME_DATE))

  home_gsr = grid_search_team(res_data,
                              man_data_home,
                              team_name,
                              opp_team_name,
                              last_n_games,
                              opp_p_hand,
                              opp_pitcher,
                              home_away,
                              ifelse(t_rank$F5_PITCHING_RANK <= opp_f5_pitching_rank,opp_f5_pitching_rank,NA),
                              ifelse(t_rank$FG_PITCHING_RANK <= opp_fg_pitching_rank,opp_fg_pitching_rank,NA),
                              ifelse(t_rank$F5_OFFENSE_RANK <= opp_f5_offense_rank,opp_f5_offense_rank,NA),
                              ifelse(t_rank$FG_OFFENSE_RANK <= opp_fg_offense_rank,opp_fg_offense_rank,NA))


  home_msgs = interpreter(home_gsr) %>%
    unique()


  msg_elem = c(
    header,
    subheader1,
    subheader2,
    subheader3,
    '---------',
    away,
    '---------',
    home_msgs,
    '__________________'
  )


  print('######################################################################')

  output_hookurl <- fromJSON(file=file.path(hook_path,'webhooks.json'))[['trends_url']]

  for(m in msg_elem){
    print(m)
    send_to_discord(m,output_hookurl)
    Sys.sleep(0.5)
  }



}

################################################################################
# Send Output to Discord

output_hookurl <- fromJSON(file=file.path(hook_path,'webhooks.json'))[['trends_url']]



for(m in msg_elem){
  print(m)
  send_to_discord(m,output_hookurl)
  Sys.sleep(0.5)
}
