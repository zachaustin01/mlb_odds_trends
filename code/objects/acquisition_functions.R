
days_results = function(d){

  # d = '20220617'

  # Build URL
  link = paste0("https://api.actionnetwork.com/web/v1/scoreboard/mlb?bookIds=15,30,76,75,123,69,68,972,71,247,79&date=",d)
  test <- GET(link,
              add_headers(
                "Host" = "api.actionnetwork.com",
                "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/100.0.4896.127 Safari/537.36 OPR/86.0.4363.59",
                "Accept" = "application/json",
                "Accept-Language" = "en-US,en;q=0.9",
                "Accept-Encoding" = "gzip",
                "Referer" = "https://www.actionnetwork.com/mlb/odds",
                "Content-Type" = "application/json; charset=utf-8",
                "Origin" = "https://www.actionnetwork.com",
                "DNT" = "1",
                "Connection" = "keep-alive",
                "Sec-Fetch-Dest" ="empty",
                "Sec-Fetch-Mode" ="cors",
                "Sec-Fetch-Site" ="same-site",
                "TE" = "trailers"))
  json_return <- jsonlite::fromJSON(content(test, as = "text"))
  base = json_return[["games"]]

  n_games = nrow(base)
  df <- data.frame(matrix(ncol = 48, nrow = n_games))
  x <- c(
    "GAME_TIME",
    "HOME_TEAM_ID",
    "HOME_TEAM_NAME",
    "HOME_TEAM_ABBR",
    "HOME_TEAM_LOGO",
    "AWAY_TEAM_ID",
    "AWAY_TEAM_NAME",
    "AWAY_TEAM_ABBR",
    "AWAY_TEAM_LOGO",
    "HOME_PITCHER_ID",
    "HOME_PITCHER_NAME",
    "HOME_PITCHER_THROW",
    "AWAY_PITCHER_ID",
    "AWAY_PITCHER_NAME",
    "AWAY_PITCHER_THROW",
    "HOME_FG_SPREAD",
    "HOME_FG_SPREAD_PRICE",
    "AWAY_FG_SPREAD",
    "AWAY_FG_SPREAD_PRICE",
    "FG_TOTAL",
    "FG_TOTAL_O_PRICE",
    "FG_TOTAL_U_PRICE",
    "FG_HOME_TEAM_TOTAL",
    "FG_HOME_TEAM_TOTAL_O_PRICE",
    "FG_HOME_TEAM_TOTAL_U_PRICE",
    "FG_AWAY_TEAM_TOTAL",
    "FG_AWAY_TEAM_TOTAL_O_PRICE",
    "FG_AWAY_TEAM_TOTAL_U_PRICE",
    "HOME_F5_SPREAD",
    "HOME_F5_SPREAD_PRICE",
    "AWAY_F5_SPREAD",
    "AWAY_F5_SPREAD_PRICE",
    "F5_TOTAL",
    "F5_TOTAL_O_PRICE",
    "F5_TOTAL_U_PRICE",
    "F5_HOME_TEAM_TOTAL",
    "F5_HOME_TEAM_TOTAL_O_PRICE",
    "F5_HOME_TEAM_TOTAL_U_PRICE",
    "F5_AWAY_TEAM_TOTAL",
    "F5_AWAY_TEAM_TOTAL_O_PRICE",
    "F5_AWAY_TEAM_TOTAL_U_PRICE",
    "FG_AWAY_RUNS",
    "FG_HOME_RUNS",
    "FG_TOTAL_RUNS",
    "F5_HOME_RUNS",
    "F5_AWAY_RUNS",
    "F5_TOTAL_RUNS",
    "FI_TOTAL_RUNS")
  colnames(df) <- x

  for(g_num in 1:n_games){

    row = df[g_num,]

    temp = base[g_num,]

    game_lines = temp$odds[[1]] %>%
      filter(type=='game',
             !is.na(spread_home)) %>%
      filter(row_number()==1)

    if(!(temp$status %in% c('postponed','cancelled')) & !is.null(temp$players[[1]]) & (nrow(game_lines)>0)){

      row$GAME_TIME = temp$start_time
      row$HOME_TEAM_ID = temp$home_team_id
      row$AWAY_TEAM_ID = temp$away_team_id

      teams_df = temp$teams[[1]] %>%
        select(id,full_name,abbr,logo) %>%
        rename('TEAM_ID' = 'id',
               'TEAM_NAME' = 'full_name',
               'TEAM_ABBR' = 'abbr',
               'TEAM_LOGO' = 'logo')

      r2 = row %>%
        left_join(teams_df,
                  by=c("HOME_TEAM_ID"="TEAM_ID")) %>%
        mutate(HOME_TEAM_NAME = TEAM_NAME,
               HOME_TEAM_ABBR = TEAM_ABBR,
               HOME_TEAM_LOGO = TEAM_LOGO) %>%
        select(-c(TEAM_NAME,TEAM_ABBR,TEAM_LOGO)) %>%
        left_join(teams_df,
                  by=c("AWAY_TEAM_ID"="TEAM_ID")) %>%
        mutate(AWAY_TEAM_NAME = TEAM_NAME,
               AWAY_TEAM_ABBR = TEAM_ABBR,
               AWAY_TEAM_LOGO = TEAM_LOGO) %>%
        select(-c(TEAM_NAME,TEAM_ABBR,TEAM_LOGO))

      row = r2
      row$FG_AWAY_RUNS = temp$boxscore$stats$away$runs
      row$FG_HOME_RUNS = temp$boxscore$stats$home$runs
      row$AWAY_PITCHER_ID = temp$boxscore$pitching$away$probable_id
      row$HOME_PITCHER_ID = temp$boxscore$pitching$home$probable_id

      linescore = temp$boxscore$linescore[[1]] %>%
        arrange(abbr) %>%
        mutate(total_home_runs = cumsum(home_points),
               total_away_runs = cumsum(away_points))

      players = temp$players[[1]] %>%
        select(id,full_name)

      handedness = temp$players[[1]][['handedness']]

      players = cbind(players,handedness) %>%
        select(-bat)

      row$F5_HOME_RUNS = linescore[5,'total_home_runs']
      row$F5_AWAY_RUNS = linescore[5,'total_away_runs']

      row$FG_TOTAL_RUNS = row$FG_AWAY_RUNS + row$FG_HOME_RUNS
      row$F5_TOTAL_RUNS = row$F5_AWAY_RUNS + row$F5_HOME_RUNS

      row$FI_TOTAL_RUNS = linescore[1,'total_home_runs'] + linescore[1,'total_away_runs']

      r2 = row %>%
        left_join(players,by=c("HOME_PITCHER_ID"="id")) %>%
        mutate(HOME_PITCHER_NAME = full_name,
               HOME_PITCHER_THROW = throw) %>%
        select(-c(full_name,throw)) %>%
        left_join(players,by=c("AWAY_PITCHER_ID"="id")) %>%
        mutate(AWAY_PITCHER_NAME = full_name,
               AWAY_PITCHER_THROW = throw) %>%
        select(-c(full_name,throw))
      row = r2

      # Set Lines
      row$HOME_FG_SPREAD = game_lines$spread_home
      row$HOME_FG_SPREAD_PRICE = game_lines$spread_home_line
      row$AWAY_FG_SPREAD = game_lines$spread_away
      row$AWAY_FG_SPREAD_PRICE = game_lines$spread_away_line

      row$FG_TOTAL = game_lines$total
      row$FG_TOTAL_O_PRICE = game_lines$over
      row$FG_TOTAL_U_PRICE = game_lines$under

      row$FG_HOME_TEAM_TOTAL = game_lines$home_total
      row$FG_HOME_TEAM_TOTAL_O_PRICE = game_lines$home_over
      row$FG_HOME_TEAM_TOTAL_U_PRICE = game_lines$home_under

      row$FG_AWAY_TEAM_TOTAL = game_lines$away_total
      row$FG_AWAY_TEAM_TOTAL_O_PRICE = game_lines$away_over
      row$FG_AWAY_TEAM_TOTAL_U_PRICE = game_lines$away_under

      f5_lines = temp$odds[[1]] %>%
        filter(type=='firstfiveinnings') %>%
        filter(row_number()==1)

      row$HOME_F5_SPREAD = f5_lines$spread_home
      row$HOME_F5_SPREAD_PRICE = f5_lines$spread_home_line
      row$AWAY_F5_SPREAD = f5_lines$spread_away
      row$AWAY_F5_SPREAD_PRICE = f5_lines$spread_away_line

      row$F5_TOTAL = f5_lines$total
      row$F5_TOTAL_O_PRICE = f5_lines$over
      row$F5_TOTAL_U_PRICE = f5_lines$under

      row$F5_HOME_TEAM_TOTAL = f5_lines$home_total
      row$F5_HOME_TEAM_TOTAL_O_PRICE = f5_lines$home_over
      row$F5_HOME_TEAM_TOTAL_U_PRICE = f5_lines$home_under

      row$F5_AWAY_TEAM_TOTAL = f5_lines$away_total
      row$F5_AWAY_TEAM_TOTAL_O_PRICE = f5_lines$away_over
      row$F5_AWAY_TEAM_TOTAL_U_PRICE = f5_lines$away_under

      df[g_num,] = row

    }
    df = df %>%
      filter(!is.na(GAME_TIME))

  }
  return(df)

}


