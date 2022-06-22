
team_rankings <- function(res_data){

  data = res_data %>%
    select(GAME_DATE,TEAM_NAME,OPP_TEAM_NAME,FG_RUNS,FG_OPP_RUNS,F5_RUNS,F5_OPP_RUNS) %>%
    unique()

  # Recalculate metrics by each day

  max_date = max(data$GAME_DATE)
  min_date = min(data$GAME_DATE)
  dates = seq(min_date, max_date, by="days")
  teams = unique(data$TEAM_NAME)

  df <- data.frame(matrix(ncol = 6, nrow = length(dates)*length(teams)))
  names(df) = c("GAME_DATE",
                "TEAM_NAME",
                "F5_AVG_RUNS",
                "F5_AVG_OPP_RUNS",
                "FG_AVG_RUNS",
                "FG_AVG_OPP_RUNS")
  df$GAME_DATE = as.Date(df$GAME_DATE)


  for(d in 1:length(dates)){
    for(t in 1:length(teams)){

      team = teams[t]
      date = dates[d]

      temp = data %>%
        filter(TEAM_NAME == team,
               GAME_DATE <= date) %>%
        group_by(TEAM_NAME) %>%
        summarize(F5_AVG_RUNS = mean(F5_RUNS,na.rm = T),
               FG_AVG_RUNS = mean(FG_RUNS,na.rm = T),
               F5_AVG_OPP_RUNS = mean(F5_OPP_RUNS,na.rm = T),
               FG_AVG_OPP_RUNS = mean(FG_OPP_RUNS,na.rm = T))

      row = df[(d-1)*length(teams)+t,]
      row$GAME_DATE = as.Date(date)
      row$TEAM_NAME = team

      if(nrow(temp)>0){
        row$F5_AVG_RUNS = temp$F5_AVG_RUNS
        row$F5_AVG_OPP_RUNS = temp$F5_AVG_OPP_RUNS
        row$FG_AVG_RUNS = temp$FG_AVG_RUNS
        row$FG_AVG_OPP_RUNS = temp$FG_AVG_OPP_RUNS


      }
      df[(d-1)*length(teams)+t,] = row
    }
  }

  df2 = df %>%
    group_by(GAME_DATE) %>%
    mutate(F5_OFFENSE_RANK = dense_rank(desc(F5_AVG_RUNS)),
           FG_OFFENSE_RANK = dense_rank(desc(FG_AVG_RUNS)),
           F5_PITCHING_RANK = dense_rank((F5_AVG_OPP_RUNS)),
           FG_PITCHING_RANK = dense_rank((FG_AVG_OPP_RUNS))) %>%
    arrange(TEAM_NAME,GAME_DATE)

  write.csv(df2,file.path(data_in,'rankings','team_rankings.csv'),row.names = F)
  return(df2)

}
