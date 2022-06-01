
################################################################################
# Function to generate spread results fields and return to main file
################################################################################

res_data_gen <- function(data){

  tidy_data = data

  hour_adj = hours(4)
  tidy_data$GAME_TIME = as_datetime(tidy_data$GAME_TIME) - hour_adj
  tidy_data$GAME_DATE = as.Date(tidy_data$GAME_TIME)

  res_data = tidy_data %>%
    mutate(
      FG_ATS = ifelse(FG_SPREAD > - (FG_RUNS - FG_OPP_RUNS),"WIN",
                      ifelse(FG_SPREAD < - (FG_RUNS - FG_OPP_RUNS),"LOSE",
                             "PUSH")),
      FG_ATS_UNITS = ifelse(FG_ATS=="WIN",
                            ifelse(FG_SPREAD_PRICE<0,-100/FG_SPREAD_PRICE,FG_SPREAD_PRICE/100),
                            ifelse(FG_ATS=="LOSE",-1,0)),
      F5_ATS = ifelse(F5_SPREAD > - (F5_RUNS - F5_OPP_RUNS),"WIN",
                      ifelse(F5_SPREAD < - (F5_RUNS - F5_OPP_RUNS),"LOSE",
                             "PUSH")),
      F5_ATS_UNITS = ifelse(F5_ATS=="WIN",
                            ifelse(F5_SPREAD_PRICE<0,-100/F5_SPREAD_PRICE,F5_SPREAD_PRICE/100),
                            ifelse(F5_ATS=="LOSE",-1,0)),
      FG_OU = ifelse(FG_TOTAL < FG_TOTAL_RUNS,"OVER",
                     ifelse(FG_TOTAL > FG_TOTAL_RUNS,"UNDER",
                            "PUSH")),
      FG_OVER_UNITS = ifelse(FG_OU=="OVER",
                             ifelse(FG_TOTAL_O_PRICE<0,-100/FG_TOTAL_O_PRICE,FG_TOTAL_O_PRICE/100),
                             ifelse(FG_OU=="UNDER",-1,0)),
      FG_UNDER_UNITS = ifelse(FG_OU=="UNDER",
                              ifelse(FG_TOTAL_U_PRICE<0,-100/FG_TOTAL_U_PRICE,FG_TOTAL_U_PRICE/100),
                              ifelse(FG_OU=="OVER",-1,0)),
      F5_OU = ifelse(F5_TOTAL < F5_TOTAL_RUNS,"OVER",
                     ifelse(F5_TOTAL > F5_TOTAL_RUNS,"UNDER",
                            "PUSH")),
      F5_OVER_UNITS = ifelse(F5_OU=="OVER",
                             ifelse(F5_TOTAL_O_PRICE<0,-100/F5_TOTAL_O_PRICE,F5_TOTAL_O_PRICE/100),
                             ifelse(F5_OU=="UNDER",-1,0)),
      F5_UNDER_UNITS = ifelse(F5_OU=="UNDER",
                              ifelse(F5_TOTAL_U_PRICE<0,-100/F5_TOTAL_U_PRICE,F5_TOTAL_U_PRICE/100),
                              ifelse(F5_OU=="OVER",-1,0))
    )
  return(res_data)
}

################################################################################
# Function to generate manually input spread results fields and return to main
# file - assumes -115 units per line
################################################################################

man_data_gen <- function(data,
                         total_fg,
                         total_o_price_fg,
                         total_u_price_fg,
                         spread_fg,
                         spread_price_fg,
                         total_f5,
                         total_o_price_f5,
                         total_u_price_f5,
                         spread_f5,
                         spread_price_f5){

  tidy_data = data

  hour_adj = hours(4)
  tidy_data$GAME_TIME = as_datetime(tidy_data$GAME_TIME) - hour_adj
  tidy_data$GAME_DATE = as.Date(tidy_data$GAME_TIME)

  res_data = tidy_data %>%
    mutate(FG_ATS = ifelse(spread_fg > - (FG_RUNS - FG_OPP_RUNS),"WIN",
                           ifelse(spread_fg < - (FG_RUNS - FG_OPP_RUNS),"LOSE",
                                  "PUSH")),
           FG_ATS_UNITS = ifelse(FG_ATS=="WIN",
                                 ifelse(spread_price_fg<0,-100/spread_price_fg,spread_price_fg/100),
                                 ifelse(FG_ATS=="LOSE",-1,0)),
           F5_ATS = ifelse(spread_f5 > - (F5_RUNS - F5_OPP_RUNS),"WIN",
                           ifelse(spread_f5 < - (F5_RUNS - F5_OPP_RUNS),"LOSE",
                                  "PUSH")),
           F5_ATS_UNITS = ifelse(F5_ATS=="WIN",
                                 ifelse(spread_price_f5<0,-100/spread_price_f5,spread_price_f5/100),
                                 ifelse(F5_ATS=="LOSE",-1,0)),
           FG_OU = ifelse(total_fg < FG_TOTAL_RUNS,"OVER",
                          ifelse(total_fg > FG_TOTAL_RUNS,"UNDER",
                                 "PUSH")),
           FG_OVER_UNITS = ifelse(FG_OU=="OVER",
                                  ifelse(total_o_price_fg<0,-100/total_o_price_fg,total_o_price_fg/100),
                                  ifelse(FG_OU=="UNDER",-1,0)),
           FG_UNDER_UNITS = ifelse(FG_OU=="UNDER",
                                   ifelse(total_u_price_fg<0,-100/total_u_price_fg,total_u_price_fg/100),
                                   ifelse(FG_OU=="OVER",-1,0)),
           F5_OU = ifelse(total_f5 < F5_TOTAL_RUNS,"OVER",
                          ifelse(total_f5 > F5_TOTAL_RUNS,"UNDER",
                                 "PUSH")),
           F5_OVER_UNITS = ifelse(F5_OU=="OVER",
                                  ifelse(total_o_price_f5<0,-100/total_o_price_f5,total_o_price_f5/100),
                                  ifelse(F5_OU=="UNDER",-1,0)),
           F5_UNDER_UNITS = ifelse(F5_OU=="UNDER",
                                   ifelse(total_u_price_f5<0,-100/total_u_price_f5,total_u_price_f5/100),
                                   ifelse(F5_OU=="OVER",-1,0)),
    )
  return(res_data)
}

################################################################################
# Function to filter and group dataset to meet required specifications asked for
# within nest
################################################################################
filter_mlb_data <- function(data,
                            team_name = NA,
                            opp_team_name = NA,
                            last_n_games = NA,
                            opp_pitcher = NA,
                            opp_p_hand = NA,
                            pitcher = NA,
                            p_hand = NA,
                            home_away = NA,
                            total_line = NA,
                            spread_line = NA,
                            total_line_f5 = NA,
                            spread_line_f5 = NA)
  {

  if(!is.na(team_name)){
    data = data %>%
      filter(TEAM_NAME==team_name) %>%
      group_by(TEAM_NAME,.add = T)
  }

  if(!is.na(opp_team_name)){
    data = data %>%
      filter(OPP_TEAM_NAME == opp_team_name) %>%
      group_by(OPP_TEAM_NAME,.add = T)
  }

  if(!is.na(home_away)){
    data = data %>%
      filter(HOME_AWAY==home_away) %>%
      group_by(HOME_AWAY,.add = T)
  }

  if(!is.na(pitcher)){
    data = data %>%
      filter(PITCHER_NAME == pitcher) %>%
      group_by(PITCHER_NAME)
  }

  if(!is.na(opp_pitcher)){
    data = data %>%
      filter(OPP_PITCHER_NAME == opp_pitcher) %>%
      group_by(OPP_PITCHER_NAME)
  }

  if(!is.na(opp_p_hand)){
    data = data %>%
      filter(OPP_PITCHER_THROW==opp_p_hand) %>%
      group_by(OPP_PITCHER_THROW,.add = T)
  }

  if(!is.na(p_hand)){
    data = data %>%
      filter(PITCHER_THROW==p_hand) %>%
      group_by(PITCHER_THROW,.add = T)
  }

  if(!is.na(total_line)){
    data = data %>%
      filter(FG_TOTAL==total_line) %>%
      group_by(FG_TOTAL,.add = T)
  }

  if(!is.na(spread_line)){
    data = data %>%
      filter(FG_SPREAD==spread_line) %>%
      group_by(FG_SPREAD,.add = T)
  }

  if(!is.na(total_line_f5)){
    data = data %>%
      filter(F5_TOTAL==total_line_f5) %>%
      group_by(F5_TOTAL,.add = T)
  }

  if(!is.na(spread_line_f5)){
    data = data %>%
      filter(F5_SPREAD==spread_line_f5) %>%
      group_by(F5_SPREAD,.add = T)
  }

  if(!is.na(last_n_games)){
    u_dates = data %>%
      select(GAME_DATE) %>%
      unique() %>%
      arrange(desc(GAME_DATE)) %>%
      mutate(g_n = row_number()) %>%
      filter(g_n <= last_n_games) %>%
      select(-g_n)
    data = data %>%
      filter(GAME_DATE %in% u_dates$GAME_DATE)
  }



  return(data)

}

################################################################################
# Team FG O / U records and profitability
################################################################################
team_ou <- function(data,
                    line = NA,
                    team_name = NA,
                    opp_team_name = NA,
                    last_n_games = NA,
                    opp_p_hand = NA,
                    opp_pitcher = NA,
                    p_hand = NA,
                    pitcher = NA,
                    home_away = NA,
                    total_line = NA,
                    spread_line = NA,
                    total_line_f5 = NA,
                    spread_line_f5 = NA)
  {

  # Filter Dataset
  d2 = data %>%
    filter_mlb_data(team_name = team_name,
                    opp_team_name = opp_team_name,
                    last_n_games = last_n_games,
                    opp_p_hand = opp_p_hand,
                    opp_pitcher = opp_pitcher,
                    p_hand = p_hand,
                    pitcher = pitcher,
                    home_away = home_away,
                    total_line = total_line,
                    spread_line = spread_line,
                    total_line_f5 = total_line_f5,
                    spread_line_f5 = spread_line_f5)

  d2 = d2 %>%
    summarize(FG_O_PROFIT = sum(FG_OVER_UNITS,na.rm = T),
              FG_U_PROFIT = sum(FG_UNDER_UNITS,na.rm = T),
              FG_O = sum(ifelse(FG_OU=="OVER",1,0),na.rm = T),
              FG_U = sum(ifelse(FG_OU=="UNDER",1,0),na.rm = T),
              FG_P = sum(ifelse(FG_OU=="PUSH",1,0),na.rm = T)) %>%
    mutate(TEAM_NAME = team_name)  %>%
    ungroup() %>%
    select(TEAM_NAME,FG_O_PROFIT,FG_U_PROFIT,FG_O,FG_U,FG_P)

  if(nrow(d2)==0){
    return(NULL)
  }

  d3 = rbind(
    d2 %>% select(TEAM_NAME,FG_O,FG_U,FG_P,FG_O_PROFIT) %>%
      rename("WIN" = "FG_O",
             "LOSS" = "FG_U",
             "PUSH" = "FG_P",
             "PROFIT" = "FG_O_PROFIT") %>%
      mutate(TYPE = "FG_OVER"),
    d2 %>% select(TEAM_NAME,FG_O,FG_U,FG_P,FG_U_PROFIT) %>%
      rename("WIN" = "FG_U",
             "LOSS" = "FG_O",
             "PUSH" = "FG_P",
             "PROFIT" = "FG_U_PROFIT") %>%
      mutate(TYPE = "FG_UNDER")
  )

  df = data.frame(team_name,opp_team_name,last_n_games,opp_p_hand,
                    opp_pitcher,p_hand,pitcher,home_away,total_line,
                    spread_line,total_line_f5,spread_line_f5)

  return(cbind(d3,df))
}

################################################################################
# Team FG ATS records and profitability
################################################################################
team_ats <- function(data,
                    team_name = NA,
                    opp_team_name = NA,
                    last_n_games = NA,
                    opp_p_hand = NA,
                    opp_pitcher = NA,
                    p_hand = NA,
                    pitcher = NA,
                    home_away = NA,
                    total_line = NA,
                    spread_line = NA,
                    total_line_f5 = NA,
                    spread_line_f5 = NA)
{

  # Filter Dataset
  d2 = data %>%
    filter_mlb_data(team_name = team_name,
                    opp_team_name = opp_team_name,
                    last_n_games = last_n_games,
                    opp_p_hand = opp_p_hand,
                    opp_pitcher = opp_pitcher,
                    p_hand = p_hand,
                    pitcher = pitcher,
                    home_away = home_away,
                    total_line = total_line,
                    spread_line = spread_line,
                    total_line_f5 = total_line_f5,
                    spread_line_f5 = spread_line_f5) %>%
    summarize(ATS_PROFIT = sum(FG_ATS_UNITS),
              ATS_W = sum(ifelse(FG_ATS=="WIN",1,0), na.rm = T),
              ATS_L = sum(ifelse(FG_ATS=="LOSE",1,0), na.rm = T),
              ATS_P = sum(ifelse(FG_ATS=="PUSH",1,0), na.rm = T)) %>%
    mutate(TEAM_NAME = team_name) %>%
    ungroup() %>%
    select(TEAM_NAME,ATS_PROFIT,ATS_W,ATS_L,ATS_P) %>%
    rename("PROFIT" = "ATS_PROFIT",
           "WIN" = "ATS_W",
           "LOSS" = "ATS_L",
           "PUSH" = "ATS_P") %>%
    mutate(TYPE = "FG_ATS")

  if(nrow(d2)==0){
    return(NULL)
  }

  df = data.frame(team_name,opp_team_name,last_n_games,opp_p_hand,
                  opp_pitcher,p_hand,pitcher,home_away,total_line,
                  spread_line,total_line_f5,spread_line_f5)

  return(cbind(d2,df))
}

################################################################################
# Team F5 O / U records and profitability
################################################################################
team_ou_f5 <- function(data,
                    team_name = NA,
                    opp_team_name = NA,
                    last_n_games = NA,
                    opp_p_hand = NA,
                    opp_pitcher = NA,
                    p_hand = NA,
                    pitcher = NA,
                    home_away = NA,
                    total_line = NA,
                    spread_line = NA,
                    total_line_f5 = NA,
                    spread_line_f5 = NA)
{

  # Filter Dataset
  d2 = data %>%
    filter_mlb_data(team_name = team_name,
                    opp_team_name = opp_team_name,
                    last_n_games = last_n_games,
                    opp_p_hand = opp_p_hand,
                    opp_pitcher = opp_pitcher,
                    p_hand = p_hand,
                    pitcher = pitcher,
                    home_away = home_away,
                    total_line = total_line,
                    spread_line = spread_line,
                    total_line_f5 = total_line_f5,
                    spread_line_f5 = spread_line_f5) %>%
    summarize(F5_O_PROFIT = sum(F5_OVER_UNITS,na.rm = T),
              F5_U_PROFIT = sum(F5_UNDER_UNITS,na.rm = T),
              F5_O = sum(ifelse(F5_OU=="OVER",1,0),na.rm = T),
              F5_U = sum(ifelse(F5_OU=="UNDER",1,0),na.rm = T),
              F5_P = sum(ifelse(F5_OU=="PUSH",1,0),na.rm = T)) %>%
    mutate(TEAM_NAME = team_name)  %>%
    ungroup() %>%
    select(TEAM_NAME,F5_O_PROFIT,F5_U_PROFIT,F5_O,F5_U,F5_P)

  if(nrow(d2)==0){
    return(NULL)
  }

  d3 = rbind(
    d2 %>% select(TEAM_NAME,F5_O,F5_U,F5_P,F5_O_PROFIT) %>%
      rename("WIN" = "F5_O",
             "LOSS" = "F5_U",
             "PUSH" = "F5_P",
             "PROFIT" = "F5_O_PROFIT") %>%
      mutate(TYPE = "F5_OVER"),
    d2 %>% select(TEAM_NAME,F5_O,F5_U,F5_P,F5_U_PROFIT) %>%
      rename("WIN" = "F5_U",
             "LOSS" = "F5_O",
             "PUSH" = "F5_P",
             "PROFIT" = "F5_U_PROFIT") %>%
      mutate(TYPE = "F5_UNDER")
  )

  df = data.frame(team_name,opp_team_name,last_n_games,opp_p_hand,
                  opp_pitcher,p_hand,pitcher,home_away,total_line,
                  spread_line,total_line_f5,spread_line_f5)

  return(cbind(d3,df))
}

################################################################################
# Team F5 ATS records and profitability
################################################################################
team_ats_f5 <- function(data,
                     team_name = NA,
                     opp_team_name = NA,
                     last_n_games = NA,
                     opp_p_hand = NA,
                     opp_pitcher = NA,
                     p_hand = NA,
                     pitcher = NA,
                     home_away = NA,
                     total_line = NA,
                     spread_line = NA,
                     total_line_f5 = NA,
                     spread_line_f5 = NA)
{

  # Filter Dataset
  d2 = data %>%
    filter_mlb_data(team_name = team_name,
                    opp_team_name = opp_team_name,
                    last_n_games = last_n_games,
                    opp_p_hand = opp_p_hand,
                    opp_pitcher = opp_pitcher,
                    p_hand = p_hand,
                    pitcher = pitcher,
                    home_away = home_away,
                    total_line = total_line,
                    spread_line = spread_line,
                    total_line_f5 = total_line_f5,
                    spread_line_f5 = spread_line_f5) %>%
    summarize(ATS_PROFIT = sum(F5_ATS_UNITS),
              ATS_W = sum(ifelse(F5_ATS=="WIN",1,0), na.rm = T),
              ATS_L = sum(ifelse(F5_ATS=="LOSE",1,0), na.rm = T),
              ATS_P = sum(ifelse(F5_ATS=="PUSH",1,0), na.rm = T)) %>%
    mutate(TEAM_NAME = team_name) %>%
    ungroup() %>%
    select(TEAM_NAME,ATS_PROFIT,ATS_W,ATS_L,ATS_P) %>%
    rename("PROFIT" = "ATS_PROFIT",
           "WIN" = "ATS_W",
           "LOSS" = "ATS_L",
           "PUSH" = "ATS_P") %>%
    mutate(TYPE = "F5_ATS")

  if(nrow(d2)==0){
    return(NULL)
  }

  df = data.frame(team_name,opp_team_name,last_n_games,opp_p_hand,
                  opp_pitcher,p_hand,pitcher,home_away,total_line,
                  spread_line,total_line_f5,spread_line_f5)

  return(cbind(d2,df))
}

################################################################################
# Team Runs
################################################################################
team_runs <- function(data,
                      team_name = NA,
                      opp_team_name = NA,
                      last_n_games = NA,
                      opp_p_hand = NA,
                      opp_pitcher = NA,
                      p_hand = NA,
                      pitcher = NA,
                      home_away = NA,
                      total_line = NA,
                      spread_line = NA,
                      total_line_f5 = NA,
                      spread_line_f5 = NA
                      )
{

  # Filter Dataset
  d2 = data %>%
    filter_mlb_data(team_name = team_name,
                    opp_team_name = opp_team_name,
                    last_n_games = last_n_games,
                    opp_p_hand = opp_p_hand,
                    opp_pitcher = opp_pitcher,
                    p_hand = p_hand,
                    pitcher = pitcher,
                    home_away = home_away,
                    total_line = total_line,
                    spread_line = spread_line,
                    total_line_f5 = total_line_f5,
                    spread_line_f5 = spread_line_f5) %>%
    summarize(SAMPLE_SIZE = n(),
              MED_F5_RUNS = median(F5_RUNS, na.rm = T),
              MED_FG_RUNS = median(FG_RUNS, na.rm = T),
              MEAN_F5_RUNS = mean(F5_RUNS, na.rm = T),
              MEAN_FG_RUNS = mean(FG_RUNS, na.rm = T),
              MED_F5_TOTAL_RUNS = median(F5_TOTAL_RUNS,na.rm = T),
              MED_FG_TOTAL_RUNS = median(FG_TOTAL_RUNS,na.rm = T),
              MEAN_F5_TOTAL_RUNS = mean(F5_TOTAL_RUNS,na.rm = T),
              MEAN_FG_TOTAL_RUNS = mean(FG_TOTAL_RUNS,na.rm = T)) %>%
    mutate(TEAM_NAME = team_name)  %>%
    ungroup()

  if(nrow(d2)==0){
    return(NULL)
  }

  df = data.frame(team_name,opp_team_name,last_n_games,opp_p_hand,
                  opp_pitcher,p_hand,pitcher,home_away,total_line,
                  spread_line,total_line_f5,spread_line_f5)

  return(cbind(d2,df))
}

################################################################################
# Grid Search Filters
################################################################################

grid_search_filters <- function(
  res_data,
  FUN=ats,
  team_name=NA,
  opp_team_name=NA,
  last_n_games=NA,
  opp_p_hand=NA,
  opp_pitcher=NA,
  pitcher=NA,
  home_away=NA
){
  f = data.frame(opp_team_name,last_n_games,opp_p_hand,opp_pitcher,pitcher,home_away)
  gsr <- data.frame(matrix(ncol = 18, nrow = 0))
  x <- c("TEAM_NAME","PROFIT","WIN","LOSS","PUSH","TYPE","team_name",
         "opp_team_name","last_n_games","opp_p_hand","opp_pitcher","p_hand",
         "pitcher","home_away","total_line","spread_line","total_line_f5",
         "spread_line_f5")
  colnames(gsr) <- x

  for(n in 1:length(f)){
    combs = combn(names(f),n)

    for(i in 1:ncol(combs)){
      apply_f = f %>% select(combs[,i])

      t = res_data %>%
        FUN(team_name = team_name,
                 opp_team_name = ifelse(is.null(apply_f$opp_team_name),NA,apply_f$opp_team_name),
                 last_n_games = ifelse(is.null(apply_f$last_n_games),NA,apply_f$last_n_games),
                 opp_p_hand = ifelse(is.null(apply_f$opp_p_hand),NA,apply_f$opp_p_hand),
                 opp_pitcher = ifelse(is.null(apply_f$opp_pitcher),NA,apply_f$opp_pitcher),
                 pitcher = ifelse(is.null(apply_f$pitcher),NA,apply_f$pitcher),
                 home_away = ifelse(is.null(apply_f$home_away),NA,apply_f$home_away))

      if(!is.null(t)){
        gsr = rbind(gsr,t)
      }
    }
  }
  return(gsr)
}

grid_search_team <- function(res_data,
                             man_data,
                             team_name=NA,
                             opp_team_name=NA,
                             last_n_games=NA,
                             opp_p_hand=NA,
                             opp_pitcher=NA,
                             pitcher=NA,
                             home_away=NA){

  assessment_functions = c(team_ats,team_ats_f5,team_ou,team_ou_f5)
  gsrs =  data.frame(matrix(ncol = 19, nrow = 0))
  x <- c("TEAM_NAME","PROFIT","WIN","LOSS","PUSH","TYPE","team_name",
         "opp_team_name","last_n_games","opp_p_hand","opp_pitcher","p_hand",
         "pitcher","home_away","total_line","spread_line","total_line_f5",
         "spread_line_f5","GSR")
  names(gsrs) = x
  for(func in assessment_functions){

    gsr = res_data %>%
      grid_search_filters(FUN=func,
                          team_name,
                          opp_team_name,
                          last_n_games,
                          opp_p_hand,
                          opp_pitcher,
                          pitcher,
                          home_away) %>%
      mutate(GSR = "RES")
    gsr_man = man_data %>%
      grid_search_filters(FUN=func,
                          team_name,
                          opp_team_name,
                          last_n_games,
                          opp_p_hand,
                          opp_pitcher,
                          pitcher,
                          home_away) %>%
      mutate(GSR = "MAN")

    gsrs = rbind(gsrs,gsr,gsr_man)

  }
  results = gsrs %>%
    mutate(NUM = WIN + LOSS + PUSH,
           FILTERS = 12 - rowSums(is.na(gsrs)))

  return(results)

}

################################################################################
# Function to Identify if information is 'interesting,' write why, and return
# blank string if not
################################################################################

interpreter <- function(data){

  t = data %>%
    arrange(desc(abs(PROFIT)*FILTERS)) %>%
    filter(
           NUM>=5)
  t2 = t %>%
    group_by(TYPE) %>%
    arrange(TYPE,desc(FILTERS)) %>%
    mutate(rn = row_number()) %>%
    filter(rn <4) %>%
    filter(!(TYPE %in% c("F5_OVER","F5_UNDER","FG_OVER","FG_UNDER") & PROFIT < 0))

  msgs = c()
  for(i in 1:nrow(t2)){
    msgs =c(msgs,writer(t2[i,]))

  }

  return(msgs)

}

writer <- function(row){

  ats = ifelse(row$TYPE %in% c("F5_ATS","FG_ATS"),TRUE,FALSE)
  full_game = ifelse(row$TYPE %in% c('FG_ATS',"FG_OVER","FG_UNDER"),TRUE,FALSE)
  over = ifelse(row$TYPE %in% c('FG_OVER',"F5_OVER"),T,F)

  names(row)

  filter_qualifier = paste0(

    ifelse(row$GSR=='MAN'," using today's line and price",''),

    ifelse(!is.na(row$last_n_games),paste0(' over their last ',row$last_n_games,' games'),''),

    ifelse(!is.na(row$pitcher) | !is.na(row$home_away),' when',''),
    ifelse(!is.na(row$home_away),paste0(' ',row$home_away),''),
    ifelse(!is.na(row$pitcher),paste0(' pitching ',row$pitcher),''),

    ifelse(!(is.na(row$opp_team_name) &
               is.na(row$opp_p_hand) &
               is.na(row$opp_pitcher)),' against',''),

    ifelse(!is.na(row$opp_team_name),paste0(' the ',row$opp_team_name,''),''),
    ifelse(!is.na(row$opp_p_hand),paste0(' ',row$opp_p_hand,'HP'),''),
    ifelse(!is.na(row$opp_pitcher),paste0(' ',row$opp_pitcher),''),


    '.'


  )
  msg = paste0(

    'The ',row$TEAM_NAME, ifelse(ats,' are ',' have gone '),
    row$WIN,'-',row$LOSS,'-',row$PUSH,ifelse(ats,' ATS ',ifelse(over,' on the Over ',' on the Under ')),ifelse(full_game,'FG','F5'),
    '(',ifelse(row$PROFIT>0,'+',''),round(row$PROFIT,2),'u)',
    # Filters
    filter_qualifier
  )


  return(msg)

}

