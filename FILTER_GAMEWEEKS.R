library(worldfootballR)
library(tidyverse)

# ALWAYS RUN
teams_urls <- fb_teams_urls("https://fbref.com/en/comps/9/Premier-League-Stats")

# CREATE GAMEWEEKS FUNCTIONS
create_gameweeks <- function(df){
  df <- df %>% 
    filter(Comp == "Premier League") %>% 
    mutate(
      GW = as.numeric(str_extract(Round, "\\d+$"))
    ) %>% 
    select(-Round)
  return (df)
}

# MAP LOGOS
map_team_logos_with_GW <- function(teams_df){
  mapping <- 
    c(
      "Arsenal" = "/Users/estebansanchez/Dropbox/Mac/Desktop/Personal/Projects/Prem Logos 23-24/Arsenal.png",
      "Aston Villa" = "/Users/estebansanchez/Dropbox/Mac/Desktop/Personal/Projects/Prem Logos 23-24/Aston Villa.png",
      "Bournemouth" = "/Users/estebansanchez/Dropbox/Mac/Desktop/Personal/Projects/Prem Logos 23-24/Bournemouth.png",
      "Brentford" = "/Users/estebansanchez/Dropbox/Mac/Desktop/Personal/Projects/Prem Logos 23-24/Brentford.png",
      "Brighton and Hove Albion" = "/Users/estebansanchez/Dropbox/Mac/Desktop/Personal/Projects/Prem Logos 23-24/Brighton.png",
      "Burnley" = "/Users/estebansanchez/Dropbox/Mac/Desktop/Personal/Projects/Prem Logos 23-24/Burnley.png",
      "Chelsea" = "/Users/estebansanchez/Dropbox/Mac/Desktop/Personal/Projects/Prem Logos 23-24/Chelsea.png",
      "Crystal Palace" = "/Users/estebansanchez/Dropbox/Mac/Desktop/Personal/Projects/Prem Logos 23-24/Crystal Palace.png",
      "Everton" = "/Users/estebansanchez/Dropbox/Mac/Desktop/Personal/Projects/Prem Logos 23-24/Everton.png",
      "Fulham" = "/Users/estebansanchez/Dropbox/Mac/Desktop/Personal/Projects/Prem Logos 23-24/Fullham.png",
      "Liverpool" = "/Users/estebansanchez/Dropbox/Mac/Desktop/Personal/Projects/Prem Logos 23-24/Liverpool.png",
      "Luton Town" = "/Users/estebansanchez/Dropbox/Mac/Desktop/Personal/Projects/Prem Logos 23-24/Luton.png",
      "Manchester City" = "/Users/estebansanchez/Dropbox/Mac/Desktop/Personal/Projects/Prem Logos 23-24/Man City.png",
      "Manchester United" = "/Users/estebansanchez/Dropbox/Mac/Desktop/Personal/Projects/Prem Logos 23-24/Man Utd.png",
      "Newcastle United" = "/Users/estebansanchez/Dropbox/Mac/Desktop/Personal/Projects/Prem Logos 23-24/Newcastle.png",
      "Nottingham Forest" = "/Users/estebansanchez/Dropbox/Mac/Desktop/Personal/Projects/Prem Logos 23-24/Forest.png",
      "Sheffield United" = "/Users/estebansanchez/Dropbox/Mac/Desktop/Personal/Projects/Prem Logos 23-24/Sheffield.png",
      "Tottenham Hotspur" = "/Users/estebansanchez/Dropbox/Mac/Desktop/Personal/Projects/Prem Logos 23-24/Tottenham.png",
      "West Ham United" = "/Users/estebansanchez/Dropbox/Mac/Desktop/Personal/Projects/Prem Logos 23-24/West Ham.png",
      "Wolverhampton Wanderers" = "/Users/estebansanchez/Dropbox/Mac/Desktop/Personal/Projects/Prem Logos 23-24/Wolves.png"
    )
  
  teams_df <- 
    teams_df %>% 
    mutate(squad_image = case_when(
      Team == "Arsenal" ~ mapping["Arsenal"],
      Team == "Aston Villa" ~ mapping["Aston Villa"],
      Team == "Bournemouth" ~ mapping["Bournemouth"],
      Team == "Brentford" ~ mapping["Brentford"],
      Team == "Brighton and Hove Albion" ~ mapping["Brighton and Hove Albion"],
      Team == "Burnley" ~ mapping["Burnley"],
      Team == "Chelsea" ~ mapping["Chelsea"],
      Team == "Crystal Palace" ~ mapping["Crystal Palace"],
      Team == "Everton" ~ mapping["Everton"],
      Team == "Fulham" ~ mapping["Fulham"],
      Team == "Liverpool" ~ mapping["Liverpool"],
      Team == "Luton Town" ~ mapping["Luton Town"],
      Team == "Manchester City" ~ mapping["Manchester City"],
      Team == "Manchester United" ~ mapping["Manchester United"],
      Team == "Newcastle United" ~ mapping["Newcastle United"],
      Team == "Nottingham Forest" ~ mapping["Nottingham Forest"],
      Team == "Sheffield United" ~ mapping["Sheffield United"],
      Team == "Tottenham Hotspur" ~ mapping["Tottenham Hotspur"],
      Team == "West Ham United" ~ mapping["West Ham United"],
      Team == "Wolverhampton Wanderers" ~ mapping["Wolverhampton Wanderers"]
    )
    )
  return(teams_df)
}




# Collecting ALL PL PLAYER CURRENT SEASON STATS
# AVOID! this just blueprint, narrow down teams first
players_urls <- c()
for (team_url in teams_urls){
  temp <- fb_player_urls(team_url, time_pause = 5)
  players_urls <- c(players_urls, temp)
}

prem_players_dfs <- list()
for (player_url in players_urls){
  temp <- fb_player_match_logs(player_url, 
                               season_end_year = 2024, 
                               stat_type = "summary",
                               time_pause = 5) %>%
    as_tibble()
  prem_players_dfs[[length(prem_players_dfs) + 1]] <- temp
}

prem_players_df <- bind_rows(prem_players_dfs)

########################################
  #### AVOIDING SUCH A BIG LOOP ####
  ## USING TEAMS INSTEAD OF PLAYERS##
#######################################
prem_teams_dfs <- list()
for (team_url in teams_urls){
  temp <- fb_team_match_log_stats(team_url,
                               stat_type = "shooting",
                               time_pause = 3) %>%
    as_tibble()
  prem_teams_dfs[[length(prem_teams_dfs) + 1]] <- temp
}

prem_teams_df <- bind_rows(prem_teams_dfs)


prem <- create_gameweeks(prem_teams_df)



prem %>% 
  filter(ForAgainst == "For" & GW >= max(GW)-4) %>% 
  select(GW, Team, xG_Expected) %>% 
  group_by(Team) %>% 
  summarise(xG_last4_For = sum(xG_Expected)) %>% 
  arrange(-xG_last4_For)
prem %>% 
  filter(ForAgainst == "Against" & GW >= max(GW)-4) %>% 
  select(GW, Team, xG_Expected) %>% 
  group_by(Team) %>% 
  summarise(xG_last4_Against = sum(xG_Expected)) %>% 
  arrange(xG_last4_Against)
  




passing <- fb_team_match_log_stats(teams_urls[1],
                        stat_type = "passing",
                        time_pause = 5) %>%
  as_tibble()
gca <- fb_team_match_log_stats(teams_urls[1],
                        stat_type = "gca",
                        time_pause = 5) %>%
  as_tibble()
defense <- fb_team_match_log_stats(teams_urls[1],
                        stat_type = "defense",
                        time_pause = 5) %>%
  as_tibble()

cbind(create_gameweeks(passing) %>% filter(GW > 18) %>% select(GW, Team, Opponent) %>% arrange(GW), 
      create_gameweeks(passing) %>% 
        group_by(GW, ForAgainst) %>% 
        summarise(PPA = sum(PPA, na.rm = T)) %>% 
        filter(GW > 18)) %>% 
  select(-4) %>% 
  mutate(ForAgainst = factor(ForAgainst, levels = c("For", "Against"))) %>%
  arrange(GW, Opponent, ForAgainst)
