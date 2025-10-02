library(worldfootballR)
library(tidyverse)

source("/Users/estebansanchez/Desktop/Footy/footy_functions.R")

df <- get_prem_players()


prem_urls <- fb_teams_urls("https://fbref.com/en/comps/9/Premier-League-Stats")
match_logs <- fb_team_match_log_stats(prem_urls, "shooting", time_pause = 5)

# player stats only useful when looking deeper into specific teams, only extra
#     info is it breaks it down into players
player_stats <- fb_team_player_stats(prem_urls[1], "shooting", time_pause = 5)


