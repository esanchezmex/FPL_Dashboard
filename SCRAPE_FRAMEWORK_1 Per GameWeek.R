# FRAMEWORK V1

library(worldfootballR)
library(tidyverse)
library(beepr)

create_gameweeks <- function(df){
  df <- df %>% 
    filter(Comp == "Premier League") %>% 
    mutate(
      GW = as.numeric(str_extract(Round, "\\d+$"))
    ) %>% 
    select(-Round)
  return (df)
}

teams_urls <- fb_teams_urls("https://fbref.com/en/comps/9/Premier-League-Stats")

selected_teams_1 <- teams_urls[1:5]
selected_teams_2 <- teams_urls[6:10]
selected_teams_3 <- teams_urls[11:15]
selected_teams_4 <- teams_urls[16:20]


########################################################
# FOR EACH SPLIT (RUN FOR EACH GROUP)
players_urls <- c()
for (team_url in selected_teams_4){ # <-- CHANGE HERE
  temp <- fb_player_urls(team_url, time_pause = runif(1, 5, 10))
  players_urls <- c(players_urls, temp)
}

prem_players_dfs <- list()
for (player_url in players_urls){
  temp <- fb_player_match_logs(player_url, 
                               season_end_year = 2025, 
                               stat_type = "summary",
                               time_pause = runif(1, 5, 10)) %>%
    as_tibble()
  prem_players_dfs[[length(prem_players_dfs) + 1]] <- temp
}

prem_players_df <- bind_rows(prem_players_dfs)

# Make sure to create a new df every time (as to not override)
df_group_4 <- create_gameweeks(prem_players_df)
#        ^ CHANGE HERE 
beep(8)
########################################################
# AFTER ALL GROUPS CREATED, Combine

df <- rbind(df_group_1, df_group_2, df_group_3, df_group_4)


# CLEAN UP
rm(df_group_1, df_group_2, df_group_3, df_group_4, 
   prem_players_df, prem_players_dfs, temp,
   player_url, players_urls, team_url, teams_urls,
   selected_teams_1, selected_teams_2, selected_teams_3, selected_teams_4)

# READY FOR ANALYSIS









