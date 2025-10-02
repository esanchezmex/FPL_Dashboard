library(worldfootballR)
library(tidyverse)


# use this (has for and against data, aggregated across whole season)
df <- fb_season_team_stats(country = "ENG", gender = "M", season_end_year = 2024, tier = "1st", stat_type = "standard")



# EXPECTED GOAL DIFFERENCE
df %>% 
  mutate(Squad = ifelse(grepl("vs", Squad), substring(Squad, 4), Squad)) %>% 
  group_by(Squad, Team_or_Opponent) %>% 
  summarise(xg = mean(xG_Expected)) %>% 
  mutate(
    xGD = diff(xg),
    xGD = lag(xGD, default = first(xGD))
  ) %>%
  slice(1) %>% 
  select(Squad, xGD) %>% 
  arrange(desc(xGD))



# Non-Penalty Goal Contribution Difference (NPGCD)
for_me <- df %>% 
  mutate(Squad = ifelse(grepl("vs", Squad), substring(Squad, 4), Squad)) %>% 
  filter(Team_or_Opponent == "team") %>% 
  select(Squad, npxG_plus_xAG_Expected) %>% 
  rename(npxG_plus_xA_for = npxG_plus_xAG_Expected)

against <- df %>% 
  mutate(Squad = ifelse(grepl("vs", Squad), substring(Squad, 4), Squad)) %>% 
  filter(Team_or_Opponent == "opponent") %>% 
  select(npxG_plus_xAG_Expected) %>% 
  rename(npxG_plus_xA_against = npxG_plus_xAG_Expected)

df_new <- cbind(for_me, against)

df_new %>% 
  mutate(
    npxGI_diff = npxG_plus_xA_for - npxG_plus_xA_against # MAKE SCATTER PLOT WITH THIS
  ) %>% 
  arrange(-npxGI_diff) %>% 
  select(Squad, npxGI_diff) %>% 
  group_by(Squad)




# FILTER FPL GAMEWEEKS
