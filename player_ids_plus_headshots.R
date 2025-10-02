# Getting Player Headshots from Fbref

library(worldfootballR)
library(tidyverse)

df <- fb_big5_advanced_season_stats(season_end_year = 2024, stat_type = "standard", team_or_player = "player")

# Get player IDs from URL
df$id <- str_extract(df$Url, "(?<=players/)\\w+")

# get Headshot URL
df$headshot_url <- paste0("https://fbref.com/req/202302030/images/headshots/", df$id, "_2022.jpg")

