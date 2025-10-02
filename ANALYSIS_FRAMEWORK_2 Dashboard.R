# ANALYSIS_FRAMEWORK_2 Dashboard

last_x_gws <- 6

# Top 4 attacks (last 6 GW)
fpl_data(df, 
         gameweek = 0,
         pos = "all",
         arrangeBy = "npxg",
         groupBy = c("Squad")
  ) %>%
  head(4) %>% 
  basic_table(last_x_gws)

# Top 4 Def (last 6)
fpl_data(df, 
         gameweek = 0,
         pos = "all",
         arrangeBy = "npxg",
         groupBy = c("Opponent")
) %>%
  tail(4) %>% 
  basic_table(last_x_gws)


# Top 10 Att (last 6)
fpl_data(df, 
         gameweek = 0,
         pos = "att",
         arrangeBy = "xGI",
         groupBy = c("Player", "Squad")
) %>%
  head(10) %>% 
  basic_table(last_x_gws)

# Top 10 Mids (last 6)
fpl_data(df, 
         gameweek = 0,
         pos = "mid",
         arrangeBy = "xGI",
         groupBy = c("Player", "Squad")
) %>%
  head(10) %>% 
  basic_table(last_x_gws)

# Top 10 Def (last 6) 
fpl_data(df, 
         gameweek = 0,
         pos = "def",
         arrangeBy = "xGI",
         groupBy = c("Player", "Squad")
) %>%
  head(10) %>% 
  basic_table(last_x_gws)

