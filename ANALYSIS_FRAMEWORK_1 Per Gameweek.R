# FRAMEWORK FOR ANALYSIS

# Once data processing ready, wrangle
fpl_data <- function(df, gameweek, pos = 'all', groupBy = c("Player"), arrangeBy = "xGI") {
  patterns <- list(
    def = "CB|RB|LB",
    mid = "CM|AM|DM|LW|RW|RM|LM",
    att = "RW|FW|LW|AM",
    gk = "GK",
    all = "CB|RB|LB|CM|AM|DM|LW|RW|RM|LM|RW|FW|LW|AM|GK"
  )
  
  att_df <- df %>% 
    filter(Season == "2024-2025") %>% 
    filter(GW > gameweek) %>%
    filter(str_detect(Pos, patterns[[pos]]))
  
  group_vars <- syms(groupBy)
  arrange_var <- sym(arrangeBy)

  att_df <- 
    att_df %>% 
    group_by(!!!group_vars) %>% 
    summarise(
      npxg = sum(npxG_Expected, na.rm = T),
      goals = sum(Gls_Performance, na.rm = T),
      xA = sum(xAG_Expected, na.rm = T),
      assists = sum(Ast_Performance, na.rm = T),
      shots = sum(Sh_Performance, na.rm = T),
      ShT = sum(SoT_Performance, na.rm = T),
      .groups = "drop"
    ) %>% 
    mutate(xGI = npxg+xA) %>% 
    filter(xGI > 0) %>% 
    mutate(GI = goals + assists) %>% 
    arrange(desc(!!arrange_var)) %>% 
    ungroup() %>% 
    mutate(rank = row_number()) %>% 
    select(rank, everything())

  return(att_df)
}

  
fpl_data_p90 <- function(df, gameweek, pos = 'all', groupBy = c("Player"), arrangeBy = "xGI_p90", min_minutes = NULL) {
  patterns <- list(
    def = "CB|RB|LB",
    mid = "CM|AM|DM|LW|RW|RM|LM",
    att = "RW|FW|LW|AM",
    gk = "GK",
    all = "CB|RB|LB|CM|AM|DM|LW|RW|RM|LM|RW|FW|LW|AM|GK"
  )
  
  att_df <- df %>% 
    filter(Season == "2024-2025") %>% 
    filter(GW > gameweek) %>%
    filter(str_detect(Pos, patterns[[pos]]))
  
  group_vars <- syms(groupBy)
  arrange_var <- sym(arrangeBy)
  
  if ("Player" %in% groupBy) {
    # Player-level calculations
    if (is.null(min_minutes)) {
      player_minutes <- att_df %>%
        group_by(Player) %>%
        summarise(total_minutes = sum(Min, na.rm = TRUE), .groups = "drop")
      min_minutes <- mean(player_minutes$total_minutes) * 1.5
      
      qualified_players <- player_minutes %>%
        filter(total_minutes > min_minutes) %>%
        pull(Player)
      
      att_df <- att_df %>%
        filter(Player %in% qualified_players)
      
      
      result_df <- att_df %>% 
        group_by(!!!group_vars) %>% 
        summarise(
          mins = sum(Min, na.rm = TRUE),
          npxg = sum(npxG_Expected, na.rm = TRUE),
          goals = sum(Gls_Performance, na.rm = TRUE),
          xA = sum(xAG_Expected, na.rm = TRUE),
          assists = sum(Ast_Performance, na.rm = TRUE),
          .groups = "drop"
        )
      
    } else {
      result_df <- att_df %>% 
        group_by(!!!group_vars) %>% 
        summarise(
          mins = sum(Min, na.rm = TRUE),
          npxg = sum(npxG_Expected, na.rm = TRUE),
          goals = sum(Gls_Performance, na.rm = TRUE),
          xA = sum(xAG_Expected, na.rm = TRUE),
          assists = sum(Ast_Performance, na.rm = TRUE),
          .groups = "drop"
        )
      
      result_df <- result_df %>% 
        filter(mins >= min_minutes)
    }
    
  } else {
    # Squad-level calculations
    result_df <- att_df %>%
      group_by(!!!group_vars, GW) %>%
      summarise(
        minutes_per_game = sum(Min, na.rm = TRUE),
        npxg = sum(npxG_Expected, na.rm = TRUE),
        goals = sum(Gls_Performance, na.rm = TRUE),
        xA = sum(xAG_Expected, na.rm = TRUE),
        assists = sum(Ast_Performance, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      group_by(!!!group_vars) %>%
      summarise(
        mins = mean(minutes_per_game),
        npxg = sum(npxg, na.rm = TRUE),
        goals = sum(goals, na.rm = TRUE),
        xA = sum(xA, na.rm = TRUE),
        assists = sum(assists, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  # Calculate per 90 stats
  result_df <- result_df %>%
    mutate(
      npxg_p90 = round((npxg / mins) * 90,2),
      gls_p90 = round((goals / mins) * 90,2),
      xA_p90 = round((xA / mins) * 90,2),
      ast_p90 = round((assists / mins) * 90,2),
      xGI_p90 = npxg_p90 + xA_p90,
      GI_p90 = gls_p90 + ast_p90
    ) %>%
    filter(xGI_p90 > 0) %>%
    arrange(desc(!!arrange_var)) %>%
    ungroup() %>%
    mutate(rank = row_number()) %>%
    select(rank, everything(), -npxg, -goals, -xA, -assists)
  
  return(result_df)
}

  
  
  
# HOW TO USE
# insights <- fpl_data(df, 
#                     gameweek = 9, 
#                     pos = "att",                # String
#                     groupBy = c(
#                       "Player", 
#                       "Opponent",               # Strings
#                       "Venue"
#                       ),                               
#                     arrangeBy = "xGI"           # String
#                     )


last_gw <- 7
fpl_data(df, 
         gameweek = (df %>% pull(GW) %>% max())-last_gw,
         arrangeBy = "npxg",
         groupBy = c("Opponent")
)
# map_team_logos() %>% 
# select(rank, squad_image, everything(), -Squad) %>% 
