# INITAL ANALYSIS FUNCTIONS

# Libraries
library(gt)
library(gtExtras)
library(dplyr)
library(worldfootballR)

# Logos
map_team_logos <- function(teams_df){
  mapping <- 
    c(
      "Arsenal" = "/Users/estebansanchez/Desktop/Footy/Prem Logos 24-25/Arsenal.png",
      "Aston Villa" = "/Users/estebansanchez/Desktop/Footy/Prem Logos 24-25/Aston Villa.png",
      "Bournemouth" = "/Users/estebansanchez/Desktop/Footy/Prem Logos 24-25/Bournemouth.png",
      "Brentford" = "/Users/estebansanchez/Desktop/Footy/Prem Logos 24-25/Brentford.png",
      "Brighton" = "/Users/estebansanchez/Desktop/Footy/Prem Logos 24-25/Brighton.png",
      "Ipswich" = "/Users/estebansanchez/Desktop/Footy/Prem Logos 24-25/Ipswich.png",
      "Chelsea" = "/Users/estebansanchez/Desktop/Footy/Prem Logos 24-25/Chelsea.png",
      "Crystal Palace" = "/Users/estebansanchez/Desktop/Footy/Prem Logos 24-25/CP.png",
      "Everton" = "/Users/estebansanchez/Desktop/Footy/Prem Logos 24-25/Everton.png",
      "Fulham" = "/Users/estebansanchez/Desktop/Footy/Prem Logos 24-25/Fulham.png",
      "Liverpool" = "/Users/estebansanchez/Desktop/Footy/Prem Logos 24-25/Liverpool.png",
      "Southampton" = "/Users/estebansanchez/Desktop/Footy/Prem Logos 24-25/Southampton.png",
      "Manchester City" = "/Users/estebansanchez/Desktop/Footy/Prem Logos 24-25/ManCity.png",
      "Manchester Utd" = "/Users/estebansanchez/Desktop/Footy/Prem Logos 24-25/ManU.png",
      "Newcastle Utd" = "/Users/estebansanchez/Desktop/Footy/Prem Logos 24-25/Newcastle.png",
      "Nott'ham Forest" = "/Users/estebansanchez/Desktop/Footy/Prem Logos 24-25/Forest.png",
      "Leicester" = "/Users/estebansanchez/Desktop/Footy/Prem Logos 24-25/Leicester.png",
      "Tottenham" = "/Users/estebansanchez/Desktop/Footy/Prem Logos 24-25/Tottenham.png",
      "West Ham" = "/Users/estebansanchez/Desktop/Footy/Prem Logos 24-25/WestHam.png",
      "Wolves" = "/Users/estebansanchez/Desktop/Footy/Prem Logos 24-25/Wolves.png"
      )
  
  teams_df <- 
    teams_df %>% 
    mutate(squad_image = case_when(
      Squad == "Arsenal" ~ mapping["Arsenal"],
      Squad == "Aston Villa" ~ mapping["Aston Villa"],
      Squad == "Bournemouth" ~ mapping["Bournemouth"],
      Squad == "Brentford" ~ mapping["Brentford"],
      Squad == "Brighton" ~ mapping["Brighton"],
      Squad == "Ipswich Town" ~ mapping["Ipswich"],
      Squad == "Chelsea" ~ mapping["Chelsea"],
      Squad == "Crystal Palace" ~ mapping["Crystal Palace"],
      Squad == "Everton" ~ mapping["Everton"],
      Squad == "Fulham" ~ mapping["Fulham"],
      Squad == "Liverpool" ~ mapping["Liverpool"],
      Squad == "Southampton" ~ mapping["Southampton"],
      Squad == "Manchester City" ~ mapping["Manchester City"],
      Squad == "Manchester Utd" ~ mapping["Manchester Utd"],
      Squad == "Newcastle Utd" ~ mapping["Newcastle Utd"],
      Squad == "Nott'ham Forest" ~ mapping["Nott'ham Forest"],
      Squad == "Leicester City" ~ mapping["Leicester"],
      Squad == "Tottenham" ~ mapping["Tottenham"],
      Squad == "West Ham" ~ mapping["West Ham"],
      Squad == "Wolves" ~ mapping["Wolves"]
    )
  )
  return(teams_df)
}


# Data functions
get_prem_players <- function(year_end = 2025, stype = "standard", team_player = "player"){
  big_5_df = fb_big5_advanced_season_stats(season_end_year = year_end,
                                           stat_type = stype,
                                           team_or_player = team_player,
                                           time_pause = 3)
  prem_players_tib <- as_tibble(big_5_df)
  prem_players <- 
    prem_players_tib %>% 
    filter(Comp == "Premier League")
  return(prem_players)
}


# Table functions
my_gt_theme_dark <- function(df, map = "yes", n = 10){
  if (map == "yes"){
    df <- map_team_logos(df)
    df <- df %>% 
      mutate(Squad = squad_image) %>% 
      select(squad_image, everything(), -squad_image)
  }
  df <- df %>% head(n)
  gt(df) %>% 
    cols_align(align = "left") %>% 
    # EDIT CELL BORDERS
    tab_style(
      style = cell_borders(sides = c("bottom", "top"), color = "#04414E"),
      locations = cells_body()
    ) %>% 
    tab_style(
      style = cell_borders(sides = c("bottom"), color = "#04414E"),
      locations = cells_column_labels()
    ) %>% 
    tab_style(
      style = cell_borders(sides = c("bottom"), color = "white", weight = px(2)),
      locations = list(cells_title(groups = "subtitle"),
                       cells_body())
    ) %>%
    # FORMAT TEXT IN CELLS
    tab_style(
      style = cell_text(color = "white"),
      locations = cells_body()
    ) %>% 
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(rows = 1)
    ) %>% 
    tab_style(
      style = cell_fill(color = "#04414E"),
      locations = cells_body()
    ) %>% 
    tab_style(
      style = cell_fill(color = "aquamarine"),
      locations = cells_body(rows = 1, columns = 1:2)
    ) %>%
    tab_style(
      style = cell_text(color = "black"), # White text color
      locations = cells_body(rows = 1)
    ) %>%
    # FORMAT NUMBERS
    fmt_number() %>%
    data_color(
      columns = everything(),
      method = "numeric",
      # palette = brewer.pal(nrow(df), "GnBu"),
      palette = c("#04414E", "aquamarine"),
      # domain = c(0.5, 1.5),
      reverse = F
    ) %>% 
    # ADD IMAGES
    gt_img_rows(
      columns = Squad, 
      img_source = "local"
    ) %>% 
    tab_source_note(
      source_note = md("Data from: **FBref**")
      ) %>% 
    tab_options(
      heading.align = "center",
      table.background.color = "#04414E",
      column_labels.font.size = 20,
      heading.subtitle.font.size = 17,
      table.width = px(600),
      heading.title.font.size = 35,
      source_notes.font.size = 10,
      source_notes.padding = 10
    )
}

my_gt_theme_light <- function(df, map = "yes", n = 10){
  if (map == "yes"){
    df <- map_team_logos(df)
    df <- df %>% 
      mutate(Squad = squad_image) %>% 
      select(squad_image, everything(), -squad_image)
  }
  df <- df %>% head(n)
  gt(df) %>% 
    cols_align(align = "left") %>% 
    # EDIT CELL BORDERS
    tab_style(
      style = cell_borders(sides = c("bottom", "top"), color = "#e5e5e5"),
      locations = cells_body()
    ) %>% 
    tab_style(
      style = cell_borders(sides = c("bottom"), color = "#e5e5e5"),
      locations = cells_column_labels()
    ) %>% 
    tab_style(
      style = cell_borders(sides = c("bottom"), color = "white", weight = px(2)),
      locations = list(cells_title(groups = "subtitle"),
                       cells_body())
    ) %>%
    # FORMAT TEXT IN CELLS
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(rows = 1, columns = 2:3)
    ) %>% 
    # tab_style(
    #   style = cell_fill(color = "#e5e5e5"),
    #   locations = cells_body()
    # ) %>% 
    tab_style(
      style = cell_fill(color = "#003566"),
      locations = cells_body(rows = 1, columns = 1:2)
    ) %>%
    tab_style(
      style = cell_text(color = "white"), # White text color
      locations = cells_body(rows = 1)
    ) %>%
    tab_style(
      style = cell_text(color = "black"), # White text color
      locations = cells_body(rows = 2:n)
    ) %>%
    # FORMAT NUMBERS
    fmt_number() %>%
    data_color(
      columns = everything(),
      method = "numeric",
      palette = c("#e5e5e5", "#003566"),
      reverse = F
    ) %>% 
    # ADD IMAGES
    gt_img_rows(
      columns = Squad, 
      img_source = "local"
    ) %>% 
    tab_source_note(
      source_note = md("Data from: **FBref**")
    ) %>% 
    tab_options(
      heading.align = "center",
      table.background.color = "#e5e5e5",
      column_labels.font.size = 20,
      heading.subtitle.font.size = 17,
      table.width = px(600),
      heading.title.font.size = 35,
      source_notes.font.size = 10,
      source_notes.padding = 10
    )
}

top_overall_att <- function(df){
  pass
}

top_xG_per90 <- function(df, n = 10, np = T, by = everything()){
  df = df %>% 
    select(squad_image, Player, xG_Per, `xG+xAG_Per`, npxG_Per, `npxG+xAG_Per`)
  colnames(df) = c("Squad", "Player", "xg", "xga", "npxg", "npxga")
  
  if (np == T){
    df %>% 
      arrange(desc(xg), desc(npxg)) %>% 
      head(n) %>% 
      gt() %>% 
      
      
      # ADD TITLE (HEADER)
      tab_header(
        title = md("**Expected Goals Per 90**"),
        subtitle = md("*Premier League 23-24*")
      ) %>% 
      
      
      # ALGIN HEADER
      opt_align_table_header(align = "center") %>% #align table header (left, center, right)
      
      
      # CHANGE COLUMN LABELS
      cols_label( # Change column names
        Squad = md("**Squad**"),
        Player = md('**Player**'),
        xg = md("**xG**"),
        npxg = md("**npxG**"),
        xga = md("**xG+xA**"),
        npxga = md("**npxG+xA**"),
      ) %>% 
      
      
      # CHANGE COLUMN WIDTHS
      cols_width( 
        Squad ~ px(90),
        Player ~ px(200),
        starts_with("x") ~ px(100),
        starts_with("n") ~ px(100)
        # ALT: everything() ~ px(280) 
      ) %>% 
      
      
      # ALIGN COLUMN HEADERS
      cols_align(align = "left") %>% 
      
      
      # EDIT CELL BORDERS
      tab_style(
        style = cell_borders(sides = c("bottom", "top"), color = "#04414E"),
        locations = cells_body()
      ) %>% 
      tab_style(
        style = cell_borders(sides = c("bottom"), color = "#04414E"),
        locations = cells_column_labels()
      ) %>% 
      tab_style(
        style = cell_borders(sides = c("bottom"), color = "white", weight = px(2)),
        locations = list(cells_title(groups = "subtitle"),
                         cells_body())
      ) %>% 
      
      
      
      # FORMAT TEXT IN CELLS
      tab_style(
        style = cell_text(color = "white"),
        locations = cells_body()
      ) %>% 
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(rows = 1)
      ) %>% 
      tab_style(
        style = cell_fill(color = "#04414E"),
        locations = cells_body()
      ) %>% 
      tab_style(
        style = cell_fill(color = "aquamarine"),
        locations = cells_body(rows = 1, columns = 1:2)
      ) %>%
      tab_style(
        style = cell_text(color = "black"), # White text color
        locations = cells_body(rows = 1)
      ) %>%
      
      # FORMAT NUMBERS
      fmt_number( 
        decimals = 2 # Default = 2
      ) %>% 
      
      # ADD IMAGES
      gt_img_rows(
        columns = Squad, 
        img_source = "local"
      ) %>% 
      
      tab_options(
        table.background.color = "#04414E",
        column_labels.font.size = 20,
        heading.subtitle.font.size = 17
      ) %>% 
      
      opt_table_font(font = "Liberation") %>%
      
      data_color(
        columns = by,
        method = "numeric",
        # palette = brewer.pal(nrow(df), "GnBu"),
        palette = c("#04414E", "aquamarine"),
        # domain = c(0.5, 1.5),
        reverse = F
      ) %>% 
      
      tab_source_note(
        source_note = md("Data from: **FBref**")
      ) %>%
      
      
      # ADJUST TABLE WIDTH
      tab_options(
        table.width = px(600),
        heading.title.font.size = 35,
        source_notes.font.size = 10,
        source_notes.padding = 10
      )
  } else {
    # REPEAT WITH PENALTIES
    df %>% 
      select(Squad, Player, xg, xga) %>% 
      arrange(desc(xg), desc(xga)) %>% 
      head(n) %>% 
      gt() %>% 
      
      
      # ADD TITLE (HEADER)
      tab_header(
        title = md("**Expected Goals Per 90**"),
        subtitle = md("*Premier League 23-24*")
      ) %>% 
      
      
      # ALGIN HEADER
      opt_align_table_header(align = "center") %>% #align table header (left, center, right)
      
      
      # CHANGE COLUMN LABELS
      cols_label( # Change column names
        Squad = md("**Squad**"),
        Player = md('**Player**'),
        xg = md("**xG**"),
        xga = md("**xG+xA**"),
      ) %>% 
      
      
      # CHANGE COLUMN WIDTHS
      cols_width( 
        Squad ~ px(90),
        Player ~ px(200),
        starts_with("x") ~ px(100),
        starts_with("n") ~ px(100)
        # ALT: everything() ~ px(280) 
      ) %>% 
      
      
      # ALIGN COLUMN HEADERS
      cols_align(align = "left") %>% 
      
      
      # EDIT CELL BORDERS
      tab_style(
        style = cell_borders(sides = c("bottom", "top"), color = "#04414E"),
        locations = cells_body()
      ) %>% 
      tab_style(
        style = cell_borders(sides = c("bottom"), color = "#04414E"),
        locations = cells_column_labels()
      ) %>% 
      tab_style(
        style = cell_borders(sides = c("bottom"), color = "white", weight = px(2)),
        locations = list(cells_title(groups = "subtitle"),
                         cells_body())
      ) %>% 
      
      
      
      # FORMAT TEXT IN CELLS
      tab_style(
        style = cell_text(color = "white"),
        locations = cells_body()
      ) %>% 
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(rows = 1)
      ) %>% 
      tab_style(
        style = cell_fill(color = "#04414E"),
        locations = cells_body()
      ) %>% 
      tab_style(
        style = cell_fill(color = "aquamarine"),
        locations = cells_body(rows = 1, columns = 1:2)
      ) %>%
      tab_style(
        style = cell_text(color = "black"), # White text color
        locations = cells_body(rows = 1)
      ) %>%
      
      # FORMAT NUMBERS
      fmt_number( 
        decimals = 2 # Default = 2
      ) %>% 
      
      # ADD IMAGES
      gt_img_rows(
        columns = Squad, 
        img_source = "local"
        ) %>% 
      
      tab_options(
        table.background.color = "#04414E",
        column_labels.font.size = 20
      ) %>% 
      
      opt_table_font(font = "Liberation") %>%
      
      data_color(
        columns = by,
        method = "numeric",
        # palette = brewer.pal(nrow(df), "GnBu"),
        palette = c("#04414E", "aquamarine"),
        # domain = c(0.5, 1.5),
        reverse = F
      ) %>% 
      
      tab_source_note(
        source_note = md("Data from: **FBref**")
      ) %>%
      
      
      # ADJUST TABLE WIDTH
      tab_options(
        table.width = px(500),
        heading.title.font.size = 35,
        source_notes.font.size = 10,
        source_notes.padding = 10,
        heading.subtitle.font.size = 17
      )
  }
}



# Metrics

defense_vulnerability <- function(season_team_stats){
  season_team_stats <- 
    season_team_stats 
  }