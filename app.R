# Premier League Dashboard Shiny App

# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(plotly)
  library(ggplot2)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(gt)
  library(gtExtras)
  library(purrr)
})

# Ensure dplyr functions take precedence over any conflicts
filter <- dplyr::filter
select <- dplyr::select
mutate <- dplyr::mutate
arrange <- dplyr::arrange
distinct <- dplyr::distinct
pull <- dplyr::pull
group_by <- dplyr::group_by
summarise <- dplyr::summarise
rename <- dplyr::rename
slice_max <- dplyr::slice_max

# Source your analysis functions
source("ANALYSIS_FRAMEWORK_1 Per Gameweek.R")
source("footy_functions.R")
source("TABLE_FUNCTIONS.R")

# Data validation - check if df exists
data_loaded <- exists("df") && is.data.frame(df) && nrow(df) > 0

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Premier League Analytics Dashboard"),
  
  dashboardSidebar(
    conditionalPanel(
      condition = "output.data_loaded == true",
      sidebarMenu(id = "tabs",
        menuItem("Team Analysis", tabName = "teams", icon = icon("users")),
        menuItem("Player Analysis", tabName = "players", icon = icon("user")),
        menuItem("Comparison", tabName = "comparison", icon = icon("chart-line"))
      ),
    
      # Sidebar controls
      hr(),
      h4("Analysis Settings", style = "color: white; margin-left: 15px;"),
      
      numericInput("gameweeks", 
                   "Last N Gameweeks:", 
                   value = 6, 
                   min = 1, 
                   max = 20,
                   step = 1),
      
      selectInput("position_filter",
                  "Position Filter:",
                  choices = list("All" = "all",
                                "Attackers" = "att", 
                                "Midfielders" = "mid",
                                "Defenders" = "def",
                                "Goalkeepers" = "gk"),
                  selected = "all"),
      
      selectInput("team_filter",
                  "Team Filter:",
                  choices = NULL,  # Will be populated dynamically
                  selected = "all"),
      
      checkboxInput("per90", "Show Per 90 Stats", value = TRUE),
      
      # Comparison Controls (only show when on comparison tab)
      conditionalPanel(
        condition = "input.tabs == 'comparison'",
        hr(),
        h4("Comparison Settings", style = "color: white; margin-left: 15px;"),
        
        selectInput("comparison_type",
                    "Compare:",
                    choices = list("Teams" = "teams", "Players" = "players"),
                    selected = "teams"),
        
        conditionalPanel(
          condition = "input.comparison_type == 'teams'",
          selectInput("compare_teams",
                      "Select Teams:",
                      choices = NULL,
                      multiple = TRUE,
                      selected = NULL),
          
          radioButtons("team_metrics",
                       "Team Metrics:",
                       choices = list("Attacking" = "attacking", "Defensive" = "defensive"),
                       selected = "attacking")
        ),
        
        conditionalPanel(
          condition = "input.comparison_type == 'players'",
          selectInput("compare_players",
                      "Select Players:",
                      choices = NULL,
                      multiple = TRUE,
                      selected = NULL)
        )
      )
    ),
    
    # Data upload section (only show when data not loaded)
    conditionalPanel(
      condition = "output.data_loaded == false",
      div(style = "padding: 20px; text-align: center;",
        h4("Data Required", style = "color: white;"),
        p("Please upload your football data CSV file to begin analysis.", 
          style = "color: white; font-size: 14px;"),
        fileInput("data_file", 
                  "Choose CSV File",
                  accept = c(".csv"),
                  buttonLabel = "Browse...",
                  placeholder = "No file selected"),
        tags$div(style = "margin-top: 10px; color: #cccccc; font-size: 12px;",
          "Expected columns: Season, GW, Player, Squad, Pos, Min, npxG_Expected, xAG_Expected, Gls_Performance, Ast_Performance, etc.")
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
      "))
    ),
    
    # Show data upload screen when no data is loaded
    conditionalPanel(
      condition = "output.data_loaded == false",
      fluidRow(
        box(
          title = "Welcome to Premier League Analytics Dashboard", 
          status = "primary", solidHeader = TRUE, width = 12, height = 400,
          div(style = "text-align: center; padding: 50px;",
            h3("Data Upload Required"),
            p("To begin your football analytics journey, please upload your CSV data file using the sidebar."),
            p("The dashboard will analyze team and player performance across multiple gameweeks."),
            br(),
            div(style = "color: #666;",
              h4("Expected Data Format:"),
              p("Your CSV should contain columns like: Season, GW, Player, Squad, Pos, Min, npxG_Expected, xAG_Expected, Gls_Performance, Ast_Performance, etc."),
              p("Once uploaded, you'll have access to comprehensive team analysis, player statistics, and comparison tools.")
            )
          )
        )
      )
    ),
    
    # Show main dashboard when data is loaded
    conditionalPanel(
      condition = "output.data_loaded == true",
      tabItems(
      # Team Analysis Tab
      tabItem(tabName = "teams",
        fluidRow(
          box(
            title = "Top Team Attacks", status = "info", solidHeader = TRUE,
            width = 6, height = 400,
            DT::dataTableOutput("team_attacks")
          ),
          box(
            title = "Top Team Defenses", status = "warning", solidHeader = TRUE,
            width = 6, height = 400,
            DT::dataTableOutput("team_defenses")
          )
        ),
        
        fluidRow(
          box(
            title = "Team Attack npxG", status = "info", solidHeader = TRUE,
            width = 6, height = 450,
            plotlyOutput("team_attack_plot", height = "350px")
          ),
          box(
            title = "Team Defense npxG Conceded", status = "warning", solidHeader = TRUE,
            width = 6, height = 450,
            plotlyOutput("team_defense_plot", height = "350px")
          )
        ),
        
        fluidRow(
          box(
            title = "Attack: npxG vs Actual Goals", status = "info", solidHeader = TRUE,
            width = 6, height = 450,
            plotlyOutput("team_attack_scatter", height = "350px")
          ),
          box(
            title = "Defense: npxG vs Goals Conceded", status = "warning", solidHeader = TRUE,
            width = 6, height = 450,
            plotlyOutput("team_defense_scatter", height = "350px")
          )
        )
      ),
      
      # Player Analysis Tab  
      tabItem(tabName = "players",
        fluidRow(
          box(
            title = "Top Players by Position", status = "primary", solidHeader = TRUE,
            width = 12, height = 550,
            DT::dataTableOutput("top_players")
          )
        ),
        
        fluidRow(
          box(
            title = "Player Performance Distribution", status = "info", solidHeader = TRUE,
            width = 6, height = 450,
            plotlyOutput("player_distribution", height = "350px")
          ),
          box(
            title = "Goals vs Expected Goals", status = "warning", solidHeader = TRUE,
            width = 6, height = 450,
            plotlyOutput("goals_vs_xg", height = "350px")
          )
        ),
        
        fluidRow(
          box(
            title = "Top 10 Players by xGI", status = "primary", solidHeader = TRUE,
            width = 12, height = 450,
            plotlyOutput("top_players_xgi", height = "350px")
          )
        )
      ),
      
      # Comparison Tab
      tabItem(tabName = "comparison",
        fluidRow(
          box(
            title = "Direct Comparison", status = "success", solidHeader = TRUE,
            width = 6, height = 450,
            plotlyOutput("direct_comparison", height = "350px")
          ),
          box(
            title = "Performance Radar Chart", status = "warning", solidHeader = TRUE,
            width = 6, height = 450,
            plotlyOutput("radar_comparison", height = "350px")
          )
        ),
        
        fluidRow(
          box(
            title = "Position Performance Overview", status = "primary", solidHeader = TRUE,
            width = 6, height = 450,
            plotlyOutput("position_comparison", height = "350px")
          ),
          box(
            title = "Team vs Individual Performance", status = "info", solidHeader = TRUE,
            width = 6, height = 450,
            plotlyOutput("team_vs_individual", height = "350px")
          )
        )
      )
      ) # Close tabItems
    ) # Close conditionalPanel for data loaded
  ) # Close dashboardBody
) # Close dashboardPage

# Define Server
server <- function(input, output, session) {
  
  # Reactive value to track if data is loaded
  values <- reactiveValues(
    df = if(exists("df") && is.data.frame(df) && nrow(df) > 0) df else NULL,
    data_loaded = exists("df") && is.data.frame(df) && nrow(df) > 0
  )
  
  # Output for conditional panels
  output$data_loaded <- reactive({
    values$data_loaded
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  # Handle file upload
  observeEvent(input$data_file, {
    req(input$data_file)
    
    tryCatch({
      # Read the uploaded CSV
      uploaded_df <- read.csv(input$data_file$datapath, stringsAsFactors = FALSE)
      
      # Basic validation
      required_cols <- c("Season", "GW", "Player", "Squad", "Pos")
      if(all(required_cols %in% names(uploaded_df))) {
        values$df <- uploaded_df
        values$data_loaded <- TRUE
        
        # Also assign to global environment for functions
        assign("df", uploaded_df, envir = .GlobalEnv)
        
        showNotification("Data loaded successfully!", type = "success")
      } else {
        showNotification(
          paste("Missing required columns:", paste(setdiff(required_cols, names(uploaded_df)), collapse = ", ")), 
          type = "error"
        )
      }
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
    })
  })
  
  # Populate team filter choices dynamically
  observe({
    req(values$data_loaded, values$df)
    
    teams <- values$df %>% 
      filter(Season == "2025-2026") %>%
      distinct(Squad) %>% 
      arrange(Squad) %>%
      pull(Squad)
    
    team_choices <- c("All Teams" = "all", setNames(teams, teams))
    
    updateSelectInput(session, "team_filter",
                     choices = team_choices,
                     selected = "all")
    
    # Also populate comparison dropdowns
    updateSelectInput(session, "compare_teams",
                     choices = teams,
                     selected = teams[1:min(3, length(teams))])
  })
  
  # Populate player comparison choices
  observe({
    req(values$data_loaded, values$df)
    analysis <- analysis_data()
    
    if (input$per90) {
      top_players <- fpl_data_p90(values$df,
                                 gameweek = analysis$analysis_from_gw,
                                 pos = input$position_filter,
                                 groupBy = c("Player", "Squad"),
                                 arrangeBy = "xGI_p90") %>%
        head(50) %>%
        mutate(player_label = paste0(Player, " (", Squad, ")"))
    } else {
      top_players <- fpl_data(values$df,
                             gameweek = analysis$analysis_from_gw,
                             pos = input$position_filter,
                             groupBy = c("Player", "Squad"),
                             arrangeBy = "xGI") %>%
        head(50) %>%
        mutate(player_label = paste0(Player, " (", Squad, ")"))
    }
    
    player_choices <- setNames(top_players$Player, top_players$player_label)
    
    updateSelectInput(session, "compare_players",
                     choices = player_choices,
                     selected = names(player_choices)[1:min(3, length(player_choices))])
  })
  
  # Reactive values for analysis period
  analysis_data <- reactive({
    req(values$data_loaded, values$df)
    current_gw <- values$df %>% pull(GW) %>% max()
    analysis_from_gw <- current_gw - input$gameweeks
    
    list(
      current_gw = current_gw,
      analysis_from_gw = analysis_from_gw,
      gameweeks = input$gameweeks
    )
  })
  
  # Team Attacks Table
  output$team_attacks <- DT::renderDataTable({
    analysis <- analysis_data()
    
    if (input$per90) {
      data <- fpl_data_p90(values$df, 
                          gameweek = analysis$analysis_from_gw,
                          pos = input$position_filter,
                          groupBy = c("Squad"),
                          arrangeBy = "xGI_p90") %>%
        select(-mins)  # Remove mins column for per90 view
    } else {
      data <- fpl_data(values$df,
                      gameweek = analysis$analysis_from_gw,
                      pos = input$position_filter,
                      groupBy = c("Squad"),
                      arrangeBy = "xGI")
    }
    
    DT::datatable(data, 
                  options = list(
                    pageLength = 4, 
                    scrollX = TRUE,
                    scrollY = "200px",
                    lengthMenu = list(c(4, 10, 20), c("Top 4", "Top 10", "All Teams")),
                    fixedColumns = list(leftColumns = 1)  # Freeze first column
                  ),
                  rownames = FALSE) %>%
      DT::formatRound(columns = sapply(data, is.numeric), digits = 2)
  })
  
  # Team Defenses Table
  output$team_defenses <- DT::renderDataTable({
    analysis <- analysis_data()
    
    if (input$per90) {
      # Use the existing functions to get defensive stats (xG conceded by opponents)
      data <- fpl_data_p90(values$df, 
                          gameweek = analysis$analysis_from_gw,
                          pos = input$position_filter,
                          groupBy = c("Opponent"),
                          arrangeBy = "xGI_p90") %>%
        rename(Squad = Opponent) %>%
        arrange(xGI_p90) %>%  # Lower is better for defense
        mutate(rank = row_number()) %>%  # Re-add rank after sorting
        select(rank, everything(), -mins)  # Remove mins column for per90 view
    } else {
      data <- fpl_data(values$df,
                      gameweek = analysis$analysis_from_gw,
                      pos = input$position_filter,
                      groupBy = c("Opponent"),
                      arrangeBy = "xGI") %>%
        rename(Squad = Opponent) %>%
        arrange(xGI) %>%  # Lower is better for defense
        mutate(rank = row_number()) %>%  # Re-add rank after sorting
        select(rank, everything())
    }
    
    DT::datatable(data,
                  options = list(
                    pageLength = 4, 
                    scrollX = TRUE,
                    scrollY = "200px",
                    lengthMenu = list(c(4, 10, 20), c("Top 4", "Top 10", "All Teams")),
                    fixedColumns = list(leftColumns = 1)  # Freeze first column
                  ),
                  rownames = FALSE) %>%
      DT::formatRound(columns = sapply(data, is.numeric), digits = 2)
  })
  
  # Top Players Table
  output$top_players <- DT::renderDataTable({
    analysis <- analysis_data()
    
    if (input$per90) {
      data <- fpl_data_p90(values$df,
                          gameweek = analysis$analysis_from_gw,
                          pos = input$position_filter,
                          groupBy = c("Player", "Squad"),
                          arrangeBy = "xGI_p90")
      
      # Remove mins column for per90 view
      if ("mins" %in% names(data)) {
        data <- data %>% select(-mins)
      }
    } else {
      data <- fpl_data(values$df,
                      gameweek = analysis$analysis_from_gw,
                      pos = input$position_filter,
                      groupBy = c("Player", "Squad"),
                      arrangeBy = "xGI")
    }
    
    # Apply team filter
    if (input$team_filter != "all") {
      data <- data %>% filter(Squad == input$team_filter)
    }
    
    DT::datatable(data,
                  options = list(
                    pageLength = 10, 
                    scrollX = TRUE,
                    scrollY = "400px",
                    lengthMenu = list(c(5, 10, 15, 20, 25), c("Top 5", "Top 10", "Top 15", "Top 20", "Top 25")),
                    fixedColumns = list(leftColumns = 2)  # Freeze Player and Squad columns
                  ),
                  rownames = FALSE) %>%
      DT::formatRound(columns = sapply(data, is.numeric), digits = 2)
  })
  
  # Team Attack npxG Plot
  output$team_attack_plot <- renderPlotly({
    analysis <- analysis_data()
    
    if (input$per90) {
      team_data <- fpl_data_p90(values$df,
                               gameweek = analysis$analysis_from_gw,
                               pos = "all",
                               groupBy = c("Squad"),
                               arrangeBy = "npxg_p90") %>%
        head(10)
      
      y_var <- "npxg_p90"
      y_label <- "npxG per 90"
      title_suffix <- "per 90"
    } else {
      team_data <- fpl_data(values$df,
                           gameweek = analysis$analysis_from_gw,
                           pos = "all",
                           groupBy = c("Squad"),
                           arrangeBy = "npxg") %>%
        head(10)
      
      y_var <- "npxg"
      y_label <- "npxG"
      title_suffix <- "Total"
    }
    
    p <- ggplot(team_data, aes_string(x = paste0("reorder(Squad, ", y_var, ")"), y = y_var)) +
      geom_col(fill = "#3498db", alpha = 0.8) +
      coord_flip() +
      labs(title = paste("Team npxG", title_suffix, "- Last", input$gameweeks, "GWs"),
           x = "Team", y = y_label) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p)
  })
  
  # Team Defense npxG Conceded Plot
  output$team_defense_plot <- renderPlotly({
    analysis <- analysis_data()
    
    if (input$per90) {
      defense_data <- fpl_data_p90(values$df,
                                  gameweek = analysis$analysis_from_gw,
                                  pos = "all",
                                  groupBy = c("Opponent"),
                                  arrangeBy = "npxg_p90") %>%
        rename(Squad = Opponent) %>%
        arrange(npxg_p90) %>%  # Lower is better for defense
        head(10)
      
      y_var <- "npxg_p90"
      y_label <- "npxG Conceded per 90"
      title_suffix <- "per 90"
    } else {
      defense_data <- fpl_data(values$df,
                              gameweek = analysis$analysis_from_gw,
                              pos = "all",
                              groupBy = c("Opponent"),
                              arrangeBy = "npxg") %>%
        rename(Squad = Opponent) %>%
        arrange(npxg) %>%  # Lower is better for defense
        head(10)
      
      y_var <- "npxg"
      y_label <- "npxG Conceded"
      title_suffix <- "Total"
    }
    
    p <- ggplot(defense_data, aes_string(x = paste0("reorder(Squad, -", y_var, ")"), y = y_var)) +
      geom_col(fill = "#e74c3c", alpha = 0.8) +
      coord_flip() +
      labs(title = paste("Team npxG Conceded", title_suffix, "- Last", input$gameweeks, "GWs"),
           x = "Team", y = y_label) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p)
  })
  
  # Player Distribution Plot
  output$player_distribution <- renderPlotly({
    analysis <- analysis_data()
    
    if (input$per90) {
      player_data <- fpl_data_p90(values$df,
                                 gameweek = analysis$analysis_from_gw,
                                 pos = input$position_filter,
                                 groupBy = c("Player", "Squad"),
                                 arrangeBy = "xGI_p90") %>%
        head(50)
      
      x_var <- "npxg_p90"
      y_var <- "xA_p90"
      x_label <- "npxG per 90"
      y_label <- "xA per 90"
      title_suffix <- "per 90"
    } else {
      player_data <- fpl_data(values$df,
                             gameweek = analysis$analysis_from_gw,
                             pos = input$position_filter,
                             groupBy = c("Player", "Squad"),
                             arrangeBy = "xGI") %>%
        head(50)
      
      x_var <- "npxg"
      y_var <- "xA"
      x_label <- "npxG"
      y_label <- "xA"
      title_suffix <- "Total"
    }
    
    # Apply team filter
    if (input$team_filter != "all") {
      player_data <- player_data %>% filter(Squad == input$team_filter)
    }
    
    p <- ggplot(player_data, aes_string(x = x_var, y = y_var, text = "paste('Player:', Player, '<br>Team:', Squad)")) +
      geom_point(size = 3, alpha = 0.7, color = "#3498db") +
      labs(title = paste("Player Performance Distribution", title_suffix),
           x = x_label, y = y_label) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p, tooltip = "text")
  })
  
  # Goals vs Expected Goals Plot
  output$goals_vs_xg <- renderPlotly({
    analysis <- analysis_data()
    
    if (input$per90) {
      player_data <- fpl_data_p90(values$df,
                                 gameweek = analysis$analysis_from_gw,
                                 pos = input$position_filter,
                                 groupBy = c("Player", "Squad"),
                                 arrangeBy = "xGI_p90") %>%
        head(30)
      
      x_var <- "npxg_p90"
      y_var <- "gls_p90"
      x_label <- "npxG per 90"
      y_label <- "Goals per 90"
      title_suffix <- "per 90"
    } else {
      player_data <- fpl_data(values$df,
                             gameweek = analysis$analysis_from_gw,
                             pos = input$position_filter,
                             groupBy = c("Player", "Squad"),
                             arrangeBy = "xGI") %>%
        head(30)
      
      x_var <- "npxg"
      y_var <- "goals"
      x_label <- "npxG"
      y_label <- "Goals"
      title_suffix <- "Total"
    }
    
    # Apply team filter
    if (input$team_filter != "all") {
      player_data <- player_data %>% filter(Squad == input$team_filter)
    }
    
    p <- ggplot(player_data, aes_string(x = x_var, y = y_var, text = "paste('Player:', Player, '<br>Team:', Squad)")) +
      geom_point(alpha = 0.7, color = "#27ae60", size = 3) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
      labs(title = paste("Goals vs Expected Goals", title_suffix),
           x = x_label, y = y_label) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p, tooltip = "text")
  })
  
  # Position Comparison Plot
  output$position_comparison <- renderPlotly({
    analysis <- analysis_data()
    
    positions <- c("att", "mid", "def")
    position_data <- map_dfr(positions, function(pos) {
      fpl_data_p90(values$df,
                  gameweek = analysis$analysis_from_gw,
                  pos = pos,
                  groupBy = c("Player", "Squad"),
                  arrangeBy = "xGI_p90") %>%
        head(10) %>%
        mutate(position = case_when(
          pos == "att" ~ "Attackers",
          pos == "mid" ~ "Midfielders", 
          pos == "def" ~ "Defenders"
        ))
    })
    
    p <- ggplot(position_data, aes(x = position, y = xGI_p90, fill = position)) +
      geom_boxplot(alpha = 0.7) +
      scale_fill_manual(values = c("#e74c3c", "#f39c12", "#3498db")) +
      labs(title = "Performance Distribution by Position",
           x = "Position", y = "xG+xA per 90") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  # Team vs Individual Performance Plot
  output$team_vs_individual <- renderPlotly({
    analysis <- analysis_data()
    
    # Get team data
    team_data <- fpl_data_p90(values$df,
                             gameweek = analysis$analysis_from_gw,
                             pos = "all",
                             groupBy = c("Squad"),
                             arrangeBy = "xGI_p90")
    
    # Get top player per team
    top_players <- fpl_data_p90(values$df,
                               gameweek = analysis$analysis_from_gw,
                               pos = "all",
                               groupBy = c("Player", "Squad"),
                               arrangeBy = "xGI_p90") %>%
      group_by(Squad) %>%
      slice_max(xGI_p90, n = 1) %>%
      ungroup()
    
    # Combine data
    combined_data <- team_data %>%
      left_join(top_players %>% select(Squad, top_player_xGI = xGI_p90, Player), by = "Squad") %>%
      filter(!is.na(top_player_xGI))
    
    p <- ggplot(combined_data, aes(x = xGI_p90, y = top_player_xGI, 
                                  text = paste("Team:", Squad, "<br>Top Player:", Player))) +
      geom_point(size = 3, alpha = 0.7, color = "#9b59b6") +
      geom_smooth(method = "lm", se = FALSE, color = "#34495e") +
      labs(title = "Team Performance vs Top Individual Player",
           x = "Team xG+xA per 90", y = "Top Player xG+xA per 90") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Team Attack Scatter: npxG vs Goals
  output$team_attack_scatter <- renderPlotly({
    analysis <- analysis_data()
    
    if (input$per90) {
      team_data <- fpl_data_p90(values$df,
                               gameweek = analysis$analysis_from_gw,
                               pos = "all",
                               groupBy = c("Squad"),
                               arrangeBy = "npxg_p90")
      
      x_var <- "npxg_p90"
      y_var <- "gls_p90"
      x_label <- "npxG per 90"
      y_label <- "Goals per 90"
      title_suffix <- "per 90"
    } else {
      team_data <- fpl_data(values$df,
                           gameweek = analysis$analysis_from_gw,
                           pos = "all",
                           groupBy = c("Squad"),
                           arrangeBy = "npxg")
      
      x_var <- "npxg"
      y_var <- "goals"
      x_label <- "npxG"
      y_label <- "Goals"
      title_suffix <- "Total"
    }
    
    p <- ggplot(team_data, aes_string(x = x_var, y = y_var, text = "paste('Team:', Squad)")) +
      geom_point(size = 3, alpha = 0.7, color = "#3498db") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
      labs(title = paste("Attack Performance", title_suffix, "- Last", input$gameweeks, "GWs"),
           x = x_label, y = y_label) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p, tooltip = "text")
  })
  
  # Team Defense Scatter: npxG vs Goals Conceded
  output$team_defense_scatter <- renderPlotly({
    analysis <- analysis_data()
    
    if (input$per90) {
      defense_data <- fpl_data_p90(values$df,
                                  gameweek = analysis$analysis_from_gw,
                                  pos = "all",
                                  groupBy = c("Opponent"),
                                  arrangeBy = "npxg_p90") %>%
        rename(Squad = Opponent)
      
      x_var <- "npxg_p90"
      y_var <- "gls_p90"
      x_label <- "npxG Conceded per 90"
      y_label <- "Goals Conceded per 90"
      title_suffix <- "per 90"
    } else {
      defense_data <- fpl_data(values$df,
                              gameweek = analysis$analysis_from_gw,
                              pos = "all",
                              groupBy = c("Opponent"),
                              arrangeBy = "npxg") %>%
        rename(Squad = Opponent)
      
      x_var <- "npxg"
      y_var <- "goals"
      x_label <- "npxG Conceded"
      y_label <- "Goals Conceded"
      title_suffix <- "Total"
    }
    
    p <- ggplot(defense_data, aes_string(x = x_var, y = y_var, text = "paste('Team:', Squad)")) +
      geom_point(size = 3, alpha = 0.7, color = "#e74c3c") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
      labs(title = paste("Defense Performance", title_suffix, "- Last", input$gameweeks, "GWs"),
           x = x_label, y = y_label) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p, tooltip = "text")
  })
  
  # Top 10 Players xGI Bar Chart
  output$top_players_xgi <- renderPlotly({
    analysis <- analysis_data()
    
    if (input$per90) {
      player_data <- fpl_data_p90(values$df,
                                 gameweek = analysis$analysis_from_gw,
                                 pos = input$position_filter,
                                 groupBy = c("Player", "Squad"),
                                 arrangeBy = "xGI_p90") %>%
        head(10)
      
      y_var <- "xGI_p90"
      y_label <- "xGI per 90"
      title_suffix <- "per 90"
    } else {
      player_data <- fpl_data(values$df,
                             gameweek = analysis$analysis_from_gw,
                             pos = input$position_filter,
                             groupBy = c("Player", "Squad"),
                             arrangeBy = "xGI") %>%
        head(10)
      
      y_var <- "xGI"
      y_label <- "xGI"
      title_suffix <- "Total"
    }
    
    # Apply team filter
    if (input$team_filter != "all") {
      player_data <- player_data %>% filter(Squad == input$team_filter)
      # If filtering by team results in fewer than 10 players, take all available
      player_data <- player_data %>% head(min(10, nrow(player_data)))
    }
    
    # Create player labels with team names
    player_data <- player_data %>%
      mutate(player_label = paste0(Player, " (", Squad, ")"))
    
    p <- ggplot(player_data, aes_string(x = paste0("reorder(player_label, ", y_var, ")"), y = y_var)) +
      geom_col(fill = "#3498db", alpha = 0.8) +
      coord_flip() +
      labs(title = paste("Top Players by xGI", title_suffix, "- Last", input$gameweeks, "GWs"),
           x = "Player", y = y_label) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 10)
      )
    
    ggplotly(p)
  })
  
  # Direct Comparison Chart
  output$direct_comparison <- renderPlotly({
    analysis <- analysis_data()
    
    if (input$comparison_type == "teams") {
      if (is.null(input$compare_teams) || length(input$compare_teams) < 2) {
        # Show placeholder
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "Select at least 2 teams to compare", size = 6) +
          theme_void()
        return(ggplotly(p))
      }
      
      if (input$team_metrics == "attacking") {
        if (input$per90) {
          comparison_data <- fpl_data_p90(values$df,
                                         gameweek = analysis$analysis_from_gw,
                                         pos = "all",
                                         groupBy = c("Squad"),
                                         arrangeBy = "xGI_p90") %>%
            filter(Squad %in% input$compare_teams)
          
          x_var <- "npxg_p90"
          y_var <- "xA_p90"
          x_label <- "npxG per 90"
          y_label <- "xA per 90"
          title_suffix <- "Attack per 90"
        } else {
          comparison_data <- fpl_data(values$df,
                                     gameweek = analysis$analysis_from_gw,
                                     pos = "all",
                                     groupBy = c("Squad"),
                                     arrangeBy = "xGI") %>%
            filter(Squad %in% input$compare_teams)
          
          x_var <- "npxg"
          y_var <- "xA"
          x_label <- "npxG"
          y_label <- "xA"
          title_suffix <- "Attack Total"
        }
      } else {
        # Defensive metrics - use Opponent grouping
        if (input$per90) {
          comparison_data <- fpl_data_p90(values$df,
                                         gameweek = analysis$analysis_from_gw,
                                         pos = "all",
                                         groupBy = c("Opponent"),
                                         arrangeBy = "npxg_p90") %>%
            rename(Squad = Opponent) %>%
            filter(Squad %in% input$compare_teams)
          
          x_var <- "npxg_p90"
          y_var <- "xA_p90"
          x_label <- "npxG Conceded per 90"
          y_label <- "xA Conceded per 90"
          title_suffix <- "Defense per 90"
        } else {
          comparison_data <- fpl_data(values$df,
                                     gameweek = analysis$analysis_from_gw,
                                     pos = "all",
                                     groupBy = c("Opponent"),
                                     arrangeBy = "npxg") %>%
            rename(Squad = Opponent) %>%
            filter(Squad %in% input$compare_teams)
          
          x_var <- "npxg"
          y_var <- "xA"
          x_label <- "npxG Conceded"
          y_label <- "xA Conceded"
          title_suffix <- "Defense Total"
        }
      }
      
      p <- ggplot(comparison_data, aes_string(x = x_var, y = y_var, color = "Squad", text = "Squad")) +
        geom_point(size = 4, alpha = 0.8) +
        labs(title = paste("Team Comparison", title_suffix),
             x = x_label, y = y_label) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))
      
    } else {
      if (is.null(input$compare_players) || length(input$compare_players) < 2) {
        # Show placeholder
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "Select at least 2 players to compare", size = 6) +
          theme_void()
        return(ggplotly(p))
      }
      
      if (input$per90) {
        comparison_data <- fpl_data_p90(values$df,
                                       gameweek = analysis$analysis_from_gw,
                                       pos = input$position_filter,
                                       groupBy = c("Player", "Squad"),
                                       arrangeBy = "xGI_p90") %>%
          filter(Player %in% input$compare_players)
        
        x_var <- "npxg_p90"
        y_var <- "xA_p90"
        x_label <- "npxG per 90"
        y_label <- "xA per 90"
        title_suffix <- "per 90"
      } else {
        comparison_data <- fpl_data(values$df,
                                   gameweek = analysis$analysis_from_gw,
                                   pos = input$position_filter,
                                   groupBy = c("Player", "Squad"),
                                   arrangeBy = "xGI") %>%
          filter(Player %in% input$compare_players)
        
        x_var <- "npxg"
        y_var <- "xA"
        x_label <- "npxG"
        y_label <- "xA"
        title_suffix <- "Total"
      }
      
      comparison_data <- comparison_data %>%
        mutate(player_label = paste0(Player, " (", Squad, ")"))
      
      p <- ggplot(comparison_data, aes_string(x = x_var, y = y_var, color = "Player", 
                                             text = "paste('Player:', Player, '<br>Team:', Squad)")) +
        geom_point(size = 4, alpha = 0.8) +
        labs(title = paste("Player Comparison", title_suffix),
             x = x_label, y = y_label) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
    }
    
    ggplotly(p, tooltip = "text")
  })
  
  # Radar Chart Comparison
  output$radar_comparison <- renderPlotly({
    analysis <- analysis_data()
    
    if (input$comparison_type == "teams") {
      if (is.null(input$compare_teams) || length(input$compare_teams) < 2) {
        # Show placeholder
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "Select at least 2 teams for radar chart", size = 6) +
          theme_void()
        return(ggplotly(p))
      }
      
      if (input$team_metrics == "attacking") {
        if (input$per90) {
          radar_data <- fpl_data_p90(values$df,
                                    gameweek = analysis$analysis_from_gw,
                                    pos = "all",
                                    groupBy = c("Squad"),
                                    arrangeBy = "xGI_p90") %>%
            filter(Squad %in% input$compare_teams) %>%
            select(Squad, npxg_p90, xA_p90, xGI_p90, gls_p90, ast_p90) %>%
            rename(npxG = npxg_p90, xA = xA_p90, xGI = xGI_p90, Goals = gls_p90, Assists = ast_p90)
        } else {
          radar_data <- fpl_data(values$df,
                                gameweek = analysis$analysis_from_gw,
                                pos = "all",
                                groupBy = c("Squad"),
                                arrangeBy = "xGI") %>%
            filter(Squad %in% input$compare_teams) %>%
            select(Squad, npxg, xA, xGI, goals, assists) %>%
            rename(npxG = npxg, Goals = goals, Assists = assists)
        }
        chart_title <- "Attacking Performance Comparison"
      } else {
        # Defensive metrics
        if (input$per90) {
          radar_data <- fpl_data_p90(values$df,
                                    gameweek = analysis$analysis_from_gw,
                                    pos = "all",
                                    groupBy = c("Opponent"),
                                    arrangeBy = "npxg_p90") %>%
            rename(Squad = Opponent) %>%
            filter(Squad %in% input$compare_teams) %>%
            select(Squad, npxg_p90, xA_p90, xGI_p90, gls_p90, ast_p90) %>%
            rename(`npxG Conceded` = npxg_p90, `xA Conceded` = xA_p90, `xGI Conceded` = xGI_p90,
                   `Goals Conceded` = gls_p90, `Assists Conceded` = ast_p90)
        } else {
          radar_data <- fpl_data(values$df,
                                gameweek = analysis$analysis_from_gw,
                                pos = "all",
                                groupBy = c("Opponent"),
                                arrangeBy = "npxg") %>%
            rename(Squad = Opponent) %>%
            filter(Squad %in% input$compare_teams) %>%
            select(Squad, npxg, xA, xGI, goals, assists) %>%
            rename(`npxG Conceded` = npxg, `xA Conceded` = xA, `xGI Conceded` = xGI,
                   `Goals Conceded` = goals, `Assists Conceded` = assists)
        }
        chart_title <- "Defensive Performance Comparison (Lower is Better)"
      }
      
      # Normalize data for radar chart (0-1 scale)
      radar_data_norm <- radar_data %>%
        mutate(across(-Squad, ~ (.x - min(.x, na.rm = TRUE)) / (max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE))))
      
      # Create a simple bar chart as radar alternative (plotly radar is complex)
      radar_long <- radar_data %>%
        pivot_longer(-Squad, names_to = "Metric", values_to = "Value")
      
      p <- ggplot(radar_long, aes(x = Metric, y = Value, fill = Squad)) +
        geom_col(position = "dodge", alpha = 0.8) +
        labs(title = chart_title) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else {
      if (is.null(input$compare_players) || length(input$compare_players) < 2) {
        # Show placeholder
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "Select at least 2 players for radar chart", size = 6) +
          theme_void()
        return(ggplotly(p))
      }
      
      if (input$per90) {
        radar_data <- fpl_data_p90(values$df,
                                  gameweek = analysis$analysis_from_gw,
                                  pos = input$position_filter,
                                  groupBy = c("Player", "Squad"),
                                  arrangeBy = "xGI_p90") %>%
          filter(Player %in% input$compare_players) %>%
          select(Player, Squad, npxg_p90, xA_p90, xGI_p90, gls_p90, ast_p90) %>%
          rename(npxG = npxg_p90, xA = xA_p90, xGI = xGI_p90, Goals = gls_p90, Assists = ast_p90)
      } else {
        radar_data <- fpl_data(values$df,
                              gameweek = analysis$analysis_from_gw,
                              pos = input$position_filter,
                              groupBy = c("Player", "Squad"),
                              arrangeBy = "xGI") %>%
          filter(Player %in% input$compare_players) %>%
          select(Player, Squad, npxg, xA, xGI, goals, assists) %>%
          rename(npxG = npxg, Goals = goals, Assists = assists)
      }
      
      # Create comparison chart
      radar_long <- radar_data %>%
        pivot_longer(-c(Player, Squad), names_to = "Metric", values_to = "Value")
      
      p <- ggplot(radar_long, aes(x = Metric, y = Value, fill = Player)) +
        geom_col(position = "dodge", alpha = 0.8) +
        labs(title = "Player Performance Comparison by Metrics") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom")
    }
    
    ggplotly(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

# Run the application
shinyApp(ui = ui, server = server)