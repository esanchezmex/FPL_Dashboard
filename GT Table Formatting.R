#### GT INTRO

# install.packages("gt")
library(gt)
library(dplyr)

#### TBLE
fra_pens %>% 
  arrange(desc(pct)) %>% 
  gt(
    # ADD STUB: rowname_col = "player_name"
    ) %>% 
  
  
  # ADD TITLE (HEADER)
  tab_header(
    title = md("Penalty **percentages**"), # md = Markdown
    subtitle = md("France World Cup Final Squad")
  ) %>% 
  
  
  # ALGIN HEADER
  opt_align_table_header(align = "center") %>% #align table header (left, center, right)
  
  
  # CHANGE COLUMN LABELS
  cols_label( # Change column names
    pct = md('**Percentage**'),
    pens_made = md("Pens **Made**"),
    pens_att = md("Pens **Attempted**"),
    player_name = md("**Player Name**")
  ) %>% 
  
  
  # CHANGE COLUMN WIDTHS
  cols_width( 
    player_name ~ px(200),
    pct ~ px(100),
    pens_made ~ px(100),
    pens_att ~ px(100)
    # ALT: everything() ~ px(280) 
  ) %>% 
  
  
  # ALIGN COLUMN HEADERS
  cols_align(align = "left") %>% 
  
  
  
  # EDIT CELL BORDERS
  tab_style(
    style = cell_borders(sides = c("bottom", "top"), # all, left, right, top, bottom
                         color = "grey",
                         weight = px(2)),
    locations = list(cells_body(columns = everything()),
                     cells_column_labels(columns = everything())
                     )
  ) %>%
  # Since title and subtitle are treated differently
  tab_style(
    style = cell_borders(sides = c("top"), color = "grey", weight = px(2)),
    locations = list(cells_title(groups = "title"), 
                     cells_footnotes())
  ) %>% 
  tab_style(
    style = cell_borders(sides = c("bottom"), color = "grey", weight = px(2)),
    locations = list(cells_title(groups = "subtitle"),
                     cells_source_notes())
  ) %>% 
  
  
  
  # FORMAT NUMBERS
  fmt_number( 
    columns = everything(),
    decimals = 2 # Default = 2
  ) %>% 
  fmt_number( 
    columns = c(pens_made, pens_att),
    decimals = 0
  ) %>% 
  
  
  
  # ADD SOURCE NOTE AT BOTTOM 
  tab_source_note( 
    md("*Data from December 2022*")
  ) %>% 
  
  
  # ADD FOOTNOTE (USE AS TEMPLATE AND COPY FOR MULTIPLE FOOTNOTES)
  tab_footnote(
    footnote = "Griezmann is shit pen-taker",
    locations =  cells_body(columns = player_name, rows = 12)
  ) %>% 
  
  
  # ADD SUMMARY ROWS for ENTIRE TABLE
  # grand_summary_rows(
  #   sides = "top", # to add rows at the top, bottom
  #   columns = c(pens_made, pens_att, pct),
  #   fns = list(
  #     Mean = ~mean(.)
  #     # Total = ~sum(.),
  #     # Median = ~median(.)
  #   )
  # ) %>% 


  # ADD IMAGES
  gt_img_rows(
    columns = Squad, 
    img_source = "local"
  ) %>% 
  
  
  # STYLE TABLE
  
    # Using tab_style
  # tab_style(
  #   locations = cells_body(
  #     columns = pct,
  #     values = 0.88
  #   ),
  #   style = list(
  #     cell_fill(color = "orange"),
  #     cell_text(color = "blue")
  #                )
  # ) %>% 


# FONTS

  # opt_table_font(font = "Helvetica",
  #                font = google_font("Montserrat")) %>%

    # Using data_color
  data_color(
    columns = pct,
    method = "numeric", 
    
    palette = c("#04414E", "aquamarine"),
    # palette = "ggsci::cyan_material", #AnyColor_material should work
    # palette = brewer.pal(nrow(fra_pens), "RdPu"),
    
    # MANUAL ALTENRATIVE
    # colors = scales::col_numeric(
    #   palette = c("white", "lightgreen", "green"),
    #   domain = NULL),
    
    reverse = FALSE # If TRUE smallest value is darkest
  ) %>% 
  
  
  # ADJUST TABLE WIDTH
  tab_options(
    table.width = px(600)
    ) %>% 
  
  # SAVE TABLE
  gtsave(filename = "france_pens.png", # PDF cant save color...
         path = "/Users/estebansanchez/Dropbox/Mac/Desktop/")



# COLOR PALLETTES
?data_color
library(RColorBrewer)
display.brewer.all()





