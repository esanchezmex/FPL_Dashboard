#### TABLE TEMPLATE

library(gt)
library(gtExtras)

basic_table <- function(data, title) {
  
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  numeric_cols <- setdiff(numeric_cols, "rank")
  
  data %>% 
    head(50) %>% 
    gt() %>% 
    tab_header(
      title = paste0("Last ", title, " GWs"), 
      # subtitle = md("PL Attacking Data")
    ) %>%
    opt_align_table_header(align = "center") %>%
    cols_align(align = "left") %>% 
    cols_label(
      rank = "PL Rank"
    ) %>% 
    tab_style(
      style = cell_borders(sides = c("bottom", "top"), # all, left, right, top, bottom
                           color = "grey",
                           weight = px(2)),
      locations = list(cells_body(columns = everything()),
                       cells_column_labels(columns = everything())
      )
    ) %>%
    tab_style(
      style = cell_borders(sides = c("top", "bottom"), color = "grey", weight = px(2)),
      locations = list(cells_title(groups = "title"), 
                       cells_footnotes())
    ) %>% 
    tab_style(
      style = cell_borders(sides = c("bottom"), color = "grey", weight = px(2)),
      locations = list(cells_title(groups = "subtitle"),
                       cells_source_notes())
    ) %>% 
    # cols_label( 
    #   squad_image = 'Squad'
    # ) %>% 
    tab_source_note( 
      md("*Data from FBref*")
    ) %>%
    # gt_img_rows(
    #   columns = squad_image, 
    #   img_source = "local"
    # ) %>% 
    data_color(
      # method = "numeric",
      columns = numeric_cols,
      palette = c("#FAFAFA", "#189ad3"),
      reverse = FALSE
    )
}
