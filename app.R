#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(tidyr)
library(plotly)
library(DT)

# --- FILE PATH SETUP ---
FILE_PATH <- "NFL_QB_3YR.xlsx"

if (!file.exists(FILE_PATH)) {
  stop(paste("File not found at path:", FILE_PATH))
}

# Read available sheet names
sheet_names <- excel_sheets(FILE_PATH)

# Safely load all sheets into memory
sheets_data <- setNames(lapply(sheet_names, function(s) {
  read_excel(FILE_PATH, sheet = s)
}), sheet_names)

# Full metric dictionary
all_metrics <- c(
  "Total EPA" = "total_epa",
  "EPA per Play" = "epa_play",
  "Player Value" = "player_val",
  "Passing Yards" = "pass_yds",
  "Passing Touchdowns" = "pass_tds",
  "Passer Rating" = "passer_rating",
  "Completion %" = "comp_pct",
  "CPOE (Comp % Over Expected)" = "cpoe",
  "Success Rate %" = "success_pct",
  "Interceptions" = "ints",
  "Sacks" = "sacks",
  "Fumbles" = "fumbles",
  "Air Yards" = "air_yds",
  "Time to Throw (Avg)" = "time_to_throw_avg",
  "Rushing Yards" = "rush_yds",
  "Rushing Touchdowns" = "rush_td",
  "Carries" = "carries",
  "Passing Value" = "passing_val",
  "Rushing Value" = "rushing_val",
  "Scoring Value" = "scoring_val",
  "Turnover Value" = "turnover_val"
)

# --- UI DEFINITION ---
ui <- page_navbar(
  title = span(
    tags$i(class = "fa-solid fa-football", style = "margin-right: 8px; color: #E63946;"),
    "NFL QB Analytics & Valuation Hub"
  ),
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1D3557",
    secondary = "#457B9D",
    success = "#2A9D8F",
    danger = "#E63946",
    bg = "#F8F9FA",
    fg = "#1D3557"
  ),
  fillable = FALSE,
  
  # ------------------------------------------------------------------
  # TAB 1: DASHBOARD OVERVIEW
  # ------------------------------------------------------------------
  nav_panel(
    title = "Dashboard Overview",
    card(
      card_header(class = "bg-dark text-white font-weight-bold", "Dashboard Metric Highlights"),
      fluidRow(
        column(4, selectInput("kpi_stat_select", "Customize 4th KPI Stat Box:", choices = all_metrics, selected = "passer_rating"))
      )
    ),
    layout_column_wrap(
      width = 1/4,
      value_box(
        title = "Total QBs Analyzed",
        value = textOutput("total_qbs"),
        theme = "primary"
      ),
      value_box(
        title = "Top EPA Leader (3-Year)",
        value = textOutput("top_epa_qb"),
        theme = "success"
      ),
      value_box(
        title = "Highest Real Cap Hit",
        value = textOutput("highest_cap_hit"),
        theme = "danger"
      ),
      value_box(
        title = textOutput("custom_kpi_title"),
        value = textOutput("custom_kpi_val"),
        theme = "info"
      )
    ),
    layout_columns(
      col_widths = c(8, 4),
      card(
        card_header(class = "bg-primary text-white font-weight-bold", "Interactive Stat Correlation Explorer"),
        card_body(
          fluidRow(
            column(6, selectInput("dash_x_metric", "X-Axis Statistic:", choices = all_metrics, selected = "pass_yds")),
            column(6, selectInput("dash_y_metric", "Y-Axis Statistic:", choices = all_metrics, selected = "total_epa"))
          ),
          plotlyOutput("overview_scatter_plot", height = "400px")
        )
      ),
      card(
        card_header(class = "bg-secondary text-white font-weight-bold", "Workbook Page Summary"),
        card_body(
          selectInput("overview_sheet_select", "Select Workbook Page:", choices = sheet_names, selected = "3YR-Sum"),
          tableOutput("sheet_summary_table")
        )
      )
    )
  ),
  
  # ------------------------------------------------------------------
  # TAB 2: PLAYER COMPARISON TOOL
  # ------------------------------------------------------------------
  nav_panel(
    title = "Player Comparison",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput("comp_sheet", "Data Source / Timeframe:", choices = c("3YR-Sum", "2YR-Sum", "2025", "2024", "2023"), selected = "3YR-Sum"),
        uiOutput("player_select_ui"),
        selectInput("comp_metric", "Primary Metric (Bar Chart):", choices = all_metrics, selected = "total_epa"),
        selectizeInput("secondary_metrics", "Table Statistics to Compare:",
                       choices = all_metrics,
                       selected = c("pass_yds", "pass_tds", "ints", "epa_play", "cpoe", "passer_rating", "rush_yds", "rush_td"),
                       multiple = TRUE)
      ),
      mainPanel(
        width = 9,
        card(
          card_header(class = "bg-primary text-white", "Head-to-Head Visual Comparison"),
          plotlyOutput("player_comp_bar_chart", height = "360px")
        ),
        card(
          card_header(class = "bg-dark text-white", "Detailed Head-to-Head Matrix"),
          DTOutput("player_comp_table")
        )
      )
    )
  ),
  
  # ------------------------------------------------------------------
  # TAB 3: YEAR-OVER-YEAR PROGRESSION
  # ------------------------------------------------------------------
  nav_panel(
    title = "YoY Progression",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        uiOutput("yoy_player_select_ui"),
        selectInput("yoy_metric", "Primary Trajectory Metric:", choices = all_metrics, selected = "epa_play"),
        selectizeInput("yoy_table_metrics", "Multi-Year Table Statistics:",
                       choices = all_metrics,
                       selected = c("total_plays", "pass_yds", "pass_tds", "ints", "epa_play", "cpoe", "passer_rating", "rush_yds"),
                       multiple = TRUE)
      ),
      mainPanel(
        width = 9,
        card(
          card_header(class = "bg-primary text-white", "Multi-Year Performance Trajectory (2023 - 2025)"),
          plotlyOutput("yoy_line_chart", height = "400px")
        ),
        card(
          card_header(class = "bg-secondary text-white", "Year-by-Year Historical Breakdown"),
          DTOutput("yoy_data_table")
        )
      )
    )
  ),
  
  # ------------------------------------------------------------------
  # TAB 4: CONTRACT VS. PERFORMANCE MATRIX
  # ------------------------------------------------------------------
  nav_panel(
    title = "Contract Valuation Matrix",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput("contract_sheet", "Select Dataset:", choices = c("3YR-Sum", "2YR-Sum"), selected = "3YR-Sum"),
        selectInput("financial_var", "Financial Metric (X-Axis):", choices = c("Cap Hit ($ Millions)" = "cap_hit_m", "Salary ($ Millions)" = "salary_m"), selected = "cap_hit_m"),
        selectInput("performance_var", "Performance Metric (Y-Axis):", choices = all_metrics, selected = "player_val")
      ),
      mainPanel(
        width = 9,
        card(
          card_header(class = "bg-primary text-white", "Contract Value vs. On-Field Output Scatter Matrix"),
          plotlyOutput("contract_matrix_plot", height = "480px")
        )
      )
    )
  ),
  
  # ------------------------------------------------------------------
  # TAB 5: WORKBOOK EXPLORER
  # ------------------------------------------------------------------
  nav_panel(
    title = "Workbook Explorer",
    card(
      card_header(class = "bg-dark text-white", "Full Dataset Browser & Exporter"),
      layout_columns(
        col_widths = c(4, 8),
        selectInput("explorer_sheet", "Select Workbook Page to View:", choices = sheet_names, selected = "NFL_QB"),
        downloadButton("download_csv", "Export View to CSV", class = "btn-danger align-self-end")
      ),
      DTOutput("raw_data_table")
    )
  )
)

# --- SERVER DEFINITION ---
server <- function(input, output, session) {
  
  # Helper function to retrieve sheet & sanitize contract values (< $200M filter)
  get_data <- function(sheet_name) {
    df <- sheets_data[[sheet_name]]
    
    if ("cap_hit" %in% names(df)) {
      df$cap_hit_clean <- ifelse(df$cap_hit > 2e8, NA, df$cap_hit)
      df$cap_hit_m <- df$cap_hit_clean / 1e6
    }
    if ("salary" %in% names(df)) {
      df$salary_clean <- ifelse(df$salary > 2e8, NA, df$salary)
      df$salary_m <- df$salary_clean / 1e6
    }
    return(df)
  }
  
  # Dashboard KPI Outputs
  output$total_qbs <- renderText({
    df <- get_data("3YR-Sum")
    nrow(df)
  })
  
  output$top_epa_qb <- renderText({
    df <- get_data("3YR-Sum")
    if ("total_epa" %in% names(df)) {
      top_row <- df %>% arrange(desc(total_epa)) %>% slice(1)
      paste0(top_row$name, " (", round(top_row$total_epa, 1), ")")
    } else {
      "N/A"
    }
  })
  
  output$highest_cap_hit <- renderText({
    df <- get_data("3YR-Sum")
    if ("cap_hit_clean" %in% names(df)) {
      max_val <- max(df$cap_hit_clean, na.rm = TRUE)
      paste0("$", sprintf("%.1f", max_val / 1e6), "M")
    } else {
      "N/A"
    }
  })
  
  # Dynamic 4th Custom KPI Box
  output$custom_kpi_title <- renderText({
    stat_name <- names(all_metrics)[which(all_metrics == input$kpi_stat_select)]
    paste("Avg", stat_name, "(2025)")
  })
  
  output$custom_kpi_val <- renderText({
    df <- get_data("2025")
    if (input$kpi_stat_select %in% names(df)) {
      avg_val <- mean(df[[input$kpi_stat_select]], na.rm = TRUE)
      round(avg_val, 2)
    } else {
      "N/A"
    }
  })
  
  # Dynamic Dashboard Overview Scatter Plot
  output$overview_scatter_plot <- renderPlotly({
    req(input$dash_x_metric, input$dash_y_metric)
    df <- get_data("3YR-Sum")
    
    x_label <- names(all_metrics)[which(all_metrics == input$dash_x_metric)]
    y_label <- names(all_metrics)[which(all_metrics == input$dash_y_metric)]
    
    plot_ly(df, x = as.formula(paste0("~", input$dash_x_metric)), 
            y = as.formula(paste0("~", input$dash_y_metric)), 
            text = ~name, type = 'scatter', mode = 'markers',
            marker = list(size = 11, color = ~get(input$dash_y_metric), colorscale = 'Viridis', showscale = TRUE,
                          line = list(color = '#1D3557', width = 1))) %>%
      layout(
        xaxis = list(title = x_label, gridcolor = '#EAEAEA'),
        yaxis = list(title = y_label, gridcolor = '#EAEAEA'),
        hovermode = "closest",
        plot_bgcolor = "#FAFAFA"
      )
  })
  
  # Summary Table Output
  output$sheet_summary_table <- renderTable({
    df <- get_data(input$overview_sheet_select)
    data.frame(
      "Property" = c("Total Players / Rows", "Total Metrics / Columns", "Sample QBs"),
      "Value" = c(nrow(df), ncol(df), paste(head(df$name, 4), collapse = ", "))
    )
  })
  
  # Player Select UI for Comparison Tab
  output$player_select_ui <- renderUI({
    df <- get_data(input$comp_sheet)
    players <- sort(unique(df$name))
    selectInput("selected_players", "Select QBs to Compare:", 
                choices = players, 
                selected = head(players, 4), 
                multiple = TRUE)
  })
  
  # Player Comparison Bar Chart
  output$player_comp_bar_chart <- renderPlotly({
    req(input$selected_players, input$comp_metric)
    df <- get_data(input$comp_sheet) %>% filter(name %in% input$selected_players)
    metric_label <- names(all_metrics)[which(all_metrics == input$comp_metric)]
    
    plot_ly(df, x = ~name, y = as.formula(paste0("~", input$comp_metric)), 
            type = 'bar', color = ~name, colors = "Dark2") %>%
      layout(
        xaxis = list(title = "Quarterback"),
        yaxis = list(title = metric_label),
        showlegend = FALSE,
        plot_bgcolor = "#FAFAFA"
      )
  })
  
  # Player Comparison Data Table
  output$player_comp_table <- renderDT({
    req(input$selected_players)
    df <- get_data(input$comp_sheet) %>% filter(name %in% input$selected_players)
    
    cols_to_show <- unique(c("name", input$comp_metric, input$secondary_metrics))
    cols_to_show <- intersect(cols_to_show, names(df))
    
    datatable(df[, cols_to_show, drop = FALSE], 
              options = list(dom = 't', pageLength = 15, scrollX = TRUE), 
              rownames = FALSE, 
              class = 'cell-border stripe hover')
  })
  
  # YoY Player Select UI
  output$yoy_player_select_ui <- renderUI({
    df <- get_data("NFL_QB")
    players <- sort(unique(df$name))
    selectInput("yoy_players", "Select QBs to Track:", choices = players, selected = head(players, 3), multiple = TRUE)
  })
  
  # YoY Progression Line Chart
  output$yoy_line_chart <- renderPlotly({
    req(input$yoy_players, input$yoy_metric)
    df <- get_data("NFL_QB") %>% filter(name %in% input$yoy_players)
    metric_label <- names(all_metrics)[which(all_metrics == input$yoy_metric)]
    
    plot_ly(df, x = ~year, y = as.formula(paste0("~", input$yoy_metric)), color = ~name, 
            type = 'scatter', mode = 'lines+markers', marker = list(size = 9),
            line = list(width = 3)) %>%
      layout(
        xaxis = list(title = "NFL Season", dtick = 1),
        yaxis = list(title = metric_label),
        plot_bgcolor = "#FAFAFA"
      )
  })
  
  # YoY Data Table Breakdown
  output$yoy_data_table <- renderDT({
    req(input$yoy_players)
    df <- get_data("NFL_QB") %>% filter(name %in% input$yoy_players)
    
    cols_to_show <- unique(c("year", "name", "team", input$yoy_metric, input$yoy_table_metrics))
    cols_to_show <- intersect(cols_to_show, names(df))
    
    datatable(df[, cols_to_show, drop = FALSE],
              options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE,
              class = 'cell-border stripe hover')
  })
  
  # Contract Matrix Scatter Plot
  output$contract_matrix_plot <- renderPlotly({
    df <- get_data(input$contract_sheet)
    req(input$financial_var, input$performance_var)
    
    x_var <- input$financial_var
    y_var <- input$performance_var
    
    df_filtered <- df %>% filter(!is.na(.data[[x_var]]))
    
    x_label <- ifelse(x_var == "cap_hit_m", "Cap Hit ($ Millions)", "Salary ($ Millions)")
    y_label <- names(all_metrics)[which(all_metrics == y_var)]
    
    plot_ly(df_filtered, x = as.formula(paste0("~", x_var)), 
            y = as.formula(paste0("~", y_var)), 
            text = ~paste0("QB: ", name, "<br>", x_label, ": $", sprintf("%.2f", get(x_var)), "M<br>", y_label, ": ", sprintf("%.2f", get(y_var))),
            hoverinfo = 'text',
            type = 'scatter', mode = 'markers+text',
            textposition = 'top center',
            marker = list(size = 12, color = as.formula(paste0("~", y_var)), colorscale = 'Portland', showscale = TRUE,
                          line = list(color = '#000000', width = 1))) %>%
      layout(
        xaxis = list(title = x_label, gridcolor = '#EAEAEA', tickprefix = "$", ticksuffix = "M"),
        yaxis = list(title = y_label, gridcolor = '#EAEAEA'),
        plot_bgcolor = "#FAFAFA",
        hovermode = "closest"
      )
  })
  
  # --- WORKBOOK EXPLORER TABLE OUTPUT WITH ACCURATE COLUMN FORMATTING ---
  output$raw_data_table <- renderDT({
    df <- get_data(input$explorer_sheet)
    
    # Replace $10 Trillion placeholders in salary/cap_hit columns if present
    if ("salary" %in% names(df)) df$salary <- ifelse(df$salary > 2e8, NA, df$salary)
    if ("cap_hit" %in% names(df)) df$cap_hit <- ifelse(df$cap_hit > 2e8, NA, df$cap_hit)
    
    # Exclude internal helper columns from DT view
    df_display <- df %>% select(-any_of(c("cap_hit_clean", "salary_clean", "cap_hit_m", "salary_m")))
    
    num_cols <- names(df_display)[sapply(df_display, is.numeric)]
    
    # Separate money columns vs integer year columns vs general stats
    money_cols <- intersect(c("salary", "cap_hit"), num_cols)
    year_cols  <- intersect(c("year", "free_agent", "seasons_played"), num_cols)
    stat_cols  <- setdiff(num_cols, c(money_cols, year_cols))
    
    dt <- datatable(
      df_display, 
      options = list(pageLength = 15, scrollX = TRUE, filter = "top"), 
      class = 'cell-border stripe hover',
      rownames = FALSE
    )
    
    # Format salary / cap_hit with commas and dollar signs
    if (length(money_cols) > 0) {
      dt <- dt %>% formatCurrency(columns = money_cols, currency = "$", digits = 2)
    }
    
    # Format general statistical metrics to 2 decimal places
    if (length(stat_cols) > 0) {
      dt <- dt %>% formatRound(columns = stat_cols, digits = 2)
    }
    
    # Ensure year and seasons columns remain plain integers without decimals or commas
    if (length(year_cols) > 0) {
      dt <- dt %>% formatRound(columns = year_cols, digits = 0)
    }
    
    return(dt)
  })
  
  # Download Handler for Explorer Tab
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0(input$explorer_sheet, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(get_data(input$explorer_sheet), file, row.names = FALSE)
    }
  )
}

# Launch Application
shinyApp(ui = ui, server = server)