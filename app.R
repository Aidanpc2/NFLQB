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

# --- PUBLISHABLE RELATIVE FILE PATH SETUP ---
FILE_PATH <- "NFL_QB_3YR.xlsx"

if (!file.exists(FILE_PATH)) {
  if (file.exists(file.path("data", FILE_PATH))) {
    FILE_PATH <- file.path("data", FILE_PATH)
  } else {
    stop(paste("File missing:", FILE_PATH, "- Please place 'NFL_QB_3YR.xlsx' in the app directory."))
  }
}

# Read available sheet names
sheet_names <- excel_sheets(FILE_PATH)

# Load all sheets into memory
sheets_data <- setNames(lapply(sheet_names, function(s) {
  read_excel(FILE_PATH, sheet = s)
}), sheet_names)

# Comprehensive metric dictionary
all_metrics <- c(
  "Player Value" = "player_val",
  "Adv Player Value" = "adv_player_val",
  "Adv Val Fixed" = "adv_val_fixed",
  "Adv Val Dynamic" = "adv_val_dynamic",
  "Player Val Dynamic" = "player_val_dynamic",
  "Player Val (No Rush)" = "player_val_norush",
  "Passing Value" = "passing_val",
  "Rushing Value" = "rushing_val",
  "Scoring Value" = "scoring_val",
  "Turnover Value" = "turnover_val",
  "Total EPA" = "total_epa",
  "EPA per Play" = "epa_play",
  "CPOE (Comp % Over Expected)" = "cpoe",
  "Success Rate %" = "success_pct",
  "Passer Rating" = "passer_rating",
  "Completion %" = "comp_pct",
  "Net Yards per Attempt (NYPA)" = "nypa",
  "TD Rate %" = "td_rate",
  "Time to Throw (Avg)" = "time_to_throw_avg",
  "Total Plays" = "total_plays",
  "Completions" = "completions",
  "Passing Yards" = "pass_yds",
  "Total Yards" = "total_yds",
  "Passing Touchdowns" = "pass_tds",
  "Total Touchdowns" = "total_tds",
  "Interceptions" = "ints",
  "First Downs" = "first_downs",
  "Air Yards" = "air_yds",
  "Longest Pass" = "longest_pass",
  "Pressures" = "pressures",
  "Sacks" = "sacks",
  "Fumbles" = "fumbles",
  "Rushing Yards" = "rush_yds",
  "Rushing Touchdowns" = "rush_td",
  "Carries" = "carries",
  "Yards per Carry (YPC)" = "ypc",
  "EPA per Carry" = "epa_carry",
  "Longest Rush" = "longest_rush",
  "Redzone Carries" = "rz_carries",
  "Redzone Carry %" = "rz_carrypct",
  "Goal Line Carries" = "gl_carries",
  "Goal Line Carry %" = "gl_carrypct",
  "Cap Hit" = "cap_hit",
  "Salary" = "salary",
  "Seasons Played" = "seasons_played",
  "Free Agent Year" = "free_agent"
)

# Extract master list of unique QB names
all_qb_names <- sort(unique(sheets_data[["3YR-Sum"]]$name))

# --- UI DEFINITION ---
ui <- page_navbar(
  id = "main_navbar",
  title = span(
    tags$head(tags$link(rel = "manifest", href = "manifest.json")),
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
    value = "tab_dashboard",
    layout_column_wrap(
      width = 1/4,
      value_box(
        title = "Top Value QB (Bang for Buck)",
        value = textOutput("top_value_qb"),
        subtitle = actionButton("btn_top_value", "View Profile", class = "btn-sm btn-light mt-1"),
        theme = "success"
      ),
      value_box(
        title = "3-Year EPA Leader",
        value = textOutput("top_epa_qb"),
        subtitle = actionButton("btn_top_epa", "View Profile", class = "btn-sm btn-light mt-1"),
        theme = "primary"
      ),
      value_box(
        title = "Highest EPA/Play Leader",
        value = textOutput("top_eff_qb"),
        subtitle = actionButton("btn_top_eff", "View Profile", class = "btn-sm btn-light mt-1"),
        theme = "info"
      ),
      value_box(
        title = "Max Cap Hit vs Median",
        value = textOutput("cap_benchmark"),
        subtitle = "Highest Cap Hit vs. League Median Cap Hit",
        theme = "danger"
      )
    ),
    layout_columns(
      col_widths = c(8, 4),
      card(
        card_header(class = "bg-primary text-white font-weight-bold", "Interactive Stat Correlation Explorer (Click any dot to view profile)"),
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
  # NEW TAB 2: INDIVIDUAL PLAYER PROFILE
  # ------------------------------------------------------------------
  nav_panel(
    title = "Player Profile",
    value = "tab_profile",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput("profile_qb_select", "Select Quarterback:", choices = all_qb_names, selected = "J.Dart"),
        hr(),
        uiOutput("profile_bio_card")
      ),
      mainPanel(
        width = 9,
        layout_column_wrap(
          width = 1/3,
          value_box(title = "3-Year Total EPA", value = textOutput("prof_epa"), theme = "primary"),
          value_box(title = "3-Year Pass Yards", value = textOutput("prof_yds"), theme = "success"),
          value_box(title = "3-Year Pass TDs / INTs", value = textOutput("prof_td_int"), theme = "info")
        ),
        card(
          card_header(class = "bg-primary text-white font-weight-bold", "Multi-Year Career Performance Trajectory"),
          plotlyOutput("profile_chart", height = "320px")
        ),
        card(
          card_header(class = "bg-dark text-white font-weight-bold", "Complete Multi-Year Season Breakdown"),
          DTOutput("profile_table")
        )
      )
    )
  ),
  
  # ------------------------------------------------------------------
  # TAB 3: PLAYER COMPARISON TOOL
  # ------------------------------------------------------------------
  nav_panel(
    title = "Player Comparison",
    value = "tab_comparison",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput("comp_sheet", "Data Source / Timeframe:", choices = c("3YR-Sum", "2YR-Sum", "2025", "2024", "2023"), selected = "3YR-Sum"),
        uiOutput("player_select_ui"),
        selectInput("comp_metric", "Primary Metric (Bar Chart):", choices = all_metrics, selected = "total_epa"),
        selectizeInput("secondary_metrics", "Table Statistics to Compare:",
                       choices = all_metrics,
                       selected = c("pass_yds", "pass_tds", "ints", "epa_play", "cpoe", "passer_rating", "rush_yds", "rush_td", "rz_carries", "sacks"),
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
  # TAB 4: YEAR-OVER-YEAR PROGRESSION
  # ------------------------------------------------------------------
  nav_panel(
    title = "YoY Progression",
    value = "tab_yoy",
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
  # TAB 5: CONTRACT VS. PERFORMANCE MATRIX
  # ------------------------------------------------------------------
  nav_panel(
    title = "Contract Valuation Matrix",
    value = "tab_contract",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput("contract_sheet", "Select Dataset:", choices = c("3YR-Sum", "2YR-Sum"), selected = "3YR-Sum"),
        selectInput("financial_var", "Financial Metric (X-Axis):", choices = c("Cap Hit ($ Millions)" = "cap_hit_m", "Salary ($ Millions)" = "salary_m"), selected = "cap_hit_m"),
        selectInput("performance_var", "Performance Metric (Y-Axis):", choices = all_metrics, selected = "player_val"),
        hr(),
        tags$b("Filter Players on Scatter Plot:"),
        uiOutput("contract_player_filter_ui"),
        actionButton("select_all_contract", "Select All", class = "btn-sm btn-outline-primary mb-2"),
        actionButton("clear_all_contract", "Clear All", class = "btn-sm btn-outline-secondary mb-2")
      ),
      mainPanel(
        width = 9,
        card(
          card_header(class = "bg-primary text-white", "Contract Value vs. On-Field Output Scatter Matrix (Click any dot to view profile)"),
          plotlyOutput("contract_matrix_plot", height = "480px")
        )
      )
    )
  ),
  
  # ------------------------------------------------------------------
  # TAB 6: WORKBOOK EXPLORER
  # ------------------------------------------------------------------
  nav_panel(
    title = "Workbook Explorer",
    value = "tab_explorer",
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
  
  # Helper function to retrieve sheet & sanitize contract values
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
  
  # Reactive reactive values for tracking top leaders
  top_qbs <- reactiveValues(value = "J.Dart", epa = "B.Purdy", eff = "B.Purdy")
  
  # HOME TAB KPI BOXES
  output$top_value_qb <- renderText({
    df <- get_data("3YR-Sum")
    df_valid <- df %>% filter(!is.na(cap_hit_clean) & cap_hit_clean > 1e6 & player_val > 0)
    if (nrow(df_valid) > 0) {
      df_valid$val_per_cap <- df_valid$player_val / df_valid$cap_hit_m
      top_row <- df_valid %>% arrange(desc(val_per_cap)) %>% slice(1)
      top_qbs$value <- top_row$name
      paste0(top_row$name, " (", round(top_row$val_per_cap, 2), " Val/$M)")
    } else {
      "N/A"
    }
  })
  
  output$top_epa_qb <- renderText({
    df <- get_data("3YR-Sum")
    if ("total_epa" %in% names(df)) {
      top_row <- df %>% arrange(desc(total_epa)) %>% slice(1)
      top_qbs$epa <- top_row$name
      paste0(top_row$name, " (", round(top_row$total_epa, 1), " EPA)")
    } else {
      "N/A"
    }
  })
  
  output$top_eff_qb <- renderText({
    df <- get_data("3YR-Sum")
    if ("epa_play" %in% names(df) && "total_plays" %in% names(df)) {
      top_row <- df %>% filter(total_plays >= 200) %>% arrange(desc(epa_play)) %>% slice(1)
      top_qbs$eff <- top_row$name
      paste0(top_row$name, " (", round(top_row$epa_play, 2), " EPA/Play)")
    } else {
      "N/A"
    }
  })
  
  output$cap_benchmark <- renderText({
    df <- get_data("3YR-Sum")
    if ("cap_hit_clean" %in% names(df)) {
      max_cap <- max(df$cap_hit_clean, na.rm = TRUE) / 1e6
      med_cap <- median(df$cap_hit_clean, na.rm = TRUE) / 1e6
      paste0("$", sprintf("%.1f", max_cap), "M / $", sprintf("%.1f", med_cap), "M")
    } else {
      "N/A"
    }
  })
  
  # --- CLICK HANDLERS TO JUMP TO PLAYER PROFILE ---
  observeEvent(input$btn_top_value, {
    updateSelectInput(session, "profile_qb_select", selected = top_qbs$value)
    nav_select("main_navbar", "tab_profile")
  })
  
  observeEvent(input$btn_top_epa, {
    updateSelectInput(session, "profile_qb_select", selected = top_qbs$epa)
    nav_select("main_navbar", "tab_profile")
  })
  
  observeEvent(input$btn_top_eff, {
    updateSelectInput(session, "profile_qb_select", selected = top_qbs$eff)
    nav_select("main_navbar", "tab_profile")
  })
  
  # Listen for click events on Dashboard Overview Plot
  observeEvent(event_data("plotly_click", source = "dash_plot"), {
    click_data <- event_data("plotly_click", source = "dash_plot")
    if (!is.null(click_data) && !is.null(click_data$key)) {
      updateSelectInput(session, "profile_qb_select", selected = click_data$key)
      nav_select("main_navbar", "tab_profile")
    }
  })
  
  # Listen for click events on Contract Matrix Plot
  observeEvent(event_data("plotly_click", source = "contract_plot"), {
    click_data <- event_data("plotly_click", source = "contract_plot")
    if (!is.null(click_data) && !is.null(click_data$key)) {
      updateSelectInput(session, "profile_qb_select", selected = click_data$key)
      nav_select("main_navbar", "tab_profile")
    }
  })
  
  # --- PLAYER PROFILE LOGIC ---
  output$profile_bio_card <- renderUI({
    req(input$profile_qb_select)
    df_3yr <- get_data("3YR-Sum") %>% filter(name == input$profile_qb_select)
    df_nfl <- get_data("NFL_QB") %>% filter(name == input$profile_qb_select)
    
    current_team <- if (nrow(df_nfl) > 0 && "team" %in% names(df_nfl)) tail(df_nfl$team, 1) else "N/A"
    cap_hit_str <- if (nrow(df_3yr) > 0 && !is.na(df_3yr$cap_hit_clean[1])) paste0("$", sprintf("%.2f", df_3yr$cap_hit_m[1]), "M") else "N/A"
    salary_str <- if (nrow(df_3yr) > 0 && !is.na(df_3yr$salary_clean[1])) paste0("$", sprintf("%.2f", df_3yr$salary_m[1]), "M") else "N/A"
    fa_year <- if (nrow(df_3yr) > 0) df_3yr$free_agent[1] else "N/A"
    pv_score <- if (nrow(df_3yr) > 0) round(df_3yr$player_val[1], 2) else "N/A"
    
    tagList(
      h4(class = "text-primary font-weight-bold", input$profile_qb_select),
      tags$ul(class = "list-group list-group-flush",
              tags$li(class = "list-group-item d-flex justify-content-between align-items-center", "Current Team", span(class = "badge bg-primary rounded-pill", current_team)),
              tags$li(class = "list-group-item d-flex justify-content-between align-items-center", "Cap Hit", span(class = "badge bg-danger rounded-pill", cap_hit_str)),
              tags$li(class = "list-group-item d-flex justify-content-between align-items-center", "Salary", span(class = "badge bg-secondary rounded-pill", salary_str)),
              tags$li(class = "list-group-item d-flex justify-content-between align-items-center", "Free Agent Year", span(class = "badge bg-warning text-dark rounded-pill", fa_year)),
              tags$li(class = "list-group-item d-flex justify-content-between align-items-center", "Player Value Score", span(class = "badge bg-success rounded-pill", pv_score))
      )
    )
  })
  
  output$prof_epa <- renderText({
    df <- get_data("3YR-Sum") %>% filter(name == input$profile_qb_select)
    if (nrow(df) > 0 && "total_epa" %in% names(df)) round(df$total_epa[1], 1) else "N/A"
  })
  
  output$prof_yds <- renderText({
    df <- get_data("3YR-Sum") %>% filter(name == input$profile_qb_select)
    if (nrow(df) > 0 && "pass_yds" %in% names(df)) format(df$pass_yds[1], big.mark = ",") else "N/A"
  })
  
  output$prof_td_int <- renderText({
    df <- get_data("3YR-Sum") %>% filter(name == input$profile_qb_select)
    if (nrow(df) > 0 && "pass_tds" %in% names(df)) paste0(df$pass_tds[1], " / ", df$ints[1]) else "N/A"
  })
  
  output$profile_chart <- renderPlotly({
    req(input$profile_qb_select)
    df <- get_data("NFL_QB") %>% filter(name == input$profile_qb_select)
    
    if (nrow(df) > 0) {
      plot_ly(df, x = ~year, y = ~pass_yds, type = 'bar', name = 'Passing Yards', marker = list(color = '#1D3557')) %>%
        add_trace(y = ~total_epa, type = 'scatter', mode = 'lines+markers', yaxis = 'y2', name = 'Total EPA', line = list(color = '#E63946', width = 3)) %>%
        layout(
          xaxis = list(title = "Season", dtick = 1),
          yaxis = list(title = "Passing Yards"),
          yaxis2 = list(title = "Total EPA", overlaying = "y", side = "right"),
          legend = list(orientation = "h", x = 0.3, y = 1.1),
          plot_bgcolor = "#FAFAFA"
        )
    }
  })
  
  output$profile_table <- renderDT({
    req(input$profile_qb_select)
    df_nfl <- get_data("NFL_QB") %>% filter(name == input$profile_qb_select)
    df_3yr <- get_data("3YR-Sum") %>% filter(name == input$profile_qb_select)
    if (nrow(df_3yr) > 0) df_3yr$year <- "3YR-Sum"
    
    cols_common <- intersect(names(df_nfl), names(df_3yr))
    df_combined <- bind_rows(df_nfl[, cols_common], df_3yr[, cols_common])
    
    num_cols <- names(df_combined)[sapply(df_combined, is.numeric)]
    stat_cols <- setdiff(num_cols, c("year", "free_agent", "seasons_played"))
    
    dt <- datatable(df_combined, options = list(dom = 't', scrollX = TRUE), rownames = FALSE, class = 'cell-border stripe hover')
    if (length(stat_cols) > 0) dt <- dt %>% formatRound(columns = stat_cols, digits = 2)
    return(dt)
  })
  
  # Dashboard Overview Scatter Plot
  output$overview_scatter_plot <- renderPlotly({
    req(input$dash_x_metric, input$dash_y_metric)
    df <- get_data("3YR-Sum")
    
    x_label <- names(all_metrics)[which(all_metrics == input$dash_x_metric)]
    y_label <- names(all_metrics)[which(all_metrics == input$dash_y_metric)]
    
    plot_ly(df, x = as.formula(paste0("~", input$dash_x_metric)), 
            y = as.formula(paste0("~", input$dash_y_metric)), 
            text = ~name, key = ~name, source = "dash_plot",
            type = 'scatter', mode = 'markers',
            marker = list(size = 11, color = ~get(input$dash_y_metric), colorscale = 'Viridis', showscale = TRUE,
                          line = list(color = '#1D3557', width = 1))) %>%
      layout(
        xaxis = list(title = x_label, gridcolor = '#EAEAEA'),
        yaxis = list(title = y_label, gridcolor = '#EAEAEA'),
        hovermode = "closest",
        plot_bgcolor = "#FAFAFA"
      )
  })
  
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
  
  output$player_comp_table <- renderDT({
    req(input$selected_players)
    df <- get_data(input$comp_sheet) %>% filter(name %in% input$selected_players)
    
    cols_to_show <- unique(c("name", input$comp_metric, input$secondary_metrics))
    cols_to_show <- intersect(cols_to_show, names(df))
    
    datatable(df[, cols_to_show, drop = FALSE], 
              options = list(dom = 't', pageLength = 20, scrollX = TRUE), 
              rownames = FALSE, 
              class = 'cell-border stripe hover')
  })
  
  output$yoy_player_select_ui <- renderUI({
    df <- get_data("NFL_QB")
    players <- sort(unique(df$name))
    selectInput("yoy_players", "Select QBs to Track:", choices = players, selected = head(players, 3), multiple = TRUE)
  })
  
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
  
  # CONTRACT MATRIX PLAYER FILTER UI & LOGIC
  output$contract_player_filter_ui <- renderUI({
    df <- get_data(input$contract_sheet)
    df_filtered <- df %>% filter(!is.na(cap_hit_m))
    players <- sort(unique(df_filtered$name))
    
    selectizeInput("contract_selected_players", "Display Players:",
                   choices = players,
                   selected = head(players, 12),
                   multiple = TRUE)
  })
  
  observeEvent(input$select_all_contract, {
    df <- get_data(input$contract_sheet)
    df_filtered <- df %>% filter(!is.na(cap_hit_m))
    players <- sort(unique(df_filtered$name))
    updateSelectizeInput(session, "contract_selected_players", selected = players)
  })
  
  observeEvent(input$clear_all_contract, {
    updateSelectizeInput(session, "contract_selected_players", selected = character(0))
  })
  
  output$contract_matrix_plot <- renderPlotly({
    df <- get_data(input$contract_sheet)
    req(input$financial_var, input$performance_var)
    
    x_var <- input$financial_var
    y_var <- input$performance_var
    
    df_filtered <- df %>% filter(!is.na(.data[[x_var]]))
    
    if (!is.null(input$contract_selected_players) && length(input$contract_selected_players) > 0) {
      df_filtered <- df_filtered %>% filter(name %in% input$contract_selected_players)
    }
    
    x_label <- ifelse(x_var == "cap_hit_m", "Cap Hit ($ Millions)", "Salary ($ Millions)")
    y_label <- names(all_metrics)[which(all_metrics == y_var)]
    
    plot_ly(df_filtered, x = as.formula(paste0("~", x_var)), 
            y = as.formula(paste0("~", y_var)), 
            text = ~paste0("QB: ", name, "<br>", x_label, ": $", sprintf("%.2f", get(x_var)), "M<br>", y_label, ": ", sprintf("%.2f", get(y_var))),
            key = ~name, source = "contract_plot",
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
  
  output$raw_data_table <- renderDT({
    df <- get_data(input$explorer_sheet)
    
    if ("salary" %in% names(df)) df$salary <- ifelse(df$salary > 2e8, NA, df$salary)
    if ("cap_hit" %in% names(df)) df$cap_hit <- ifelse(df$cap_hit > 2e8, NA, df$cap_hit)
    
    df_display <- df %>% select(-any_of(c("cap_hit_clean", "salary_clean", "cap_hit_m", "salary_m")))
    
    num_cols <- names(df_display)[sapply(df_display, is.numeric)]
    
    money_cols <- intersect(c("salary", "cap_hit"), num_cols)
    year_cols  <- intersect(c("year", "free_agent", "seasons_played"), num_cols)
    stat_cols  <- setdiff(num_cols, c(money_cols, year_cols))
    
    dt <- datatable(
      df_display, 
      options = list(pageLength = 15, scrollX = TRUE, filter = "top"), 
      class = 'cell-border stripe hover',
      rownames = FALSE
    )
    
    if (length(money_cols) > 0) {
      dt <- dt %>% formatCurrency(columns = money_cols, currency = "$", digits = 2)
    }
    if (length(stat_cols) > 0) {
      dt <- dt %>% formatRound(columns = stat_cols, digits = 2)
    }
    if (length(year_cols) > 0) {
      dt <- dt %>% formatRound(columns = year_cols, digits = 0)
    }
    
    return(dt)
  })
  
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