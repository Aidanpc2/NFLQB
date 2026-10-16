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
library(googlesheets4)
library(dplyr)
library(tidyr)
library(plotly)
library(DT)

# ==============================================================================
# 1. LIVE GOOGLE SHEET & LOCAL FILE INITIALIZATION
# ==============================================================================

# Disable interactive Google authentication requirement for public read access
gs4_deauth()

# Public Google Sheet URL for live updates
COLLEGE_SHEET_URL <- "https://docs.google.com/spreadsheets/d/1JhS5bqyzdDVxR0ZMWlEzx5MEPnp35RH9CxMEXaJ7xBY/edit?gid=0#gid=0"

# Publishable relative path setup for local Excel workbook
FILE_PATH <- "NFL_QB_3YR.xlsx"
if (!file.exists(FILE_PATH)) {
  if (file.exists(file.path("data", FILE_PATH))) {
    FILE_PATH <- file.path("data", FILE_PATH)
  } else {
    stop(paste("File missing:", FILE_PATH, "- Please place 'NFL_QB_3YR.xlsx' in the app directory."))
  }
}

# Read local Excel workbook sheets
sheet_names <- excel_sheets(FILE_PATH)
sheets_data <- setNames(lapply(sheet_names, function(s) {
  read_excel(FILE_PATH, sheet = s)
}), sheet_names)

# Master list of integer count statistics (formatted with 0 decimals)
integer_count_cols <- c(
  "total_plays", "completions", "pass_yds", "total_yds", "pass_tds", "total_tds",
  "first_downs", "ints", "sacks", "fumbles", "air_yds", "longest_pass", "pressures",
  "rush_yds", "carries", "rush_td", "longest_rush", "rz_carries", "gl_carries",
  "year", "free_agent", "seasons_played", "attempts", "pass_td", "int"
)

# Comprehensive NFL metric dictionary
all_metrics <- c(
  "Adv Player Value" = "adv_player_val",
  "Player Value" = "player_val",
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

# College metrics dictionary
college_metrics <- c(
  "Total EPA" = "total_epa",
  "Pass EPA per Play" = "pass_epa_play",
  "Passing Yards" = "pass_yds",
  "Total Yards" = "total_yds",
  "Passing Touchdowns" = "pass_td",
  "Total Touchdowns" = "total_tds",
  "Interceptions" = "int",
  "Attempts" = "attempts",
  "Completions" = "completions",
  "Completion %" = "comp_pct",
  "Net Yards per Attempt (NYPA)" = "nypa",
  "Average Depth of Target (aDOT)" = "adot",
  "Air Yard Share %" = "air_yd_share",
  "Deep Attempt Rate %" = "deep_att_rate",
  "EPA per Carry" = "epa_carry",
  "Carries" = "carries",
  "Rushing Yards" = "rush_yds",
  "Yards per Carry (YPC)" = "ypc",
  "Rushing Touchdowns" = "rush_td",
  "Longest Rush" = "longest_rush",
  "Total Plays" = "total_plays"
)

# Extract unique NFL QB names
all_qb_names <- sort(unique(sheets_data[["3YR-Sum"]]$name))

# ==============================================================================
# 2. UI DEFINITION
# ==============================================================================
ui <- page_navbar(
  id = "main_navbar",
  title = span(
    tags$head(tags$link(rel = "manifest", href = "manifest.json")),
    tags$i(class = "fa-solid fa-football", style = "margin-right: 8px; color: #E63946;"),
    "NFL & College QB Analytics Hub"
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
      actionLink(
        inputId = "click_top_value",
        label = value_box(
          title = "Top Value QB (Bang for Buck)",
          value = textOutput("top_value_qb"),
          subtitle = "Click square to view player profile →",
          theme = "success"
        ),
        style = "text-decoration: none; color: inherit; cursor: pointer; display: block;"
      ),
      actionLink(
        inputId = "click_top_epa",
        label = value_box(
          title = "3-Year EPA Leader",
          value = textOutput("top_epa_qb"),
          subtitle = "Click square to view player profile →",
          theme = "primary"
        ),
        style = "text-decoration: none; color: inherit; cursor: pointer; display: block;"
      ),
      actionLink(
        inputId = "click_top_eff",
        label = value_box(
          title = "Highest EPA/Play Leader",
          value = textOutput("top_eff_qb"),
          subtitle = "Click square to view player profile →",
          theme = "info"
        ),
        style = "text-decoration: none; color: inherit; cursor: pointer; display: block;"
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
  # TAB 2: INDIVIDUAL PLAYER PROFILE (NFL)
  # ------------------------------------------------------------------
  nav_panel(
    title = "Player Profile",
    value = "tab_profile",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput("profile_qb_select", "Select Quarterback:", choices = all_qb_names, selected = "D.Maye"),
        hr(),
        uiOutput("profile_bio_card")
      ),
      mainPanel(
        width = 9,
        layout_column_wrap(
          width = 1/3,
          value_box(title = "2025 Total EPA", value = textOutput("prof_epa"), theme = "primary"),
          value_box(title = "2025 Passing Yards", value = textOutput("prof_yds"), theme = "success"),
          value_box(title = "2025 Pass TDs / INTs", value = textOutput("prof_td_int"), theme = "info")
        ),
        card(
          card_header(class = "bg-primary text-white font-weight-bold", "Customizable Career Trajectory Chart"),
          card_body(
            fluidRow(
              column(6, selectInput("prof_x_metric", "X-Axis Statistic:", choices = all_metrics, selected = "pass_yds")),
              column(6, selectInput("prof_y_metric", "Y-Axis Statistic:", choices = all_metrics, selected = "total_epa"))
            ),
            plotlyOutput("profile_chart", height = "360px")
          )
        ),
        card(
          card_header(class = "bg-dark text-white font-weight-bold", "Complete Multi-Year Season Breakdown"),
          DTOutput("profile_table")
        )
      )
    )
  ),
  
  # ------------------------------------------------------------------
  # TAB 3: NFL PLAYER COMPARISON TOOL
  # ------------------------------------------------------------------
  nav_panel(
    title = "NFL Player Comparison",
    value = "tab_comparison",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput("comp_sheet", "Data Source / Timeframe:", choices = c("3YR-Sum", "2YR-Sum", "2025", "2024", "2023"), selected = "3YR-Sum"),
        uiOutput("player_select_ui"),
        selectInput("comp_metric", "Primary Metric (Bar Chart):", choices = all_metrics, selected = "total_epa"),
        selectizeInput("secondary_metrics", "Table Statistics to Compare:",
                       choices = all_metrics,
                       selected = c("pass_yds", "pass_tds", "ints", "epa_play", "cpoe", "passer_rating", "rush_yds", "rush_td", "first_downs", "sacks"),
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
  # TAB 6: COLLEGE PROSPECTS & PROFILES
  # ------------------------------------------------------------------
  nav_panel(
    title = "College Prospects (2027)",
    value = "tab_college",
    navset_card_tab(
      id = "college_subtabs",
      nav_panel(
        title = "Full Prospects Explorer",
        value = "college_explorer",
        card_header(class = "bg-primary text-white font-weight-bold", "Live 2027 College QB Draft Prospects (Auto-updated Weekly)"),
        card_body(
          DTOutput("college_prospects_table")
        )
      ),
      nav_panel(
        title = "Scatterplot Explorer",
        value = "college_scatterplot",
        card(
          card_header(class = "bg-primary text-white font-weight-bold", "Prospect Correlation Explorer (Click any dot to view profile)"),
          card_body(
            fluidRow(
              column(6, selectInput("college_scatter_x", "X-Axis Metric:", choices = college_metrics, selected = "pass_yds")),
              column(6, selectInput("college_scatter_y", "Y-Axis Metric:", choices = college_metrics, selected = "total_epa"))
            ),
            plotlyOutput("college_master_scatter_plot", height = "420px")
          )
        )
      ),
      nav_panel(
        title = "College Player Profile",
        value = "college_profile",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            uiOutput("college_single_player_select_ui"),
            hr(),
            uiOutput("college_profile_bio_card")
          ),
          mainPanel(
            width = 9,
            layout_column_wrap(
              width = 1/3,
              value_box(title = "Total EPA", value = textOutput("college_prof_epa"), theme = "primary"),
              value_box(title = "Passing Yards", value = textOutput("college_prof_yds"), theme = "success"),
              value_box(title = "Pass TDs / INTs", value = textOutput("college_prof_td_int"), theme = "info")
            ),
            card(
              card_header(class = "bg-primary text-white font-weight-bold", "Prospect Metric Scatter / Correlation Explorer"),
              card_body(
                fluidRow(
                  column(6, selectInput("college_prof_x_metric", "X-Axis Statistic:", choices = college_metrics, selected = "pass_yds")),
                  column(6, selectInput("college_prof_y_metric", "Y-Axis Statistic:", choices = college_metrics, selected = "total_epa"))
                ),
                plotlyOutput("college_profile_chart", height = "360px")
              )
            ),
            card(
              card_header(class = "bg-dark text-white font-weight-bold", "Prospect Detailed Performance Breakdown"),
              DTOutput("college_profile_table")
            )
          )
        )
      ),
      nav_panel(
        title = "College Player Comparison",
        value = "college_comparison",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            uiOutput("college_player_select_ui"),
            selectInput("college_comp_metric", "Primary Metric (Bar Chart):", choices = college_metrics, selected = "total_epa"),
            selectizeInput("college_secondary_metrics", "Table Statistics to Compare:",
                           choices = college_metrics,
                           selected = c("school", "pass_yds", "pass_td", "int", "pass_epa_play", "comp_pct", "nypa", "rush_yds", "rush_td"),
                           multiple = TRUE)
          ),
          mainPanel(
            width = 9,
            card(
              card_header(class = "bg-primary text-white", "Head-to-Head Visual Prospect Comparison (Click any bar to view profile)"),
              plotlyOutput("college_comp_bar_chart", height = "360px")
            ),
            card(
              card_header(class = "bg-dark text-white", "Detailed Prospect Matrix"),
              DTOutput("college_comp_table")
            )
          )
        )
      )
    )
  ),
  
  # ------------------------------------------------------------------
  # TAB 7: WORKBOOK EXPLORER
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

# ==============================================================================
# 3. SERVER DEFINITION
# ==============================================================================
server <- function(input, output, session) {
  
  # Reactive polling for Google Sheet updates every 10 minutes (600,000 ms)
  fetch_college_qb_data <- reactivePoll(
    intervalMillis = 600000, 
    session = session,
    checkFunc = function() {
      Sys.time()
    },
    valueFunc = function() {
      tryCatch({
        read_sheet(COLLEGE_SHEET_URL)
      }, error = function(e) {
        message("Error reading Google Sheet live stream: ", e$message)
        return(NULL)
      })
    }
  )
  
  # Helper function to retrieve local Excel sheet & sanitize cap values
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
  
  # Helper function to apply clean DT table formatting
  format_dt_table <- function(dt, df_cols) {
    num_cols <- intersect(names(df_cols), names(df_cols)[sapply(df_cols, is.numeric)])
    money_cols <- intersect(c("salary", "cap_hit"), num_cols)
    int_cols <- intersect(integer_count_cols, num_cols)
    dec_cols <- setdiff(num_cols, c(money_cols, int_cols))
    
    if (length(money_cols) > 0) dt <- dt %>% formatCurrency(columns = money_cols, currency = "$", digits = 0)
    if (length(int_cols) > 0)   dt <- dt %>% formatRound(columns = int_cols, digits = 0)
    if (length(dec_cols) > 0)   dt <- dt %>% formatRound(columns = dec_cols, digits = 2)
    
    return(dt)
  }
  
  top_qbs <- reactiveValues(value = "D.Maye", epa = "B.Purdy", eff = "B.Purdy")
  
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
  
  # ENTIRE SQUARE CLICK HANDLERS (NFL)
  observeEvent(input$click_top_value, {
    updateSelectInput(session, "profile_qb_select", selected = top_qbs$value)
    nav_select("main_navbar", "tab_profile")
  })
  
  observeEvent(input$click_top_epa, {
    updateSelectInput(session, "profile_qb_select", selected = top_qbs$epa)
    nav_select("main_navbar", "tab_profile")
  })
  
  observeEvent(input$click_top_eff, {
    updateSelectInput(session, "profile_qb_select", selected = top_qbs$eff)
    nav_select("main_navbar", "tab_profile")
  })
  
  observeEvent(event_data("plotly_click", source = "dash_plot"), {
    click_data <- event_data("plotly_click", source = "dash_plot")
    if (!is.null(click_data) && !is.null(click_data$key)) {
      updateSelectInput(session, "profile_qb_select", selected = click_data$key)
      nav_select("main_navbar", "tab_profile")
    }
  })
  
  observeEvent(event_data("plotly_click", source = "contract_plot"), {
    click_data <- event_data("plotly_click", source = "contract_plot")
    if (!is.null(click_data) && !is.null(click_data$key)) {
      updateSelectInput(session, "profile_qb_select", selected = click_data$key)
      nav_select("main_navbar", "tab_profile")
    }
  })
  
  # CLICK HANDLERS FOR COLLEGE SCATTERPLOT AND COMPARISON BAR CHART
  observeEvent(event_data("plotly_click", source = "college_scatter_source"), {
    click_data <- event_data("plotly_click", source = "college_scatter_source")
    if (!is.null(click_data) && !is.null(click_data$key)) {
      updateSelectInput(session, "selected_college_profile_player", selected = click_data$key)
      nav_select("college_subtabs", "college_profile")
    }
  })
  
  observeEvent(event_data("plotly_click", source = "college_comp_source"), {
    click_data <- event_data("plotly_click", source = "college_comp_source")
    if (!is.null(click_data) && !is.null(click_data$x)) {
      updateSelectInput(session, "selected_college_profile_player", selected = click_data$x)
      nav_select("college_subtabs", "college_profile")
    }
  })
  
  # PLAYER PROFILE SIDEBAR (NFL)
  output$profile_bio_card <- renderUI({
    req(input$profile_qb_select)
    df_2025 <- get_data("2025")
    df_3yr  <- get_data("3YR-Sum") %>% filter(name == input$profile_qb_select)
    
    total_2025_qbs <- nrow(df_2025)
    qb_2025_row    <- df_2025 %>% filter(name == input$profile_qb_select)
    
    adv_val_val <- "N/A"
    adv_val_rank_str <- "N/A"
    if ("adv_player_val" %in% names(df_2025) && nrow(qb_2025_row) > 0) {
      df_ranked <- df_2025 %>% arrange(desc(adv_player_val)) %>% mutate(rank = row_number())
      player_rank_row <- df_ranked %>% filter(name == input$profile_qb_select)
      if (nrow(player_rank_row) > 0) {
        adv_val_val <- sprintf("%.2f", player_rank_row$adv_player_val[1])
        adv_val_rank_str <- paste0("#", player_rank_row$rank[1], " of ", total_2025_qbs)
      }
    }
    
    pv_2025_rank_str <- "N/A"
    if ("player_val" %in% names(df_2025) && nrow(qb_2025_row) > 0) {
      df_pv_ranked <- df_2025 %>% arrange(desc(player_val)) %>% mutate(rank = row_number())
      p_pv_row <- df_pv_ranked %>% filter(name == input$profile_qb_select)
      if (nrow(p_pv_row) > 0) {
        pv_2025_rank_str <- paste0("#", p_pv_row$rank[1], " of ", total_2025_qbs)
      }
    }
    
    current_team <- if (nrow(qb_2025_row) > 0 && "team" %in% names(qb_2025_row)) qb_2025_row$team[1] else "N/A"
    cap_hit_str  <- if (nrow(df_3yr) > 0 && !is.na(df_3yr$cap_hit_clean[1])) paste0("$", sprintf("%.2f", df_3yr$cap_hit_m[1]), "M") else "N/A"
    salary_str   <- if (nrow(df_3yr) > 0 && !is.na(df_3yr$salary_clean[1])) paste0("$", sprintf("%.2f", df_3yr$salary_m[1]), "M") else "N/A"
    fa_year      <- if (nrow(df_3yr) > 0) df_3yr$free_agent[1] else "N/A"
    
    tagList(
      h4(class = "text-primary font-weight-bold mb-1", input$profile_qb_select),
      p(class = "text-muted mb-3", paste("2025 Season Bio & Rankings")),
      tags$ul(class = "list-group list-group-flush",
              tags$li(class = "list-group-item d-flex justify-content-between align-items-center", "2025 Team", span(class = "badge bg-primary rounded-pill", current_team)),
              tags$li(class = "list-group-item d-flex justify-content-between align-items-center", "Adv Player Val (2025)", span(class = "badge bg-success rounded-pill", adv_val_val)),
              tags$li(class = "list-group-item d-flex justify-content-between align-items-center", "Adv Val 2025 Rank", span(class = "badge bg-dark rounded-pill", adv_val_rank_str)),
              tags$li(class = "list-group-item d-flex justify-content-between align-items-center", "Player Val 2025 Rank", span(class = "badge bg-info rounded-pill", pv_2025_rank_str)),
              tags$li(class = "list-group-item d-flex justify-content-between align-items-center", "Cap Hit", span(class = "badge bg-danger rounded-pill", cap_hit_str)),
              tags$li(class = "list-group-item d-flex justify-content-between align-items-center", "Salary", span(class = "badge bg-secondary rounded-pill", salary_str)),
              tags$li(class = "list-group-item d-flex justify-content-between align-items-center", "Free Agent Year", span(class = "badge bg-warning text-dark rounded-pill", fa_year))
      )
    )
  })
  
  output$prof_epa <- renderText({
    df <- get_data("2025") %>% filter(name == input$profile_qb_select)
    if (nrow(df) > 0 && "total_epa" %in% names(df)) round(df$total_epa[1], 1) else "N/A"
  })
  
  output$prof_yds <- renderText({
    df <- get_data("2025") %>% filter(name == input$profile_qb_select)
    if (nrow(df) > 0 && "pass_yds" %in% names(df)) format(df$pass_yds[1], big.mark = ",") else "N/A"
  })
  
  output$prof_td_int <- renderText({
    df <- get_data("2025") %>% filter(name == input$profile_qb_select)
    if (nrow(df) > 0 && "pass_tds" %in% names(df)) paste0(df$pass_tds[1], " / ", df$ints[1]) else "N/A"
  })
  
  output$profile_chart <- renderPlotly({
    req(input$profile_qb_select, input$prof_x_metric, input$prof_y_metric)
    df <- get_data("NFL_QB") %>% filter(name == input$profile_qb_select)
    
    x_label <- names(all_metrics)[which(all_metrics == input$prof_x_metric)]
    y_label <- names(all_metrics)[which(all_metrics == input$prof_y_metric)]
    
    if (nrow(df) > 0) {
      plot_ly(df, x = as.formula(paste0("~", input$prof_x_metric)), 
              y = as.formula(paste0("~", input$prof_y_metric)), 
              text = ~paste0("Season: ", year, "<br>", x_label, ": ", get(input$prof_x_metric), "<br>", y_label, ": ", get(input$prof_y_metric)),
              type = 'scatter', mode = 'lines+markers+text',
              textposition = 'top right',
              marker = list(size = 12, color = '#E63946'),
              line = list(color = '#1D3557', width = 3)) %>%
        layout(
          xaxis = list(title = x_label, gridcolor = '#EAEAEA'),
          yaxis = list(title = y_label, gridcolor = '#EAEAEA'),
          plot_bgcolor = "#FAFAFA"
        )
    }
  })
  
  output$profile_table <- renderDT({
    req(input$profile_qb_select)
    df_nfl <- get_data("NFL_QB") %>% filter(name == input$profile_qb_select)
    df_3yr <- get_data("3YR-Sum") %>% filter(name == input$profile_qb_select)
    
    if (nrow(df_nfl) > 0) df_nfl$year <- as.character(df_nfl$year)
    if (nrow(df_3yr) > 0) df_3yr$year <- "3YR-Sum"
    
    cols_common <- intersect(names(df_nfl), names(df_3yr))
    df_combined <- bind_rows(df_nfl[, cols_common], df_3yr[, cols_common])
    
    dt <- datatable(df_combined, options = list(dom = 't', scrollX = TRUE), rownames = FALSE, class = 'cell-border stripe hover')
    return(format_dt_table(dt, df_combined))
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
  
  # Player Select UI for NFL Comparison Tab
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
    
    df_sub <- df[, cols_to_show, drop = FALSE]
    dt <- datatable(df_sub, options = list(dom = 't', pageLength = 20, scrollX = TRUE), rownames = FALSE, class = 'cell-border stripe hover')
    return(format_dt_table(dt, df_sub))
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
    
    df_sub <- df[, cols_to_show, drop = FALSE]
    dt <- datatable(df_sub, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE, class = 'cell-border stripe hover')
    return(format_dt_table(dt, df_sub))
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
  
  # LIVE COLLEGE PROSPECTS TABLE OUTPUT
  output$college_prospects_table <- renderDT({
    df_college <- fetch_college_qb_data()
    req(df_college)
    
    dt <- datatable(
      df_college, 
      options = list(pageLength = 15, scrollX = TRUE, filter = "top"), 
      class = 'cell-border stripe hover',
      rownames = FALSE
    )
    
    return(format_dt_table(dt, df_college))
  })
  
  # COLLEGE MASTER SCATTERPLOT EXPLORER LOGIC
  output$college_master_scatter_plot <- renderPlotly({
    req(input$college_scatter_x, input$college_scatter_y)
    df_college <- fetch_college_qb_data()
    req(df_college)
    
    x_label <- names(college_metrics)[which(college_metrics == input$college_scatter_x)]
    y_label <- names(college_metrics)[which(college_metrics == input$college_scatter_y)]
    
    plot_ly(df_college, x = as.formula(paste0("~", input$college_scatter_x)), 
            y = as.formula(paste0("~", input$college_scatter_y)), 
            text = ~name, key = ~name, source = "college_scatter_source",
            type = 'scatter', mode = 'markers+text',
            textposition = 'top center',
            marker = list(size = 12, color = ~get(input$college_scatter_y), colorscale = 'Viridis', showscale = TRUE,
                          line = list(color = '#1D3557', width = 1))) %>%
      layout(
        xaxis = list(title = x_label, gridcolor = '#EAEAEA'),
        yaxis = list(title = y_label, gridcolor = '#EAEAEA'),
        hovermode = "closest",
        plot_bgcolor = "#FAFAFA"
      )
  })
  
  # COLLEGE PLAYER PROFILE LOGIC
  output$college_single_player_select_ui <- renderUI({
    df_college <- fetch_college_qb_data()
    req(df_college)
    players <- unique(df_college$name)
    selectInput("selected_college_profile_player", "Select College Player:", 
                choices = players, 
                selected = players[1])
  })
  
  output$college_profile_bio_card <- renderUI({
    req(input$selected_college_profile_player)
    df_college <- fetch_college_qb_data()
    req(df_college)
    
    # Filter strictly first 10 active prospect rows for ranking calculation
    active_prospects <- head(df_college, 10)
    
    # Check if selected player is in active prospects list
    is_active_prospect <- input$selected_college_profile_player %in% active_prospects$name
    
    qb_row <- df_college %>% filter(name == input$selected_college_profile_player)
    
    school_str <- if (nrow(qb_row) > 0 && "school" %in% names(qb_row)) qb_row$school[1] else "N/A"
    ht_str     <- if (nrow(qb_row) > 0 && "ht" %in% names(qb_row)) qb_row$ht[1] else "N/A"
    wt_str     <- if (nrow(qb_row) > 0 && "wt" %in% names(qb_row)) qb_row$wt[1] else "N/A"
    url_str    <- if (nrow(qb_row) > 0 && "Profile_URL" %in% names(qb_row)) qb_row$Profile_URL[1] else NULL
    
    # Calculate ranks among active prospects (rows 1-10)
    epa_play_val <- "N/A"
    epa_play_rank_str <- "Legacy Player"
    
    yds_val <- "N/A"
    yds_rank_str <- "Legacy Player"
    
    td_val <- "N/A"
    td_rank_str <- "Legacy Player"
    
    if (is_active_prospect) {
      # Pass EPA/Play Rank
      if ("pass_epa_play" %in% names(active_prospects)) {
        ranked_epa <- active_prospects %>% arrange(desc(pass_epa_play)) %>% mutate(rank = row_number())
        p_row <- ranked_epa %>% filter(name == input$selected_college_profile_player)
        if (nrow(p_row) > 0) {
          epa_play_val <- sprintf("%.3f", p_row$pass_epa_play[1])
          epa_play_rank_str <- paste0("#", p_row$rank[1], " of 10")
        }
      }
      
      # Passing Yards Rank
      if ("pass_yds" %in% names(active_prospects)) {
        ranked_yds <- active_prospects %>% arrange(desc(pass_yds)) %>% mutate(rank = row_number())
        p_row <- ranked_yds %>% filter(name == input$selected_college_profile_player)
        if (nrow(p_row) > 0) {
          yds_val <- format(p_row$pass_yds[1], big.mark = ",")
          yds_rank_str <- paste0("#", p_row$rank[1], " of 10")
        }
      }
      
      # Total Touchdowns Rank
      if ("total_tds" %in% names(active_prospects)) {
        ranked_tds <- active_prospects %>% arrange(desc(total_tds)) %>% mutate(rank = row_number())
        p_row <- ranked_tds %>% filter(name == input$selected_college_profile_player)
        if (nrow(p_row) > 0) {
          td_val <- as.character(p_row$total_tds[1])
          td_rank_str <- paste0("#", p_row$rank[1], " of 10")
        }
      }
    } else if (nrow(qb_row) > 0) {
      if ("pass_epa_play" %in% names(qb_row)) epa_play_val <- sprintf("%.3f", qb_row$pass_epa_play[1])
      if ("pass_yds" %in% names(qb_row)) yds_val <- format(qb_row$pass_yds[1], big.mark = ",")
      if ("total_tds" %in% names(qb_row)) td_val <- as.character(qb_row$total_tds[1])
    }
    
    tagList(
      h4(class = "text-primary font-weight-bold mb-1", input$selected_college_profile_player),
      p(class = "text-muted mb-3", if(is_active_prospect) "2027 Active Prospect Bio & Ranks" else "Legacy Player Profile"),
      tags$ul(class = "list-group list-group-flush mb-3",
              tags$li(class = "list-group-item d-flex justify-content-between align-items-center", "School", span(class = "badge bg-primary rounded-pill", school_str)),
              tags$li(class = "list-group-item d-flex justify-content-between align-items-center", "Height / Weight", span(class = "badge bg-secondary rounded-pill", paste(ht_str, "/", wt_str))),
              tags$li(class = "list-group-item d-flex justify-content-between align-items-center", "Pass EPA/Play", span(class = "badge bg-success rounded-pill", epa_play_val)),
              tags$li(class = "list-group-item d-flex justify-content-between align-items-center", "EPA/Play Rank", span(class = "badge bg-dark rounded-pill", epa_play_rank_str)),
              tags$li(class = "list-group-item d-flex justify-content-between align-items-center", "Pass Yards Rank", span(class = "badge bg-info rounded-pill", yds_rank_str)),
              tags$li(class = "list-group-item d-flex justify-content-between align-items-center", "Total TDs Rank", span(class = "badge bg-warning text-dark rounded-pill", td_rank_str))
      ),
      if (!is.null(url_str)) tags$a(href = url_str, target = "_blank", class = "btn btn-sm btn-outline-primary w-100", "View Full Saturdaysavant Bio")
    )
  })
  
  output$college_prof_epa <- renderText({
    req(input$selected_college_profile_player)
    df_college <- fetch_college_qb_data()
    req(df_college)
    qb_row <- df_college %>% filter(name == input$selected_college_profile_player)
    if (nrow(qb_row) > 0 && "total_epa" %in% names(qb_row)) round(qb_row$total_epa[1], 1) else "N/A"
  })
  
  output$college_prof_yds <- renderText({
    req(input$selected_college_profile_player)
    df_college <- fetch_college_qb_data()
    req(df_college)
    qb_row <- df_college %>% filter(name == input$selected_college_profile_player)
    if (nrow(qb_row) > 0 && "pass_yds" %in% names(qb_row)) format(qb_row$pass_yds[1], big.mark = ",") else "N/A"
  })
  
  output$college_prof_td_int <- renderText({
    req(input$selected_college_profile_player)
    df_college <- fetch_college_qb_data()
    req(df_college)
    qb_row <- df_college %>% filter(name == input$selected_college_profile_player)
    if (nrow(qb_row) > 0 && "pass_td" %in% names(qb_row) && "int" %in% names(qb_row)) paste0(qb_row$pass_td[1], " / ", qb_row$int[1]) else "N/A"
  })
  
  output$college_profile_chart <- renderPlotly({
    req(input$selected_college_profile_player, input$college_prof_x_metric, input$college_prof_y_metric)
    df_college <- fetch_college_qb_data()
    req(df_college)
    
    x_label <- names(college_metrics)[which(college_metrics == input$college_prof_x_metric)]
    y_label <- names(college_metrics)[which(college_metrics == input$college_prof_y_metric)]
    
    qb_row <- df_college %>% filter(name == input$selected_college_profile_player)
    
    plot_ly(df_college, x = as.formula(paste0("~", input$college_prof_x_metric)), 
            y = as.formula(paste0("~", input$college_prof_y_metric)), 
            text = ~name,
            type = 'scatter', mode = 'markers',
            marker = list(size = 10, color = '#457B9D', opacity = 0.5)) %>%
      add_trace(data = qb_row, x = as.formula(paste0("~", input$college_prof_x_metric)),
                y = as.formula(paste0("~", input$college_prof_y_metric)),
                text = ~name,
                mode = 'markers+text',
                textposition = 'top center',
                marker = list(size = 16, color = '#E63946', line = list(color = '#000000', width = 2)),
                showlegend = FALSE) %>%
      layout(
        xaxis = list(title = x_label, gridcolor = '#EAEAEA'),
        yaxis = list(title = y_label, gridcolor = '#EAEAEA'),
        showlegend = FALSE,
        plot_bgcolor = "#FAFAFA"
      )
  })
  
  output$college_profile_table <- renderDT({
    req(input$selected_college_profile_player)
    df_college <- fetch_college_qb_data()
    req(df_college)
    
    qb_row <- df_college %>% filter(name == input$selected_college_profile_player)
    
    dt <- datatable(qb_row, options = list(dom = 't', scrollX = TRUE), rownames = FALSE, class = 'cell-border stripe hover')
    return(format_dt_table(dt, qb_row))
  })
  
  # COLLEGE PLAYER COMPARISON LOGIC
  output$college_player_select_ui <- renderUI({
    df_college <- fetch_college_qb_data()
    req(df_college)
    players <- unique(df_college$name)
    selectInput("selected_college_players", "Select Prospects to Compare:", 
                choices = players, 
                selected = head(players, 4), 
                multiple = TRUE)
  })
  
  output$college_comp_bar_chart <- renderPlotly({
    req(input$selected_college_players, input$college_comp_metric)
    df_college <- fetch_college_qb_data()
    req(df_college)
    
    df_filtered <- df_college %>% filter(name %in% input$selected_college_players)
    metric_label <- names(college_metrics)[which(college_metrics == input$college_comp_metric)]
    
    plot_ly(df_filtered, x = ~name, y = as.formula(paste0("~", input$college_comp_metric)), 
            source = "college_comp_source",
            type = 'bar', color = ~name, colors = "Set1") %>%
      layout(
        xaxis = list(title = "College Prospect"),
        yaxis = list(title = metric_label),
        showlegend = FALSE,
        plot_bgcolor = "#FAFAFA"
      )
  })
  
  output$college_comp_table <- renderDT({
    req(input$selected_college_players)
    df_college <- fetch_college_qb_data()
    req(df_college)
    
    df_filtered <- df_college %>% filter(name %in% input$selected_college_players)
    
    cols_to_show <- unique(c("name", input$college_comp_metric, input$college_secondary_metrics))
    cols_to_show <- intersect(cols_to_show, names(df_college))
    
    df_sub <- df_filtered[, cols_to_show, drop = FALSE]
    dt <- datatable(df_sub, options = list(dom = 't', pageLength = 20, scrollX = TRUE), rownames = FALSE, class = 'cell-border stripe hover')
    return(format_dt_table(dt, df_sub))
  })
  
  output$raw_data_table <- renderDT({
    df <- get_data(input$explorer_sheet)
    
    if ("salary" %in% names(df)) df$salary <- ifelse(df$salary > 2e8, NA, df$salary)
    if ("cap_hit" %in% names(df)) df$cap_hit <- ifelse(df$cap_hit > 2e8, NA, df$cap_hit)
    
    df_display <- df %>% select(-any_of(c("cap_hit_clean", "salary_clean", "cap_hit_m", "salary_m")))
    
    dt <- datatable(
      df_display, 
      options = list(pageLength = 15, scrollX = TRUE, filter = "top"), 
      class = 'cell-border stripe hover',
      rownames = FALSE
    )
    
    return(format_dt_table(dt, df_display))
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

# ==============================================================================
# 4. LAUNCH APPLICATION
# ==============================================================================
shinyApp(ui = ui, server = server)