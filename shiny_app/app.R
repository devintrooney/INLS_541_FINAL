# app.R
# CPD Unit 189 (Narcotics) — 2016 Officer Co-event Network

library(shiny)
library(visNetwork)
library(dplyr)
library(tidyr)
library(igraph)
library(ggplot2)
library(scales)
library(lubridate)

# ---- Data loading ----
data <- readRDS("data.rds")
NODES           <- data$NODES
EDGE_CONTRIBS   <- data$EDGE_CONTRIBS
SCOPE_FOR_SHINY <- data$SCOPE_FOR_SHINY

EDGE_DURATION_DAYS <- 14
EDGE_DURATION_SEC  <- EDGE_DURATION_DAYS * 86400
HIT_RATE_MIN_SEARCHES_PER_OFFICER <- 3
SEARCH_RATE_MIN_STOPS_PER_OFFICER <- 3
HISTOGRAM_MIN_OFFICERS <- 15

YEAR_START <- as.POSIXct("2016-01-01", tz = "UTC")
YEAR_END   <- as.POSIXct("2016-12-31", tz = "UTC")

daily_dates <- seq(YEAR_START, YEAR_END, by = "day")

edges_per_day <- sapply(daily_dates, function(d) {
  ws <- d - EDGE_DURATION_SEC
  EDGE_CONTRIBS %>%
    filter(EVENT_TIME >= ws, EVENT_TIME <= d) %>%
    distinct(FROM, TO) %>% nrow()
})

stops_per_day <- sapply(daily_dates, function(d) {
  ws <- d - 86400
  SCOPE_FOR_SHINY %>%
    filter(CONTACT_DATE >= ws, CONTACT_DATE <= d) %>%
    distinct(CARD_NO) %>% nrow()
})

timeline_df <- data.frame(date = daily_dates, edges = edges_per_day, stops = stops_per_day)

COLOR_BG       <- "#1a1a1a"
COLOR_PANEL    <- "#262626"
COLOR_TEXT     <- "#e0e0e0"
COLOR_ACCENT   <- "#4caf50"
COLOR_EDGE     <- "rgba(160,160,160,0.4)"
COLOR_HIGHLIGHT <- "#fbbf24"
COLOR_NA       <- "#555555"

# Define the ramp once so legend and nodes use the same colors
RAMP_STOPS <- c("#3b82f6", "#a78bfa", "#f97316", "#ef4444")
# CSS gradient string for the legend bar
RAMP_CSS <- sprintf("linear-gradient(to right, %s)",
                    paste(RAMP_STOPS, collapse = ", "))

rate_palette <- function(values) {
  pal <- colorRampPalette(RAMP_STOPS)(100)
  values_clean <- ifelse(is.na(values), 0.5, pmax(0, pmin(1, values)))
  pal[round(values_clean * 99) + 1]
}

hist_titles <- list(
  stops       = "Stops per officer",
  searches    = "Searches per officer",
  hit_rate    = "Hit rate per officer",
  search_rate = "Search rate per officer",
  partners    = "Partners per officer"
)

stats_titles <- list(
  network = "Network statistics",
  officer = "Officer details"
)

color_titles <- list(
  degree      = "Network — colored by partner count",
  hit_rate    = "Network — colored by hit rate",
  search_rate = "Network — colored by search rate"
)

guide_content <- div(
  h4("How to read this", style = "color: #ccc; margin-top: 0;"),
  tags$p("Each circle is a CPD officer assigned to Unit 189 (Narcotics) in 2016."),
  tags$p("An edge appears if two officers shared a stop event whose 14-day active window overlaps the chosen timeframe. Treating partnerships as 14-day windows reflects the idea that working relationships are durable, not single events — but you can adjust your interpretation as you brush."),
  h4("Color (toggleable)", style = "color: #ccc; margin-top: 18px;"),
  tags$ul(
    tags$li(tags$b("Partner count"), ": how many distinct officers each one is connected to in the chosen timeframe. Cool = few partners, warm = many."),
    tags$li(tags$b("Hit rate"), ": fraction of an officer's searches that found contraband. Warmer = higher hit rate."),
    tags$li(tags$b("Search rate"), ": fraction of an officer's stops that became formal searches. Warmer = more aggressive search behavior.")
  ),
  tags$p(style = "color: #888; font-size: 13px;",
    "Gray nodes = not enough data to compute (e.g., no searches yet for hit rate)."),
  h4("Size", style = "color: #ccc; margin-top: 18px;"),
  tags$p("Larger nodes = more total stops in 2016 (full-year value, doesn't change with the time slider)."),
  h4("Time slider", style = "color: #ccc; margin-top: 18px;"),
  tags$p("Drag the endpoints to brush a time period. Network and statistics recompute live for the selected window."),
  h4("Charts and panes", style = "color: #ccc; margin-top: 18px;"),
  tags$ul(
    tags$li(tags$b("Timeline"), ": stops per day or active partnerships per day across the full year. Highlighted band marks the chosen timeframe."),
    tags$li(tags$b("Histogram"), ": per-officer distribution within the chosen timeframe. Toggle through five metrics."),
    tags$li(tags$b("Statistics"), ": numerical summaries — toggle between network-wide stats and the details of a specific officer.")
  ),
  h4("Interaction", style = "color: #ccc; margin-top: 18px;"),
  tags$ul(
    tags$li("Hover any officer to see details and dim others"),
    tags$li("Click an officer to see their statistics in the right-side pane"),
    tags$li("Drag nodes to rearrange. The layout is damped — pulls connected officers along but settles quickly."),
    tags$li("Mouse wheel to zoom, drag empty space to pan")
  )
)

toggle_js <- "
$(document).on('click', '.toggle-row .btn', function() {
  var $btn = $(this);
  var $group = $btn.closest('.toggle-row');
  $group.find('.btn').removeClass('active');
  $btn.addClass('active');
});
"

# ---- UI ----
ui <- fluidPage(
  tags$head(
    tags$style(HTML(sprintf("
      body { background: %s; color: %s; font-family: 'Helvetica Neue', Arial, sans-serif; font-size: 15px; }
      .container-fluid { padding: 20px; }
      h1 { font-weight: 300; letter-spacing: -0.5px; margin-bottom: 5px; font-size: 32px; }
      h1 .accent { color: %s; }
      .subtitle { color: #888; font-size: 16px; margin-bottom: 25px; line-height: 1.5; }
      .header-row { display: flex; align-items: flex-start; justify-content: space-between; }
      .help-btn {
        background: #333; color: #aaa; border: 1px solid #444;
        font-size: 14px; padding: 7px 14px; border-radius: 4px;
        margin-top: 5px;
      }
      .help-btn:hover { background: #3a3a3a; color: #fff; }
      .panel-card {
        background: %s; padding: 16px; border-radius: 6px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.3); margin-bottom: 15px;
      }
      .panel-title {
        font-size: 13px; text-transform: uppercase; letter-spacing: 1px;
        color: #888; margin-bottom: 12px;
      }
      .panel-title-dynamic {
        color: %s; text-transform: none; letter-spacing: 0; font-size: 15px;
      }
      .stat-row { display: flex; justify-content: space-between; align-items: center; margin-bottom: 12px; }
      .stat-row:last-child { margin-bottom: 0; }
      .stat-label-inline { font-size: 14px; color: #888; }
      .stat-value-inline { font-size: 22px; font-weight: 200; color: %s; line-height: 1; }
      .toggle-row {
        display: flex; gap: 5px; margin-top: 12px;
        flex-wrap: wrap;
      }
      .toggle-row .btn {
        flex: 1; background: #333 !important; color: #aaa !important;
        border: 1px solid #444 !important;
        font-size: 12px; padding: 7px 10px; border-radius: 3px;
        text-transform: uppercase; letter-spacing: 0.5px;
        min-width: 60px;
        transition: background 0.15s, color 0.15s, border-color 0.15s;
      }
      .toggle-row .btn:hover {
        background: #3a3a3a !important; color: #ccc !important;
      }
      .toggle-row .btn.active {
        background: %s !important; color: #fff !important; border-color: %s !important;
      }
      .empty-state {
        color: #888; font-size: 14px; text-align: center; padding: 24px 12px;
        font-style: italic;
      }
      .legend-collapsed { font-size: 13px; color: #aaa; line-height: 1.5; }
      .legend-collapsed .item { display: inline-block; margin-right: 14px; }
      .legend-collapsed small { font-size: 12px; }
      .ramp-bar {
        height: 14px; width: 100%%; border-radius: 3px;
        margin-top: 4px; margin-bottom: 4px;
      }
      .ramp-labels {
        display: flex; justify-content: space-between;
        font-size: 12px; color: #888;
        margin-bottom: 8px;
      }
      .ramp-label-text { font-size: 12px; color: #aaa; margin-bottom: 4px; }
      .legend-expanded { font-size: 14px; color: #aaa; }
      .legend-expanded h4 { font-size: 14px; margin-bottom: 6px; }
      .legend-expanded p { margin-bottom: 8px; line-height: 1.5; }
      .legend-expanded ul { padding-left: 18px; margin-bottom: 8px; }
      .legend-expanded li { margin-bottom: 4px; line-height: 1.5; }
      .modal-content { background: %s; color: %s; font-size: 15px; }
      .modal-body { font-size: 15px; line-height: 1.5; }
      .modal-body p { font-size: 15px; }
      .modal-body li { font-size: 15px; }
      .modal-header, .modal-footer { border-color: #333; }
      .modal-title { color: %s; font-size: 18px; }
      .irs--shiny .irs-bar { background: %s; border-color: %s; }
      .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background: %s; }
      .irs--shiny .irs-line { background: #333; border-color: #333; }
      .irs--shiny .irs-grid-text { color: #888; font-size: 11px; }
      .irs--shiny .irs-min, .irs--shiny .irs-max { color: #888; background: transparent; }
    ",
    COLOR_BG, COLOR_TEXT, COLOR_ACCENT, COLOR_PANEL, COLOR_TEXT, COLOR_TEXT,
    COLOR_ACCENT, COLOR_ACCENT,
    COLOR_PANEL, COLOR_TEXT, COLOR_TEXT,
    COLOR_ACCENT, COLOR_ACCENT, COLOR_ACCENT))),
    tags$script(HTML(toggle_js))
  ),

  div(class = "header-row",
    div(
      h1("The Shape of a Police Unit: Co-event Networks in Chicago's Narcotics Unit"),
      div(class = "subtitle",
          "Chicago Police Department Narcotics unit, 2016. ",
          "Drag the slider to brush a time period — the network and statistics recompute live.")
    ),
    actionButton("show_help", "?  Guide", class = "help-btn")
  ),

  fluidRow(
    column(width = 12,
      div(class = "panel-card",
        div(class = "panel-title", "Time window"),
        sliderInput("time_window",
          label = NULL,
          min = YEAR_START,
          max = YEAR_END,
          value = c(YEAR_START, YEAR_END),
          timeFormat = "%b %d",
          timezone = "+0000",
          width = "100%",
          step = 86400
        ),
        plotOutput("timeline_chart", height = "100px"),
        div(class = "toggle-row",
          actionButton("timeline_stops",  "Stops",        class = "active"),
          actionButton("timeline_edges",  "Partnerships")
        )
      )
    )
  ),

  fluidRow(
    column(width = 8,
      div(class = "panel-card",
        div(class = "panel-title panel-title-dynamic",
            textOutput("network_title", inline = TRUE)),
        visNetworkOutput("network", height = "600px"),
        div(class = "toggle-row",
          actionButton("color_degree",      "Partner Count", class = "active"),
          actionButton("color_hit_rate",    "Hit Rate"),
          actionButton("color_search_rate", "Search Rate")
        )
      )
    ),
    column(width = 4,
      div(class = "panel-card",
        div(class = "panel-title panel-title-dynamic",
            textOutput("histogram_title", inline = TRUE)),
        plotOutput("histogram", height = "240px"),
        div(class = "toggle-row",
          actionButton("hist_stops",       "Stops",       class = "active"),
          actionButton("hist_searches",    "Searches"),
          actionButton("hist_hit_rate",    "Hit Rate"),
          actionButton("hist_search_rate", "Search Rate"),
          actionButton("hist_partners",    "Partners")
        )
      ),
      div(class = "panel-card",
        div(class = "panel-title panel-title-dynamic",
            textOutput("stats_title", inline = TRUE)),
        uiOutput("stats_pane"),
        div(class = "toggle-row",
          actionButton("stats_network", "Network", class = "active"),
          actionButton("stats_officer", "Officer")
        )
      ),
      div(class = "panel-card",
        div(class = "panel-title",
          span("Legend"),
          actionLink("toggle_guide", "Show full guide",
                     style = "float: right; font-size: 12px; color: #aaa; text-transform: none; letter-spacing: 0; text-decoration: underline;")
        ),
        uiOutput("legend_pane")
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {

  observe({
    showModal(modalDialog(
      title = "Welcome",
      guide_content,
      easyClose = TRUE,
      size = "l",
      footer = modalButton("Got it")
    ))
  }, priority = -1)

  observeEvent(input$show_help, {
    showModal(modalDialog(
      title = "Guide",
      guide_content,
      easyClose = TRUE,
      size = "l",
      footer = modalButton("Close")
    ))
  })

  timeline_view  <- reactiveVal("stops")
  histogram_view <- reactiveVal("stops")
  stats_view     <- reactiveVal("network")
  color_view     <- reactiveVal("degree")
  guide_expanded <- reactiveVal(FALSE)
  selected_officer <- reactiveVal(NULL)

  observeEvent(input$timeline_stops, { timeline_view("stops") })
  observeEvent(input$timeline_edges, { timeline_view("edges") })

  observeEvent(input$hist_stops,       { histogram_view("stops") })
  observeEvent(input$hist_searches,    { histogram_view("searches") })
  observeEvent(input$hist_hit_rate,    { histogram_view("hit_rate") })
  observeEvent(input$hist_search_rate, { histogram_view("search_rate") })
  observeEvent(input$hist_partners,    { histogram_view("partners") })

  observeEvent(input$stats_network, { stats_view("network") })
  observeEvent(input$stats_officer, { stats_view("officer") })

  observeEvent(input$color_degree,      { color_view("degree") })
  observeEvent(input$color_hit_rate,    { color_view("hit_rate") })
  observeEvent(input$color_search_rate, { color_view("search_rate") })

  observeEvent(input$toggle_guide, { guide_expanded(!guide_expanded()) })

  observeEvent(input$network_selected, {
    if (!is.null(input$network_selected) && input$network_selected != "") {
      selected_officer(input$network_selected)
      stats_view("officer")
    }
  })

  filtered_edges <- reactive({
    ws <- input$time_window[1]; we <- input$time_window[2]
    EDGE_CONTRIBS %>%
      filter(EVENT_TIME >= ws - EDGE_DURATION_SEC, EVENT_TIME <= we) %>%
      mutate(EDGE_ACTIVE_FROM = EVENT_TIME,
             EDGE_ACTIVE_TO   = EVENT_TIME + EDGE_DURATION_SEC) %>%
      filter(EDGE_ACTIVE_TO >= ws, EDGE_ACTIVE_FROM <= we) %>%
      group_by(FROM, TO) %>%
      summarise(N_EVENTS = n(), .groups = "drop")
  })

  filtered_scope <- reactive({
    ws <- input$time_window[1]; we <- input$time_window[2]
    SCOPE_FOR_SHINY %>% filter(CONTACT_DATE >= ws, CONTACT_DATE <= we)
  })

  filtered_graph <- reactive({
    edges <- filtered_edges()
    if (nrow(edges) == 0) return(make_empty_graph(directed = FALSE))
    graph_from_data_frame(
      d = edges %>% select(FROM, TO, N_EVENTS),
      directed = FALSE,
      vertices = NODES %>% select(OFFICER_ID, everything())
    )
  })

  active_officer_ids <- reactive({
    edges <- filtered_edges()
    if (nrow(edges) == 0) return(character(0))
    unique(c(edges$FROM, edges$TO))
  })

  per_officer_stats <- reactive({
    active <- active_officer_ids()
    if (length(active) == 0) return(data.frame())
    scope <- filtered_scope() %>% filter(OFFICER_ID %in% active)
    if (nrow(scope) == 0) return(data.frame())

    g <- filtered_graph()
    sub <- induced_subgraph(g, active)
    deg <- degree(sub)
    deg_df <- data.frame(OFFICER_ID = names(deg), partners = as.integer(deg))

    scope %>%
      group_by(OFFICER_ID) %>%
      summarise(
        stops    = n(),
        searches = sum(SEARCHED, na.rm = TRUE),
        hits     = sum(HIT, na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      mutate(
        hit_rate    = ifelse(searches > 0, hits / searches, NA_real_),
        search_rate = ifelse(stops > 0, searches / stops, NA_real_)
      ) %>%
      left_join(deg_df, by = "OFFICER_ID")
  })

  output$histogram_title <- renderText({
    paste0(hist_titles[[histogram_view()]], " (in chosen timeframe)")
  })
  output$stats_title <- renderText({
    stats_titles[[stats_view()]]
  })
  output$network_title <- renderText({
    color_titles[[color_view()]]
  })

  output$network <- renderVisNetwork({
    edges <- filtered_edges()
    active_ids <- active_officer_ids()
    cv <- color_view()

    if (length(active_ids) == 0) {
      return(visNetwork(
        data.frame(id = character(0), label = character(0)),
        data.frame(from = character(0), to = character(0))
      ) %>% visOptions(highlightNearest = FALSE))
    }

    pos <- per_officer_stats()
    color_lookup <- NODES %>% filter(OFFICER_ID %in% active_ids)

    # Normalize a numeric vector to 0-1 using min-max. Returns gray for everyone
    # if there's no variation (min == max).
    normalize_metric <- function(vals) {
      v <- vals
      vmin <- suppressWarnings(min(v, na.rm = TRUE))
      vmax <- suppressWarnings(max(v, na.rm = TRUE))
      if (!is.finite(vmin) || !is.finite(vmax) || vmin == vmax) {
        return(rep(NA_real_, length(v)))
      }
      (v - vmin) / (vmax - vmin)
    }

    if (cv == "degree") {
      color_lookup <- color_lookup %>%
        left_join(pos %>% select(OFFICER_ID, partners), by = "OFFICER_ID") %>%
        mutate(
          metric_value = normalize_metric(partners),
          color = ifelse(is.na(metric_value), COLOR_NA, rate_palette(metric_value))
        )
    } else {
      metric <- if (cv == "hit_rate") "hit_rate" else "search_rate"
      color_lookup <- color_lookup %>%
        left_join(pos %>% select(OFFICER_ID, !!sym(metric)), by = "OFFICER_ID") %>%
        mutate(
          metric_value = normalize_metric(.data[[metric]]),
          color = ifelse(is.na(metric_value), COLOR_NA, rate_palette(metric_value))
        )
    }

    nodes_vis <- color_lookup %>%
      mutate(
        id = OFFICER_ID,
        label = OFFICER_ID,
        title = sprintf(
          "<b>Officer %s</b><br/>%d stops in 2016<br/>First seen: %s<br/>Last seen: %s",
          OFFICER_ID, N_STOPS,
          format(FIRST_SEEN, "%b %d"), format(LAST_SEEN, "%b %d")
        ),
        size = pmin(50, 10 + sqrt(N_STOPS) * 1.5)
      )

    edges_vis <- edges %>%
      transmute(from = FROM, to = TO, value = N_EVENTS,
                title = sprintf("%d shared event(s)", N_EVENTS))

    visNetwork(nodes_vis, edges_vis, background = COLOR_PANEL) %>%
      visPhysics(
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(
          gravitationalConstant = -30,
          centralGravity = 0.005,
          springLength = 100,
          springConstant = 0.08,
          damping = 0.9,
          avoidOverlap = 0.5
        ),
        stabilization = list(iterations = 200),
        timestep = 0.4
      ) %>%
      visEdges(
        width = 1, smooth = FALSE,
        color = list(color = COLOR_EDGE, highlight = COLOR_HIGHLIGHT, hover = COLOR_HIGHLIGHT)
      ) %>%
      visNodes(
        borderWidth = 0, shadow = FALSE,
        font = list(color = COLOR_TEXT, size = 12)
      ) %>%
      visOptions(
        highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
        nodesIdSelection = FALSE
      ) %>%
      visEvents(
        selectNode = "function(props) {
          Shiny.setInputValue('network_selected', props.nodes[0]);
        }",
        deselectNode = "function(props) {
          Shiny.setInputValue('network_selected', '');
        }"
      ) %>%
      visInteraction(hover = TRUE, dragNodes = TRUE, zoomView = TRUE,
                     navigationButtons = FALSE) %>%
      visLayout(randomSeed = 42)
  })

  output$timeline_chart <- renderPlot({
    ws <- input$time_window[1]; we <- input$time_window[2]
    view <- timeline_view()
    df <- timeline_df %>%
      mutate(value = if (view == "stops") stops else edges)
    y_label <- if (view == "stops") "Stops per day" else "Active edges"

    ggplot(df, aes(x = date, y = value)) +
      annotate("rect", xmin = ws, xmax = we, ymin = -Inf, ymax = Inf,
               fill = COLOR_ACCENT, alpha = 0.18) +
      geom_area(fill = COLOR_TEXT, alpha = 0.4) +
      geom_line(color = COLOR_TEXT, linewidth = 0.4) +
      scale_x_datetime(date_breaks = "1 month", date_labels = "%b") +
      labs(x = NULL, y = y_label) +
      theme_minimal(base_size = 11) +
      theme(
        panel.background = element_rect(fill = COLOR_PANEL, color = NA),
        plot.background = element_rect(fill = COLOR_PANEL, color = NA),
        panel.grid.major = element_line(color = "#333"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "#888"),
        axis.title.y = element_text(color = "#888", size = 10),
        axis.title.x = element_blank(),
        plot.margin = margin(0, 5, 0, 5)
      )
  }, bg = "transparent")

  output$histogram <- renderPlot({
    view <- histogram_view()
    stats <- per_officer_stats()

    if (nrow(stats) == 0) {
      return(ggplot() + theme_void() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "No active officers in window",
                 color = "#888", size = 4.5) +
        theme(plot.background = element_rect(fill = COLOR_PANEL, color = NA)))
    }

    plot_data <- switch(view,
      "stops"       = list(df = stats, col = "stops",       xlab = "Stops",       ref = mean(stats$stops, na.rm = TRUE)),
      "searches"    = list(df = stats, col = "searches",    xlab = "Searches",    ref = mean(stats$searches, na.rm = TRUE)),
      "partners"    = list(df = stats, col = "partners",    xlab = "Partners",    ref = mean(stats$partners, na.rm = TRUE)),
      "hit_rate"    = {
        f <- stats %>% filter(searches >= HIT_RATE_MIN_SEARCHES_PER_OFFICER)
        list(df = f, col = "hit_rate", xlab = "Hit rate",
             ref = if (nrow(f) > 0) sum(f$hits) / sum(f$searches) else NA,
             min_n = HISTOGRAM_MIN_OFFICERS,
             empty_msg = sprintf("Window too narrow for stable hit rate (need %d+ officers with %d+ searches)",
                                 HISTOGRAM_MIN_OFFICERS, HIT_RATE_MIN_SEARCHES_PER_OFFICER))
      },
      "search_rate" = {
        f <- stats %>% filter(stops >= SEARCH_RATE_MIN_STOPS_PER_OFFICER)
        list(df = f, col = "search_rate", xlab = "Search rate",
             ref = if (nrow(f) > 0) sum(f$searches) / sum(f$stops) else NA,
             min_n = HISTOGRAM_MIN_OFFICERS,
             empty_msg = sprintf("Window too narrow for stable search rate (need %d+ officers with %d+ stops)",
                                 HISTOGRAM_MIN_OFFICERS, SEARCH_RATE_MIN_STOPS_PER_OFFICER))
      }
    )

    df <- plot_data$df

    if (!is.null(plot_data$min_n) && nrow(df) < plot_data$min_n) {
      return(ggplot() + theme_void() +
        annotate("text", x = 0.5, y = 0.5,
                 label = plot_data$empty_msg,
                 color = "#888", size = 4) +
        theme(plot.background = element_rect(fill = COLOR_PANEL, color = NA)))
    }

    is_rate <- view %in% c("hit_rate", "search_rate")
    p <- ggplot(df, aes(x = .data[[plot_data$col]])) +
      geom_histogram(bins = if (is_rate) 20 else 15,
                     fill = COLOR_ACCENT, alpha = 0.85, color = NA)

    if (!is.na(plot_data$ref)) {
      p <- p + geom_vline(xintercept = plot_data$ref, color = COLOR_HIGHLIGHT,
                          linewidth = 0.6, linetype = "dashed")
    }
    if (is_rate) {
      p <- p + scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1))
    }

    p + labs(x = plot_data$xlab, y = "Officers") +
      theme_minimal(base_size = 12) +
      theme(
        panel.background = element_rect(fill = COLOR_PANEL, color = NA),
        plot.background = element_rect(fill = COLOR_PANEL, color = NA),
        panel.grid.major = element_line(color = "#333"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "#aaa"),
        axis.title = element_text(color = "#aaa", size = 11),
        plot.margin = margin(5, 10, 5, 5)
      )
  }, bg = "transparent")

  output$stats_pane <- renderUI({
    view <- stats_view()
    stats <- per_officer_stats()
    active <- active_officer_ids()

    stat_row <- function(label, value) {
      div(class = "stat-row",
          div(class = "stat-label-inline", label),
          div(class = "stat-value-inline", value))
    }

    if (view == "network") {
      n_partnerships <- nrow(filtered_edges())
      total_stops    <- if (nrow(stats) > 0) sum(stats$stops) else 0
      total_searches <- if (nrow(stats) > 0) sum(stats$searches) else 0

      tagList(
        stat_row("Active officers", length(active)),
        stat_row("Active partnerships", n_partnerships),
        stat_row("Total stops", format(total_stops, big.mark = ",")),
        stat_row("Total searches", format(total_searches, big.mark = ","))
      )
    } else {
      sel <- selected_officer()
      if (is.null(sel) || sel == "") {
        return(div(class = "empty-state",
                   "Click an officer in the network to see their stats"))
      }
      if (!sel %in% stats$OFFICER_ID) {
        return(div(class = "empty-state",
                   sprintf("Officer %s has no activity in the chosen timeframe", sel)))
      }
      row <- stats %>% filter(OFFICER_ID == sel)
      tagList(
        stat_row("Officer", sel),
        stat_row("Stops", format(row$stops, big.mark = ",")),
        stat_row("Searches", format(row$searches, big.mark = ",")),
        stat_row("Hit rate",
                 if (is.na(row$hit_rate)) "—" else percent(row$hit_rate, accuracy = 0.1)),
        stat_row("Search rate",
                 if (is.na(row$search_rate)) "—" else percent(row$search_rate, accuracy = 0.1)),
        stat_row("Partners", row$partners)
      )
    }
  })

  # ---- Legend pane (collapsed = ramp + min/max; expanded = full guide) ----
  output$legend_pane <- renderUI({
    if (guide_expanded()) {
      return(div(class = "legend-expanded", guide_content))
    }

    cv <- color_view()
    stats <- per_officer_stats()

    # Compute label, formatter, and min/max based on color view
    if (cv == "degree") {
      label_text <- "Color: partner count"
      vals <- if (nrow(stats) > 0) stats$partners else integer(0)
      vmin <- if (length(vals) > 0) min(vals, na.rm = TRUE) else NA
      vmax <- if (length(vals) > 0) max(vals, na.rm = TRUE) else NA
      fmt <- function(x) if (is.na(x)) "—" else as.character(x)
      footer_note <- "Node size = total stops in 2016. Click a node for details."
    } else if (cv == "hit_rate") {
      label_text <- "Color: hit rate"
      f <- if (nrow(stats) > 0) stats %>% filter(searches >= 1) else stats
      vals <- if (nrow(f) > 0) f$hit_rate else numeric(0)
      vals <- vals[!is.na(vals)]
      vmin <- if (length(vals) > 0) min(vals) else NA
      vmax <- if (length(vals) > 0) max(vals) else NA
      fmt <- function(x) if (is.na(x)) "—" else percent(x, accuracy = 1)
      footer_note <- "Gray = not enough searches to compute. Node size = total stops in 2016."
    } else {
      label_text <- "Color: search rate"
      f <- if (nrow(stats) > 0) stats %>% filter(stops >= 1) else stats
      vals <- if (nrow(f) > 0) f$search_rate else numeric(0)
      vals <- vals[!is.na(vals)]
      vmin <- if (length(vals) > 0) min(vals) else NA
      vmax <- if (length(vals) > 0) max(vals) else NA
      fmt <- function(x) if (is.na(x)) "—" else percent(x, accuracy = 1)
      footer_note <- "Gray = no stops in window. Node size = total stops in 2016."
    }

    has_range <- !is.na(vmin) && !is.na(vmax) && vmin != vmax

    div(class = "legend-collapsed",
      div(class = "ramp-label-text", label_text),
      div(class = "ramp-bar", style = sprintf("background: %s;", RAMP_CSS)),
      div(class = "ramp-labels",
        span(if (is.na(vmin)) "—" else fmt(vmin)),
        span(if (!has_range && !is.na(vmin)) "(single value)" else ""),
        span(if (is.na(vmax)) "—" else fmt(vmax))
      ),
      tags$small(style = "color: #777;", footer_note)
    )
  })

  observe({
    label <- if (guide_expanded()) "Hide full guide" else "Show full guide"
    updateActionButton(session, "toggle_guide", label = label)
  })
}

shinyApp(ui = ui, server = server)
