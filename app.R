library(shiny)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(ggplot2)
library(DT)
library(lubridate)

source("R/utils.R")
source("R/fetch.R")

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

ui <- page_navbar(
  title = "SkyView",
  theme = bs_theme(bootswatch = "darkly", base_font = font_google("Inter")),

  nav_panel(
    "Live Map",
    icon = bsicons::bs_icon("map"),
    layout_sidebar(
      sidebar = sidebar(
        width = 280,
        h6("Filters"),
        textInput("search_callsign", "Search callsign", placeholder = "e.g. UAL123"),
        checkboxInput("show_military", "Highlight military", value = TRUE),
        checkboxInput("airborne_only", "Airborne only", value = TRUE),
        hr(),
        h6("Data window"),
        sliderInput("hours_back", "Hours of history", 1, 72, 24, step = 1),
        hr(),
        actionButton("refresh", "Refresh now", class = "btn-sm btn-primary w-100"),
        hr(),
        uiOutput("last_fetch_ui")
      ),
      leafletOutput("map", height = "80vh")
    )
  ),

  nav_panel(
    "Activity",
    icon = bsicons::bs_icon("bar-chart"),
    layout_columns(
      col_widths = c(6, 6, 12),
      card(
        card_header("Aircraft by Hour"),
        plotOutput("hour_plot", height = 260)
      ),
      card(
        card_header("Top Callsigns"),
        DTOutput("callsign_table", height = 260)
      ),
      card(
        card_header("Heatmap (all positions)"),
        leafletOutput("heatmap", height = 400)
      )
    )
  ),

  nav_panel(
    "Search",
    icon = bsicons::bs_icon("search"),
    card(
      card_header("Aircraft History"),
      layout_columns(
        col_widths = c(4, 8),
        div(
          textInput("search_icao", "ICAO24 or callsign"),
          actionButton("do_search", "Search", class = "btn-primary btn-sm")
        ),
        DTOutput("search_results")
      )
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  # Reactive data ----
  raw_data <- reactiveVal(tibble())

  load_data <- function() {
    df <- load_snapshots(hours_back = isolate(input$hours_back)) |>
      flag_military()
    raw_data(df)
  }

  observe({ load_data() })
  observeEvent(input$refresh, {
    append_snapshot()
    load_data()
  })
  observeEvent(input$hours_back, { load_data() })

  # Auto-refresh every 5 minutes
  auto_refresh <- reactiveTimer(5 * 60 * 1000)
  observeEvent(auto_refresh(), { load_data() })

  filtered_live <- reactive({
    df <- latest_snapshot(raw_data())
    if (input$airborne_only) df <- df |> filter(!on_ground | is.na(on_ground))
    if (nchar(input$search_callsign) > 0)
      df <- df |> filter(grepl(input$search_callsign, callsign, ignore.case = TRUE))
    df
  })

  # Last fetch label ----
  output$last_fetch_ui <- renderUI({
    df <- raw_data()
    if (nrow(df) == 0) return(p("No data yet", style = "color:gray;font-size:0.8em"))
    t <- max(df$fetched_at, na.rm = TRUE)
    p(format(t, "Last fetch: %H:%M"), style = "color:gray;font-size:0.8em;text-align:center")
  })

  # Live Map ----
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.DarkMatter) |>
      setView(lng = -74.0, lat = 41.0, zoom = 9)
  })

  observe({
    df <- filtered_live()
    req(nrow(df) > 0)

    pal <- colorFactor(c("cyan", "orange"), domain = c(FALSE, TRUE))

    leafletProxy("map") |>
      clearMarkers() |>
      addCircleMarkers(
        data = df,
        lng = ~longitude, lat = ~latitude,
        radius = 6,
        color = ~pal(is_military),
        fillOpacity = 0.85,
        stroke = FALSE,
        popup = ~paste0(
          "<b>", callsign, "</b><br>",
          "ICAO: ", icao24, "<br>",
          "Country: ", origin_country, "<br>",
          "Alt: ", round(baro_altitude), " m<br>",
          "Speed: ", round(velocity), " m/s<br>",
          "Squawk: ", squawk,
          if_else(is_military, "<br><b style='color:orange'>⚠ Military</b>", "")
        )
      ) |>
      addLegend("bottomright",
        colors = c("cyan", "orange"),
        labels = c("Civilian", "Military"),
        opacity = 0.8
      )
  })

  # Hour bar chart ----
  output$hour_plot <- renderPlot({
    df <- raw_data()
    req(nrow(df) > 0)
    activity_by_hour(df) |>
      ggplot(aes(hour, n_aircraft)) +
      geom_col(fill = "#0dcaf0") +
      scale_x_continuous(breaks = 0:23) +
      labs(x = "Hour (local)", y = "Unique aircraft") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = "#222", color = NA),
            panel.background = element_rect(fill = "#222", color = NA),
            text = element_text(color = "white"),
            axis.text = element_text(color = "#aaa"))
  }, bg = "#222")

  # Top callsigns table ----
  output$callsign_table <- renderDT({
    df <- raw_data()
    req(nrow(df) > 0)
    top_callsigns(df) |>
      datatable(
        rownames = FALSE,
        options = list(pageLength = 10, dom = "tp"),
        class = "table-dark table-sm"
      )
  })

  # Heatmap ----
  output$heatmap <- renderLeaflet({
    df <- raw_data()
    req(nrow(df) > 0)
    df <- df |> filter(!is.na(latitude), !is.na(longitude))
    leaflet(df) |>
      addProviderTiles(providers$CartoDB.DarkMatter) |>
      setView(lng = -74.0, lat = 41.0, zoom = 8) |>
      addHeatmap(lng = ~longitude, lat = ~latitude,
                 blur = 20, max = 0.05, radius = 15)
  })

  # Search panel ----
  search_result <- eventReactive(input$do_search, {
    q <- tolower(trimws(input$search_icao))
    req(nchar(q) > 0)
    raw_data() |>
      filter(
        grepl(q, tolower(icao24)) | grepl(q, tolower(callsign))
      ) |>
      arrange(desc(fetched_at))
  })

  output$search_results <- renderDT({
    search_result() |>
      select(fetched_at, callsign, icao24, origin_country,
             latitude, longitude, baro_altitude, velocity, squawk) |>
      datatable(rownames = FALSE,
                options = list(pageLength = 15, dom = "ltp"),
                class = "table-dark table-sm")
  })
}

shinyApp(ui, server)
