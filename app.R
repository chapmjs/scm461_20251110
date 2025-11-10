# ---- Plant Performance Dashboard (Starter) ----
# Week 2: Basic UI, inputs/outputs/layout
# Packages
library(shiny)
library(tidyverse)

# --- Simulated data generator (you may replace later with real CSVs) ---
make_data <- function(n_days = 120, seed = 42) {
  set.seed(seed)
  depts <- c("Stamping", "Machining", "Assembly", "Painting", "Packaging")
  start <- Sys.Date() - n_days + 1
  dates <- seq.Date(start, by = "day", length.out = n_days)

  base_map <- tibble(
    department = depts,
    base_plan = c(320, 260, 200, 220, 240)
  )

  # Build daily plan/production and simulate WIP accumulation by dept
  df <- crossing(date = dates, department = depts) %>%
    left_join(base_map, by = "department") %>%
    mutate(
      # mild weekly seasonality and noise
      dow = as.integer(format(date, "%u")),
      plan = round(base_plan * (1 + 0.06 * sin(2 * pi * dow / 7)), 0),
      produced = pmax(0, round(rnorm(n(), mean = plan * 0.93, sd = plan * 0.08))),
      efficiency = produced / pmax(plan, 1)
    ) %>%
    group_by(department) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(
      wip = pmax(0, cumsum(plan - produced) + round(rnorm(n(), 0, 15))),
      on_time = pmin(1, pmax(0, 0.96 - (wip / 5000) + rnorm(n(), 0, 0.02)))
    ) %>%
    ungroup()

  df
}

# ---- UI ----
ui <- fluidPage(
  titlePanel("UniCo Plant Performance Dashboard — Week 2"),

  sidebarLayout(
    sidebarPanel(
      h4("Filters"),
      dateRangeInput("daterange", "Reporting period:",
                     start = Sys.Date() - 29, end = Sys.Date()),
      selectInput("dept", "Department:",
                  choices = c("All Departments", "Stamping", "Machining", "Assembly", "Painting", "Packaging"),
                  selected = "All Departments"),
      sliderInput("wip_thresh", "WIP high-water threshold:", min = 0, max = 2000, value = 800, step = 50),
      actionButton("refresh", "Refresh data"),
      tags$hr(),
      h5("Downloads"),
      downloadButton("dl_csv", "Filtered data (CSV)"),
      downloadButton("dl_plot", "Throughput plot (PNG)")
    ),

    mainPanel(
      h3("Key Metrics"),
      fluidRow(
        column(4, verbatimTextOutput("kpi_throughput")),
        column(4, verbatimTextOutput("kpi_wip")),
        column(4, verbatimTextOutput("kpi_ontime"))
      ),
      tags$hr(),
      h3("Throughput Trend"),
      plotOutput("throughput_plot", height = "320px"),
      tags$hr(),
      h3("WIP Inventory (by Department)"),
      tableOutput("wip_table"),
      tags$hr(),
      h4("Notes"),
      verbatimTextOutput("notes")
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {

  # Base data lives in a reactiveVal so students can simulate "new" data on Refresh
  data_rv <- reactiveVal(make_data())

  # Keep date picker in sync with the data range
  observe({
    d <- data_rv()
    updateDateRangeInput(session, "daterange",
                         start = max(min(d$date), Sys.Date() - 29),
                         end   = max(d$date),
                         min   = min(d$date),
                         max   = max(d$date))
  })

  # "Refresh" regenerates the synthetic data (pretend new week of logs arrived)
  observeEvent(input$refresh, {
    data_rv(make_data())
  })

  # Filtered view
  filtered <- reactive({
    d <- data_rv()
    req(input$daterange)
    d <- d %>% filter(date >= input$daterange[1], date <= input$daterange[2])
    if (input$dept != "All Departments") d <- d %>% filter(department == input$dept)
    req(nrow(d) > 0)
    d
  })

  # KPIs
  output$kpi_throughput <- renderText({
    d <- filtered()
    daily <- d %>% group_by(date) %>% summarise(prod = sum(produced), .groups = "drop")
    paste0("Avg Throughput: ", round(mean(daily$prod), 1), " units/day")
  })
  output$kpi_wip <- renderText({
    d <- filtered()
    paste0("Avg WIP: ", round(mean(d$wip), 0), " units | Max WIP: ", max(d$wip))
  })
  output$kpi_ontime <- renderText({
    d <- filtered()
    paste0("On-Time Ship Estimate: ", round(mean(d$on_time) * 100, 1), "%")
  })

  # Plot
  output$throughput_plot <- renderPlot({
    d <- filtered()
    agg <- d %>%
      group_by(date) %>%
      summarise(produced = sum(produced), .groups = "drop")

    ggplot(agg, aes(date, produced)) +
      geom_line() +
      geom_hline(yintercept = mean(agg$produced), linetype = "dashed") +
      labs(x = NULL, y = "Units Produced", subtitle = "Dashed = date-range average") +
      theme_minimal()
  })

  # WIP table (dept summary vs threshold)
  output$wip_table <- renderTable({
    d <- filtered()
    d %>%
      group_by(department) %>%
      summarise(
        avg_WIP = round(mean(wip), 0),
        p95_WIP = round(quantile(wip, 0.95), 0),
        days_over_thresh = sum(wip > input$wip_thresh),
        .groups = "drop"
      ) %>%
      arrange(desc(avg_WIP))
  })

  # Notes (verbatimTextOutput)
  output$notes <- renderText({
    d <- filtered()
    msg <- if (mean(d$wip) > input$wip_thresh) {
      "Risk: average WIP is over the threshold. Expect schedule pressure and expedites."
    } else {
      "WIP is generally under the threshold; flow risk looks manageable in this window."
    }
    paste(
      msg,
      "\nTip: widen the date range and flip departments to see where queues persist."
    )
  })

  # Downloads
  output$dl_csv <- downloadHandler(
    filename = function() {
      paste0("plant_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(filtered(), file)
    }
  )
  output$dl_plot <- downloadHandler(
    filename = function() paste0("throughput_", Sys.Date(), ".png"),
    content = function(file) {
      d <- filtered() %>% group_by(date) %>% summarise(produced = sum(produced), .groups = "drop")
      p <- ggplot(d, aes(date, produced)) + geom_line() +
        labs(x = NULL, y = "Units Produced") + theme_minimal()
      ggsave(file, plot = p, width = 8, height = 4, dpi = 150)
    }
  )
}

shinyApp(ui, server)
