library(shiny)
library(tidyverse)

# Load data
orders <- read_csv("orders.csv")
parts_list <- read_csv("parts_list.csv")

# Define UI
ui <- fluidPage(
  titlePanel("UniCo Order Status Tracker"),

  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "order_id",
        label = "Select Customer Order:",
        choices = orders$order_num,
        selected = 41427
      )
    ),

    mainPanel(
      h3("Order Information"),
      verbatimTextOutput("order_info"),

      h3("Required Parts"),
      tableOutput("parts_table")
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  # Display order information
  output$order_info <- renderPrint({
    selected_order <- orders %>%
      filter(order_num == input$order_id)

    cat("Order Number:", selected_order$order_num, "\n")
    cat("Customer:", selected_order$customer, "\n")
    cat("Product:", selected_order$product, "\n")
    cat("Quantity:", selected_order$quantity, "\n")
    cat("Due Date:", as.character(selected_order$due_date), "\n")
    cat("Status:", selected_order$status, "\n")
    cat("Completion:", selected_order$completion_pct, "%\n")

    if (selected_order$status == "LATE") {
      cat("\n*** ALERT: ORDER IS", selected_order$days_overdue, "DAYS OVERDUE ***\n")
    }
  })

  # Display parts table
  output$parts_table <- renderTable({
    parts_list %>%
      filter(order_num == input$order_id) %>%
      select(part_number, part_description, quantity_needed, quantity_complete, status)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
