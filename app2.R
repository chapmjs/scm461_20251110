library(shiny)
library(tidyverse)

# Load data
orders <- read_csv("orders.csv")
parts_list <- read_csv("parts_list.csv")

# Define UI
ui <- fluidPage(
  titlePanel("UniCo Plant Order Status Tracker"),
  
  # Add custom CSS for status colors
  tags$head(
    tags$style(HTML("
      .status-late { 
        background-color: #ffcccc; 
        padding: 10px; 
        border-left: 4px solid #cc0000;
        margin: 10px 0;
      }
      .status-risk { 
        background-color: #fff4cc; 
        padding: 10px; 
        border-left: 4px solid #ff9900;
        margin: 10px 0;
      }
      .status-ok { 
        background-color: #ccffcc; 
        padding: 10px; 
        border-left: 4px solid #00cc00;
        margin: 10px 0;
      }
      .emergency-banner {
        background-color: #cc0000;
        color: white;
        padding: 15px;
        text-align: center;
        font-weight: bold;
        margin-bottom: 20px;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4("Order Selection"),
      selectInput(
        inputId = "order_id",
        label = "Customer Order Number:",
        choices = orders$order_num,
        selected = 41427
      ),
      
      hr(),
      
      h5("Quick Stats"),
      textOutput("quick_stats")
    ),
    
    mainPanel(
      # Emergency banner for late orders
      uiOutput("emergency_banner"),
      
      # Status card
      uiOutput("status_card"),
      
      h3("Order Details"),
      tableOutput("order_details"),
      
      h3("Required Parts"),
      tableOutput("parts_table"),
      
      # Parts completion summary
      plotOutput("parts_chart", height = "250px")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression for selected order (avoid duplication)
  selected_order <- reactive({
    orders %>% filter(order_num == input$order_id)
  })
  
  # Reactive expression for selected order parts
  selected_parts <- reactive({
    parts_list %>% filter(order_num == input$order_id)
  })
  
  # Emergency banner for late orders
  output$emergency_banner <- renderUI({
    order <- selected_order()
    
    if (order$status == "LATE") {
      div(
        class = "emergency-banner",
        paste("⚠ EMERGENCY: This order is", order$days_overdue, "days overdue! ⚠")
      )
    }
  })
  
  # Status card with color coding
  output$status_card <- renderUI({
    order <- selected_order()
    
    status_class <- case_when(
      order$status == "LATE" ~ "status-late",
      order$status == "At Risk" ~ "status-risk",
      TRUE ~ "status-ok"
    )
    
    div(
      class = status_class,
      h4(paste("Status:", order$status)),
      p(paste("Completion:", order$completion_pct, "%"))
    )
  })
  
  # Order details table
  output$order_details <- renderTable({
    order <- selected_order()
    
    tibble(
      Field = c("Order Number", "Customer", "Product", "Quantity", 
                "Order Date", "Due Date"),
      Value = c(order$order_num, order$customer, order$product, 
                order$quantity, as.character(order$order_date), 
                as.character(order$due_date))
    )
  })
  
  # Quick stats in sidebar
  output$quick_stats <- renderText({
    order <- selected_order()
    parts <- selected_parts()
    
    total_parts <- nrow(parts)
    complete_parts <- sum(parts$status == "Complete")
    
    paste0(
      "Total Parts: ", total_parts, "\n",
      "Complete: ", complete_parts, "\n",
      "In Progress: ", total_parts - complete_parts
    )
  })
  
  # Parts table
  output$parts_table <- renderTable({
    selected_parts() %>%
      mutate(
        completion_pct = round(quantity_complete / quantity_needed * 100, 1)
      ) %>%
      select(
        `Part #` = part_number,
        `Description` = part_description,
        `Needed` = quantity_needed,
        `Complete` = quantity_complete,
        `% Done` = completion_pct,
        `Status` = status
      )
  })
  
  # Parts completion chart
  output$parts_chart <- renderPlot({
    parts <- selected_parts() %>%
      mutate(completion_pct = quantity_complete / quantity_needed * 100)
    
    ggplot(parts, aes(x = part_number, y = completion_pct)) +
      geom_col(aes(fill = completion_pct < 100)) +
      geom_hline(yintercept = 100, linetype = "dashed", color = "red") +
      scale_fill_manual(values = c("TRUE" = "#ffcccc", "FALSE" = "#ccffcc"),
                        labels = c("Complete", "Incomplete"),
                        name = "") +
      labs(
        title = "Parts Completion Status",
        x = "Part Number",
        y = "% Complete"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
