# Create sample orders dataset
library(tidyverse)

# Set seed for reproducibility
set.seed(123)

# Create customer orders similar to UniCo plant
orders <- tibble(
  order_num = c(41427, 41510, 41822, 42019, 42156, 42287, 42404, 42555, 42689, 42711),
  customer = c("Burnside Industries", "Granby Corp", "VanHorn & Associates", 
               "Markham Enterprises", "Burnside Industries", "Consolidated Manufacturing",
               "Hilton Products", "Burnside Industries", "Universal Parts", "Granby Corp"),
  product = c("Model 12", "Model 12", "Model 14", "Model 12", "Model 14",
              "Model 12", "Model 12", "Model 14", "Model 12", "Model 14"),
  quantity = c(100, 75, 50, 120, 60, 90, 80, 100, 70, 55),
  order_date = as.Date(c("2025-01-15", "2025-01-20", "2025-01-22", "2025-02-01",
                         "2025-02-05", "2025-02-08", "2025-02-10", "2025-02-15",
                         "2025-02-18", "2025-02-20")),
  due_date = as.Date(c("2025-03-01", "2025-03-15", "2025-03-20", "2025-03-25",
                       "2025-04-01", "2025-04-05", "2025-04-08", "2025-04-15",
                       "2025-04-20", "2025-04-22")),
  status = c("LATE", "On Schedule", "At Risk", "On Schedule", "On Schedule",
             "At Risk", "On Schedule", "On Schedule", "At Risk", "On Schedule"),
  completion_pct = c(85, 45, 60, 30, 40, 55, 35, 25, 50, 20),
  days_overdue = c(12, 0, 0, 0, 0, 0, 0, 0, 0, 0)
)

# Create parts required for orders
parts_list <- tibble(
  order_num = c(rep(41427, 4), rep(41510, 4), rep(41822, 4)),
  part_number = c("A101", "A102", "B215", "C330",
                  "A101", "A102", "B215", "C330",
                  "A105", "A106", "B218", "C335"),
  part_description = c("Base Assembly", "Control Panel", "Motor Housing", "Power Unit",
                       "Base Assembly", "Control Panel", "Motor Housing", "Power Unit",
                       "Base Assembly Mk2", "Control Panel Pro", "Motor Housing HD", "Power Unit Pro"),
  quantity_needed = c(100, 100, 100, 100,
                      75, 75, 75, 75,
                      50, 50, 50, 50),
  quantity_complete = c(100, 100, 75, 100,  # Order 41427 is missing 25 of B215!
                        50, 40, 30, 40,
                        35, 30, 28, 30),
  status = c("Complete", "Complete", "IN PROGRESS", "Complete",
             "In Progress", "In Progress", "In Progress", "In Progress",
             "In Progress", "In Progress", "In Progress", "In Progress")
)

# Save data
write_csv(orders, "orders.csv")
write_csv(parts_list, "parts_list.csv")
