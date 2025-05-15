library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)

sales <- read.csv("sales.csv", stringsAsFactors = FALSE)
shoes <- read.csv("shoes.csv", stringsAsFactors = FALSE)

# دمج البيانات
sales_data <- sales %>%
  left_join(shoes, by = "shoe_id")

# تحويل التاريخ
sales_data$sale_date <- as.Date(sales_data$sale_date)

# واجهة المستخدم
ui <- dashboardPage(
  dashboardHeader(title = "Women's Shoe Store Sales Performance"),
  dashboardSidebar(
    selectInput("branch", "Select Branch:", choices = c("All", unique(sales_data$branch_id)), selected = "All", multiple = TRUE),
    selectInput("category", "Select Category:", choices = c("All", unique(sales_data$category)), selected = "All", multiple = TRUE),
    dateRangeInput("date_range", "Date Range:", start = min(sales_data$sale_date), end = max(sales_data$sale_date))
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("total_sales"),
      valueBoxOutput("invoice_count"),
      valueBoxOutput("avg_invoice")
    ),
    fluidRow(
      box(title = "Sales Over Time", status = "primary", solidHeader = TRUE, width = 6, plotOutput("sales_time")),
      box(title = "Sales by Category", status = "warning", solidHeader = TRUE, width = 6, plotOutput("category_sales"))
    ),
    fluidRow(
      box(title = "Data Table", width = 12, DTOutput("table"))
    )
  )
)

# منطق التطبيق
server <- function(input, output) {
  
  filtered_data <- reactive({
    data <- sales_data
    if (!"All" %in% input$branch) {
      data <- data[data$branch_id %in% input$branch, ]
    }
    if (!"All" %in% input$category) {
      data <- data[data$category %in% input$category, ]
    }
    data <- data[data$sale_date >= input$date_range[1] & data$sale_date <= input$date_range[2], ]
    return(data)
  })
  
  output$total_sales <- renderValueBox({
    total <- sum(filtered_data()$total_price, na.rm = TRUE)
    valueBox(paste0("SAR ", round(total, 2)), "Total Sales", icon = icon("shopping-cart"), color = "green")
  })
  
  output$invoice_count <- renderValueBox({
    count <- nrow(filtered_data())
    valueBox(count, "Number of Invoices", icon = icon("file-invoice"), color = "blue")
  })
  
  output$avg_invoice <- renderValueBox({
    avg <- mean(filtered_data()$total_price, na.rm = TRUE)
    valueBox(paste0("SAR ", round(avg, 2)), "Average Invoice", icon = icon("calculator"), color = "yellow")
  })
  
  output$sales_time <- renderPlot({
    filtered_data() %>%
      group_by(sale_date) %>%
      summarise(Sales = sum(total_price, na.rm = TRUE)) %>%
      ggplot(aes(x = sale_date, y = Sales)) +
      geom_line(color = "steelblue") +
      labs(title = "Sales Over Time", x = "Date", y = "Sales") +
      theme_minimal()
  })
  
  output$category_sales <- renderPlot({
    filtered_data() %>%
      group_by(category) %>%
      summarise(Sales = sum(total_price, na.rm = TRUE)) %>%
      ggplot(aes(x = reorder(category, Sales), y = Sales, fill = category)) +
      geom_col() +
      coord_flip() +
      labs(title = "Sales by Category", x = "Category", y = "Sales") +
      theme_minimal()
  })
  
  output$table <- renderDT({
    datatable(filtered_data())
  })
}

# تشغيل التطبيق
shinyApp(ui, server)
