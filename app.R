rm(list = ls())
gc()
cat("\014")

pacman::p_load(
  readr, dplyr, snakecase, stringr, purrr, lubridate, 
  shiny, shinyWidgets, scales, ggplot2, forcats, bslib
)

r_files <- list.files("R", full.names = TRUE)

walk(r_files, source)

# ------------------------------------------------------------------------------

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      fileInput("file", "Choose CSV File", accept = ".csv"),
      uiOutput("filters"),
      selectInput("type", "Summary Type", c("Expense", "Income")),
      uiOutput("date_range"),
      selectInput(
        "summary_of", 
        "Summary Of", 
        c("Amount", "Number of Transactions")
      ),
      selectInput(
        "summary_by", 
        "Summary By", 
        c("Payee", "Category", "Category Group")
      ),
      numericInput("show_top", "Show Top", 10, step = 1),
      div(
        class = "mt-3", 
        switchInput("include_other", 'Include "Other" Group', inline = TRUE)
      )
    ),
    mainPanel(
      width = 8,
      card(
        card_header("Summary Plot"),
        card_body(plotOutput("plot"))
      ),
      card(
        card_header("Summary Table"),
        card_body(tableOutput("table"))
      )
    )
  )
)

server <- function(input, output) {
  transactions <- reactive({
    req(input$file)
    
    read_transactions(input$file$datapath)
  })
  
  output$filters <- renderUI({
    req(transactions())
    
    cols <- c("account", "category_group", "category")
    
    filter_inputs <- map(cols, \(col) get_filter_input(transactions(), col))
    
    tagList(filter_inputs)
  })
  
  output$date_range <- renderUI({
    req(transactions())
    
    min_date <- min(pull(transactions(), date))
    
    max_date <- max(pull(transactions(), date))
    
    dateRangeInput(
      "date_range", 
      "Date Range", 
      min_date, 
      max_date,
      min_date,
      max_date,
      format = "mm/dd/yyyy"
    )
  })
  
  summary <- reactive(
    get_summary_table(
      transactions(),
      input$account,
      input$category_group,
      input$category,
      input$type, 
      input$date_range,
      input$summary_of,
      input$summary_by,
      input$show_top,
      input$include_other
    )
  )
  
  summary_formatted <- reactive(
    format_summary_table(summary())
  )
  
  plot <- reactive(
    get_plot(summary(), input$type, input$summary_of, input$summary_by)
  )
  
  output$table <- renderTable(
    summary_formatted()
  )
  
  output$plot <- renderPlot(
    plot()
  )
}

get_filter_input <- function(data, col) {
  choices <- data |> 
    distinct(.data[[col]]) |> 
    arrange(.data[[col]]) |> 
    pull()
  
  filter_input <- pickerInput(
    col,
    to_title_case(col),
    choices,
    selected = choices,
    multiple = TRUE,
    options = list(`actions-box` = TRUE)
  )
  
  filter_input
}

# ------------------------------------------------------------------------------

shinyApp(ui, server)
