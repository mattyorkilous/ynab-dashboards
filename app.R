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
      fileInput("file", "Choose YNAB Ledger File", accept = ".csv"),
      uiOutput("filters"),
      selectInput("type", "Summary Type", c("Expense", "Income")),
      selectInput(
        "date_range", 
        "Date Range",
        c(
          "This Month",
          str_c("Last ", c(3, 6, 12), " Months"),
          "Year To Date",
          "Last Year",
          "All Dates",
          "Custom"
        ),
        selected = "All Dates"
      ),
      uiOutput("custom_date_range"),
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
  
  abs_max_date <- reactive({
    max(pull(distinct(transactions(), date)))
  })
  
  abs_min_date <- reactive({
    min(pull(distinct(transactions(), date)))
  })
  
  min_date <- reactiveVal()
  
  max_date <- reactiveVal()
  
  observeEvent(transactions(), {
    min_date(abs_min_date())
    
    max_date(abs_max_date())
  })
  
  observeEvent(
    input$date_range, 
    handle_date_range(
      input$date_range, 
      abs_min_date(), 
      abs_max_date(), 
      min_date, 
      max_date
    )
  )
  
  output$custom_date_range <- renderUI({
    req(input$date_range == "Custom")
    
    dateRangeInput(
      "custom_date_range", 
      NULL, 
      min_date(), 
      max_date(),
      min_date(),
      max_date(),
      format = "mm/dd/yyyy"
    )
  })
  
  observeEvent(input$custom_date_range, {
    min_date(input$custom_date_range[[1]])
    
    max_date(input$custom_date_range[[2]])
  })
  
  summary <- reactive(
    get_summary_table(
      transactions(),
      input$account,
      input$category_group,
      input$category,
      input$type, 
      min_date(),
      max_date(),
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
  
  output$plot <- renderPlot({
    req(plot())
       
    plot()
  })
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

handle_date_range <- function(input_date_range, 
                              abs_min_date, 
                              abs_max_date, 
                              min_date, 
                              max_date) {
  if (input_date_range != "Last Year") {
    max_date(abs_max_date)
  } else {
    max_date(floor_date(abs_max_date, "year") - days(1))
  }
  
  if (input_date_range %in% c("All Dates", "Custom")) {
    min_date(abs_min_date)
  } else if (input_date_range == "This Month") {
    min_date(floor_date(abs_max_date, "month"))
  } else if (str_detect(input_date_range, "Last \\d+ Months")) {
    n_months_back <- as.integer(str_extract(input_date_range, "\\d+"))
    
    min_date(floor_date(abs_max_date %m-% months(n_months_back), "month"))
  } else if (input_date_range == "Year To Date") {
    min_date(floor_date(abs_max_date, "year"))
  } else if (input_date_range == "Last Year") {
    min_date(floor_date(max_date(), "year"))
  }
}

# ------------------------------------------------------------------------------

shinyApp(ui, server)
