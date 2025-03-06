rm(list = ls())
gc()
cat("\014")

pacman::p_load(
  readr, dplyr, snakecase, stringr, purrr, lubridate, shiny, 
  shinyWidgets, scales, ggplot2, forcats, bslib, shinyjs
)

r_files <- list.files("R", full.names = TRUE)

walk(r_files, source)

# ------------------------------------------------------------------------------

ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(bootswatch = "flatly"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      fileInput("file", "Choose YNAB Ledger File", accept = ".csv"),
      actionButton("use_example_data", "...Or, Click to Use Example Data"),
      uiOutput("filters"),
      div(
        id = "inputs",
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
        numericInput("show_top", "Show Top", 8, step = 1),
        div(
          class = "mt-3", 
          switchInput("include_other", 'Include "Other" Group', inline = TRUE)
        )
      ),
    ),
    mainPanel(
      width = 8,
      card(
        card_body(plotOutput("plot"))
      ),
      card(
        card_body(tableOutput("table"))
      )
    )
  )
)

server <- function(input, output) {
  transactions <- initialize_transactions(input)
  
  output$filters <- renderUI({
    get_filter_inputs(
      transactions, 
      c("account", "category_group", "category")
    )
  })
  
  dates <- handle_dates(transactions, input, output)
  
  summary <- reactive(
    get_summary_table(transactions(), input, dates)
  )
  
  output$table <- renderTable(
    format_summary_table(summary())
  )
  
  output$plot <- renderPlot({
    get_plot(summary(), input, dates)
  })
}

initialize_transactions <- function(input) {
  file_path <- reactiveVal()
  
  observeEvent(input$use_example_data, {
    reset("file")
    file_path("data/example_data.csv")
  })
  
  observeEvent(input$file, {
    file_path(input$file$datapath)
  })
  
  transactions <- reactive({
    req(file_path())
    read_transactions(file_path())
  })
  
  observeEvent(transactions(), {
    reset("inputs")
  })
  
  transactions
}

get_filter_inputs <- function(transactions, cols) {
  req(transactions())
  
  filter_inputs <- map(
    cols, 
    \(col) get_filter_input(transactions(), col)
  )
  
  tagList(filter_inputs)
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
    options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE)
  )
  
  filter_input
}

handle_dates <- function(transactions, input, output) {
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
    handle_date_range_text(
      input, 
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
  
  list(min_date = min_date, max_date = max_date)
}

handle_date_range_text <- function(input, 
                                   abs_min_date, 
                                   abs_max_date, 
                                   min_date, 
                                   max_date) {
  if (input$date_range != "Last Year") {
    max_date(abs_max_date)
  } else {
    max_date(floor_date(abs_max_date, "year") - days(1))
  }
  
  if (input$date_range %in% c("All Dates", "Custom")) {
    min_date(abs_min_date)
  } else if (input$date_range == "This Month") {
    min_date(floor_date(abs_max_date, "month"))
  } else if (str_detect(input$date_range, "Last \\d+ Months")) {
    n_months_back <- as.integer(str_extract(input$date_range, "\\d+"))
    min_date(floor_date(abs_max_date %m-% months(n_months_back), "month"))
  } else if (input$date_range == "Year To Date") {
    min_date(floor_date(abs_max_date, "year"))
  } else if (input$date_range == "Last Year") {
    min_date(floor_date(max_date(), "year"))
  }
}

# ------------------------------------------------------------------------------

shinyApp(ui, server)
