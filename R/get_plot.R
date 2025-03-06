get_plot <- function(summary, input, dates) {
  summary_of <- to_snake_case(input$summary_of)
  
  summary_by <- to_snake_case(input$summary_by)
  
  plot <- summary |> 
    mutate(amount = (if (input$type == "Expense") -1 else 1) * amount_total) |> 
    ggplot(
      aes(
        x = .data[[summary_by]], 
        weight = .data[[summary_of]], 
        fill = .data[[summary_by]]
      )
    ) +
    geom_bar() +
    guides(fill = "none") + 
    scale_x_discrete(label = \(x) str_trunc(x, 16)) +
    scale_y_continuous(
      labels = 
        if (input$summary_of == "Amount") label_dollar() else label_comma()
    ) + 
    labs(
      title = get_title(input),
      subtitle = get_subtitle(dates)
    ) + 
    xlab(input$summary_by) +
    ylab(input$summary_of) + 
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12)
    )
  
  plot
}

get_title <- function(input) {
  summary_by_plural <- if_else(
    input$summary_by == "Category", 
    "Categories",
    str_c(input$summary_by, "s")
  )
  
  is_plural <- input$show_top > 1
  
  n <- if (is_plural) str_glue(" {input$show_top}") else ""
  
  summary_by <- if (is_plural) summary_by_plural else input$summary_by
  
  title <- str_glue("Top{n} {summary_by} by {input$summary_of}")
  
  title
}

get_subtitle <- function(dates) {
  subtitle <- str_glue(
    "{format(dates$min_date(), '%B %d, %Y')} - ",
    "{format(dates$max_date(), '%B %d, %Y')}"
  )
  
  subtitle
}