get_plot <- function(summary, 
                     input_type, 
                     input_summary_of, 
                     input_summary_by,
                     input_show_top,
                     min_date,
                     max_date) {
  summary_of <- to_snake_case(input_summary_of)
  
  summary_by <- to_snake_case(input_summary_by)
  
  plot <- summary |> 
    mutate(amount = (if (input_type == "Expense") -1 else 1) * amount_total) |> 
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
        if (input_summary_of == "Amount") label_dollar() else label_comma()
    ) + 
    labs(
      title = get_title(input_show_top, input_summary_by, input_summary_of),
      subtitle = get_subtitle(min_date, max_date)
    ) + 
    xlab(input_summary_by) +
    ylab(input_summary_of) + 
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12)
    )
  
  plot
}

get_title <- function(input_show_top, input_summary_by, input_summary_of) {
  summary_by_plural <- if_else(
    input_summary_by == "Category", 
    "Categories",
    str_c(input_summary_by, "s")
  )
  
  is_plural <- input_show_top > 1
  
  n <- if (is_plural) str_glue(" {input_show_top}") else ""
  
  summary_by <- if (is_plural) summary_by_plural else input_summary_by
  
  title <- str_glue("Top{n} {summary_by} by {input_summary_of}")
  
  title
}

get_subtitle <- function(min_date, max_date) {
  subtitle <- str_glue(
    "{format(min_date, '%B %d, %Y')} - ",
    "{format(max_date, '%B %d, %Y')}"
  )
  
  subtitle
}