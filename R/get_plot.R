get_plot <- function(summary, 
                     input_type, 
                     input_summary_of, 
                     input_summary_by) {
  summary_of <- to_snake_case(input_summary_of)
  
  summary_by <- to_snake_case(input_summary_by)
  
  plot <- summary |> 
    mutate(amount = (if (input_type == "Expense") -1 else 1) * amount) |> 
    ggplot(
      aes(
        x = .data[[summary_by]], 
        weight = .data[[summary_of]], 
        fill = .data[[summary_by]]
      )
    ) +
    geom_bar() +
    guides(fill = "none") + 
    ylab(input_summary_of) + 
    xlab(input_summary_by) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12)
    ) +
    scale_y_continuous(
      labels = 
        if (input_summary_of == "Amount") label_dollar() else label_comma()
    )
  
  plot
}