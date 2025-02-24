get_summary_table <- function(transactions,
                              input_account,
                              input_category_group,
                              input_category,
                              input_type,
                              min_date,
                              max_date,
                              input_summary_of,
                              input_summary_by,
                              input_show_top,
                              input_include_other) {
  summary_of <- to_snake_case(input_summary_of)
  
  summary_by <- to_snake_case(input_summary_by)
  
  summary <- transactions |> 
    filter(
      account %in% input_account,
      category_group %in% input_category_group,
      category %in% input_category,
      if (input_type == "Income") category == "Ready to Assign" 
        else category != "Ready to Assign",
      between(date, min_date, max_date)
    ) |> 
    summarise(
      amount = sum(inflow - outflow),
      number_of_transactions = n(),
      .by = all_of(summary_by)
    ) |>
    arrange(
      if (input_type == "Expense" & summary_of == "amount") .data[[summary_of]] 
        else desc(.data[[summary_of]]),
      .data[[summary_by]]
    ) |> 
    mutate(
      "{summary_by}" := lump_order(
        .data[[summary_by]], 
        (if (input_type == "Expense" & summary_of == "amount") -1 else 1) 
          * .data[[summary_of]],
        input_show_top
      )
    ) |> 
    summarise(
      across(c(amount, number_of_transactions), sum), 
      .by = all_of(summary_by)
    ) |> 
    filter(
      if (input_include_other) TRUE else .data[[summary_by]] != "Other"
    )
  
  summary
}

lump_order <- function(fct, weight, n) {
  lumped_ordered <- fct |> 
    fct_lump_n(
      n, 
      w = ({{ weight }} - min({{ weight }}) + 1), # ensures positive weights
      ties.method = "first"
    ) |> 
    fct_reorder(-{{ weight }}) |> 
    fct_relevel("Other", after = Inf)
  
  lumped_ordered
}

format_summary_table <- function(summary, input_include_other) {
  summary_formatted <- summary |> 
    mutate(amount = label_dollar()(amount)) |> 
    rename_with(to_title_case)
  
  summary_formatted
}