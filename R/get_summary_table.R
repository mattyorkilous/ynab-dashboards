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
    mutate(
      amount = inflow - outflow,
      number_of_transactions = 1
    ) |> 
    mutate(
      sum_of_by = sum(.data[[summary_of]]),
      .by = all_of(summary_by)
    ) |> 
    mutate(
      "{summary_by}" := lump_order(
        .data[[summary_by]], 
        n = input_show_top, 
        w = sum_of_by * 
          (if (input_type == "Expense" & summary_of == "amount") -1 else 1)
      )
    ) |> 
    summarise(
      amount_total = sum(amount),
      number_of_transactions = sum(number_of_transactions),
      amount_mean = mean(amount),
      amount_median = median(amount),
      .by = all_of(summary_by)
    ) |> 
    arrange(.data[[summary_by]]) |> 
    filter(
      if (input_include_other) TRUE else .data[[summary_by]] != "Other"
    )
  
  summary
}

lump_order <- function(fct, n, w) {
  lumped_ordered <- fct |> 
    fct_lump_n(
      n, 
      w = ({{ w }} - min({{ w }}) + 1), # ensures positive weights
      ties.method = "first"
    ) |> 
    fct_reorder(-{{ w }}) |> 
    fct_relevel("Other", after = Inf)
  
  lumped_ordered
}

format_summary_table <- function(summary, input_include_other) {
  summary_formatted <- summary |> 
    rename(
      "amount" = amount_total,
      "mean_amount" = amount_mean,
      "median_amount" = amount_median
    ) |> 
    mutate(
      across(ends_with("amount"), label_dollar()),
      number_of_transactions = label_number()(number_of_transactions)
    ) |> 
    rename_with(to_title_case)
  
  summary_formatted
}