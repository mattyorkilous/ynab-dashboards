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
    lump_and_reorder(
      n = input_show_top,
      summary_by,
      descending = (input_type == "Expense" & summary_of == "amount")
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

lump_and_reorder <- function(.data, n, summary_by, descending) {
  top_n_groups <- .data |> 
    distinct(.data[[summary_by]], sum_of_by) |> 
    slice_max(
      sum_of_by * ((-1) ^ descending), 
      n = n, 
      with_ties = FALSE
    ) |> 
    pull(.data[[summary_by]])
  
  data_lumped_and_reordered <- .data |> 
    mutate(
      "{summary_by}" := if_else(
        .data[[summary_by]] %in% top_n_groups,
        .data[[summary_by]],
        "Other"
      ) |> 
        fct_relevel(c(top_n_groups, "Other"))
    )
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