read_transactions <- function(file_path) {
  transactions_raw <- read_csv(file_path)
  
  transactions <- clean_transactions(transactions_raw)
  
  transactions
}

clean_transactions <- function(transactions_raw) {
  transactions <- transactions_raw |> 
    rename_with(to_snake_case) |> 
    mutate(
      date = mdy(date),
      across(c(outflow, inflow), currency_to_number)
    ) |> 
    filter(
      payee != "Starting Balance",
      !str_detect(payee, "Transfer :")
    ) |> 
    select(account, date, payee, category_group, category, outflow, inflow)
  
  transactions
}

currency_to_number <- function(currency) {
  number <- str_remove_all(currency, "\\$") |> 
    as.numeric()
  
  number
}