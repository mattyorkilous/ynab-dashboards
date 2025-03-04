rm(list = ls())

pacman::p_load(lubridate, dplyr, purrr, tidyr, truncnorm, readr)

main <- function() {
  dates <- tibble(
    date = seq(ymd("2021-01-01"), today(), by = 1),
    n_transactions = generate_normal_integers(length(date), 0, 6)
  )
  
  category_groups <- get_category_groups()
  
  payees <- category_groups |> 
    unnest(categories) |> 
    unnest(categories)
  
  transactions_irregular <- get_irregular_transactions(dates, payees)
  
  transactions_regular <- get_regular_transactions(dates)
  
  transactions <- bind_rows(transactions_irregular, transactions_regular) |> 
    arrange(date) |> 
    select(date, category_group, category, payee, account, inflow, outflow)
  
  write_csv(transactions, "data/sample_data.csv")
  
  walk(ls(), \(x) assign(x, get(x), .GlobalEnv))
}

generate_normal_integers <- function(n, min, max) {
  values <- min:max
  
  densities <- dnorm(values, mean(values), sd = 1)
  
  probs <- densities / sum(densities)
  
  out <- sample(values, size = n, replace = TRUE, prob = probs)
  
  out
}

get_category_groups <- function() {
  tibble(
    category_group = c(
      "Food",
      "Transportation",
      "Shopping"
    ),
    categories = list(
      list(
        tibble(
          category = "Groceries",
          payee = c("Safeway", "Giant", "Whole Foods", "Trader Joe's"),
          account = c(
            "Capital One Savor", 
            "Capital One Savor", 
            "Amazon", 
            "Capital One Savor"
          ),
          mean = 15,
          weight = c(1, 3/5, 1/10, 1/3)
        ),
        tibble(
          category = "Dining Out",
          payee = c(
            "McDonald's", 
            "Chipotle", 
            "Chick Fil-A", 
            "Dunkin'", 
            "Subway"
          ),
          account = "Capital One Savor",
          mean = c(10, 15, 12, 10, 10),
          weight = 1/3
        )
      ),
      list(
        tibble(
          category = "Gas",
          payee = c("Shell", "7-Eleven", "Citgo"),
          account = "Citi Custom Cash",
          mean = 30,
          weight = 1/10
        ),
        tibble(
          category = "Public Transportation",
          payee = c("Metro", "Bus"),
          account = "Citi Double Cash",
          mean = 3,
          weight = 1/5
        ),
        tibble(
          category = "Rideshare",
          payee = c("Uber", "Lyft"),
          account = c("Capital One Savor", "Citi Double Cash"),
          mean = 10,
          weight = 1/6
        ),
        tibble(
          category = "Parking",
          payee = c("Parkmobile", "Parking Garage"),
          account = "Citi Double Cash",
          mean = 4,
          weight = 1/8
        ) 
      ),
      list(
        tibble(
          category = "Entertainment",
          payee = c("Alamo", "AMC", "Washington Nationals"),
          account = "Capital One Savor",
          mean = c(12, 12, 20),
          weight = 1/8
        ),
        tibble(
          category = "Clothes",
          payee = c("Target", "Madewell", "Zara"),
          account = "Citi Double Cash",
          mean = c(15, 40, 50),
          weight = c(1/6, 1/8, 1/10)
        ),
        tibble(
          category = "Merchandise",
          payee = c("Target", "Amazon", "Walmart"),
          account = "Citi Double Cash",
          mean = 20,
          weight = 1/3
        )  
      )
    )
  ) |> 
    mutate(inflow = 0)
}

get_irregular_transactions <- function(dates, payees) {
  dates |> 
    rowwise() |> 
    mutate(
      payees = list(
        slice_sample(payees, n = n_transactions, weight_by = weight)
      )
    ) |> 
    ungroup() |> 
    unnest(payees) |> 
    mutate(outflow = rtruncnorm(length(mean), a = 0, mean = mean))
}

get_regular_transactions <- function(dates) {
  dates |> 
    mutate(date = floor_date(date, "month")) |> 
    distinct(date) |> 
    rowwise() |> 
    mutate(
      transactions = list(
        tibble(
          category_group = c(
            "Home", 
            "Home", 
            "Subscriptions", 
            "Inflow", 
            "Inflow"
          ),
          category = c(
            "Rent", 
            "Utilities", 
            "Netflix", 
            "Ready to Assign", 
            "Ready to Assign"
          ),
          payee = c(
            "55555 Main Street", 
            "Electric Company", 
            "Netflix",
            "My Job, Inc.",
            "Interest"
          ),
          account = c(
            "Bank of America Checking", 
            "Bank of America Checking", 
            "Capital One Savor",
            "Bank of America Checking",
            "Capital One Savings"
          ),
          outflow = c(1000, 100, 15, 0, 0),
          inflow = c(0, 0, 0, 4000, 100)
        )
      )
    ) |> 
    ungroup() |> 
    unnest(transactions)
}

main()
