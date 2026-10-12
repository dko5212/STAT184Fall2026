library(dplyr); library(tidyr); library(tibble)
library(readr); library(stringr); library(forcats); library(purrr)

# Problem 1

sales_raw <- tibble(
  sale_id = 1:8,
  customer_id = c("C1", "C2", "C1", "C3", "C2", "C4", "C5", "C2"),
  item_raw = c(" Pen", "PEN ", "Notebook", " pen ",
               "FOLDER", "Folder ", " ERASER ", "pen"),
  amount_raw = c("$10.00", "$20.00", NA, "$15.00",
                 "$30.00", "$25.00", NA, "$20.00"),
  discount_raw = c("0%", "10%", "5%", "0%", "20%", "10%", "0%", "0%"),
  code_raw = c("ST-101", "ON-102", "ST-103", "ON-104",
               "bad", "ST-106", "ON-107X", "on-108"),
  tags = c("gift;urgent", "online", "gift", "member",
           "online;member", "gift", "member", "gift;member")
)

# Part A

sales_clean <- sales_raw |>
  mutate(
    item_clean = str_to_lower(str_squish(item_raw)),
    amount = parse_number(amount_raw),
    discount_rate = parse_number(discount_raw) / 100,
    net_amount = amount * (1 - discount_rate),
    size = case_when(
      is.na(net_amount) ~ "unrecorded",
      net_amount >= 20 ~ "large",
      .default = "small"
    )
  )

sales_clean |>
  select(sale_id, item_clean, amount, discount_rate, net_amount, size)

# Part B

sales_codes <- sales_clean |>
  mutate(
    code_clean = str_to_upper(str_trim(code_raw)),
    valid_code = str_detect(code_clean, "^(ST|ON)-[0-9]{3}$")
  )

invalid_codes <- sales_codes |>
  filter(!valid_code) |>
  select(sale_id, code_raw, code_clean)
invalid_codes

valid_code_parts <- sales_codes |>
  filter(valid_code) |>
  separate_wider_delim(code_clean, delim = "-",
                       names = c("channel", "number"),
                       cols_remove = FALSE) |>
  select(sale_id, code_clean, channel, number)
valid_code_parts

sale_tags <- sales_clean |>
  select(sale_id, tags) |>
  separate_longer_delim(tags, delim = ";")
sale_tags

# separate_wider_delim() splits one column into several new columns in the same row,
# while separate_longer_delim() splits one column into several rows, one per piece.

# Part C

customer_summary <- sales_clean |>
  summarise(
    sales_n = n(),
    known_n = sum(!is.na(net_amount)),
    large_n = sum(net_amount >= 20, na.rm = TRUE),
    large_share = if (known_n > 0) large_n / known_n else NA_real_,
    .by = customer_id
  ) |>
  arrange(customer_id)
customer_summary

# For C1, large_share = 0 / 1 = 0. The numerator is large_n = 0 because sale 1 has a
# net amount of 10, which is below 20. The denominator is known_n = 1 because sale 3
# has a missing amount, so only sale 1 has a known net amount.

sales_factors <- sales_clean |>
  mutate(
    item_factor = fct_infreq(factor(item_clean)),
    item_group = fct_lump_n(factor(item_clean), n = 2, other_level = "Other")
  )

levels(sales_factors[["item_factor"]])
levels(sales_factors[["item_group"]])
sales_factors |> count(item_factor)
sales_factors |> count(item_group)

# fct_infreq() only reorders the levels from most to least frequent and keeps all four
# items, so the counts are unchanged. fct_lump_n() combines categories: it keeps the
# two most common items (pen, folder) and merges notebook and eraser into "Other",
# which reduces the number of categories and adds their counts together.

# Problem 2

reports_raw <- tibble(
  form = c("A", "A", "A", "B", "B", "B"),
  line = c(1, 2, 3, 1, 2, 3),
  customer_id = c("C1", NA, "C2", "C3", NA, "C4"),
  round = c(1, 3, 1, 1, 2, 1),
  primary_amount = c(10, -999, 20, NA, 0, 30),
  backup_amount = c(NA, 15, 25, NA, 99, NA)
)

customers <- tibble(
  customer_id = c("C1", "C2", "C3", "C5"),
  region = c("North", "South", NA, "West")
)

# Part A

reports_labeled <- reports_raw |>
  arrange(form, line) |>
  group_by(form) |>
  fill(customer_id, .direction = "down") |>
  ungroup()

reports_clean <- reports_labeled |>
  mutate(
    primary_clean = na_if(primary_amount, -999),
    amount = coalesce(primary_clean, backup_amount)
  )

reports_clean |>
  select(form, line, customer_id, round, primary_amount, backup_amount, amount)

# The backup is only for when the primary is missing. A recorded primary value,
# including 0, is real data, so coalesce() keeps it; 0 is not missing, so the
# backup of 99 is not used for form B line 2.

reports_complete <- reports_clean |>
  select(customer_id, round, amount) |>
  mutate(recorded_row = TRUE) |>
  complete(customer_id, round = 1:3, fill = list(recorded_row = FALSE))
reports_complete

tibble(
  original_rows = sum(reports_complete[["recorded_row"]]),
  added_rows = sum(!reports_complete[["recorded_row"]]),
  missing_before = sum(is.na(reports_clean[["amount"]])),
  missing_after = sum(is.na(reports_complete[["amount"]]))
)

# 6 original rows, 6 added rows, 1 missing amount before and 7 after completion.
# Explicit missing: C3 round 1 exists as a row but its amount is NA.
# Implicit missing: C1 round 2 has no row at all before complete().

# Part B

customers |> count(customer_id) |> filter(n > 1)
sum(is.na(customers[["customer_id"]]))
reports_clean |> count(customer_id, round) |> filter(n > 1)

# No duplicate or missing IDs in customers, and no repeated customer_id-round
# combination in reports_clean.

customers_marked <- customers |>
  mutate(customer_found = TRUE)

reports_joined <- reports_clean |>
  left_join(customers_marked, by = join_by(customer_id),
            relationship = "many-to-one", na_matches = "never") |>
  mutate(customer_found = replace_na(customer_found, FALSE))

reports_joined |>
  select(customer_id, round, amount, region, customer_found)

reports_clean |>
  anti_join(customers, by = join_by(customer_id), na_matches = "never") |>
  distinct(customer_id)

customers |>
  anti_join(reports_clean, by = join_by(customer_id), na_matches = "never") |>
  distinct(customer_id)

# Only in reports_clean: C4. Only in customers: C5.
# C3 has customer_found = TRUE, so it exists in customers but its region is not
# recorded. C4 has customer_found = FALSE, so it has no customer record at all.
# Both show region = NA, so the missing region alone cannot tell these cases apart.

# Part C

customers_bad <- tibble(
  customer_id = c("C1", "C2", "C2", "C3", "C5"),
  region = c("North", "South", "East", NA, "West")
)

reports_duplicated <- reports_clean |>
  left_join(customers_bad, by = join_by(customer_id),
            relationship = "many-to-many")

tibble(
  rows_before = nrow(reports_clean),
  rows_after = nrow(reports_duplicated),
  total_before = sum(reports_clean[["amount"]], na.rm = TRUE),
  total_after = sum(reports_duplicated[["amount"]], na.rm = TRUE)
)

reports_duplicated |> count(customer_id, round) |> filter(n > 1)

# Rows go from 6 to 7 and the total goes from 75 to 95. The C2 round 1 report is
# repeated because C2 appears twice in customers_bad (South and East), so its
# amount of 20 is counted twice.

# With relationship = "many-to-one", left_join() would throw an error because a
# row in reports_clean matches more than one row in customers_bad. Allowing
# "many-to-many" does not fix the conflict; it only hides it by duplicating the
# report. The C2 region conflict must be resolved in customers_bad itself.

reports_wide <- reports_complete |>
  select(customer_id, round, amount) |>
  pivot_wider(names_from = round, values_from = amount, names_prefix = "round_")
reports_wide

reports_wide |>
  pivot_longer(starts_with("round_"), names_to = "round", names_prefix = "round_",
               values_to = "amount", values_drop_na = TRUE) |>
  nrow()

# 5 rows would remain (12 cells minus 7 NA). Dropping NA removes those cells; it does
# not recover the unknown amounts, because the true values were never recorded and
# the explicit NA (C3 round 1) and added rows are both just deleted.

# Problem 3

# Part A

two_sample_t <- function(x, y, equal_var = FALSE, alpha = 0.05) {
  n_x <- length(x)
  n_y <- length(y)
  mean_x <- mean(x)
  mean_y <- mean(y)
  var_x <- var(x)
  var_y <- var(y)

  if (equal_var) {
    method <- "Pooled"
    sp2 <- ((n_x - 1) * var_x + (n_y - 1) * var_y) / (n_x + n_y - 2)
    se <- sqrt(sp2 * (1 / n_x + 1 / n_y))
    df <- n_x + n_y - 2
  } else {
    method <- "Welch"
    se <- sqrt(var_x / n_x + var_y / n_y)
    df <- (var_x / n_x + var_y / n_y)^2 /
      ((var_x / n_x)^2 / (n_x - 1) + (var_y / n_y)^2 / (n_y - 1))
  }

  t_stat <- (mean_x - mean_y) / se
  p_value <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)

  list(
    method = method,
    t_stat = t_stat,
    df = df,
    p_value = p_value,
    reject = p_value < alpha
  )
}

x_check <- c(12, 15, 14, 10, 13)
y_check <- c(9, 11, 8, 10, 7, 12, 9, 8)

welch_res <- two_sample_t(x_check, y_check)
pooled_res <- two_sample_t(x_check, y_check, equal_var = TRUE)
welch_res
pooled_res

welch_ref <- t.test(x_check, y_check, var.equal = FALSE)
pooled_ref <- t.test(x_check, y_check, var.equal = TRUE)

tibble(
  method = c("Welch", "Pooled"),
  t_diff = c(abs(welch_res[["t_stat"]] - unname(welch_ref[["statistic"]])),
             abs(pooled_res[["t_stat"]] - unname(pooled_ref[["statistic"]]))),
  df_diff = c(abs(welch_res[["df"]] - unname(welch_ref[["parameter"]])),
              abs(pooled_res[["df"]] - unname(pooled_ref[["parameter"]]))),
  p_diff = c(abs(welch_res[["p_value"]] - welch_ref[["p.value"]]),
             abs(pooled_res[["p_value"]] - pooled_ref[["p.value"]]))
) |>
  mutate(all_below_1e8 = t_diff < 1e-8 & df_diff < 1e-8 & p_diff < 1e-8)

# Welch: t = 3.403, df = 7.679, p = 0.0099. Pooled: t = 3.526, df = 11, p = 0.0047.
# Both reject at 0.05, and all differences from t.test() are smaller than 1e-8.

# Part B

B <- 1000
n_x <- 20
n_y <- 40
alpha <- 0.05

scenarios <- data.frame(
  scenario = c("equal_null", "unequal_null",
               "equal_shift", "unequal_shift"),
  mean_x = c(0, 0, 0, 0),
  mean_y = c(0, 0, 1, 1),
  sd_x = c(1, 3, 1, 3),
  sd_y = c(1, 1, 1, 1)
)

set.seed(2026)

n_total <- nrow(scenarios) * B * 2
scenario_vec <- character(n_total)
replicate_vec <- integer(n_total)
method_vec <- character(n_total)
t_vec <- numeric(n_total)
df_vec <- numeric(n_total)
p_vec <- numeric(n_total)
reject_vec <- logical(n_total)

row <- 0
for (i in seq_len(nrow(scenarios))) {
  for (b in seq_len(B)) {
    x <- rnorm(n_x, mean = scenarios[["mean_x"]][i], sd = scenarios[["sd_x"]][i])
    y <- rnorm(n_y, mean = scenarios[["mean_y"]][i], sd = scenarios[["sd_y"]][i])
    for (eq in c(FALSE, TRUE)) {
      res <- two_sample_t(x, y, equal_var = eq, alpha = alpha)
      row <- row + 1
      scenario_vec[row] <- scenarios[["scenario"]][i]
      replicate_vec[row] <- b
      method_vec[row] <- res[["method"]]
      t_vec[row] <- res[["t_stat"]]
      df_vec[row] <- res[["df"]]
      p_vec[row] <- res[["p_value"]]
      reject_vec[row] <- res[["reject"]]
    }
  }
}

simulation_results <- data.frame(
  scenario = scenario_vec,
  replicate = replicate_vec,
  method = method_vec,
  t_stat = t_vec,
  df = df_vec,
  p_value = p_vec,
  reject = reject_vec
)

head(simulation_results)
nrow(simulation_results)

simulation_summary <- simulation_results |>
  summarise(
    mean_t = mean(t_stat),
    sd_t = sd(t_stat),
    rejection_rate = mean(reject),
    .by = c(scenario, method)
  ) |>
  mutate(across(c(mean_t, sd_t, rejection_rate), \(v) round(v, 3)))
simulation_summary

# Short report: Under equal_null both tests reject close to 0.05, and under
# unequal_null Welch stays near 0.05 while the pooled test rejects far more often
# (about 0.14), so pooled has an inflated Type I error when the smaller sample has
# the larger variance. When the means differ, both tests have high power (about 0.95)
# in equal_shift, and in unequal_shift the pooled test rejects more often than Welch
# (about 0.48 vs 0.28). That higher pooled rate is not a real advantage, because the
# same test already rejects too often when H0 is true, so power only means something
# when Type I error is controlled. Small differences from these values (around
# +/- 0.014 near 0.05) are expected from simulation variation with B = 1000.
