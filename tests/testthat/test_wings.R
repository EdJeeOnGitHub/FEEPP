library(tidyverse)
library(testthat)

## Tests ##
# Table 3 Column 2


table_3_col_2_df <- read_csv("../../results/wings/table_3_column_2.csv")

reference_table <- tribble(
  ~outcome, ~estimate, ~SE,
  "Any nonfarm self-employment", 0.401, 0.03,
  "Started Enterprise", 0.487, 0.025,
  "Average Work Hours", 9.391, 1.608,
  "Agri Average Work Hours", 3.496, 1.389,
  "NonAgri Average Work Hours", 5.895, 0.893,  
  "Average Hours Chores", 0.305, 1.013,
  "Index of Income", 0.464, 0.068,
  "Monthly Cash Earnings", 10.372, 3.443,
  "Durable Consumption Assets",  0.327, 0.067,
  "Non-Durable Consumption", 31.031, 5.010,
  "Durable Assets (production)",  0.402, 0.064,
  "Times Went Hungry", -0.098, 0.039,           
  "Usual Meals per day", 0.057, 0.028         
)  



test_that("Table 3 Column 2 Replicates", {
  expect_true(all_equal(table_3_col_2_df %>% filter(outcome %in% reference_table$outcome), reference_table, convert = TRUE, ignore_row_order = TRUE))
})
