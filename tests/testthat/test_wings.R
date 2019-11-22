library(tidyverse)
library(testthat)

## Tests ##
# Table 3 Column 2


table_3_col_2_df <- read_csv("../../results/wings/table_3_column_2.csv")
table_3_col_3_df <- read_csv("../../results/wings/table_3_column_3.csv")

table_3_col_2_reference_table <- tribble(
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


table_3_col_3_reference_table <- tribble(
  ~outcome, ~estimate, ~SE,
  "Any nonfarm self-employment", 0.409, 0.033,
  "Started Enterprise", 0.485, 0.025,
  "Average Work Hours", 9.877, 1.794,
  "Agri Average Work Hours", 4.002, 1.3,
  "NonAgri Average Work Hours", 5.875, 0.916,  
  "Average Hours Chores", 1.416, 1.203,
  "Index of Income", 0.616, 0.08,
  "Monthly Cash Earnings", 23.39, 4.607,
  "Durable Consumption Assets",  0.384, 0.068,
  "Non-Durable Consumption", 33.439, 5.227,
  "Durable Assets (production)",  0.397, 0.058,
  "Times Went Hungry", -0.084, 0.036,           
  "Usual Meals per day", 0.078, 0.031         
)  


test_that("Table 3 Column 2 Replicates", {
  expect_true(all_equal(table_3_col_2_df %>% filter(outcome %in% table_3_col_2_reference_table$outcome), table_3_col_2_reference_table, convert = TRUE, ignore_row_order = TRUE))
})


test_that("Table 3 Column 3 Replicates", {
  expect_true(all_equal(table_3_col_3_df %>% filter(outcome %in% table_3_col_3_reference_table$outcome), table_3_col_3_reference_table, convert = TRUE, ignore_row_order = TRUE))
  
})
