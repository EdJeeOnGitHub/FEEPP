library(haven)
library(tidyverse)
library(sandwich)
library(broom)
library(testthat)
wings_df <- read_dta("data/WINGS/Data/WINGS.dta")



outcomes_T4 <- c("feltfree_grant_p1e",
                 "granthholdgaveamt_p1e_r",
                 "grantcommgaveamt_p1e_r",
                 "askedpaidbribe_p1e",
                 "belongs_to_group_p1e",
                 "groupsin_p1e",
                 "allnames_p1e",
                 "allmeet_resc_p1e",
                 "allcomm_resc_p1e",
                 "allsave_resc_p1e",
                 "allsupp_resc_p1e",
                 "allbiz_resc_p1e",
                 "allcoop_p1e",
                 "group1meet_resc_p1e",
                 "group1comm_resc_p1e",
                 "group1save_resc_p1e",
                 "group1supp_resc_p1e",
                 "group1biz_resc_p1e",
                 "group1coop_p1e",
                 "tgivenamt_dup_p1ep99_r",
                 "treceivedamt_dup_p1ep99_r",
                 "lgivenamt_dup_p1ep99_r",
                 "lreceivedamt_dup_p1ep99_r",
                 "savingsgroup_p1e",
                 "savings_p1ep99_r",
                 "currloans_p1ep99_r",
                 "bizadvice_p1e",
                 "harvest_earnings_p1e")

outcomes_T3 <- c("pettybiz_dum_p1e",
                 "biznow_p1e",
                 "igastart_p1e",
                 "total_hrs7d_s2p99_p1e",
                 "agri_hrs7d_s2p99_p1e",
                 "nonagri_hrs7d_s2p99_p1e", 
                 "chores_hrs7d_p99_p1e",
                 "zero_employment_p1e", 
                 "incomeindex_p1e",
                 "total_profit4w_r_s2p99_p1e",
                 "consdur_p1e",
                 "consagg_p1ep99_r",
                 "consaggpercap2_p1ep99_r",
                 "proddur_p1e",
                 "bedhungry_p1e",
                 "meals_p1e")



controls <- c("age_bas", "female", "hhsize_bas", "partner_bas", "onlyearner_bas", "biochildren_bas", "nonacholi_bas", # D
                 "stillenrolled_bas", "attainment_dup_p1e", "writingskills_bas", "speakengl_bas", "trainlength_bas", "stddigitrecall",# H
                 "pettybiz_dum_bas", "dugo1_hrs7d_p99_bas", "duge1_hrs7d_p99_bas", "casual_hrs7d_p99_bas",  "brew_hrs7d_p99_bas", "buy_hrs7d_p99_bas", "others_hrs7d_p99_bas", "chores_hrs7d_p99_bas", "zero_employment_bas", #E
                 "wealthindex_all_bas", "cashtotal4w_basp99_1000", "otherhh_earnings_bas","savingsgroup_bas", "savings_basp99_1000", "currloans_basp99_1000", "creditaccess15_bas", "creditaccess100_bas", #K
                 "hhsupport_z_bas", "commparticipation_z_bas", "neighborsupport_z_bas", "groupsin_bas", "comm_maltreat_bas", "physemo_abuse_z_bas", "decision_making_z_bas", "womens_rights_z_bas", "relatedchief_bas", #F
                 "healthindex_bas", "hiv_bas", "APAI_R_z_bas", #H2
                 "war_exper_bas_z", "recruited_bas", "carriedgun_bas", "forciblymarried_bas", "borechild_bas", #W
                 "risk_aversion_bas", "patience_zscore", #P
                 "vilpopulation", "c_np_educ2", "siteid_sample_p1", "remoteness", "pricelevel", "rentland_sq", "camp", "ngo_total", "c_vending_p99", "c_kiosk_p99", "c_shops_p99", "c_tailoring_p99", "c_hotel_p99", "c_othercommarket", "distancetocapital", "district1", # V
                 "D_T_bar_4_b50", "D_T_4_b50" #spillovers
)



subset_df <- wings_df %>% 
  select(outcomes_T3,
         sample_p1,
         found_p1e,
         controls_LOL,
         siteid,
         assigned_p1,
         assigned_p1_gd) %>% 
  filter(sample_p1 == 1 & found_p1e == 1) %>% 
  select(-sample_p1,
         -found_p1e)


subset_df_long <- subset_df %>% 
  mutate_at(vars(pettybiz_dum_p1e:meals_p1e), as.numeric) %>% 
  gather(outcome_key, 
         outcome_value,
         pettybiz_dum_p1e:meals_p1e)
group_formula <- as.formula(paste0(paste("outcome_value", paste(controls_LOL, collapse=" + "), sep=" ~ "), "+ assigned_p1  + assigned_p1_gd - siteid"))




fit_function <- function(dataset,
                         formula){
  ols_model <- lm(data = dataset,
                  formula = formula)
  ols_SE <- vcovCL(ols_model,
                   cluster = ols_model$model$siteid) %>% 
    diag() %>% 
    sqrt() %>% 
    enframe() %>% 
    rename(term = name,
           SE = value)
  
  tidy_ols <- ols_model %>% 
    tidy(quick = TRUE) %>% 
    inner_join(ols_SE,
               by = "term") %>% 
    mutate(N = nobs(ols_model))
  
  return(tidy_ols) 
}


models <- map_df(outcomes_T3, ~ subset_df_long %>% 
                   filter(outcome_key == .x) %>% 
                   fit_function(dataset = .,
                                formula = group_formula) %>% 
                   mutate(outcome = .x))


model_temp_table <- models %>% 
  filter(term == "assigned_p1") %>% 
  mutate(tstat = estimate/SE,
         pval = 2*pt(-abs(tstat), df = N)) %>% 
  select(outcome,
         estimate, 
         SE) %>% 
  mutate(outcome = factor(outcome),
         outcome = fct_recode(outcome,
                              "Any nonfarm self-employment"             = "biznow_p1e",
                              "Started Enterprise"                    ="igastart_p1e",
                              "Average Work Hours"                    ="total_hrs7d_s2p99_p1e",
                              "Agri Average Work Hours"               ="agri_hrs7d_s2p99_p1e",
                              "NonAgri Average Work Hours"            ="nonagri_hrs7d_s2p99_p1e",
                              "Average Hours Chores"                  ="chores_hrs7d_p99_p1e",
                              "Index of Income"                       ="incomeindex_p1e",
                              "Monthly Cash Earnings"                 ="total_profit4w_r_s2p99_p1e",
                              "Durable Consumption Assets"            ="consdur_p1e",
                              "Non-Durable Consumption"               ="consagg_p1ep99_r",
                              "Durable Assets (production)"           ="proddur_p1e",
                              "Times Went Hungry"                     ="bedhungry_p1e",
                              "Usual Meals per day"                   ="meals_p1e")) %>% 
  mutate_if(is.numeric, round, 3)   



## Tests ##
# Table 3 Column 2

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
  expect_true(all_equal(model_temp_table %>% filter(outcome %in% reference_table$outcome), reference_table, convert = TRUE, ignore_row_order = TRUE))
})
