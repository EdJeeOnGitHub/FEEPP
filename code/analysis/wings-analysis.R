library(haven)
library(tidyverse)
library(sandwich)
library(broom)
library(testthat)
# Whether to run extra bootstrapped explorations
extra <- FALSE
# Loading data
wings_df <- read_dta("data/wings-data/WINGS Dataverse Files/Data/WINGS.dta")

if(nrow(wings_df) != 6630) {
  stop("Observations missing")
}

if(ncol(wings_df) != 6190) {
  stop("Columns missing")
}

# Table 3 outcome variables
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
# Table 4 outcome variables
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
# Each row corresponds to each local in Stata .do file
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





# Generate dataset for table 3
table_3_df <- wings_df %>% 
  select(outcomes_T3,
         sample_p1,
         found_p1e,
         controls,
         siteid,
         assigned_p1,
         assigned_p1_gd) %>% 
  filter(sample_p1 == 1 & found_p1e == 1) %>% 
  select(-sample_p1,
         -found_p1e)

# Reshape to long
table_3_long_df <- table_3_df %>% 
  mutate_at(vars(pettybiz_dum_p1e:meals_p1e), as.numeric) %>% 
  mutate(col_3_treatment = assigned_p1_gd - assigned_p1) %>% 
  gather(outcome_key, 
         outcome_value,
         pettybiz_dum_p1e:meals_p1e)

# Create regression equations for each outcome
col_2_group_formula <- as.formula(paste0(paste("outcome_value", paste(controls, collapse=" + "), sep=" ~ "), "+ assigned_p1  + assigned_p1_gd - siteid"))
col_3_group_formula <- as.formula(paste0(paste("outcome_value", paste(controls, collapse=" + "), sep=" ~ "), "+ assigned_p1 + col_3_treatment - siteid"))
#' Fit Wings  Table 3 Column 2 regression
#'
#' @param dataset Table 3 data
#' @param formula Regression equation
#'
#' @return Tidy fitted models.
fit_function <- function(dataset,
                         formula){
  ols_model <- lm(data = dataset,
                  formula = formula)
  # Cluster on siteid
  ols_SE <- vcovCL(ols_model,
                   cluster = ols_model$model$siteid) %>% 
    diag() %>% 
    sqrt() %>% 
    enframe() %>% 
    rename(term = name,
           SE = value)
  # Join back onto df
  tidy_ols <- ols_model %>% 
    tidy(quick = TRUE) %>% 
    inner_join(ols_SE,
               by = "term") %>% 
    mutate(N = nobs(ols_model))
  
  return(tidy_ols) 
}


table_3_col_2_models <- map_df(outcomes_T3, ~table_3_long_df %>% 
                   filter(outcome_key == .x) %>% 
                   fit_function(dataset = .,
                                formula = col_2_group_formula) %>% 
                   mutate(outcome = .x))


table_3_col_3_models <- map_df(outcomes_T3, ~table_3_long_df %>% 
                                 filter(outcome_key == .x) %>% 
                                 fit_function(dataset = .,
                                              formula = col_3_group_formula) %>% 
                                 mutate(outcome = .x))


recode_outcome_var <- function(df){
  df <- df %>% 
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
                                "Usual Meals per day"                   ="meals_p1e"))
  return(df)
}
clean_table_3_col_2_models <- table_3_col_2_models %>% 
  filter(term == "assigned_p1") %>% 
  mutate(tstat = estimate/SE,
         pval = 2*pt(-abs(tstat), df = N)) %>% 
  select(outcome,
         estimate, 
         SE) %>%
  recode_outcome_var() %>% 
  mutate_if(is.numeric, round, 3)   


clean_table_3_col_3_models <- table_3_col_3_models %>% 
  filter(term == "assigned_p1" ) %>%   
  select(outcome, estimate, SE) %>% 
  recode_outcome_var() %>% 
  mutate_if(is.numeric, round, 3)  



  
  
clean_table_3_col_2_models
clean_table_3_col_3_models
# Write output to results

write_csv(clean_table_3_col_2_models, "results/wings/table_3_column_2.csv")
write_csv(clean_table_3_col_3_models, "results/wings/table_3_column_3.csv")





## Extra
if (extra == TRUE) {
  
  # Bootstrap results
  library(modelr)
  boots <- table_3_long_df %>% 
    filter(outcome_key == "biznow_p1e") %>%
    bootstrap(n = 1000)  
  
  
  boot_models <- boots %>% 
    mutate(model = map(strap, fit_function, formula = group_formula))
  
  
  
  boot_models %>% 
    unnest(model) %>% 
    filter(term == "assigned_p1") %>% 
    ggplot(aes(x = estimate)) +
    geom_histogram() +
    geom_vline(xintercept = model_temp_table %>% 
                 filter(outcome == "Any nonfarm self-employment") %>% 
                 select(estimate) %>% 
                 pull(),
               linetype = "longdash") +
    theme_minimal()
  
}
