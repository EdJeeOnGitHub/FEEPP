library(tidyverse)
library(haven)


df <- read_dta("data/powerful-women/Does Exposure Reduce Bias - Replication/s24para.dta")        

##### Stata:  append the South 24 Paraganas data
df <- df %>%
  mutate(blockname = ifelse(blockname == "",
                            NA,
                            blockname)) %>% 
  fill(blockname)

create_res_f <- function(df, year){
  year_var <- paste0("res", year)
  res_df <- df %>% 
    mutate(!!paste0("res_f", year) := !!sym(year_var) %in% c("SCW",
                                                        "STW",
                                                        "W",
                                                        "SC(W)",
                                                        "ST(W)"))
  return(res_df)
}

# Could have used purrr here, ah well
df <- df %>% 
  create_res_f(1998) %>% 
  create_res_f(2003) %>% 
  create_res_f(2008)

##### Stata: append the Hooghly data