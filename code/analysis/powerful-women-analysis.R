library(tidyverse)
library(haven)


df <- read_dta("data/powerful-women/Does Exposure Reduce Bias - Replication/s24para.dta")        


df <- df %>%
  mutate(blockname = ifelse(blockname == "",
                            NA,
                            blockname)) %>% 
  fill(blockname)

