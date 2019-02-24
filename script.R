library(tidyverse)
library(readr)
library(gt)

poll <- read_csv("ps_4_elections-poll-nc09-3.csv",
            col_types =  cols(
              .default = col_character(),
              turnout_scale = col_double(),
              turnout_score = col_double(),
              w_LV = col_double(),
              w_RV = col_double(),
              final_weight = col_double(),
              timestamp = col_datetime(format = ""))) %>%
  filter(response %in% c("Dem", "Rep", "Und")) 

q1a_setup <- poll %>% 
  group_by(response) %>% 
  filter(response == "Dem") %>%
  count()

q1a_answer <- q1_setup$n[1]

q1b_setup <- poll %>% 
  group_by(response) %>% 
  count()

q1b_answer <- q2_setup$n[2]-q2_setup$n[3]

q1c_setup <- poll %>%
  filter(gender != gender_combined) %>% 
  count()

q1c_answer <- q1c_setup$n[1]

q1d_setup <- poll %>%
  filter(race_eth == "White", file_race_black != "White") %>% 
  count()

q1d_answer <- q1d_setup$n[1]

q1e_setup_dems <- poll %>% 
  select(response, timestamp) %>% 
  filter(response == "Dem") %>%
  arrange(timestamp)

q1e_setup_reps <- poll %>% 
  select(response, timestamp) %>% 
  filter(response == "Rep") %>% 
  arrange(timestamp)

q1e_answer <- round(as.numeric(q1e_setup_reps$timestamp[1] - q1e_setup_dems$timestamp[1]))

orig <- read_csv(file = "ps_4_elections-poll-nc09-3.csv",
                 col_types =  cols(
                   .default = col_character(),
                   turnout_scale = col_double(),
                   turnout_score = col_double(),
                   w_LV = col_double(),
                   w_RV = col_double(),
                   final_weight = col_double(),
                   timestamp = col_datetime(format = ""))) %>%
  select(educ4, response, final_weight) %>% 
  filter(response %in% c("Dem", "Rep", "Und"), educ4 != "[DO NOT READ] Don't know/Refused") %>% 
  group_by(response, educ4) %>% 
  summarize(total = sum(final_weight)) %>% 
  spread(key = response, value = total) %>%
  mutate(all = Rep + Dem + Und) %>%
  mutate(Dem = Dem / all, Rep = Rep/ all, Und = Und / all)

