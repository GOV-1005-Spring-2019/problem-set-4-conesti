library(tidyverse)
library(readr)
library(gt)
library(ggplot2)


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
  select(race_eth, response, final_weight) %>% 
  filter(response %in% c("Dem", "Rep", "Und"), race_eth %in% c("Asian", "Black", "White", "Hispanic", "Other")) %>% 
  group_by(response, race_eth) %>% 
  summarize(total = sum(final_weight)) %>%
  spread(key = response, value = total)
  orig[is.na(orig)] <- 0
  orig %>% mutate(all = Rep + Dem + Und) %>% 
  mutate(Dem = Dem / all, Rep = Rep/ all, Und = Und / all)

#Question 3
poll$educ<-factor(poll$educ, levels = poll$educ[order(poll$final_weight)])
poll %>% filter(educ != "[DO NOT READ] Refused") %>%
  ggplot(aes(x = educ, y = final_weight)) + 
    geom_violin() + 
    coord_flip() +
    labs(title = "More Educated Matter Less in North Carolina 9th", subtitle = "Poll gives more weight to people who are less likely to participate in polls", caption = "New York Times Upshot/Siena College 2018 live polls") +
    ylab("Weight Given to Respondant in Calculating Poll Results") +
    xlab(NULL) +
    geom_jitter(alpha = .4)

#Question 4

num_rep <- sum(poll$partyid == "Republican")
num_dem <- sum(poll$partyid == "Democrat")

poll %>% 
  select(response, partyid, ager, final_weight) %>% 
  filter(partyid %in% c("Republican", "Democrat", "Independent (No party)")) %>% 
  group_by(response, partyid) %>%
  summarize(total = sum(final_weight)) %>%
  ggplot(aes(x = partyid, y = total)) + geom_col(aes(fill = response)) +
  coord_flip() +
  labs(title = "Republican Success Relies on Independent Vote", subtitle = "Despite a minority of registered voters, Republicans hold an edge with independent vote") + 
  xlab("Party Affiliation") + 
  ylab("Vote Count") + 
  theme(legend.title = element_blank())

