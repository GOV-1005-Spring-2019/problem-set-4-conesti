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
              timestamp = col_datetime(format = "")))  

q1a_setup <- poll %>% 
  group_by(response) %>% 
  filter(response == "Dem") %>%
  count()

q1a_answer <- q1a_setup$n[1]

q1b_setup <- poll %>% 
  group_by(response) %>% 
  count()

q1b_answer <- q1b_setup$n[3]-q1b_setup$n[4]

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
  filter(response %in% c("Dem", "Rep", 3, "Und"), race_eth %in% c("Asian", "Black", "White", "Hispanic", "Other")) %>% 
  mutate(race_eth = fct_relevel(race_eth, c("White", "Black", "Hispanic", "Asian", "Other"))) %>% 
  group_by(response, race_eth) %>% 
  summarize(total = sum(final_weight)) %>%
  spread(key = response, value = total, fill = 0) %>% 
  mutate(all = Rep + Dem + Und + `3`) %>%
  mutate(Dem = Dem / all, Rep = Rep/ all, Und = Und / all) %>%
  select(race_eth, Dem, Rep, Und) %>% na_if(0)



 orig %>% gt() %>% 
  tab_header(
    title = "Polling Results in North Carolina 9th Congressional District") %>% 
   tab_source_note("Source: New York Times Upshot/Siena College 2018 live polls") %>%
 #   fmt_missing(missing_text = "--") %>%
  cols_label(
    race_eth = "",
    Dem = "DEM.",
    Rep = "REP.",
    Und = "UND."
  ) %>%
  
  fmt_percent(columns = vars(Dem, Rep, Und),
              decimals = 0) #%>% 
  
  # This little pipe is that incantation to take this pretty table, turn it
  # into html, and send it to the md file we are creating. Future versions of
  # gt will probably have a better way of doing this. Indeed, does anyone know
  # of one?
  
  #as_raw_html() %>% as.character() %>% cat()


#Question 3
poll %>%
  filter(educ != "[DO NOT READ] Refused") %>%
  mutate(educ = fct_relevel(educ, c("Grade school", "High school", "Some college or trade school", "Bachelors' degree", "Graduate or Professional Degree"))) %>%
  ggplot(aes(x = educ, y = final_weight)) + 
    geom_violin() + 
    coord_flip() +
    labs(title = "More Educated Matter Less in North Carolina 9th", subtitle = "Poll gives more weight to people who are less likely to participate in polls", caption = "New York Times Upshot/Siena College 2018 live polls") +
    ylab("Weight Given to Respondant in Calculating Poll Results") +
    xlab(NULL) +
    geom_jitter(alpha = .4, width = .2)

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

