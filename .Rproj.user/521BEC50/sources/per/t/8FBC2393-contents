########################################################
## read Harry's PrimaryPolls
##
## simon jackman
## simon.jackman@sydney.edu.au
## ussc, univ of sydney
## 2019-11-08 09:38:26
########################################################

library(tidyverse)
library(here)
library(lubridate)
library(ussc)

library(readxl)

## results data set
results <- read_xlsx(here::here("data/Presidential primary results, 1968-2016.xlsx"),
                     sheet = 1)

## polls
polls <- read_xlsx(here::here("data/PrimaryPollsSince1980.xlsx"),sheet = 1) %>%
  mutate(st=state.abb[match(state,state.name)]) %>%
  mutate(ex = interval(as_date(start_date),as_date(end_date)),
         mid_date = int_start(ex) + ((int_end(ex)-int_start(ex))/2)) %>%
  select(-ex)

## question is a unique identifier for poll
## answer_id is a unique identifier for candidate

table(polls$st,polls$cycle)

## write contest date onto polls
polls <- polls %>% 
  left_join(results %>% select(contest,date) %>% unique())