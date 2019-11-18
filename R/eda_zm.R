##################################################
## Project: Exploratory data analysis
## Date: Sat Nov  9 17:40:30 2019
## Author: Zoe Meers
##################################################


knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message=FALSE)
options(knitr.kable.NA = '')
library(ussc)
library(tidyverse)
library(here)
library(knitr)
library(kableExtra)
library(fs)
library(readxl)
library(lubridate)

# get list of files

dir_ls(here::here("data"))

endorse <- read_excel(here::here("data/Endorsementpointsbyday.xlsx"), sheet = 1) %>% 
  separate(contest, c("election_year", "party"), sep = 4, remove = FALSE)

endorsements <- ggplot(endorse %>% 
         group_by(endorsee) %>% 
         mutate(label = if_else(endorsement_points > 100 &
                                days_to_iowa == max(days_to_iowa), 
                                as.character(endorsee), 
                                NA_character_)) %>% 
         ungroup(), 
       aes(days_to_iowa, 
           endorsement_points, 
           group = endorsee,
           color = nominee)) +
  geom_vline(xintercept = 0, color = ussc_colours("black"), alpha = 0.8) +
  geom_line(show.legend = FALSE) +
  scale_x_continuous(expand=c(0, 2), 
                     "Days to/since Iowa") +
  ggrepel::geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   nudge_y = 50,
                   segment.color = "transparent",
                   na.rm = TRUE, 
                   show.legend = FALSE) +
  facet_wrap(election_year~party) +
  scale_colour_ussc("light_blue_to_orange") +
  labs(y = "Endorsement points") + 
  theme_ussc() +
  theme(panel.grid.minor = element_blank())

endorsements


polls <- read_excel(here::here("data/PrimaryPollsSince1980.xlsx"), sheet = 1) %>% 
  separate(contest, c("election_year", "party"), sep = 4, remove = FALSE) %>% 
  mutate(st=state.abb[match(state,state.name)]) %>%
  mutate(ex = interval(as_date(start_date),as_date(end_date)),
         mid_date = int_start(ex) + ((int_end(ex)-int_start(ex))/2)) %>%
  select(-ex)


polls_2008 <- polls %>% 
  filter(election_year == 2008)

ggplot(polls_2008 %>% filter(party == "D" &
                             sample_size > 800 &
                             mid_date > "2007-11-04 00:00:00") %>% 
       filter(last_name %in% c("Clinton", "Obama", "Edwards",
                               "Kucinich", "Richardson", "Biden")), 
       aes(mid_date, share),
       colour = ussc_colours("light blue")) +
  geom_vline(xintercept =  as.POSIXct(as.Date("2008-01-03 00:00:00")),
             color = "black") +
  geom_point(show.legend = FALSE,
             colour = ussc_colours("light blue"),
             alpha = 0.7) +
  geom_smooth(show.legend = FALSE,
              colour = ussc_colours("light blue")) + 
  scale_x_datetime(date_breaks = "2 months", date_labels ="%b %y") +
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60)) +
  facet_wrap(~last_name) + 
  theme_ussc() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = NULL,
       y = "Share of poll",
       subtitle = "Vertical line is the Iowa caucus.")

  