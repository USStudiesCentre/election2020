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


polls <- read_excel(here::here("data/PrimaryPollsSince1980.xlsx"), sheet = 1) %>% 
  select(-`...6`) %>% 
  filter(state == "Iowa") %>%
  separate(contest, c("election_year", "party"), sep = 4, remove = FALSE) %>% 
  mutate(st=state.abb[match(state,state.name)]) %>%
  # grab mid date for poll
  mutate(ex = interval(as_date(start_date),as_date(end_date)),
         mid_date = int_start(ex) + ((int_end(ex)-int_start(ex))/2)) %>%
  select(-ex) %>% 
  # weight observations
  mutate(weight = sqrt(sample_size)) %>% 
  filter(!(last_name %in% c("other/undecided", "undecided", "undecided_and_other", "wont_vote", "Other",
                            "Other/Undecided", "other", "Undecided", "Undecided (Vol)", "Undecided (Vol.)",
                            "refused/undecided", "Refused", "Refused (Vol.)")))

iowa_results <- read_excel(here::here("data/Presidential primary results, 1968-2016.xlsx"), sheet = 1)  %>% 
  filter(state == "Iowa") %>%
  select(iowa_votes = votes,
         iowa_pct = pct,
         contest,
         true_name, 
         iowa_date = date) %>% 
# the set should be filtered for those who received >10% of vote
  filter(iowa_pct >= 10) %>% 
  filter(!true_name %in% c("Uncommitted", "No preference", "Others")) %>% 
# some candidates are repeated twice (different sources). They're all pretty similar, so I'll just select one but this should be sorted out properly sooner rather than later
  distinct(contest, true_name, .keep_all = TRUE) 
# %>% 
# add 1992 candidates back in as Harkin appears to be an outlier in terms of support from Iowans (not surprising given that he was their rep)
  # bind_rows(., read_excel(here::here("data/Presidential primary results, 1968-2016.xlsx"), sheet = 1)  %>% 
  #             filter(state == "Iowa") %>%
  #             select(iowa_votes = votes,
  #                    iowa_pct = pct,
  #                    contest,
  #                    true_name, 
  #                    iowa_date = date) %>% 
  #             filter(contest == "1992D"))

iowa_polls <- polls %>% 
  drop_na(true_name) %>% 
# subset candidates who received more than 10% of caucus support
  inner_join(., iowa_results %>% 
               distinct(contest, true_name, iowa_date, iowa_pct)) %>% 
  filter(share>0) %>% 
  filter(as.Date(end_date) <= as.Date(iowa_date)) %>% 
  mutate(year_before = iowa_date - years(2)) %>% 
  # filter for year out to remove outliers on x axis 
  filter(as.Date(end_date) >= as.Date(year_before)) %>%
  # remove 1992 candidates except for Harkin and Clinton
  # filter(!last_name %in% c("Kerrey", "Tsongas", "Brown")) %>% 
  mutate(year_party = paste0(cycle, ": ", candidateparty)) %>% 
  mutate(last_name_winner = case_when(
    last_name == "Reagan" & cycle %in% c(1980, 1984) ~ paste0("bold(", last_name, ")"),
    last_name == "Bush" & cycle %in% c(1988, 2000, 2004) ~ paste0("bold(", last_name, ")"),
    last_name == "Clinton" & cycle %in% c(1992, 1996) ~ paste0("bold(", last_name, ")"),
    last_name == "Obama" & cycle %in% c(2008, 2012) ~ paste0("bold(", last_name, ")"),
    last_name == "Trump" & cycle == 2016 ~ paste0("bold(", last_name, ")"),
    TRUE ~ last_name
  )) %>% 
  group_by(year_party) %>%
  arrange(iowa_pct, .by_group = TRUE)  %>% 
  ungroup() %>% 
  mutate(year_party = factor(year_party))
  
iowa_eda_plot <- ggplot(iowa_polls, aes(x = end_date, y = share, weight = weight)) +
  geom_point(alpha = 0.7, color = ussc_colours("grey")) +
  scale_y_continuous(limits = c(0,85))+
  stat_smooth(method = "loess", aes(weight = weight), color = ussc_colours("light blue"), se = FALSE, span = 1) +
  geom_hline(aes(yintercept = iowa_pct), color = ussc_colours("black")) +
  geom_vline(aes(xintercept = as.POSIXct(as.Date(iowa_date))), color = ussc_colours("black")) +
  geom_point(aes(x = iowa_date, y = iowa_pct), color = ussc_colours("black")) +
  theme_minimal() +
  facet_wrap(year_party~last_name_winner, 
             scales = "free_x",
             labeller = label_parsed,
             nrow = 5) +
  scale_x_datetime(
    #breaks = "4 months", 
    date_labels = "%b\n'%y") +
  labs(x = "Date",
       y = "Share of poll",
       subtitle = "The crosshairs show the date of the Iowa caucus by the amount the candidate won.\nWinning presidential candidates are in bold.") +
  theme(panel.grid.minor = element_blank(),
        text = element_text(family = "Halis GR"))

iowa_eda_plot_logo <- plot_ussc_logo(iowa_eda_plot)
#ggsave(iowa_eda_plot_logo , file = here::here("graphs/iowa_eda.png"), width = 16, height = 12)


# extract `stat_smooth` data

iowa_data <- ggplot_build(iowa_eda_plot)

loess_model <- iowa_data[["data"]][[2]] %>% 
  group_by(PANEL) %>% 
  arrange(desc(x)) %>% 
  slice(1) %>% 
  select(PANEL, y) %>% 
  ungroup()

# extract real result from `geom_hline`

actual_result <- iowa_data[["data"]][[3]] %>% 
  group_by(PANEL) %>% 
  slice(1)%>% 
  select(PANEL, yintercept) %>% 
  ungroup() 

# join both data frames, find the difference

diff <- left_join(loess_model, actual_result, by = c("PANEL" = "PANEL")) %>%
  rename(actual = yintercept,
         loess = y) %>% 
  mutate(difference = abs(round(loess-actual, 1))) %>% 
  arrange(desc(difference)) %>% 
  # add facet labels in
  left_join(iowa_data[["layout"]][["layout"]] %>% 
              select(PANEL, 
                     contest = year_party, 
                     candidate = last_name_winner)) %>% 
  select(-PANEL) %>% 
  mutate(candidate = gsub(".*\\(", "", candidate),
         candidate = gsub("\\)", "", candidate)) %>% 
  separate(contest, c("cycle", "party"), sep = ": ") %>%
  mutate(party = str_to_title(party)) %>% 
  select(candidate, cycle, party, everything())


# grab viable candidates who are not included in Iowa analysis

viable <- read_excel(here::here("data/NationalPrimaryWinnerswithhomestateandFEC.xlsx"), sheet = 1) %>% 
  inner_join(., read_excel(here::here("data/Presidential primary results, 1968-2016.xlsx"), sheet = 1)  %>% 
               filter(state == "Iowa") %>%
               select(iowa_votes = votes,
                      iowa_pct = pct,
                      contest,
                      true_name, 
                      iowa_date = date) %>% 
               distinct(contest, true_name, iowa_date, iowa_pct)) %>%  
  filter(iowa_pct < 10) %>% 
  filter(nationalvotepercentage>15) %>% 
  group_by(contest) %>% 
  distinct(true_name, .keep_all = TRUE) %>% 
  ungroup() %>% 
  select(-contest, -last_name)

