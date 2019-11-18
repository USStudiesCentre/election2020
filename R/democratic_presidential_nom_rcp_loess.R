##################################################
## Project: Democratic primary candidates - polls
## Date: Thu Jun 20 22:58:33 2019
## Author: Zoe Meers
##################################################

library(tidyverse)
library(here)
library(rvest)
library(janitor)

dem_cand_2020 <- read_html("https://www.realclearpolitics.com/epolls/2020/president/us/2020_democratic_presidential_nomination-6730.html#polls") %>%
    html_nodes("table") %>% 
    .[[4]] %>% 
    html_table() %>%
    slice(-c(1:2)) %>%
    clean_names() %>%
    separate(date, c("start_date", "end_date"), sep = " - ") %>% 
    select(-spread) %>% 
    gather(candidate, poll_value, -poll, -start_date, -end_date) %>% 
    mutate(poll_value = as.numeric(as.character(poll_value)),
           end_date = paste0(end_date, "/2019"),
           end_date = lubridate::as_date(lubridate::mdy(end_date, 
                                                        truncated = 2019)),
           end_date = lubridate::ymd(format(end_date, "2019/%m/%d")),
           start_date = paste0(start_date, "/2019"),
           start_date = lubridate::as_date(lubridate::mdy(start_date, 
                                                        truncated = 2019)),
           start_date = lubridate::ymd(format(start_date, "2019/%m/%d"))) %>% 
    mutate(
        pollster = case_when(
            poll %in% c(
                "BloombergBloomberg",
                "Bloomberg*Bloomberg*"
            ) ~ "Bloomberg",
            poll %in% c(
                "The AtlanticThe Atlantic",
                "The Atlantic/PRRIThe Atlantic"
            ) ~ "The Atlantic/PRRI",
            poll %in% c(
                "ABC/Wash Post TrackingABC/WP Tracking",
                "ABC News/Wash PostABC/WP",
                "ABC News TrackingABC News Tracking"
            ) ~ "ABC/Washington Post",
            poll %in% c(
                "GravisGravis",
                "Gravis*Gravis*"
            ) ~ "Gravis",
            poll %in% c(
                "NBC/WSJNBC/WSJ",
                "NBC News/Wall St. JrnlNBC/WSJ"
            ) ~ "NBC News/WSJ",
            poll %in% c(
                "CNN/ORCCNN/ORC",
                "CNN/Opinion ResearchCNN/ORC"
            ) ~ "CNN/ORC",
            poll %in% c(
                "LA Times/USC TrackingLA Times",
                "LA Times/USCLA Times/USC"
            ) ~ "LA Times/USC",
            poll == "NBC News/SMNBC News" ~ "NBC News/Survey Monkey",
            poll == "IBD/TIPP TrackingIBD/TIPP Tracking" ~ "IBD/TIPP Tracking",
            poll == "Economist/YouGovEconomist" ~ "Economist/YouGov",
            poll == "FOX NewsFOX News" ~ "FOX News",
            poll == "MonmouthMonmouth" ~ "Monmouth",
            poll == "Reuters/IpsosReuters" ~ "Reuters/Ipsos",
            poll == "Rasmussen ReportsRasmussen" ~ "Rasmussen",
            poll == "CBS NewsCBS News" ~ "CBS News",
            poll == "McClatchy/MaristMcClatchy" ~ "McClatchy/Marist",
            poll == "CBS News/NY TimesCBS/NYT" ~ "CBS News/NY Times",
            poll == "Pew ResearchPew" ~ "Pew Research",
            poll == "CNBCCNBC" ~ "CNBC",
            poll == "USA Today/SuffolkUSA Today" ~ "USA Today/Suffolk",
            poll == "Associated Press-GfKAP-GfK" ~ "Associated Press-GfK",
            poll == "QuinnipiacQuinnipiac" ~ "Quinnipiac",
            poll == "Boston GlobeBoston Globe" ~ "Boston Globe",
            poll == "GWU/BattlegroundGWU/Battleground" ~ "GWU/Battleground",
            poll == "PPP (D)PPP (D)" ~ "PPP (D)",
            poll == "IBD/TIPPIBD/TIPP" ~ "IBD/TIPP",
            poll == "MSNBC/Telemundo/MaristMSNBC/Telemundo/Marist" ~ "MSNBC/Telemundo/Marist",
            poll == "Politico/Morning ConsultPolitico" ~ "Politico/Morning Consult",
            poll == "The Hill/HarrisXThe Hill" ~ "The Hill/Harris",
            poll == "CNNCNN" ~ "CNN",
            poll == "EmersonEmerson" ~ "Emerson",
            poll == "Harvard-HarrisHarris" ~ "Harvard-Harris",
            poll == "SurveyUSASUSA" ~ "SurveyUSA",
            poll == "Guardian/SurveyUSAGuardian/SUSA" ~ "Guardian/SurveyUSA",
            TRUE ~ poll
        )
    ) %>% 
    mutate(candidate = str_to_title(candidate),
           candidate = str_replace(candidate, "O_rourke", "O'Rourke"))

top_six_candidates <- dem_cand_2020 %>% 
    group_by(candidate) %>% 
    arrange(desc(end_date)) %>% 
    slice(1) %>% 
    ungroup() %>% 
    arrange(desc(poll_value)) %>% 
    slice(1:6) 


## viz
 
ggplot(dem_cand_2020 %>% 
           filter(candidate %in% c(top_six_candidates$candidate)), 
       aes(end_date, 
           poll_value,
           group = candidate)) + 
    stat_smooth(show.legend = F,
                color = ussc::ussc_colours("dark grey"),
                alpha = 0.4,
                size = 1) + 
        geom_point(show.legend = F, 
                   shape = 1,
                   size = 1.5,
                   color = ussc::ussc_colours("dark blue")) + 
    ussc::theme_ussc() + 
    scale_y_continuous(breaks = c(0,10,20,30,40,50,60),
                       labels = c("0%","10%","20%","30%","40%","50%","60%")) + 
    labs(x = NULL,
         y = NULL,
         title = '2020 Democratic primary polling averages',
         subtitle = "Loess smoothing model") + 
    theme(panel.grid.minor = element_blank()) + 
    facet_wrap(~candidate) 

ggsave(last_plot(),
       width = 8,
       height = 8,
       file = here::here(paste0("graphs/dem_candidate_", format(Sys.time(), "%Y-%m-%d"), ".png")))


