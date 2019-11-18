##################################################
## Project: Democratic primary candidates - polls
## Date: Thu Jun 20 22:58:33 2019
## Author: Zoe Meers
##################################################

library(tidyverse)
library(here)
library(rvest)
library(janitor)
library(rstan)

options(mc.cores = parallel::detectCores())


# grab data from RCP, do basic cleaning
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
    select(-poll) %>% 
    mutate(candidate = str_to_title(candidate),
           candidate = str_replace(candidate, "O_rourke", "O'Rourke")) %>% 
    spread(candidate, poll_value)

# modeling

# prep for stan

stan_candidate <- function(df, candidate){
    
    cand_name <- rlang::enquo(candidate)
    
    # Stretch out to get missing values for days with no polls
    polls <- left_join(tibble(end_date = seq(
        from = min(df$end_date),
        to = max(df$end_date),
        by = "day"
    )), df) %>%
        group_by(end_date) %>%
        mutate(N = 1:n())
    
    return(polls %>%
               select(!!cand_name) %>% 
               select(-end_date) %>%
               as.data.frame() %>%
               as.matrix()
    )
    
}

# Bernie Sanders

sanders_matrix <- stan_candidate(df = dem_cand_2020, candidate = Sanders) 

sanders_matrix[is.na(sanders_matrix)] <- -9

sanders_model <- stan("state_space_polls.stan", 
                      data = list(T = nrow(sanders_matrix), 
                                  polls = ncol(sanders_matrix), 
                                  Y = sanders_matrix, 
                                  sigma = sigma,
                                  initial_prior = 50))


# Elizabeth Warren

warren_matrix <- stan_candidate(df = dem_cand_2020, candidate = Warren) 

waren_matrix[is.na(warren_matrix)] <- -9

warren_model <- stan("state_space_polls.stan", 
                      data = list(T = nrow(warren_matrix), 
                                  polls = ncol(warren_matrix), 
                                  Y = warren_matrix, 
                                  sigma = sigma,
                                  initial_prior = 50))

# Biden

biden_matrix <- stan_candidate(df = dem_cand_2020, candidate = Biden) 

biden_matrix[is.na(biden_matrix)] <- -9

biden_model <- stan("state_space_polls.stan", 
                     data = list(T = nrow(biden_matrix), 
                                 polls = ncol(biden_matrix), 
                                 Y = biden_matrix, 
                                 sigma = sigma,
                                 initial_prior = 50))




# Pull the state vectors

mu_biden <- extract(biden_model, pars = "mu", permuted = T)[[1]] %>% 
    as.data.frame

mu_warren <- extract(warren_model, pars = "mu", permuted = T)[[1]] %>% 
    as.data.frame

mu_sanders <- extract(sanders_model, pars = "mu", permuted = T)[[1]] %>% 
    as.data.frame


# Rename to get dates
names(mu_biden) <- unique(paste0(dem_cand_2020$end_date))
names(mu_warren) <- unique(paste0(dem_cand_2020$end_date))
names(mu_sanders) <- unique(paste0(dem_cand_2020$end_date))



# summarise uncertainty for each date

mu_ts_warren <- mu_warren %>% melt %>% 
    mutate(date = as.Date(variable)) %>% 
    group_by(date) %>% 
    summarise(median = median(value),
              lower = quantile(value, 0.025),
              upper = quantile(value, 0.975),
              candidate = "Warren")

mu_ts_biden <- mu_biden %>% melt %>% 
    mutate(date = as.Date(variable)) %>% 
    group_by(date) %>% 
    summarise(median = median(value),
              lower = quantile(value, 0.025),
              upper = quantile(value, 0.975),
              candidate = "Biden")

mu_ts_sanders <- mu_sanders %>% melt %>% 
    mutate(date = as.Date(variable)) %>% 
    group_by(date) %>% 
    summarise(median = median(value),
              lower = quantile(value, 0.025),
              upper = quantile(value, 0.975),
              candidate = "Sanders")



# Plot results


bind_rows(mu_ts_sanders, mu_ts_biden, mu_ts_warren) %>% 
    ggplot(aes(x = date)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = candidate),alpha = 0.1) +
    geom_line(aes(y = median, colour = candidate)) +
    ylim(30, 60) +
    scale_colour_ussc("blue", guide = FALSE) + 
    scale_fill_ussc("blue", guide = FALSE) +
    geom_point(data = dem_cand_2020, aes(x = end_date, y = Warren), size = 0.2, 
               colour = ussc_colours("grey")) +
    geom_point(data = dem_cand_2020, aes(x = end_date, y = Sanders), size = 0.2, 
               colour =  ussc_colours("grey")) +
    geom_point(data = dem_cand_2020, aes(x = end_date, y = Trump), size = 0.2, 
               colour =  ussc_colours("grey")) +
    theme_ussc() + 
    labs(x = "Date",
         y = "Implied vote share",
         title = "Poll aggregation with state-space smoothing")
