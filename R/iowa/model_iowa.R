##################################################
## Project: Predictive modeling
## Date: Sat Nov  9 17:40:30 2019
## Author: Zoe Meers
##################################################

library(ussc)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(tidyr)
library(tibble)
library(here)
library(fs)
library(readxl)
library(lubridate)
library(stats)
library(nls.multstart) # devtools::install_github('padpadpadpad/nls.multstart')
library(brms)
library(slide) #remotes::install_github("DavisVaughan/slide") -- for rolling windows in purrr
library(mgcv)
library(nls2)

future::plan(future::multicore)
options(future.globals.maxSize = 2050 *1024^2)



if(file.exists(here::here("data/rolling_iowa_polls.RData"))){
  load(here::here("data/rolling_iowa_polls.RData"))
}

if(!file.exists(here::here("data/rolling_iowa_polls.RData"))){
  # Data cleaning 
  polls <- read_excel(here::here("data/PrimaryPollsSince1980.xlsx"), sheet = 1) 
  
  polls <- polls %>% 
    as_tibble() %>% 
    lazy_dt() %>% 
    select(-`...6`) %>% 
    filter(state == "Iowa") %>%
    as_tibble() %>% 
    separate(contest, c("election_year", "party"), sep = 4, remove = FALSE) %>% 
    mutate(st=state.abb[match(state,state.name)]) %>%
    # grab mid date for poll
    mutate(ex = interval(as_date(start_date),as_date(end_date)),
           mid_date = int_start(ex) + ((int_end(ex)-int_start(ex))/2)) %>%
    select(-ex) %>% 
    # weight observations
    mutate(weight = sqrt(sample_size))  %>% 
    as_tibble()
  
  iowa_results <- read_excel(here::here("data/Presidential primary results, 1968-2016.xlsx"), sheet = 1) 
  
  iowa_results <- iowa_results %>%
    as_tibble() %>% 
    lazy_dt() %>% 
    filter(state == "Iowa") %>%
    select(votes,
           pct,
           contest,
           true_name, 
           date) %>% 
    rename("iowa_votes" = votes,
           "iowa_date" = date,
           "iowa_pct" = pct) %>% 
    # the set should be filtered for those who received >10% of vote
    filter(iowa_pct >= 10) %>% 
    filter(!true_name %in% c("Uncommitted", "No preference", "Others")) %>% 
    # some candidates are repeated twice (different sources). They're all pretty similar, so I'll just select one but this should be sorted out properly sooner rather than later
    distinct(contest, true_name, .keep_all = TRUE) %>% 
    as_tibble()
  
  iowa_polls <- polls %>% 
    drop_na(true_name) %>% 
    # subset candidates who received more than 10% of caucus support
    inner_join(., iowa_results %>% 
                 distinct(contest, true_name, iowa_date, iowa_pct)) %>% 
    filter(share>0) %>% 
    filter(as.Date(end_date) <= as.Date(iowa_date)) %>% 
    mutate(year_party = paste0(cycle, ": ", candidateparty)) %>% 
    group_by(year_party) %>%
    arrange(iowa_pct, .by_group = TRUE)  %>% 
    ungroup() %>% 
    mutate(year_party = factor(year_party)) %>% 
    as_tibble()

  
  
  candidate_endorsements <- read_excel(here::here("data/Endorsementpointsbyday.xlsx"), sheet = 1) 
  
  candidate_endorsements <- candidate_endorsements %>% 
    separate(contest, c("election_year", "party"), sep = 4, remove = FALSE) 
  
  
  # the next bit of code creates a rolling data frame for each candidate and poll
  
  rolling_iowa_polls <- left_join(iowa_polls, candidate_endorsements,
                                  by = c("contest", 
                                         "last_name" = "endorsee",
                                         "end_date" = "as_of_date", 
                                         "party", 
                                         "election_year")) %>% 
    group_by(contest, last_name, end_date) %>% 
    fill(endorsement_points) %>% 
    ungroup() %>% 
    filter(sample_size != 0) %>% 
    mutate(new_weight = sqrt(abs((share * (1 - share))/sample_size))) %>% 
    mutate(t = round(difftime(iowa_date, start_date, units = "days")),
           t_rev = round(difftime(start_date, iowa_date, units = "days")),
           t = as.numeric(gsub("\\sdays", "", t)),
           t_rev = as.numeric(gsub("\\sdays", "", t_rev)),
           p_ct = share/100,
           y_c = iowa_pct/100,
           e_ct = endorsement_points,
           # replace NA (i.e. no endorsement points) with zero
           e_ct = replace_na(e_ct, 0)) %>% 
    filter(t<=365) %>% 
    group_by(contest, last_name) %>%
    arrange(t_rev) %>%
    nest() %>%
    ungroup() %>% 
    mutate(data = furrr::future_map(data, ~ slide_index(., .i = .$t_rev,  ~.x, .before = Inf) %>% 
                                      tibble::enframe()
    )
    ) %>% 
    unnest(data) %>% 
    mutate(date = furrr::future_map(value, ~.x %>% 
                                      slice(n()) %>% 
                                      select(mid_date)
    ) %>% 
      map("mid_date") %>% 
      reduce(c),
    last_date_of_poll = furrr::future_map(value, ~.x %>% 
                                      slice(n()) %>% 
                                      select(end_date)
    ) %>% 
      map("end_date") %>% 
      reduce(c),
    iowa_date = furrr::future_map(value, ~.x %>% 
                                    slice(n()) %>% 
                                    select(iowa_date)) %>% 
      map("iowa_date") %>% 
      reduce(c),
    t = furrr::future_map(value, ~.x %>% 
                            slice(n()) %>% 
                            select(t)) %>% 
      map_dbl("t")
    ) %>% 
    rename(rolling_t = value) %>% 
    mutate(rolling_t = map(rolling_t, ~select(., c(y_c, t, p_ct, e_ct, new_weight, 
                                                   pollster, 
                                                   mid_date, start_date, end_date,
                                                   iowa_date, iowa_pct, sample_size)))) %>% 
    select(contest, last_name, iowa_date, date, days_to_iowa = t, rolling_t) %>% 
    arrange(contest, last_name)
  
   # within each window expand dates and t
   # rolling_iowa_polls <- rolling_iowa_polls %>% 
   #  mutate(rolling_t = furrr::future_map(rolling_t, ~ .x %>% 
   #                                      mutate(., date = start_date) %>% 
   #                                      as_tibble() %>% 
   #                                      complete(.x, date = seq(min(.$date), 
   #                                                       max(.$end_date), by = "day")) %>% 
   #                                      mutate(t = round(difftime(iowa_date, 
   #                                           
   #  ))
  
  
  save(rolling_iowa_polls, file = here::here("data/rolling_iowa_polls.RData"))
  
}
  

# Option 1: Most recent polls
# for each additional poll, run a new model to incorporate the new information
# e.g. iowa_polls %>% mutate(models = map(rolling_t, ~gam(y_c~s(p_ct), data = .x))



iowa_polls_models <- rolling_iowa_polls %>%
   mutate(rolling_t = purrr::map(rolling_t, ~drop_na(.x, e_ct))) %>%
   mutate(rolling_t = purrr::map(rolling_t, ~drop_na(.x, p_ct))) %>%
  # find distinct covariates for splines
  # data must have >= 5 polls in each window
   filter(purrr::map_int(rolling_t, nrow) >= 5) %>% 
   filter(last_name != "Harkin") %>% 
  # find distinct e_ct and p_ct
   mutate(unique_ect = purrr::map(rolling_t, ~distinct(.x, e_ct)),
          unique_pct = purrr::map(rolling_t, ~distinct(.x, p_ct))) %>% 
   filter(purrr::map_int(unique_ect, nrow) >= 4) %>%
   filter(purrr::map_int(unique_pct, nrow) >= 4) %>%
   select(-c(unique_ect, unique_pct)) %>% 
   mutate(gam_models = furrr::future_map(rolling_t,
                                 ~broom::tidy(gam(y_c~s(p_ct, 
                                                        k = 3) + 
                                                    s(e_ct, 
                                                      k = 3), 
                                                 data = .x,
                                                 weights = new_weight
                                                 ))
         ),
     # still not really working, ugh, so I'm going to try a basic GAM in ggplot and take the results from that
     gam_graphs = furrr::future_map(rolling_t,
                                   ~ggplot(.x,  aes(t, p_ct)) +
                                     geom_point() +
                                     geom_hline(aes(yintercept = y_c)) +
                                     geom_smooth(method = 'gam',
                                                 aes(predict(gam(formula = y_c ~ s(p_ct, k = 3) + 
                                                                   s(e_ct, k = 3),
                                                                 data=.x,
                                                                 weights = new_weight),
                                                             .x))
                                                 ) +
                                     scale_x_reverse() + 
                                     theme_ussc()
     ))

# we can extract y_c - \hat{y_{ct}} for each ct

extract_iowa_estimates <- iowa_polls_models %>% 
  mutate(ggplot_build = furrr::future_map(gam_graphs,  # this allows us to grab to data in ggplot
                                          ~ggplot_build(.x)),
         # extract GAM parameters
         gam_result = furrr::future_map(ggplot_build, 
                                        ~.x[["data"]][[3]] %>% 
                            slice(n())) %>%  
           map_dbl("y", .null = NA_real_), 
         # extract actual result in Iowa caucus
         actual_result = furrr::future_map(ggplot_build, 
                                           ~.x[["data"]][[2]] %>% 
                               slice(1)) %>% 
           map_dbl("yintercept", .null = NA_real_), 
         # take forecast error for ct
         forecast_error = actual_result - gam_result
  ) %>% 
  select(-ggplot_build)

save(extract_iowa_estimates, file = here::here("data/iowa_extracted_spline_estimates.RData"))

# Plot the differences over time
# We want to see the differences shrink to zero as election day approaches.

ggplot(extract_iowa_estimates, aes(date, forecast_error)) +
  # geom_point() + # might be nice to do this with geom_ribbon once we have CIs
  geom_line() + 
  geom_hline(yintercept = 0, 
             color = ussc::ussc_colours("light blue")) +
  facet_wrap(contest~last_name,
             nrow = 5,
             scales = "free_x") +
  scale_x_datetime(date_labels = "%b\n'%y") +
  theme_ussc() +
  labs(x = NULL,
       y = latex2exp::TeX("y_c - \\hat{y}_{ct}")) +
  theme(panel.grid.minor = element_blank(),
        axis.title.y = element_text(angle = 0),
        panel.spacing.x=unit(1, "lines")) 



ggsave(last_plot(), 
       file = here::here('graphs/iowa_past_splines.png'), 
       width = 8, height = 8)
  


# Option 2: Decaying parameter.

# This link seems useful:
# http://douglas-watson.github.io/post/2018-09_exponential_curve_fitting/
# http://www.hiercourse.com/docs/SS_compendium.html
# define the function f
# rate of decay -> y=a(1-b)^x
f <- function(x_ct, a, b){
  sum(x_ct*a^b)
}
f1 <- function(x_ct, a, b, k){
  sum(x_ct* (a(1-b)^x_ct)^k)
}


iowa_polls_max <- rolling_iowa_polls %>% 
  group_by(contest, last_name) %>% 
  slice(n()) %>% 
  ungroup() %>% 
  filter(!contest %in% c("1980D", "1980R")) %>% 
  mutate(rolling_t = map(rolling_t, ~drop_na(.x, y_c) %>% 
                           mutate(k = nrow(.) - 1)
                         )
         ) %>% 
  # I don't think this is right...
  mutate(
    nls_models = furrr::future_map(rolling_t, 
                                  ~broom::tidy(nls2::nls2(y_c ~ sum(p_ct*w^k) + e_ct,
                                                   start = c(w=1, k=1), 
                                                   algorithm = "brute-force",
                                               data = .x)
                                               )
                                  ),
  nls_models1 = furrr::future_map(rolling_t, 
                                  ~broom::tidy(nls(y_c ~ SSasymp(p_ct, Asym, R0, lrc) + 
                                                                  s(e_ct),
                                                                data = .x)
                                               )
  )
  )
           
           



save(iowa_polls_models, file = here::here("data/rolling_iowa_polls_models.RData"))
