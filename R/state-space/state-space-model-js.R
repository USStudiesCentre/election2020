# Code courtesy of Jim Savage: https://statmodeling.stat.columbia.edu/2016/08/06/state-space-poll-averaging-model/

library(rvest); library(dplyr); library(ggplot2); library(rstan); library(reshape2); library(stringr); library(lubridate)
options(mc.cores = parallel::detectCores())

# The polling data
realclearpolitics_all <- read_html("http://www.realclearpolitics.com/epolls/2016/president/us/general_election_trump_vs_clinton-5491.html#polls")

# Scrape the data
rcp_polls <- realclearpolitics_all %>% 
    html_node(xpath = '//*[@id="polling-data-full"]/table') %>% 
    html_table() %>% 
    filter(Poll != "RCP Average")

# Function to convert string dates to actual dates
get_first_date <- function(x){
    last_year <- cumsum(x=="12/22 - 12/23")>0
    dates <- str_split(x, " - ")
    dates <- lapply(1:length(dates), function(x) as.Date(paste0(dates[[x]], 
                                                                ifelse(last_year[x], "/2015", "/2016")), 
                                                         format = "%m/%d/%Y"))
    first_date <- lapply(dates, function(x) x[1]) %>% unlist
    second_date <- lapply(dates, function(x) x[2])%>% unlist
    data_frame(first_date = as.Date(first_date, origin = "1970-01-01"), 
               second_date = as.Date(second_date, origin = "1970-01-01"))
}

# Convert dates to dates, impute MoE for missing polls with average of non-missing, 
# and convert MoE to standard deviation (assuming MoE is the full 95% two sided interval length)
polls <- polls %>% 
    mutate(start_date = get_first_date(Date)[[1]],
           end_date = get_first_date(Date)[[2]],
           N = as.numeric(gsub("[A-Z]*", "", Sample)),
           MoE = as.numeric(MoE))%>% 
    select(end_date, `Clinton (D)`, `Trump (R)`, MoE) %>% 
    mutate(MoE = ifelse(is.na(MoE), mean(MoE, na.rm = T), MoE),
           sigma = MoE/4) %>% 
    arrange(end_date)


# Stretch out to get missing values for days with no polls
polls3 <- left_join(data_frame(end_date = seq(from = min(polls$end_date), 
                                              to= max(polls$end_date), 
                                              by = "day")), polls) %>% 
    group_by(end_date) %>%
    mutate(N = 1:n()) %>%
    rename(Clinton = `Clinton (D)`,
           Trump = `Trump (R)`)


# One row for each day, one column for each poll on that day, -9 for missing values
Y_clinton <- polls3 %>% dcast(end_date ~ N, value.var = "Clinton") %>% 
    dplyr::select(-end_date) %>% 
    as.data.frame %>% as.matrix
Y_clinton[is.na(Y_clinton)] <- -9

Y_trump <- polls3 %>% dcast(end_date ~ N, value.var = "Trump") %>% 
    dplyr::select(-end_date) %>% 
    as.data.frame %>% as.matrix
Y_trump[is.na(Y_trump)] <- -9

# Do the same for margin of errors for those polls
sigma <- polls3 %>% dcast(end_date ~ N, value.var = "sigma")%>% 
    dplyr::select(-end_date)%>% 
    as.data.frame %>% as.matrix
sigma[is.na(sigma)] <- -9

# Run the two models

clinton_model <- stan("state_space_polls.stan", 
                      data = list(T = nrow(Y_clinton), 
                                  polls = ncol(Y_clinton), 
                                  Y = Y_clinton, 
                                  sigma = sigma,
                                  initial_prior = 50))

trump_model <- stan("state_space_polls.stan", 
                    data = list(T = nrow(Y_trump), 
                                polls = ncol(Y_trump), 
                                Y = Y_trump, 
                                sigma = sigma,
                                initial_prior = 30))


# Pull the state vectors

mu_clinton <- extract(clinton_model, pars = "mu", permuted = T)[[1]] %>% 
    as.data.frame

mu_trump <- extract(trump_model, pars = "mu", permuted = T)[[1]] %>% 
    as.data.frame

# Rename to get dates
names(mu_clinton) <- unique(paste0(polls3$end_date))
names(mu_trump) <- unique(paste0(polls3$end_date))

# summarise uncertainty for each date

mu_ts_clinton <- mu_clinton %>% melt %>% 
    mutate(date = as.Date(variable)) %>% 
    group_by(date) %>% 
    summarise(median = median(value),
              lower = quantile(value, 0.025),
              upper = quantile(value, 0.975),
              candidate = "Clinton")

mu_ts_trump <- mu_trump %>% melt %>% 
    mutate(date = as.Date(variable)) %>% 
    group_by(date) %>% 
    summarise(median = median(value),
              lower = quantile(value, 0.025),
              upper = quantile(value, 0.975),
              candidate = "Trump")

# Plot results


bind_rows(mu_ts_clinton, mu_ts_trump) %>% 
    ggplot(aes(x = date)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = candidate),alpha = 0.1) +
    geom_line(aes(y = median, colour = candidate)) +
    ylim(30, 60) +
    scale_colour_manual(values = c("blue", "red"), "Candidate") +
    scale_fill_manual(values = c("blue", "red"), guide = F) +
    geom_point(data = polls3, aes(x = end_date, y = `Clinton`), size = 0.2, colour = "blue") +
    geom_point(data = polls3, aes(x = end_date, y = Trump), size = 0.2, colour = "red") +
    lendable::theme_lendable() +
    xlab("Date") +
    ylab("Implied vote share") +
    ggtitle("Poll aggregation with state-space smoothing", 
            subtitle= paste("Prior of 50% initial for Clinton, 30% for Trump on", min(polls3$end_date)))