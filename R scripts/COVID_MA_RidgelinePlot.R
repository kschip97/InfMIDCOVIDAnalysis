## COVID_MA_RidgelinePlot.R ##
## Programmer: Kees Schipper ##
## Created: 2021-02-26 ##
## last updated: 2021-02-26 ##

library(tidyverse)
library(ggridges)
library(ggpubr)

load('cleaned data/COVID_master_2021-02-24.RData')

nons_county <- c('Dukes and Nantucket', "Unassigned", "Out of MA", "Dukes", "Nantucket")
dates_of_int <- as.Date(c("2020-04-25", "2020-11-26", "2020-12-25", "2021-01-01"))

`%notin%` <- Negate(`%in%`)

COVID_MA <- COVID_master %>%
  dplyr::filter(state == "Massachusetts" & county %notin% nons_county)


# create aggregate data on all of MA --------------------------------------

total_outcomes <- COVID_MA %>%
  group_by(date) %>%
  summarise(MA_tot_cases = sum(daily_cases),
            MA_tot_deaths = sum(daily_deaths),
            Population = sum(Population)) %>%
  mutate(county = "State_rate",
         daily_cases_100k = (MA_tot_cases/Population)*100000,
         daily_deaths_100k = (MA_tot_deaths/Population)*100000) %>%
  dplyr::select(county, date, daily_cases_100k, daily_deaths_100k)

COVID_plotting <- COVID_MA %>%
  dplyr::select(county, date, daily_cases_100k, daily_deaths_100k) %>%
  arrange(date, county)

plot_data <- rbind(COVID_plotting, total_outcomes) %>%
  mutate(smooth7_cases = ave(daily_cases_100k, county, 
                             FUN = function(x) lowess(x, f = 21/nrow(COVID_master))$y),
         smooth7_deaths = ave(daily_deaths_100k, county,
                              FUN = function(x) lowess(x, f = 21/nrow(COVID_master))$y),
         county = factor(county),
         county = fct_relevel(county, "State_rate"))


caseridge <- plot_data %>%
  ggplot(aes(x = date, height = scales::rescale(smooth7_cases,to = c(0, 1.0), from = c(0, max(smooth7_cases))), y = county)) +
  geom_ridgeline(aes(min_height = 0), fill = "coral") +
  theme(legend.position = 'none')

deathridge <- plot_data %>%
  ggplot(aes(x = date, height = scales::rescale(smooth7_deaths, to = c(0, 1.0), from = c(0, max(smooth7_deaths))), y = county)) +
  geom_ridgeline(aes(min_height = 0), fill = "skyblue") +
  theme(legend.position = 'none',
        axis.text.y = element_blank())

ggarrange(caseridge, deathridge)


# plot with no smoother ---------------------------------------------------



caseridge <- plot_data %>%
  ggplot(aes(x = date,
             height = scales::rescale(daily_cases_100k,to = c(0, 1.0), from = c(0, max(daily_cases_100k))), y = county)) +
  geom_ridgeline(aes(min_height = 0), fill = "#f1a340", color = "black") + #"#f1a340"
#  geom_vline(xintercept = dates_of_int, color = "cyan", alpha = 1.0, linetype = 'dashed') +
  labs(title = "Daily Cases per 100,000 Population") +
  theme_ridges() +
  theme(legend.position = 'none')


deathridge <- plot_data %>%
  ggplot(
    aes(x = date,
        height = scales::rescale(daily_deaths_100k, to = c(0, 1.0), from = c(0, max(daily_deaths_100k))),
        y = county)) +
  geom_ridgeline(aes(min_height = 0), fill = '#998ec3', color = 'black') + #'#998ec3'
 # geom_vline(xintercept = dates_of_int, color = "cyan", alpha = 0.5, linetype = 'longdash') +
  labs(title = "Daily Deaths per 100,000 Population") +
  theme_ridges() +
  theme(legend.position = 'none',
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) 

ggarrange(caseridge, deathridge)
