## MDSX_COVID_ANALYSIS.R ##
## Author: Kees Schipper ##
## Created: 2021-01-29 ##
## Last Updated: 2021-01-29 ##

rm(list = ls())

# Installs all the packages that you could ever dream of
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if (!require(stringr)) install.packages("stringr"); library(stringr)
if (!require(readxl)) install.packages("readxl"); library(readxl)
if (!require(data.table)) install.packages("data.table"); library(data.table)
if (!require(kza)) install.packages("kza"); library(kza)
if (!require(lubridate)) install.packages("lubridate"); library(lubridate)
if (!require(MASS)) install.packages("MASS"); library(MASS)
if (!require(broom)) install.packages("broom"); library(broom)
if (!require(forecast)) install.packages("forecast"); library(forecast)
if (!require(conflicted)) install.packages("conflicted"); library(conflicted)
if (!require(segmented)) install.packages("segmented"); library(segmented)
if (!require(sjPlot)) install.packages("sjPlot"); library(sjPlot)
if (!require(gt)) install.packages("gt"); library(gt)
if (!require(mgcv)) install.packages("mgcv"); library(mgcv)
if (!require(emmeans)) install.packages("emmeans"); library(emmeans)
if (!require(moments)) install.packages("moments"); library(moments)
if (!require(arsenal)) install.packages("arsenal"); library(arsenal)
if (!require(FSA)) install.packages("FSA"); library(FSA)
if (!require(segmented)) install.packages("segmented"); library(segmented)
if (!require(sjPlot)) install.packages("sjPlot"); library(sjPlot)
if (!require(gt)) install.packages("gt"); library(gt)
if (!require(forestplot)) install.packages("forestplot"); library(forestplot)
if (!require(ggpubr)) install.packages("ggpubr"); library(ggpubr)
if (!require(lmtest)) install.packages("lmtest"); library(lmtest)
if (!require(ggthemes)) install.packages("ggthemes"); library(ggthemes)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# make sure that you have both of these R scripts, as this script depends on them
source('R scripts/out of use/COVID_JHU_DATA.R')
source('R scripts/COVID_Analysis_Utilities.R')
# color <- rev(c("#67a9cf", "#ef8a62", "#f7f7f7", "#67a9cf"))
color <- rev(c("#ef8a62", "#67a9cf", "#f7f7f7", "#ef8a62")) # lighter colors
# color <- rev(c("#ca0020", "#0571b0", "grey70", "#ca0020"))

MDPH_data <- read_csv(paste0("cleaned data/from_MDPH_web_Middlesex_data_2021-01-28.csv")) %>%
  select(Date, County, Cases, Deaths, Tests, dtests) %>%
  rename(cases = Cases,
         deaths = Deaths,
         tests = Tests)

files <- list.files("Raw Data/MA DPH COVID Dashboard Raw")[1:4]

MDPH_COVID <- vector(mode = "list")

for (i in 1:length(files)){
  
  MDPH_COVID[[i]] <- read_csv(paste0("Raw Data/MA DPH COVID Dashboard Raw/", files[i]))
  
}

MDPH_Mdsx <- lapply(
  MDPH_COVID, 
  function(x)
    x <- x %>% 
    filter(County == "Middlesex") %>%
    filter(!is.na(cases) & !is.na(deaths)) %>%
    mutate(dcases = ave(cases, County, FUN = function(x) c(0, diff(x))),
           ddeaths = ave(deaths, County, FUN = function(x) c(0, diff(x))))
)


# Clean data and make variables for smoothers, time periods, week count -----------------------

MA_tot_pop <- COVID_stmobil %>%
  filter(State == "Massachusetts") %>%
  select(Population, State, County) %>%
  unique() %>%
  summarise(MA_pop = sum(Population))


Middlesex <- COVID_stmobil %>%
  filter(Date > "2020-03-01" & Date < "2021-01-20") %>% # Cases don't come into MA until after beginning of march
  filter(State == "Massachusetts" & County == "Middlesex") %>%
  mutate(smoothcase = kz(dcper100k, m = 20, k = 2), # creates a KZ smoother of case rates
         logsmoothcase = kz(logcases, m = 7, k = 2), # log version of the above
         smoothdeath = kz(ddper100k, m = 7, k = 2), # KZ smoother of deaths
         logsmoothdeath = kz(logdeaths, m = 7, k = 2)) %>% # log version of above
  mutate(pre_out = 0, # pre outbreak period indicator
         accel_out = ifelse(row_number() < 48, 1, 0), #acceleration to peak indicator
         dec_out = ifelse(row_number() >= 48 & row_number() < 102, 1, 0), # decrease to nadir indicator
         plat = ifelse(row_number() >= 102 & row_number() < 215, 1, 0), # plateau indicator
         accel_new = ifelse(time >= 215, 1, 0)) %>% # second wave indicator
  mutate(period = ifelse(pre_out == 1, "T1",
                         ifelse(accel_out == 1, "T2",
                                ifelse(dec_out == 1, "T3",
                                       ifelse(plat == 1, "T4",
                                              ifelse(accel_new == 1, "T5", NA)))))) %>%
  mutate(T2 = row_number(),
         T3 = ifelse(row_number() >= 48, row_number()-47, 0), # Create time series variables for 
         T4 = ifelse(row_number() >= 102, row_number()-101, 0),
         T5 = ifelse(row_number() >= 215, row_number()-214, 0)) %>%
  mutate(T2D = row_number(),
         T3D = ifelse(row_number() >= 65, row_number()-64, 0),
         T4D = ifelse(row_number() >= 153, row_number()-152, 0),
         T5D = ifelse(row_number() > 230, row_number()-229, 0),
         week = 0,
         MA_tot_pop = MA_tot_pop$MA_pop[1]) #going to start the week count on the first Sunday of the data


MDPH_Mdsx_bind <- rbind(MDPH_Mdsx[[1]], MDPH_Mdsx[[2]], MDPH_Mdsx[[3]]) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
         Population = Middlesex$Population[1],
         dcper100k = (dcases/Population)*100000,
         dcper100k = ifelse(is.nan(dcper100k), NA, dcper100k),
         ddper100k = (ddeaths/Population)*100000,
         ddper100k = ifelse(is.nan(ddper100k), NA, ddper100k)) %>%
  arrange(Date)

# create a counter that makes a week number variable in Middlesex Data
weeknumb <- 1
for (i in 1:nrow(Middlesex)){
  Middlesex$week[i] <- weeknumb
  
  if (Middlesex$weekfac[i] == "Sunday"){
    weeknumb <- weeknumb + 1
  }
}
# for later grouping if needed
Middlesex <- Middlesex %>%
  mutate(week = as.character(week))



# Create variable of corrected death counts -------------------------------
# Corrected change of deaths per 100 days will involve finding strings of zeros followed by a spike
# in deaths. I've found some instances in the Massachusetts Department of Public health data where
# these strings of zeros don't exist, and therefore, I've taken those datapoints and manually replaced
# them with the following code. Other instances where we have a string of zeros, followed by a spike
# will be calculated as the mean of the spike distributed over the set of zeros

ind <- c()
val <- c()
change_ind <- c()
change_date <- c()


for (i in 40:nrow(Middlesex)){
  if (Middlesex$ddper100k[i] == 0){
    ind <- c(ind, i)
    val <- c(val, Middlesex$ddper100k[i])
    
  } else if (Middlesex$ddper100k[i] > 0 & length(ind) >= 1){
    ind <- c(ind, i)
    val <- c(val, Middlesex$ddper100k[i])
    
    i1 <- ind[1]
    i2 <- ind[length(ind)]
    
    # #print check for indicies and values
    # print(ind)
    # print(val)
    # print(Middlesex$Date[ind])
    
    if (sum(val) >= 1){
      Middlesex$ddper100k[i1:i2] <- sum(val)/length(val)
      change_ind <- c(change_ind, ind)
      change_date <- Middlesex$Date[change_ind]
    }
    
    #reset indicies and values
    ind <- c()
    val <- c()
  }
  print(change_ind)
  print(change_date)
}
change_date_mean <- change_date[14:20]
change_date <- change_date[c(1:13, 21:22)]


from_MDPH <- which(MDPH_Mdsx_bind$Date %in% change_date)
from_JHU <- which(Middlesex$Date %in% change_date)

JHU_mean <- which(Middlesex$Date %in% change_date_mean)

Middlesex$ddper100k[from_JHU] <- MDPH_Mdsx_bind$ddper100k[from_MDPH]



# create lowess curves for cases and deaths
caselo <- lowess(Middlesex$dcper100k ~ Middlesex$time, f = .20) # lowess with 3 robustifying iterations
deathlo <- lowess(Middlesex$ddper100k ~ Middlesex$time, f = .20) # and spans of 0.2

# add caselo and deathlo to Middlesex data frame
Middlesex$caselo <- caselo$y
Middlesex$deathlo <- deathlo$y

# make caselo and deathlo values that are less than 0 equal to zero for intuitive sense
Middlesex <- Middlesex %>%
  mutate(deathlo = ifelse(deathlo < 0, 0, deathlo),
         caselo = ifelse(caselo < 0, 0, caselo),
         dcases = (dcper100k/100000)*Population,
         ddeaths = (ddper100k/100000)*Population) %>%
  left_join(MDPH_data, by = c("County", "Date"), suffix = c("", ".MDPH")) %>%
  mutate(dtper100k = (dtests/MA_tot_pop)*100000,
         dtests = na.interp(dtests, linear = TRUE),
         smoothtests = kz(dtper100k, m = 15, k = 3),
         testlo = lowess(dtper100k ~ time, f = 0.20)[[2]],
         testdiff = ave(smoothtests, FUN = function(x)c(0, diff(x))))

date_vline <- Middlesex$Date[c(48, 102, 215)]

# plot smoothers with vertical lines representing critical periods
ggplot(Middlesex, aes(x = Date, y = smoothcase)) +
  geom_line(size = 1) +
  geom_vline(xintercept = date_vline, col = "firebrick3") +
  labs(title = "Smoothed COVID Cases Over Time in Middlesex County",
       subtitle = "Vertical lines from left to right: Outbreak onset, Outbreak Peak,
       outbreak resolution, Outbreak Acceleration",
       caption = paste0("Time Series from 2020-03-01 to ", Sys.Date()),
       xlab = "Smoothed Cases/100k Population/Day")

# same as above graph but using log of cases
ggplot(Middlesex, aes(x = Date, y = logsmoothcase)) +
  geom_line(size = 1) +
  geom_vline(xintercept = date_vline, col = "firebrick3") +
  labs(title = "Smoothed Logarithm of COVID Cases Over Time in Middlesex County",
       subtitle = "Vertical lines from left to right: Outbreak onset, Outbreak Peak,
       outbreak resolution, Outbreak Acceleration",
       caption = paste0("Time Series from 2020-03-01 to ", Sys.Date()),
       xlab = "Smoothed Cases/100k Population/Day")




# Fit glm model with weekday effects --------------------------------------

# create separate dataset with only variables needed for models
Mid_mod_dat <- Middlesex %>%
  dplyr::select(period, T2:T5, T2D:T5D, weekfac, caselo, deathlo, dtper100k, dcper100k, ddper100k, Date, weekday, testlo) %>%
  mutate(dcper100k = ifelse(Date %in% as.Date(c("2020-11-26", "2020-12-25", "2021-01-01")), NA, dcper100k),
         dcper100k = na.interp(dcper100k, linear = T),
         dtper100k = na.interp(dtper100k, linear = T))

Middlesex %>%
  select(period, T2:T5, weekfac, caselo, dtper100k, dcper100k, ddper100k, Date, weekday, County,
         State, FIPS, cases, deaths, dcases, ddeaths, deathlo, MA_tot_pop, T2D:T5D, dtests) %>%
  mutate(dcper100k = ifelse(Date %in% as.Date(c("2020-11-26", "2020-12-25", "2021-01-01")), NA, dcper100k),
         dcper100k = na.interp(dcper100k, linear = T)) %>%
  write_csv("cleaned data/Middlesex_Analysis_20200205_KS.csv")

# create lag distributed variable.


# create a negative binomial model for predicting case counts
case_seg_mod <- glm.nb(dcper100k ~ caselo + weekfac + T2 + I(T2^2) + T3 + 
                         I(T3^2) + T4 + I(T4^2) + T5 + I(T5^2),
                       data = Mid_mod_dat)
case_seg_mod_test <- glm.nb(dcper100k ~ caselo + weekfac + dtper100k + T2 + I(T2^2) + T3 + 
                              I(T3^2) + T4 + I(T4^2) + T5 + I(T5^2),
                            data = Mid_mod_dat)


tidy(case_seg_mod, conf.int = T, exponentiate = T)
tidy(case_seg_mod_test, conf.int = T, exponentiate = T)
lrtest(case_seg_mod_test, case_seg_mod)
plot(case_seg_mod_test$fitted.values, type = "o", col = "green")
lines(case_seg_mod$fitted.values, type = "o", col = "red")
lines(Mid_mod_dat$dcper100k, type = "o", col = "blue")

# Compare test vs. no test models -----------------------------------------

to.rm <- c("deathlo", "T2", "T3", "T4", "T5", "I(T2^2)", "I(T3^2)", "I(T4^2)", "I(T5^2)", "caselo", "dtper100k")
term.labels = c("Friday", "Thursday", "Wednesday", "Tuesday", "Monday", "Saturday")
plot_model(
  case_seg_mod, show.p = TRUE, rm.terms = to.rm, vline.color = "gray",
  show.values = TRUE, value.offset = .2, title = "DoW effect in Middlesex COVID-19 Case Rates (no testing)",
  colors = "blue3", axis.labels = term.labels
) + ylim(0.5, 2) +
  theme_grey()

plot_model(
  case_seg_mod_test, show.p = TRUE, rm.terms = c(to.rm), vline.color = "gray",
  show.values = TRUE, value.offset = .2, title = "DoW effect in Middlesex COVID-19 Case Rates\nadjusting for testing data",
  colors = "blue3", axis.labels = term.labels
) + ylim(0.75, 1.75) +
  theme_gray()
# make a simple table with your model
tab_model(case_seg_mod, case_seg_mod_test, show.reflvl = T, show.ci = 0.95, digits = 3, 
          rm.terms = to.rm, show.aic = T, show.loglik = T) 


#make your model into a data frame
(tidy_casemod <- tidy(case_seg_mod_test, exponentiate = T, conf.int = T))

# add fitted values and residuals from your model onto the Middlesex data
Mid_mod_dat$casefit <-case_seg_mod_test$fitted.values
Mid_mod_dat$caseresid <- Middlesex$dcper100k - case_seg_mod_test$fitted.values

dc <- Middlesex$Date[c(48, 102, 215)]
mindate <- as.Date("2020-03-01")
maxdate <- as.Date("2021-01-20")

#making colored rectangles for critical period visualization
pre_rect <- data.frame(xmin = mindate, xmax = dc[1], ymin = -Inf, ymax = Inf)
out_rect <- data.frame(xmin = dc[1], xmax = dc[2], ymin = -Inf, ymax = Inf)
dec_rect <- data.frame(xmin = dc[2], xmax = dc[3], ymin = -Inf, ymax = Inf)
plat_rect <- data.frame(xmin = dc[3], xmax = maxdate, ymin = -Inf, ymax = Inf)

# use rectobj function to make a rectangular object for shading. Takes a df with x and y min and max
# color, and alpha variables

labels <- data.frame("T1", "T2", "T3", "T4", "T5")

# plot predicted cases over actual cases (red)
# yellow: #EBC20D
caseplot <- ggplot() +
  rectobj(pre_rect, color[1], 0.9) + # blue
  rectobj(out_rect, color[3], 0.9) + # orange
  rectobj(dec_rect, color[2], 0.9) +  # grey
  rectobj(plat_rect, color[4], 0.9) + # blue
  geom_area(data = Mid_mod_dat, aes(x = Date, y = dcper100k),
            fill = "grey50", alpha = 1) +
  geom_line(data = Mid_mod_dat, aes(x = Date, y = casefit), color = "#D41E08", size = 1) +
  geom_text(
    aes(x = dc[1] - 25, y = 80, label = "T1"),
    data = labels
  ) +
  geom_text(
    aes(x = dc[2] - 30, y = 80, label = "T2"),
    data = labels
  ) +
  geom_text(
    aes(x = dc[3] - 60, y = 80, label = "T3"),
    data = labels
  ) +
  geom_text(
    aes(x = dc[3] + 50, y = 80, label = "T4"),
    data = labels
  ) +
  # labs(title = "Raw Cases/100k (grey) and Fitted Negative Binomial for Middlesex County (red)",
  #      subtitle = "(T1) acceleration; (T2) return to nadir; (T3) plateau; (T4) second wave") +
       #caption = paste0("Time series from 2020-03-01 to 2021-01-20")) +
  ylab("Cases/100,000") +
  # scale_x_date(date_labels = '%b (%y)',
  #              breaks = function(x) seq.Date(from = min(x), 
  #                                            to = max(x), 
  #                                            by = "2 months")) +
  theme(axis.title.x=element_blank(),
        axis.title.y = element_blank()) +
  scale_x_date(breaks = NULL,
               minor_breaks = NULL,
               expand = c(0, 0)) +
  geom_text(
    aes(x = as.Date('2020-04-10'), y = 0.95*max(dcper100k), label = "cases/100,000"),
    color = 'grey1', size = 4, data = Mid_mod_dat
  ) +  
  scale_y_continuous(
    expand = expansion(add = 0.02*max(Mid_mod_dat$dcper100k)),
    breaks = c(10, 30, 50, 70, 90)
  )
  # xlab("Days")
  # theme(plot.background = element_rect(fill = "snow"),
  #       panel.background = element_rect(fill = "snow",
  #                                 colour = "snow",
  #                                 size = 0.5, linetype = "solid"))


# Run Summary Statistics for segment of the curve -------------------------

Case_summ_raw <- Mid_mod_dat %>%
  pivot_longer(cols = c("dcper100k"), # pivot to long form
               names_to = "variables",
               values_to = "values") %>%
  group_by(variables, period, weekfac) %>% # group by these three variables for summary stats
  summarise(
    `Mean (Var)` = paste0(round(mean(values), digits = 1), " (", round(var(values), digits = 1), ")")
  ) %>% # making a variable for mean and variance for each time period on each day
  pivot_longer(cols = c("Mean (Var)"),
               names_to = "statistics",
               values_to = "Values") %>%
  pivot_wider(names_from = "period",
              values_from = "Values") %>%
  select(weekfac, variables, statistics, T2:T5) %>%
  mutate(variables = str_replace_all(variables, c("dcper100k" = "Cases/100kPop/Day", "casefit" = "Model Predicted"))) %>%
  pivot_longer(cols = c("T2", "T3", "T4", "T5"),
               names_to = "period",
               values_to = "values") %>%
  pivot_wider(names_from = "statistics",
              values_from = "values") %>%
  pivot_wider(names_from = "period",
              values_from = c("Mean (Var)")) %>%
  arrange(variables) %>%
  group_by(weekfac)

# in case you want to save the resulting table
# write_csv(Case_summ, "summary tables/NB_case_fit_summary_20201027.csv")


# Do same thing for deaths ------------------------------------------------

#identify critical periods

deathdates <- Middlesex$Date[c(65, 153, 230)]

ggplot(Middlesex, aes(x = Date, y = deathlo)) +
  geom_line(size = 1) +
  geom_vline(xintercept = deathdates, col = "firebrick3") +
  labs(title = "Smoothed COVID Deaths Over Time in Middlesex County",
       subtitle = "Vertical lines from left to right: Outbreak onset, Outbreak Peak,
       outbreak resolution, Outbreak Acceleration",
       caption = paste0("Time Series from 1/22/2020 to ", Sys.Date()),
       xlab = "Smoothed Cases/100k Population/Day") +
  theme_pubclean()

ggplot(Middlesex, aes(x = Date, y = logsmoothdeath)) +
  geom_line(size = 1) +
  geom_vline(xintercept = deathdates, col = "firebrick3") +
  labs(title = "Smoothed Logarithm of COVID Cases Over Time in Middlesex County",
       subtitle = "Vertical lines from left to right: Outbreak onset, Outbreak Peak,
       outbreak resolution, Outbreak Acceleration",
       caption = paste0("Time Series from 1/22/2020 to ", Sys.Date()),
       xlab = "Smoothed Cases/100k Population/Day")




# Negative binomial model for deaths --------------------------------------
# Model 2.A.
death_seg_mod <- glm.nb(ddper100k ~ weekfac + deathlo + T2D + I(T2D^2) + T3D + 
                          I(T3D^2) + T4D + I(T4D^2) + T5D + I(T5D^2), data = Mid_mod_dat)
# Model 2.B.
death_seg_mod_test <- glm.nb(ddper100k ~ weekfac + deathlo + T2D + I(T2D^2) + T3D + 
                               I(T3D^2) + T4D + I(T4D^2) + T5D + I(T5D^2) + dtper100k, data = Mid_mod_dat)

Middlesex$deathfit <- death_seg_mod_test$fitted.values

tidy(death_seg_mod, exponentiate = T, conf.int = T)
summary(death_seg_mod_test)
lrtest(death_seg_mod_test, death_seg_mod)

# plot(Middlesex$Date, death_seg_mod$fitted.values, type = "o", col = "blue")
# lines(Middlesex$Date, death_seg_mod_test$fitted.values, type = "o", col = "red")
# lines(Middlesex$Date, Middlesex$ddper100k, type = "o", col = "green")
# testing data doesn't significantly add to the death model.



# plotting death models over cleaned data ---------------------------------

dd <- Middlesex$Date[c(65, 153, 230)]
mindate <- as.Date("2020-03-01")
maxdate <- as.Date("2021-01-20")

#making colored rectangles for critical period visualization
d_pre_rect <- data.frame(xmin = mindate, xmax = dd[1], ymin = -Inf, ymax = Inf)
d_out_rect <- data.frame(xmin = dd[1], xmax = dd[2], ymin = -Inf, ymax = Inf)
d_dec_rect <- data.frame(xmin = dd[2], xmax = dd[3], ymin = -Inf, ymax = Inf)
d_plat_rect <- data.frame(xmin = dd[3], xmax = maxdate, ymin = -Inf, ymax = Inf)

# use rectobj function to make a rectangular object for shading. Takes a df with x and y min and max
# color, and alpha variables

labels <- data.frame("T1", "T2", "T3", "T4", "T5")

# plot predicted cases over actual cases (red)
deathplot <- ggplot() +
  rectobj(d_pre_rect, color[1], 0.9) +
  rectobj(d_out_rect, color[3], 0.9) +
  rectobj(d_dec_rect, color[2], 0.9) +
  rectobj(d_plat_rect, color[4], 0.9) +
  geom_area(data = Middlesex, aes(x = Date, y = ddper100k),
            fill = "grey50", alpha = 1) +
  geom_line(data = Middlesex, aes(x = Date, y = deathfit), color = "#D41E08", size = 1) +
  geom_text(
    aes(x = dd[1] - 30, y = 4, label = "T1"),
    data = labels
  ) +
  geom_text(
    aes(x = dd[2] - 45, y = 4, label = "T2"),
    data = labels
  ) +
  geom_text(
    aes(x = dd[3] - 35, y = 4, label = "T3"),
    data = labels
  ) +
  geom_text(
    aes(x = dd[3] + 40, y = 4, label = "T4"),
    data = labels
  ) +
  labs(
    # subtitle = "(T1) acceleration; (T2) return to nadir; (T3) plateau; (T4) second wave",
    caption = paste0("Time series from 2020-03-01 to 2021-01-20")
    ) +
  ylab("Deaths/100,000") + 
  # xlab("Date") +
  scale_x_date(date_labels = '%b',
               limits = as.Date(c("2020-03-01","2021-01-20")),
               # breaks = function(x) seq.Date(from = min(x), 
               #                               to = max(x), 
               #                               by = "2 months"),
               breaks = as.Date(c("2020-03-01", "2020-05-01", "2020-07-01", "2020-09-01",
                                  "2020-11-01", "2021-01-01")),
               expand = c(0, 0)
               ) +
  scale_y_continuous(
    expand = expansion(add = 0.02*max(Mid_mod_dat$ddper100k))
  ) +
  geom_text(
    aes(x = as.Date('2020-04-10'), y = 0.95*max(ddper100k), label = "deaths/100,000"),
    color = 'grey1', size = 4, data = Mid_mod_dat
  ) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
  # theme(axis.text.x = element_text(hjust = 1, angle = 45))
  # theme(plot.background = element_rect(fill = "snow"),
  #       panel.background = element_rect(fill = "snow",
  #                                       colour = "snow",
  #                                       size = 0.5, linetype = "solid"))


# Generating Summary Tables for variance ---------------------------------------


Death_summ_raw <- Middlesex %>%
  pivot_longer(cols = c("ddper100k"),
               names_to = "variables",
               values_to = "values") %>%
  group_by(variables, period, weekfac) %>%
  summarise(
    `Mean (Var)` = paste0(round(mean(values), digits = 1), " (", round(var(values), digits = 1), ")")
  ) %>%
  pivot_longer(cols = c("Mean (Var)"),
               names_to = "statistics",
               values_to = "Values") %>%
  pivot_wider(names_from = "period",
              values_from = "Values") %>%
  select(weekfac, variables, statistics, T2:T5) %>%
  rename(T2D = T2,
         T3D = T3,
         T4D = T4,
         T5D = T5) %>%
  mutate(variables = str_replace_all(variables, c("ddper100k" = "Deaths/100kPop/Day", "deathfit" = "Model Predicted"))) %>%
  pivot_longer(cols = c("T2D", "T3D", "T4D", "T5D"),
               names_to = "period",
               values_to = "values") %>%
  pivot_wider(names_from = "statistics",
              values_from = "values") %>%
  pivot_wider(names_from = "period",
              values_from = c("Mean (Var)")) %>%
  rename(T2 = T2D,
         T3 = T3D,
         T4 = T4D,
         T5 = T5D) %>%
  group_by(weekfac)


# write_csv(Death_summ, "summary tables/NB_deaths_fit_summary_20201027.csv")


# Calculate ratios between day of week effects and Sunday for cases -------


Case_summ_ratio_num <- Mid_mod_dat %>%
  pivot_longer(cols = c("dcper100k"),
               names_to = "variables",
               values_to = "values") %>%
  group_by(variables, period, weekday) %>%
  summarise(
    Mean = mean(values),
    Var = var(values)
  ) %>%
  arrange(period, weekday) %>%
  pivot_wider(names_from = "weekday",
              values_from = c("Mean", "Var")) %>%
  group_by(period, variables) %>%
  select(1, 2, 3:5, 7:9, 6, 10:12, 14:16, 13) %>%
  relocate(Mean_Sunday, .after = Mean_Wednesday) %>%
  mutate_at(vars(Mean_Friday:Mean_Sunday), ~round(`/`(., Mean_Sunday), digits = 2)) %>%
  mutate_at(vars(Var_Friday:Var_Sunday), ~round(`/`(., Var_Sunday), digits = 2))

Case_summ_ratio <- Case_summ_ratio_num %>%
  mutate(Monday = paste0(Mean_Monday, " (", Var_Monday, ")"),
         Tuesday = paste0(Mean_Tuesday, " (", Var_Tuesday, ")"),
         Wednesday = paste0(Mean_Wednesday, " (", Var_Wednesday, ")"),
         Thursday = paste0(Mean_Thursday, " (", Var_Thursday, ")"),
         Friday = paste0(Mean_Friday, " (", Var_Friday, ")"),
         Saturday = paste0(Mean_Saturday, " (", Var_Saturday, ")"),
         Sunday = paste0(Mean_Sunday, " (", Var_Sunday, ")")) %>%
  select(-c(3:16)) %>%
  pivot_longer(cols = -c("variables", "period"),
               names_to = "weekday",
               values_to = "Values") %>%
  pivot_wider(names_from = "period",
              values_from = "Values") %>%
  select(weekday, variables, T2:T5) %>%
  mutate(variables = str_replace_all(variables, c("dcper100k" = "Cases/100kPop/Day",
                                                  "casefit" = "Model Predicted"))) %>%
  ungroup() %>%
  mutate(weekday = factor(weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday",
                                                 "Sunday"),
                          labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday",
                                    "Sunday"))) %>%
  arrange(weekday, variables) %>%
  group_by(weekday)


Death_summ_ratio_num <- Middlesex %>%
  pivot_longer(cols = c("ddper100k"),
               names_to = "variables",
               values_to = "values") %>%
  group_by(variables, period, weekday) %>%
  summarise(
    Mean = mean(values),
    Var = var(values)
  ) %>%
  arrange(period, weekday) %>%
  pivot_wider(names_from = "weekday",
              values_from = c("Mean", "Var")) %>%
  group_by(period, variables) %>%
  select(1, 2, 3:5, 7:9, 6, 10:12, 14:16, 13) %>%
  relocate(Mean_Sunday, .after = Mean_Wednesday) %>%
  relocate(Var_Sunday, .after = Var_Wednesday) %>%
  mutate_at(vars(Mean_Friday:Mean_Sunday), ~round(`/`(., Mean_Sunday), digits = 2)) %>%
  mutate_at(vars(Var_Friday:Var_Sunday), ~round(`/`(., Var_Sunday), digits = 2))

Death_summ_ratio <- Death_summ_ratio_num %>%
  mutate(Monday = paste0(Mean_Monday, " (", Var_Monday, ")"),
         Tuesday = paste0(Mean_Tuesday, " (", Var_Tuesday, ")"),
         Wednesday = paste0(Mean_Wednesday, " (", Var_Wednesday, ")"),
         Thursday = paste0(Mean_Thursday, " (", Var_Thursday, ")"),
         Friday = paste0(Mean_Friday, " (", Var_Friday, ")"),
         Saturday = paste0(Mean_Saturday, " (", Var_Saturday, ")"),
         Sunday = paste0(Mean_Sunday, " (", Var_Sunday, ")")) %>%
  select(-c(3:16)) %>%
  pivot_longer(cols = -c("variables", "period"),
               names_to = "weekday",
               values_to = "Values") %>%
  pivot_wider(names_from = "period",
              values_from = "Values") %>%
  select(weekday, variables, T2:T5) %>%
  mutate(variables = str_replace_all(variables, c("ddper100k" = "Deaths/100kPop/Day",
                                                  "deathfit" = "Model Predicted"))) %>%
  ungroup() %>%
  mutate(weekday = factor(weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday",
                                                "Sunday"),
                            labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday",
                                       "Sunday"))) %>%
  arrange(weekday, variables) %>%
  group_by(weekday)



# Calculate critical periods, model, and summ stats for tests -------------

Middlesex <- Middlesex %>%
  mutate(dtests = na.interp(dtests, linear = TRUE),
         dtper100k = (dtests/MA_tot_pop)*100000,
         smoothtests = kz(dtper100k, m = 15, k = 3),
         testlo = lowess(dtper100k ~ time, f = 0.20)[[2]],
         testdiff = ave(smoothtests, FUN = function(x)c(0, diff(x))))

infl_neg <- c()
infl_pos <- c()
for (i in 2:length(Middlesex$testdiff)){
  
  if (Middlesex$testdiff[i-1] < 0 & Middlesex$testdiff[i] > 0){
    infl_neg <- c(infl_neg, Middlesex$Date[i-1])
  } else if (Middlesex$testdiff[i-1] > 0 & Middlesex$testdiff[i] < 0){
    infl_pos <- c(infl_pos, Middlesex$Date[i-1])
  }
  
}



# Create simple test model ------------------------------------------------

all_test <- glm.nb(dtper100k ~ testlo + weekfac + T2 + T3 + T4 + T5 + I(T2**2) + I(T3**2) +
                     I(T4**2) + I(T5**2), data = Mid_mod_dat)

Middlesex$test.fitted <- all_test$fitted.values

vline_dates <- c(infl_neg, infl_pos)

testplot <- Middlesex %>%
  ggplot() +
  rectobj(pre_rect, color[1], 0.9) + # blue
  rectobj(out_rect, color[3], 0.9) + # orange
  rectobj(dec_rect, color[2], 0.9) +    # grey
  rectobj(plat_rect, color[4], 0.9) + # blue
  geom_area(fill = "grey50", alpha = 1.0, aes(x = Date, y = dtper100k)) + #"#4A4948"
  geom_line(aes(x = Date, y = test.fitted), color = "#D41E08", size = 1) +
  geom_text(
    aes(x = dc[1] - 25, y = 450, label = "T1"),
    data = labels
  ) +
  geom_text(
    aes(x = dc[2] - 30, y = 450, label = "T2"),
    data = labels
  ) +
  geom_text(
    aes(x = dc[3] - 60, y = 450, label = "T3"),
    data = labels
  ) +
  geom_text(
    aes(x = dc[3] + 50, y = 450, label = "T4"),
    data = labels
  ) +
  geom_text(
    aes(x = as.Date('2020-04-10'), y = 0.95*max(dtper100k), label = "tests/100,000"),
    color = 'grey1', size = 4
  ) +
  scale_x_date(
    breaks = NULL,
    minor_breaks = NULL,
    expand = c(0, 0)
               ) +
  scale_y_continuous(
    expand = expansion(add = 0.02*max(Mid_mod_dat$dtper100k))
  ) +
  # scale_x_date(date_labels = '%b (%y)',
  #              breaks = function(x) seq.Date(from = min(x), 
  #                                            to = max(x), 
  #                                            by = "2 months")) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  # theme(axis.text.x = element_text(hjust = 1, angle = 45))
  labs(y = "Tests/100,000")


# create summary table of tests/day/100k by CP ----------------------------

Test_summ_raw <- Mid_mod_dat %>%
  pivot_longer(cols = c("dtper100k"), # pivot to long form
               names_to = "variables",
               values_to = "values") %>%
  group_by(variables, period, weekfac) %>% # group by these three variables for summary stats
  summarise(
    `Mean (Var)` = paste0(round(mean(values), digits = 1), " (", round(var(values), digits = 1), ")")
  ) %>% # making a variable for mean and variance for each time period on each day
  pivot_longer(cols = c("Mean (Var)"),
               names_to = "statistics",
               values_to = "Values") %>%
  pivot_wider(names_from = "period",
              values_from = "Values") %>%
  select(weekfac, variables, statistics, T2:T5) %>%
  mutate(variables = str_replace_all(variables, c("dtper100k" = "Tests/100kPop/Day"))) %>%
  pivot_longer(cols = c("T2", "T3", "T4", "T5"),
               names_to = "period",
               values_to = "values") %>%
  pivot_wider(names_from = "statistics",
              values_from = "values") %>%
  pivot_wider(names_from = "period",
              values_from = c("Mean (Var)")) %>%
  arrange(variables) %>%
  group_by(weekfac)



# combine case, death and testing summaries -------------------------------

# Case_summ_raw
# Death_summ_raw
# Test_summ_raw

summ_combined <- rbind(Case_summ_raw, Death_summ_raw, Test_summ_raw)

comb_arr <- summ_combined %>%
  mutate(variables = factor(variables, levels = c("Tests/100,000/Day", "Cases/100,000/Day", "Deaths/100,000/Day"))) %>%
  arrange(weekfac, variables)


# make function for making tables -----------------------------------------
# feel free to check out the function but it's a little difficult to figure out how it
# works without having some experience working with the "gt" package

Make_summary_table <- function(input_df, vars, spanlab, title, subtitle, footnotes){
  
  if (footnotes == "cases"){
    footnotes = "T1: March 1 - April 18; T2: April 19 -June 11; T3: June 12 - October 2; T4: October 3 - January 20, 2021"
  } else if (footnotes == "deaths"){
    footnotes = "T1: March 1 - May 5; T2: May 6 - August 1; T3: August 2 - October 17; T4: October 17 - January 20, 2021"
  } else if (footnotes == "all"){
    footnotes = c("For Cases and Tests: T1: March 1 - April 18; T2: April 19 -June 11; 
                  T3: June 12 - October 2; T4: October 3 - January 20, 2021. For Deaths:
                  T1: March 1 - May 5; T2: May 6 - August 1; T3: August 2 - October 17; T4:
                  October 17 - January 20, 2021")
  }
  
  output <- input_df %>%
    gt() %>%
    cols_label(
      variables = "",
      T2 = "T1",
      T3 = "T2", 
      T4 = "T3", 
      T5 = "T4"
    ) %>%
    row_group_order(
      groups = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    ) %>%
    tab_style(
      style = cell_text(color = "black", weight = "bold"),
      locations = list(
        cells_row_groups(),
        cells_column_labels(everything()),
        cells_body()
      )
    ) %>%
    opt_table_lines(extent = "default") %>%
    tab_options(
      row_group.padding = 1,
      data_row.padding = 5,
      footnotes.padding = 2,
      table.font.size = 10,
      column_labels.border.top.color = "white",
      column_labels.border.top.width = px(3),
      column_labels.border.bottom.color = "black",
      table_body.hlines.color = "white",
      table.border.bottom.color = "black",
      table.border.bottom.width = px(3),
      row_group.background.color = "#CCCCCC"
    ) %>%
    tab_options(
      table.font.size = 16
    ) %>%
    data_color(
      columns = 3,
      colors = rep(color[1], 7),
      alpha = .8
    ) %>%
    data_color(
      columns = 4,
      colors = rep(color[3], 7),
      alpha = 0.8
    ) %>%
    data_color(
      columns = 5,
      colors = rep(color[2], 7),
      alpha = 0.8
    ) %>%
    data_color(
      columns = 6,
      colors = rep(color[4], 7),
      alpha = 0.8
    ) %>%
    tab_spanner(
      label = spanlab,
      columns = 3:6
    ) %>%
    tab_header(
      title = md(paste0("**",title,"**")),
      subtitle = html(paste0("<em>",subtitle,"</em>"))
    ) %>%
    tab_footnote(
      footnote = footnotes,
      locations = cells_column_labels(2)
    ) 
  
  return(output)
}

# TODO make separate tables for cases, deaths, and tests, with only the data,
# not the predicted values.
# TODO fix footnote of tables so that footnotes are horizontal and take less space

Make_summary_table(comb_arr, vars = c("T2", "T3", "T4", "T5"),
                   subtitle = '',
                   spanlab = "Mean (Variance) of Daily Counts by Period",
                   footnotes = 'all',
                   title = NULL)

Make_summary_table(Test_summ_raw, vars = c("T2", "T3", "T4", "T5"),
                   spanlab = "Daily Mean (Variance) of Tests by Weekday and Time Period",
                   title = "Mean and Variance of Tests/100kPop/Day",
                   subtitle = "Middlesex, MA | Data from March 1 - January 20th, 2021",
                   footnotes = "cases")


Make_summary_table(Case_summ_raw, vars = c("T2", "T3", "T4", "T5"),
                   spanlab = "Daily Mean (Variance) of Cases by Weekday and Time Period",
                   title = "Mean and Variance of Cases/100kPop/Day",
                   subtitle = "Middlesex, MA | Data from March 1 - January 20th, 2021",
                   footnotes = "cases")

Make_summary_table(Death_summ_raw, vars = c("T2", "T3", "T4", "T5"),
                   spanlab = "Daily Mean (Variance) of Deaths by Weekday and Time Period",
                   title = "Mean and Variance of Deaths/100kPop/Day",
                   subtitle = "Middlesex, MA | Data from March 1 - January 20th, 2021",
                   footnotes = "deaths")
# 
# Make_summary_table(Death_summ_ratio, vars = c("T1", "T2", "T3", "T4"),
#                    spanlab = "Ratio of Daily Mean and Var to Sunday Mean and Var",
#                    title = "Relative Rates of Death Counts Compared to Sundays",
#                    subtitle = "Middlesex, MA | Data from March 1-January 20th, 2021",
#                    footnotes = "deaths")
# 
# Make_summary_table(Case_summ_ratio, vars = c("T1", "T2", "T3", "T4"),
#                    spanlab = "Ratio of Daily Mean and Var to Sunday Mean and Var",
#                    title = "Relative Rates of Case Counts Compared to Sundays",
#                    subtitle = "Middlesex, MA | Data from March 1 - January 20th, 2021",
#                    footnotes = "cases")



# Save Middlesex data set for records -------------------------------------

Middlesex %>%
  select(period, T2:T5, weekfac, caselo, dtper100k, dcper100k, ddper100k, Date, weekday, County,
         State, FIPS, cases, deaths, dcases, ddeaths, deathlo, MA_tot_pop, T2D:T5D, dtests) %>%
  write_csv("cleaned data/Middlesex_Analysis_20200205_KS.csv")

COVID_plot <- ggarrange(testplot, caseplot, deathplot, ncol = 1, labels = c("A", "B", "C"),
          align = "v")
annotate_figure(COVID_plot, top = text_grob("COVID-19 Daily Testing, Case, and Death Time Series",
                                            just = "centre", color = "black", face = "bold",
                                            size = 14))

