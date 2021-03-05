## Middlesex_Negative_Binomial.R ##
## Author: Kees Schipper ##
## Created: 10/20/2020 ##
## Last Updated: 01/23/2021 ##


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
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# make sure that you have both of these R scripts, as this script depends on them
source('R scripts/COVID_JHU_DATA.R')
source('R scripts/COVID_Daysumm_and_weights_function.R')

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

Middlesex <- COVID_stmobil %>%
  filter(Date > "2020-03-01" & Date < "2021-01-20") %>% # Cases don't come into MA until after beginning of march
  filter(State == "Massachusetts" & County == "Middlesex") %>%
  mutate(smoothcase = kz(dcper100k, m = 7, k = 2), # creates a KZ smoother of case rates
         logsmoothcase = kz(logcases, m = 7, k = 2), # log version of the above
         smoothdeath = kz(ddper100k, m = 7, k = 2), # KZ smoother of deaths
         logsmoothdeath = kz(logdeaths, m = 7, k = 2)) %>% # log version of above
  mutate(pre_out = ifelse(time < 43, 1, 0), # pre outbreak period indicator
         accel_out = ifelse(time >= 43 & time < 88, 1, 0), #acceleration to peak indicator
         dec_out = ifelse(time >= 88 & time < 140, 1, 0), # decrease to nadir indicator
         plat = ifelse(time >= 140 & time < 200, 1, 0), # plateau indicator
         accel_new = ifelse(time >= 200, 1, 0)) %>% # second wave indicator
  mutate(period = ifelse(pre_out == 1, "T1",
                         ifelse(accel_out == 1, "T2",
                                ifelse(dec_out == 1, "T3",
                                       ifelse(plat == 1, "T4",
                                              ifelse(accel_new == 1, "T5", NA)))))) %>%
  mutate(T2 = row_number(),
         T3 = ifelse(time >= 88, row_number()-87, 0), # Create time series variables for 
         T4 = ifelse(time >= 140, row_number()-139, 0),
         T5 = ifelse(time >= 200, row_number()-199, 0)) %>%
  mutate(T2D = ifelse(time >= 65, row_number()-64, 0),
         T3D = ifelse(time >= 98, row_number()-97, 0),
         T4D = ifelse(time >= 140, row_number()-139, 0),
         T5D = ifelse(time > 205, row_number()-204, 0),
         week = 0) #going to start the week count on the first Sunday of the data


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
    
    #print check for indicies and values
    print(ind)
    print(val)
    print(Middlesex$Date[ind])
    
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
Middlesex<- Middlesex %>%
  mutate(deathlo = ifelse(deathlo < 0, 0, deathlo),
         caselo = ifelse(caselo < 0, 0, caselo),
         dcases = (dcper100k/100000)*Population,
         ddeaths = (ddper100k/100000)*Population) %>%
  left_join(MDPH_data, by = c("County", "Date"), suffix = c(".JHU", ".MDPH"))

Middlesex <- Middlesex %>%
  mutate(dtests = na.interp(dtests, linear = TRUE),
         dtper100k = (dtests/100000)*Population,
         testlo = lowess(dtper100k ~ time, f = 0.40)[[2]])

# plot smoothers with vertical lines representing critical periods
ggplot(Middlesex, aes(x = time, y = smoothcase)) +
  geom_line(size = 1) +
  geom_vline(xintercept = c(43, 88, 140, 200), col = "firebrick3") +
  labs(title = "Smoothed COVID Cases Over Time in Middlesex County",
       subtitle = "Vertical lines from left to right: Outbreak onset, Outbreak Peak,
       outbreak resolution, Outbreak Acceleration",
       caption = paste0("Time Series from 2020-03-01 to ", Sys.Date()),
       xlab = "Smoothed Cases/100k Population/Day")

# same as above graph but using log of cases
ggplot(Middlesex, aes(x = time, y = logsmoothcase)) +
  geom_line(size = 1) +
  geom_vline(xintercept = c(43, 88, 140, 200), col = "firebrick3") +
  labs(title = "Smoothed Logarithm of COVID Cases Over Time in Middlesex County",
       subtitle = "Vertical lines from left to right: Outbreak onset, Outbreak Peak,
       outbreak resolution, Outbreak Acceleration",
       caption = paste0("Time Series from 2020-03-01 to ", Sys.Date()),
       xlab = "Smoothed Cases/100k Population/Day")




# Fit glm model with weekday effects --------------------------------------
# create a negative binomial model for predicting case counts
case_seg_mod <- glm.nb(dcper100k ~ caselo + T2 + I(T2^2) + T3 + I(T3^2) + T4 +
                       I(T4^2) + T5 + I(T5^2) + weekfac,
                       data = Middlesex)
case_seg_mod_test <- glm.nb(dcper100k ~ caselo + T2 + I(T2^2) + T3 + I(T3^2) + T4 +
                            I(T4^2) + T5 + I(T5^2) + weekfac + dtper100k,
                            data = Middlesex)


summary(case_seg_mod)
summary(case_seg_mod_test)
lrtest(case_seg_mod, case_seg_mod_test)
plot(case_seg_mod_test$fitted.values, type = "o", col = "green")
lines(case_seg_mod_test$fitted.values, type = "o", col = "blue")

# Compare NB to poisson model with loglik ratio test ----------------------

case_mod_offset <- glm.nb(ceiling(dcases) ~ caselo + T2 + I(T2^2) + T3 + I(T3^2) + T4 +
                         I(T4^2) + T5 + I(T5^2) + weekfac + offset(log(Population)),
                       data = Middlesex)

case_mod_pois <- glm(ceiling(dcases) ~ caselo + T2 + I(T2^2) + T3 + I(T3^2) + T4 +
                      I(T4^2) + T5 + I(T5^2) + weekfac, offset = log(Population),
                     family = "poisson",
                     data = Middlesex)

lrtest(case_mod_pois, case_mod_offset)

# plot model in forest plot
to.rm <- c("deathlo", "caselo", "T2", "T3", "T4", "T5", "I(T2^2)", "I(T3^2)", "I(T4^2)", "I(T5^2)")
term.labels = c("Friday", "Thursday", "Wednesday", "Tuesday", "Monday", "Saturday")
plot_model(
  case_seg_mod, show.p = TRUE, rm.terms = to.rm, vline.color = "red",
  show.values = TRUE, value.offset = .2, title = "DoW effect in Middlesex COVID-19 Case Rates",
  colors = "Dark2", axis.labels = term.labels
  ) + ylim(0.25, 2.5)

# make a simple table with your model
tab_model(case_seg_mod, show.reflvl = T, show.ci = 0.95, digits = 3)

#make your model into a data frame
(tidy_casemod <- tidy(case_seg_mod, exponentiate = T, conf.int = T))

# add fitted values and residuals from your model onto the Middlesex data
Middlesex$casefit <- case_seg_mod$fitted.values
Middlesex$caseresid <- Middlesex$dcper100k - case_seg_mod$fitted.values

#making colored rectangles for critical period visualization
pre_rect <- data.frame(xmin = -Inf, xmax = 43, ymin = -Inf, ymax = Inf)
out_rect <- data.frame(xmin = 43, xmax = 88, ymin = -Inf, ymax = Inf)
dec_rect <- data.frame(xmin = 88, xmax = 140, ymin = -Inf, ymax = Inf)
plat_rect <- data.frame(xmin = 140, xmax = 200, ymin = -Inf, ymax = Inf)
acc_rect <- data.frame(xmin = 200, xmax = Inf, ymin = -Inf, ymax = Inf)
rectobj <- function(df, color, alpha){
  geom_rect(data = df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = color, alpha = alpha)
}


labels <- data.frame("T1", "T2", "T3", "T4", "T5")

# plot predicted cases over actual cases (red)
ggplot() +
  rectobj(pre_rect, "gold", 0.15) +
  rectobj(out_rect, "blue", 0.45) +
  rectobj(dec_rect, "blue", 0.25) +
  rectobj(plat_rect, "gold", 0.15) +
  rectobj(acc_rect, "blue", 0.15) +
  geom_area(data = Middlesex, aes(x = time, y = dcper100k),
           fill = "firebrick3", alpha = 0.5) +
  geom_line(data = Middlesex, aes(x = time, y = casefit), color = "black", size = 1) +
  geom_text(
    aes(x = 60, y = 50, label = "T1"),
    data = labels
  ) +
  geom_text(
    aes(x = 110, y = 50, label = "T2"),
    data = labels
  ) +
  geom_text(
    aes(x = 170, y = 50, label = "T3"),
    data = labels
  ) +
  geom_text(
    aes(x = 240, y = 50, label = "T4"),
    data = labels
  ) +
  labs(title = "Raw Cases/100k (red) and Fitted Negative Binomial (black) \n for Middlesex County",
       subtitle = "(T1) acceleration; (T2) return to nadir; (T3) plateau; (T4) second wave",
       caption = paste0("Time series from 2020-03-01 to 2020-11-18")) +
       ylab("Cases per 100,000 population") + xlab("Days")

# plot residuals with the same critical periods
ggplot() +
  rectobj(pre_rect, "gold", 0.15) +
  rectobj(out_rect, "blue", 0.45) +
  rectobj(dec_rect, "blue", 0.25) +
  rectobj(plat_rect, "gold", 0.15) +
  rectobj(acc_rect, "blue", 0.15) +
  geom_text(
    aes(x = 60, y = 10, label = "T1"),
    data = labels
  ) +
  geom_text(
    aes(x = 110, y = 10, label = "T2"),
    data = labels
  ) +
  geom_text(
    aes(x = 170, y = 10, label = "T3"),
    data = labels
  ) +
  geom_text(
    aes(x = 240, y = 10, label = "T4"),
    data = labels
  ) +
  geom_area(data = Middlesex, aes(x = time, y = caseresid), col = "black") +
  geom_hline(yintercept = 0, col = "firebrick3", size = 1) +
  labs(title = "Residuals of Negative Binomial Model on Cases/100k Population",
       caption = paste0("Time series from 2020-03-01 to 2020-11-18")) +
       xlab("time") +
       ylab("Residual")



# Run Summary Statistics for segment of the curve -------------------------

Case_summ_raw <- Middlesex %>%
  pivot_longer(cols = c("dcper100k", "casefit"), # pivot to long form
               names_to = "variables",
               values_to = "values") %>%
  group_by(variables, period, weekfac) %>% # group by these three variables for summary stats
  summarise(
    `Mean (Var)` = paste0(round(mean(values), digits = 2), " (", round(var(values), digits = 2), ")")
  ) %>% # making a variable for mean and variance for each time period on each day
  pivot_longer(cols = c("Mean (Var)"),
               names_to = "statistics",
               values_to = "Values") %>%
  pivot_wider(names_from = "period",
              values_from = "Values") %>%
  select(weekfac, variables, statistics, T2:T5) %>%
  mutate(variables = str_replace_all(variables, c("dcper100k" = "Cases/100kPop/Day", "casefit" = "Predicted"))) %>%
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

ggplot(Middlesex, aes(x = time, y = deathlo)) +
  geom_line(size = 1) +
  geom_vline(xintercept = c(65, 98, 140, 205), col = "firebrick3") +
  labs(title = "Smoothed COVID Deaths Over Time in Middlesex County",
       subtitle = "Vertical lines from left to right: Outbreak onset, Outbreak Peak,
       outbreak resolution, Outbreak Acceleration",
       caption = paste0("Time Series from 1/22/2020 to ", Sys.Date()),
       xlab = "Smoothed Cases/100k Population/Day")

ggplot(Middlesex, aes(x = time, y = logsmoothdeath)) +
  geom_line(size = 1) +
  geom_vline(xintercept = c(65, 98, 140, 205), col = "firebrick3") +
  labs(title = "Smoothed Logarithm of COVID Cases Over Time in Middlesex County",
       subtitle = "Vertical lines from left to right: Outbreak onset, Outbreak Peak,
       outbreak resolution, Outbreak Acceleration",
       caption = paste0("Time Series from 1/22/2020 to ", Sys.Date()),
       xlab = "Smoothed Cases/100k Population/Day")

Middlesex$Date[c(65, 98, 140, 205)]


# Negative binomial model for deaths --------------------------------------
# Negative binomial model for deaths
death_seg_mod <- glm.nb(ddper100k ~ weekfac + deathlo + T2D + I(T2D^2) + T3D + 
                          I(T3D^2) + T4D + I(T4D^2) + T5D + I(T5D^2), data = Middlesex)


# Check improvement in model fit btwn pois and nb models ------------------

death_mod_offset <- glm.nb(floor(ddeaths) ~ weekfac + deathlo + T2D + I(T2D^2) + T3D + 
                          I(T3D^2) + T4D + I(T4D^2) + T5D + I(T5D^2) + offset(log(Population)),
                        data = Middlesex)

death_mod_pois <- glm(floor(ddeaths) ~ weekfac + deathlo + T2D + I(T2D^2) + T3D + 
                         I(T3D^2) + T4D + I(T4D^2) + T5D + I(T5D^2), family = "poisson",
                         offset = log(Population),
                         data = Middlesex)

lrtest(death_mod_pois, death_mod_offset)




# plot death model
to.rm <- c("deathlo", "caselo", "T2D", "T3D", "T4D", "T5D", "I(T2D^2)", "I(T3D^2)", "I(T4D^2)", "I(T5D^2)")
plot_model(
  death_seg_mod, show.p = TRUE, rm.terms = to.rm, vline.color = "red",
  show.values = TRUE, value.offset = .2, title = "DoW effect in Middlesex COVID-19 Death Rates",
  colors = "Dark2", axis.labels = term.labels, line.size = 1
) + ylim(0.25, 3)

# make a table for your model
tab_model(death_seg_mod, show.reflvl = T, show.ci = 0.95, digits = 3, digits.p = 4)

# make model into a data frame with exponentiation
tidy_deathmod <- tidy(death_seg_mod, exponentiate = T, conf.int = T)

# add fitted values and residuals to Middlesex data
Middlesex$deathfit <- death_seg_mod$fitted.values
Middlesex$deathresid <- Middlesex$ddper100k - death_seg_mod$fitted.values

# rectangles for visualizing critical perios
pre_rect <- data.frame(xmin = -Inf, xmax = 65, ymin = -Inf, ymax = Inf)
out_rect <- data.frame(xmin = 65, xmax = 98, ymin = -Inf, ymax = Inf)
dec_rect <- data.frame(xmin = 98, xmax = 140, ymin = -Inf, ymax = Inf)
plat_rect <- data.frame(xmin = 140, xmax = 205, ymin = -Inf, ymax = Inf)
acc_rect <- data.frame(xmin = 205, xmax = Inf, ymin = -Inf, ymax = Inf)
rectobj <- function(df, color, alpha){
  geom_rect(data = df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = color, alpha = alpha)
}


labels <- data.frame("T1D", "T2D", "T3D", "T4D", "T5D")
# plot predicted vs. reported deaths
ggplot(data = Middlesex) +
  rectobj(pre_rect, "gold", 0.15) +
  rectobj(out_rect, "blue", 0.45) +
  rectobj(dec_rect, "blue", 0.25) +
  rectobj(plat_rect, "gold", 0.15) +
  rectobj(acc_rect, "blue", 0.15) +
  geom_area(data = Middlesex, aes(x = time, y = ddper100k), fill = "red", alpha = 0.5) +
  geom_line(data = Middlesex, aes(x = time, y = deathfit), color = "black", size = 1) +
  geom_text(
    aes(x = 80, y = 5, label = "T1"),
    data = labels
  ) +
  geom_text(
    aes(x = 120, y = 5, label = "T2"),
    data = labels
  ) +
  geom_text(
    aes(x = 180, y = 5, label = "T3"),
    data = labels
  ) +
  geom_text(
    aes(x = 250, y = 5, label = "T4"),
    data = labels
  ) +
  labs(title = "Raw Deaths/100k (red) and Fitted Negative Binomial for Middlesex County",
       subtitle = "4 periods: (T1) acceleration to peak; (T2) decline; (T3) plateau; (T4) and 2nd wave",
       caption = "Time series from 2020-03-01 to 2020-11-18") +
       ylab("Deaths per 100,000 population") + xlab("Days")

# plot residuals
ggplot() +
  rectobj(pre_rect, "gold", 0.15) +
  rectobj(out_rect, "blue", 0.45) +
  rectobj(dec_rect, "blue", 0.25) +
  rectobj(plat_rect, "gold", 0.15) +
  rectobj(acc_rect, "blue", 0.15) +
  geom_text(
    aes(x = 80, y = 3, label = "T1"),
    data = labels
  ) +
  geom_text(
    aes(x = 120, y = 3, label = "T2"),
    data = labels
  ) +
  geom_text(
    aes(x = 180, y = 3, label = "T3"),
    data = labels
  ) +
  geom_text(
    aes(x = 250, y = 3, label = "T4"),
    data = labels
  ) +
  geom_area(data = Middlesex, aes(x = time, y = deathresid), fill = "black") +
  geom_hline(yintercept = 0, col = "firebrick3", size = 1) +
  labs(title = "Residuals of Negative Binomial Model on deaths/100k Population",
       caption = "Time series from 2020-03-01 to 2020-11-18") +
  ylab("Residuals") +
  xlab("Days")



# make summary tables for case and death data --------------------------------------

Case_summ_raw <- Middlesex %>%
  pivot_longer(cols = c("dcper100k", "casefit"),
               names_to = "variables",
               values_to = "values") %>%
  group_by(variables, period, weekfac) %>%
  summarise(
    `Mean (Var)` = paste0(round(mean(values), digits = 2), " (", round(var(values), digits = 2), ")")
  ) %>%
  arrange(period, weekfac) %>%
  pivot_longer(cols = c("Mean (Var)"),
               names_to = "statistics",
               values_to = "Values") %>%
  pivot_wider(names_from = "period",
              values_from = "Values") %>%
  select(weekfac, variables, statistics, T2:T5) %>%
  mutate(variables = str_replace_all(variables, c("dcper100k" = "Cases/100kPop/Day", "casefit" = "Predicted"))) %>%
  pivot_longer(cols = c("T2", "T3", "T4", "T5"),
               names_to = "period",
               values_to = "values") %>%
  pivot_wider(names_from = "statistics",
              values_from = "values") %>%
  pivot_wider(names_from = "period",
              values_from = c("Mean (Var)")) %>%
  arrange(variables) %>%
  group_by(weekfac)


Death_summ_raw <- Middlesex %>%
  pivot_longer(cols = c("ddper100k", "deathfit"),
               names_to = "variables",
               values_to = "values") %>%
  group_by(variables, period, weekfac) %>%
  summarise(
    `Mean (Var)` = paste0(round(mean(values), digits = 2), " (", round(var(values), digits = 2), ")")
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
  mutate(variables = str_replace_all(variables, c("ddper100k" = "Deaths/100kPop/Day", "deathfit" = "Predicted"))) %>%
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


Case_summ_ratio <- Middlesex %>%
  pivot_longer(cols = c("dcper100k", "casefit"),
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
  relocate(Mean_Sunday, .after = Var_Saturday) %>%
  relocate(Var_Sunday, .after = Mean_Sunday) %>%
  mutate_at(vars(Mean_Monday:Mean_Saturday, Mean_Sunday), ~round(`/`(., Mean_Sunday), digits = 2)) %>%
  mutate_at(vars(Var_Monday:Var_Saturday, Var_Sunday), ~round(`/`(., Var_Sunday), digits = 2)) %>%
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
                                                  "casefit" = "Predicted"))) %>%
  ungroup() %>%
  arrange(weekday, variables) %>%
  group_by(weekday)


Death_summ_ratio <- Middlesex %>%
  pivot_longer(cols = c("ddper100k", "deathfit"),
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
  relocate(Mean_Sunday, .after = Var_Saturday) %>%
  relocate(Var_Sunday, .after = Mean_Sunday) %>%
  mutate_at(vars(Mean_Monday:Mean_Saturday, Mean_Sunday), ~round(`/`(., Mean_Sunday), digits = 2)) %>%
  mutate_at(vars(Var_Monday:Var_Saturday, Var_Sunday), ~round(`/`(., Var_Sunday), digits = 2)) %>%
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
                                                  "deathfit" = "Predicted"))) %>%
  ungroup() %>%
  arrange(weekday, variables) %>%
  group_by(weekday)





# make function for making tables -----------------------------------------
# feel free to check out the function but it's a little difficult to figure out how it
# works without having some experience working with the "gt" package

Make_summary_table <- function(input_df, vars, spanlab, title, subtitle, footnotes){
  
  if (footnotes == "cases"){
    footnotes = c("March 5 - April 18", "April 19 -June 9",
                  "June 10 - August 8", "August 9 - November 18")
  } else if (footnotes == "deaths"){
    footnotes = c("March 27 - April 28", "April 29 -June 9",
                  "June 10 - August 13", "August 14 - November 18")
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
    cells_column_labels(everything())
  )
) %>%
  opt_table_lines(extent = "default") %>%
  tab_options(
    row_group.padding = 1,
    data_row.padding = 1,
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
  tab_spanner(
    label = spanlab,
    columns = 3:6
  ) %>%
  tab_header(
    title = md(paste0("**",title,"**")),
    subtitle = html(paste0("<em>",subtitle,"</em>"))
  ) %>%
  tab_footnote(
    footnote = footnotes[1],
    locations = cells_column_labels(2)
  ) %>%
  tab_footnote(
    footnote = footnotes[2],
    locations = cells_column_labels(3)
  ) %>%
  tab_footnote(
    footnote = footnotes[3],
    locations = cells_column_labels(4)
  ) %>%
  tab_footnote(
    footnote = footnotes[4],
    locations = cells_column_labels(5)
  ) #%>%
  # tab_footnote(
  #   footnote = footnotes[5],
  #   locations = cells_column_labels(6)
  # )

return(output)
}

Make_summary_table(Case_summ_raw, vars = c("T2", "T3", "T4", "T5"),
                   spanlab = "Daily Mean (Variance) of Cases by Weekday and Time Period",
                   title = "Mean and Variance of Cases/100kPop/Day",
                   subtitle = "Middlesex, MA | Data from March 1 - November 18",
                   footnotes = "cases")

Make_summary_table(Death_summ_raw, vars = c("T2", "T3", "T4", "T5"),
                   spanlab = "Daily Mean (Variance) of Deaths by Weekday and Time Period",
                   title = "Mean and Variance of Deaths/100kPop/Day",
                   subtitle = "Middlesex, MA | Data from March 1 - November 18",
                   footnotes = "deaths")

Make_summary_table(Death_summ_ratio, vars = c("T1", "T2", "T3", "T4"),
                   spanlab = "Ratio of Daily Mean (Variance) to Sunday Mean (Variance)",
                   title = "Relative Rates of Death Counts Variance Ratios Compared to Sunday",
                   subtitle = "Middlesex, MA | Data from March 1-November 18",
                   footnotes = "deaths")

Make_summary_table(Case_summ_ratio, vars = c("T1", "T2", "T3", "T4"),
                   spanlab = "Ratio of Daily Mean (Variance) to Sunday Mean (Variance)",
                   title = "Relative Rates of Case Counts Variance Ratios Compared to Sunday",
                   subtitle = "Middlesex, MA | Data from March 1 - November 18",
                   footnotes = "cases")


# Make dataset with negative binomial residuals ---------------------------

Midd_resid <- Middlesex %>%
  ungroup() %>%
  select(FIPS:Date, cases, deaths, dcper100k, ddper100k, weekfac, retail_rec:weekend, T2:T5, T2D:T5D, caselo:deathresid) %>%
  mutate(time = row_number(),
         lowss_caser = lowess(caseresid ~ time, f = 0.10)[[2]], # make lowess smoother for case and death
         lowss_deathr = lowess(deathresid ~ time, f = 0.10)[[2]]) # residuals

ggplot(data = Midd_resid, aes(x = time)) +
  geom_line(aes(y = caseresid), col = "black") +
  geom_area(aes(y = lowss_caser), fill = "blue", alpha = 0.5)

ggplot(data = Midd_resid, aes(x = time)) +
  geom_line(aes(y = deathresid), col = "black") +
  geom_area(aes(y = lowss_deathr), fill = "firebrick3", alpha = 0.5)

# First case in Middlesex: March 2, 2020, t = 1
# First death in Middlesex: March 22, 2020, t = 21

# plot standardized lowess curves of cases and deaths on top of each other
ggplot(data = Midd_resid, aes(x = time)) +
  geom_area(aes(y = caselo/max(caselo)), fill = "blue", alpha = 0.5) +
  geom_area(aes(y = deathlo/max(deathlo)), fill = "firebrick3", alpha = 0.5) 

# plot raw residuals overtop of each other
ggplot(data = Midd_resid, aes(x = time)) +
  geom_line(aes(y = caseresid/max(caseresid)), color = "blue") +
  geom_line(aes(y = deathresid/max(deathresid)), color = "firebrick3")

# plot lowess curves of the case and death residuals on top of each other
ggplot(data = Midd_resid, aes(x = time)) +
  geom_line(aes(y = lowss_caser/max(lowss_caser)), color = "blue", size = 1) +
  geom_line(aes(y = lowss_deathr/max(abs(lowss_deathr))), color = "firebrick3", size = 1)

# create histograms of residuals for cases and deaths
ggplot(data = Midd_resid) + 
  geom_histogram(aes(x = caseresid, y = ..density..), fill = "blue")

ggplot(data = Midd_resid) +
  geom_histogram(aes(x = deathresid, y = ..density..), fill = "firebrick3")

CC <- ccf(Midd_resid$lowss_deathr, Midd_resid$lowss_caser, lag.max = 40)
AC <- CC[[1]][41:81]
(maxcorr <- max(AC))
(maxlag <- CC$lag[which(CC$acf == maxcorr)])


2*(1 - pnorm(maxcorr, mean = 0, sd = 1/sqrt(CC$n.used)))

ggplot(data = Midd_resid, aes(x = T2)) +
  geom_area(aes(y = lag(lowss_caser/max(lowss_caser), 28)), fill = "blue", alpha = 0.5) +
  geom_area(aes(y = lowss_deathr/max(lowss_deathr)), fill = "firebrick3", alpha = 0.5) +
  labs(title = "Standardized Lowess Smoothers of Case and Death residuals", 
       subtitle = "Case residuals lagged by 28 days \ndeaths in red, cases in blue",
       caption = "Time series from March 1-November 18, 2020") +
  xlab("days") +
  ylab("Standardized Case and Death Residuals") +
  theme_minimal()

# TODO
# 1. Fit day-of-the-week curve-->Done
# 2. Predict case values for that regression-->Done
# 3. Subtract predicted values from raw data to create de-trended variable-->Done
# 4. Fit LOWESS curve on de-trended variable and death variable-->think this is supposed to be a 
#    lowess curve on the raw data, and then cross-correlate them. I've fitted both
#    -->Done
# 5.1. Create lagged variables for 1-21 days for cases, run serial correlation on deaths 
# 5.2. Create lagged variables with weight serial correlation per Elena’s paper
# 5.3. Identify lagged associations between case and death data
# 6.1. Evaluate inflection points of de-trended case data by taking derivative
# 6.2. Compare inflection points to social calendar and calendar of policy events
# 6.3. Create segments of time series and estimate differences in rates across intervals


# Insert Dates into Case Curves -------------------------------------------

# March 15th -- Restaurant Dine-in services banned
# March 23rd -- stay at home advisory
# May 1st -- mandatory face coverings
# May 25th -- Memorial Day
# June 1st -- MA starts reporting probable cases and deaths from COVID-19 in their
#             data
# June 3rd -- George Floyd Protests
# June 8th -- Phase II of MA reopening plan
# July 4th -- Independence day
# July 6th -- First stage of phase III of reopening plan
# September 7th -- labor day and school openings (potentially)

dates <- c("2020-03-15", "2020-03-23", "2020-05-01", "2020-05-25", "2020-06-01",
           "2020-06-03", "2020-06-08", "2020-07-04", "2020-07-06", "2020-09-07")

Ind2Date <- function(df = Midd_resid, date) {
  index <- which(df$Date == date)
  print(index)
}

ind <- c()
for (i in 1:length(dates)){
  temp <- Ind2Date(date = dates[i])
  ind <- c(ind, temp)
}

# iterate mean value of poisson distributed variable to find optimum lag --------

modelnumb <- c()
pvals <- c()
terms <- c()
incubation <- c()
sumterms <- c()
RMSE <- c()
inc1 <- c()
mod <- 0

for (j in seq(1, 13, 0.1)){
mod <- mod + 1
  
lamda <- exp(-j)

distr <- c()
lamdavect <- c()

for (i in 1:20){
  lamdai <- lamda*(j**i/factorial(i))
  print(lamdai)
  lamdavect <- c(lamdavect, lamdai)
}

# Add distributed variable to Midd_resid dataset --------------------------

Midd_resid$Laborday <- 0
Midd_resid$Laborday[190:209] <- lamdavect
Midd_resid$MskMandate <- 0
Midd_resid$MskMandate[22:41] <- lamdavect
Midd_resid$Memday <- 0
Midd_resid$Memday[85:104] <- lamdavect
Midd_resid$July4 <- 0
Midd_resid$July4[125:144] <- lamdavect
Midd_resid$Easter <- 0
Midd_resid$Easter[42:61] <- lamdavect
Midd_resid$FloydProt <- 0
Midd_resid$FloydProt[88:107] <- lamdavect


Holiday_mod <- tidy(lm(deathresid ~ Laborday + Memday + July4 + MskMandate + Easter, data = Midd_resid))
param_calc <- lm(deathresid ~ Laborday + Memday + July4 + MskMandate + Easter, data = Midd_resid)

incubation <- c(incubation, rep(j, 5))
modelnumb <- c(modelnumb, rep(mod, 5))
terms <- c(terms, Holiday_mod$term[2:6])
pvals <- c(pvals, Holiday_mod$p.value[2:6])
RMSEcalc <- sqrt(sum((param_calc$fitted.values-Midd_resid$deathresid)**2)/nrow(Midd_resid))
inc1 <- c(inc1, j)
RMSE <- c(RMSE, RMSEcalc)

}

parms_death <- data.frame(modnum = modelnumb,
                       term = terms,
                       p.val = pvals,
                       incub_mean = incubation) 

ggplot(data = parms_death, aes(x = incub_mean)) +
  geom_line(aes(y = log(p.val), color = term)) +
  labs(title = "Log(p.val) of Death Model Parameters") +
  xlab("Model Mean incubation period")

findmin <- parms_death %>%
  group_by(incub_mean) %>%
  summarise(comb_p = sum(log(p.val)))

ggplot(data = findmin, aes(x = incub_mean, y = comb_p)) +
  geom_line(size = 1, col = "blue")

plot(inc1, RMSE, type = "l", col = "blue")

(min_death <- inc1[which(RMSE == min(RMSE))])




# Iterate incubation period for cases -------------------------------------

modelnumb <- c()
pvals <- c()
terms <- c()
incubation <- c()
sumterms <- c()
RMSE <- c()
mod <- 0

for (j in seq(1, 13, 0.1)){
  mod <- mod + 1
  
  lamda <- exp(-j)
  
  distr <- c()
  lamdavect <- c()
  
  for (i in 1:20){
    lamdai <- lamda*(j**i/factorial(i))
    print(lamdai)
    lamdavect <- c(lamdavect, lamdai)
  }
  
# Add distributed variable to Midd_resid dataset --------------------------
  
  Midd_resid$Laborday <- 0
  Midd_resid$Laborday[190:209] <- lamdavect
  Midd_resid$MskMandate <- 0
  Midd_resid$MskMandate[22:41] <- lamdavect
  Midd_resid$Memday <- 0
  Midd_resid$Memday[85:104] <- lamdavect
  Midd_resid$July4 <- 0
  Midd_resid$July4[125:144] <- lamdavect
  Midd_resid$Easter <- 0
  Midd_resid$Easter[42:61] <- lamdavect
  Midd_resid$FloydProt <- 0
  Midd_resid$FloydProt[88:107] <- lamdavect
  Midd_resid$probable <- 0
  Midd_resid$probable[92:111] <- lamdavect
  
  
  Holiday_mod <- tidy(lm(caseresid ~ Laborday + Memday + July4 + MskMandate + Easter, data = Midd_resid))
  param_calc <- lm(caseresid ~ Laborday + Memday + July4 + MskMandate + Easter, data = Midd_resid)
  
  incubation <- c(incubation, rep(j, 5))
  modelnumb <- c(modelnumb, rep(mod, 5))
  terms <- c(terms, Holiday_mod$term[2:6])
  pvals <- c(pvals, Holiday_mod$p.value[2:6])
  RMSEcalc <- sum(sqrt((param_calc$fitted.values-Midd_resid$caseresid)**2/nrow(Midd_resid)))
  RMSE <- c(RMSE, RMSEcalc)
  
}

parms_case <- data.frame(modnum = modelnumb,
                       term = terms,
                       p.val = pvals,
                       incub_mean = incubation)

# plot p-values for individual parameters over different incubation periods
ggplot(data = parms_case, aes(x = incub_mean)) +
  geom_line(aes(y = log(p.val), color = term)) +
  labs(title = "log p-values of Calendar Effects for Cases") +
  xlab("Model Mean incubation period")

# find incubation-mean parameters that minimize the log(p.val) of your parameterss
findmin <- parms_case %>%
  group_by(incub_mean) %>%
  summarise(comb_p = sum(log(p.val)))

ggplot(data = findmin, aes(x = incub_mean, y = comb_p)) +
  geom_line(size = 1, col = "blue")

min <- findmin %>%
  filter(comb_p == min(comb_p))
min_case <- min[[1]]


# Create poisson distributed variable for modeling holidays --------------

lamda <- exp(-min_death)

distr <- c()
lamdavect <- c()

for (i in 1:20){
  lamdai <- lamda*(min_death**i/factorial(i))
  print(lamdai)
  lamdavect <- c(lamdavect, lamdai)
}

plot(lamdavect, type = "o", main = "Incubation Period variable", ylab = "weights")


# Add distributed variable to Midd_resid dataset --------------------------

Midd_resid$Laborday <- 0
Midd_resid$Laborday[190:209] <- lamdavect
Midd_resid$MskMandate <- 0
Midd_resid$MskMandate[22:41] <- lamdavect
Midd_resid$Memday <- 0
Midd_resid$Memday[85:104] <- lamdavect
Midd_resid$July4 <- 0
Midd_resid$July4[125:144] <- lamdavect
Midd_resid$Easter <- 0
Midd_resid$Easter[42:61] <- lamdavect
Midd_resid$FloydProt <- 0
Midd_resid$FloydProt[88:107] <- lamdavect
Midd_resid$probable <- 0
Midd_resid$probable[92:111] <- lamdavect


# Model holidays for death residuals -----------------------------------------

if (!require(sandwich)) install.packages("sandwich"); library(sandwich)
if (!require(lmtest)) install.packages("lmtest"); library(lmtest)

plot(Midd_resid$deathresid, type = "o")

acf(Midd_resid$deathresid)
Holiday_mod <- lm(deathresid ~ Laborday + Memday + July4 + MskMandate + Easter, 
                  data = Midd_resid)

hist(Holiday_mod$residuals, breaks = 20, main = "Residuals from Holiday Model")
plot(Holiday_mod)

# Robust standard errors?
robust <- coeftest(Holiday_mod, vcov = vcovHC(Holiday_mod, type = "HC3"))
robust_ci <- coefci(Holiday_mod, vcov. = vcovHC(Holiday_mod, type = "HC3"))
robustest <- as.data.frame(round(robust[1:6, 1:4], digits = 3))
robustci <- as.data.frame(round(robust_ci[1:6, 1:2], digits = 3))
robustest <- cbind(robustest, robustci)

write.csv(robustest, "cleaned data/robustholidayest.csv")

Holmod <- tidy(Holiday_mod, conf.int = TRUE)

tab_model(Holiday_mod, show.p = TRUE, show.aic = T, show.obs = T)
plot(Holiday_mod$residuals, type = "o", main = "Residuals from Holiday-Effect lm")
abline(h = 0, col = "red")
plot(Holiday_mod$fitted, type = "o")

Midd_resid$pointdeath <- Holiday_mod$fitted.values

ggplot(data = Midd_resid, aes(x = time)) +
  geom_area(aes(y = deathresid), fill = "firebrick3") +
  geom_line(aes(y = pointdeath), color = "blue", size = 1) +
  ylim(-1.5, 3) + 
  labs(title = "Holiday Effects Plotted over Death Residuals",
       subtitle = "Mask Mandate, Easter, Memorial Day, July 4th, Labor Day",
       caption = "Time series from March 1 to November 18, 2020") +
  ylab("Death Residuals and Holiday Fitted values") +
  xlab("days")

deaths_day_Memday <- lamdavect*5.20
sum(deaths_day_Memday)
deaths_day_Easter <- lamdavect*5.50
sum(deaths_day_Easter)


# Create poisson distributed variable for modelling holidays --------------

lamda <- exp(-5.5)

distr <- c()
lamdavect <- c()

for (i in 1:20){
  lamdai <- lamda*(5.5**i/factorial(i))
  print(lamdai)
  lamdavect <- c(lamdavect, lamdai)
}

plot(lamdavect, type = "o", ylab = "weights")


# Add distributed variable to Midd_resid dataset --------------------------

Midd_resid$Laborday <- 0
Midd_resid$Laborday[190:209] <- lamdavect
Midd_resid$MskMandate <- 0
Midd_resid$MskMandate[22:41] <- lamdavect
Midd_resid$Memday <- 0
Midd_resid$Memday[85:104] <- lamdavect
Midd_resid$July4 <- 0
Midd_resid$July4[125:144] <- lamdavect
Midd_resid$Easter <- 0
Midd_resid$Easter[42:61] <- lamdavect
Midd_resid$FloydProt <- 0
Midd_resid$FloydProt[88:107] <- lamdavect
# acf(Midd_resid$caseresid)
Holiday_case <- lm(caseresid ~ Laborday + Memday + July4 + MskMandate + Easter, data = Midd_resid)
summary(Holiday_case)


Midd_resid$pointcase <- Holiday_case$fitted.values
Midd_resid$pointresid <- Holiday_case$residuals

ggplot(data = Midd_resid, aes(x = T2)) +
  geom_area(aes(y = caseresid), fill = "firebrick3") +
  geom_line(aes(y = pointcase), color = "blue", size = 1.5) 

coeftest(Holiday_case, vcov = vcovHC(Holiday_case, type = "HC0"))
  


plot(Midd_resid$caseresid, type = "o")


#TODO: Should I be doing a weighted least squares regression, as the residuals for my linear
# model are clearly not homoskedastic. In addition, log, sqrt, and inverse transforms don't work,
# as they provide nonsense values. Maybe a box-cox transform would fit?

Country <- COVID_global %>%
  ungroup() %>%
  filter(Country == "US" & Date > "2020-02-15") %>%
  mutate(weekday = weekdays(Date),
         time = row_number(),
         caselo = lowess(dcases ~ time, f = 0.2)[[2]],
         deathlo = lowess(ddeaths ~ time, f = 0.2)[[2]])

plot(Country$dcases, type = "o")
boxplot(dcases~weekday, data = Country)

death_Korea <- glm.nb(data = Country, ddeaths ~ weekday + deathlo)
case_Korea <- glm.nb(data = Country, dcases ~ weekday + caselo)

plot(death_Korea$fitted.values, type = "o")
lines(Country$ddeaths, col = "blue", type = "o")
plot(Country$ddeaths - death_Korea$fitted.values, type = "o", col = "blue")
abline(h = 0, col = "red")

cases <- ggplot(data = Country, aes(x = Date)) +
  geom_area(aes(y = dcases), fill = "blue", alpha = 0.8) +
  theme_minimal() +
  ylab("daily cases") +
  xlab("Days") +
  labs(title = "United States Daily COVID-19 Cases",
       caption = paste0("time series from March 1 to ", Sys.Date()))

deaths <- ggplot(data = Country, aes(x = Date)) +
  geom_area(aes(y = ddeaths), fill = "firebrick3", alpha = 0.8) +
  theme_minimal() +
  ylab("daily deaths") +
  xlab("Days") +
  labs(title = "United States Daily COVID-19 Deaths",
       caption = paste0("time series from March 1 to ", Sys.Date()))

ggarrange(cases, deaths, nrow = 2)
       
