## Middlesex_Negative_Binomial.R ##
## Author: Kees Schipper ##
## Created: 10/20/2020 ##
## Last Updated: 10/20/2020 ##

source('R scripts/COVID_timeseries_analysis.R')
source('R scripts/COVID_Daysumm_and_weights_function.R')

if (!require(segmented)) install.packages("segmented"); library(segmented)
if (!require(sjPlot)) install.packages("sjPlot"); library(sjPlot)
if (!require(gt)) install.packages("gt"); library(gt)



rm(list= ls()[! (ls() %in% c('COVID_states', 'COVID_global', 'COVID_county', 'COVID_Mobility',
                             'COVID_stmobil', 'interp_weights', 'Mob_sum_day', 'Mob_sum_stats',
                             'Mob_sum_tot', "daysumm_county", "compare_ccf"))])

# create 7-day loess smoothing terms for deaths and cases
caselo <- loess(dcper100k ~ time, span = 7/nrow(Middlesex), data = Middlesex)
deathlo <- loess(ddper100k ~ time, span = 7/nrow(Middlesex), data = Middlesex)

COVID_stmobil <- COVID_stmobil %>%
  mutate(logcases = log(((dcases + 1)/Population)*100000),
         logdeaths = log(((ddeaths + 1)/Population)*100000),
         weekfac = factor(weekfac, ordered = F),
         weekfac = relevel(weekfac, ref = "Sunday"))


# cross-correlate counties in Massachusetts -------------------------------
## list of counties in MA
# MA_data <- COVID_stmobil %>%
#   filter(State == "Massachusetts")
# 
# MA_counties <- unique(MA_data$County)
# 
# 
# comp_Middlesex <- compare_ccf("Massachusetts", "Middlesex", var = "logcases", compvar = "logdeaths", m = 7, k = 2,
#                      plot = T, intercounty = F, lagunits = "days")
# 
# MA_counties <- COVID_stmobil %>%
#   filter(State == "Massachusetts") %>%
#   summarize(counties = unique(County)) %>%
#   filter(counties != "Dukes" & counties != "Nantucket")
# 
# XCorr_allcounty <- c()
# i <- 1
# for (i in 1:nrow(MA_counties)){
#   
#   temp <- compare_ccf("Massachusetts", "Middlesex", as.character(MA_counties[i,]), var = "logcases", m = 14, k = 3,
#                             plot = T, intercounty = T, lagunits = "days")
#   XCorr_allcounty <- c(XCorr_allcounty, list(temp))
#   names(XCorr_allcounty)[i] <- MA_counties[i,]
# }


# Fit naive negative binomial regression for day of week effect -----------------------

# Create variables for time period indicators
Middlesex <- COVID_stmobil %>%
  filter(State == "Massachusetts" & County == "Middlesex") %>%
  mutate(smoothcase = kz(dcper100k, m = 7, k = 2),
         logsmoothcase = kz(logcases, m = 7, k = 2),
         smoothdeath = kz(ddper100k, m = 7, k = 2),
         logsmoothdeath = kz(logdeaths, m = 7, k = 2)) %>%
  mutate(ddper100k = ifelse(ddper100k > 10, NA, ddper100k),
         ddper100k = na.interp(ddper100k),
         pre_out = ifelse(time < 43, 1, 0),
         accel_out = ifelse(time >= 43 & time < 88, 1, 0),
         dec_out = ifelse(time >= 88 & time < 140, 1, 0),
         plat = ifelse(time >= 140 & time < 200, 1, 0),
         accel_new = ifelse(time >= 200, 1, 0)) %>%
  mutate(period = ifelse(pre_out == 1, "pre_out",
                         ifelse(accel_out == 1, "accel_out",
                                ifelse(dec_out == 1, "dec_out",
                                       ifelse(plat == 1, "plat",
                                              ifelse(accel_new == 1, "accel_new", NA)))))) %>%
  mutate(T1 = row_number(),
         T2 = ifelse(time >= 43, row_number()-42, 0),
         T3 = ifelse(time >= 88, row_number()-87, 0),
         T4 = ifelse(time >= 140, row_number()-139, 0),
         T5 = ifelse(time >= 200, row_number()-199, 0)) %>%
  mutate(T1D = row_number(),
         T2D = ifelse(time >= 65, row_number()-64, 0),
         T3D = ifelse(time >= 98, row_number()-97, 0),
         T4D = ifelse(time >= 140, row_number()-139, 0),
         T5D = ifelse(time > 205, row_number()-204, 0))

Middlesex$caselo <- caselo$fitted
Middlesex$deathlo <- deathlo$fitted

Middlesex <- Middlesex %>%
  mutate(deathlo = ifelse(deathlo < 0, 0, deathlo))

ggplot(Middlesex, aes(x = time, y = smoothcase)) +
  geom_line(size = 1) +
  geom_vline(xintercept = c(43, 88, 140, 200), col = "red") +
  labs(title = "Smoothed COVID Cases Over Time in Middlesex County",
       subtitle = "Vertical lines from left to right: Outbreak onset, Outbreak Peak,
       outbreak resolution, Outbreak Acceleration",
       caption = paste0("Time Series from 1/22/2020-", Sys.Date()),
       xlab = "Smoothed Cases/100k Population/Day")

ggplot(Middlesex, aes(x = time, y = logsmoothcase)) +
  geom_line(size = 1) +
  geom_vline(xintercept = c(43, 88, 140, 200), col = "red") +
  labs(title = "Smoothed Logarithm of COVID Cases Over Time in Middlesex County",
       subtitle = "Vertical lines from left to right: Outbreak onset, Outbreak Peak,
       outbreak resolution, Outbreak Acceleration",
       caption = paste0("Time Series from 1/22/2020-", Sys.Date()),
       xlab = "Smoothed Cases/100k Population/Day")

Middlesex$Date[c(43, 88, 140, 200)]


# Fit glm model with weekday effects --------------------------------------
# interaction terms:
# (1) pre_out = before outbreak period; (2) accel_out = acceleration of outbreak;
# (3) dec_out = outbreak after peak; (4) splat = summer plateau; (5) accel_new = second wave;

case_seg_mod <- glm.nb(dcper100k ~ caselo + T2 + I(T2^2) + T3 + I(T3^2) + T4 +
                         I(T4^2) + T5 + I(T5^2) + weekfac,
                       data = Middlesex)

tab_model(case_seg_mod, show.reflvl = T, show.ci = 0.95, digits = 3)

options(scipen = 999)
tidy_casemod <- tidy(case_seg_mod, exponentiate = T)
  # select(-statistic, -p.value) %>%
  # mutate(term = str_remove_all(term, "weekfac"),
  #        term = str_replace_all(term, c("period" = "")),
  #        std.error = exp(std.error)) %>%
  # mutate_if(is.numeric, round, 3) %>%
  # #filter(grepl(":", term)) %>%
  # separate(term, into = c("weekday", "period"), sep = ":") %>%
  # unite(col = "Estimate ± SE", estimate, std.error, sep = " ± ") %>%
  # mutate(period = as.factor(period),
  #        weekday = as.factor(weekday)) %>%
  # pivot_wider(names_from = "period",
  #             values_from = "Estimate ± SE") %>%
  # mutate_at(vars(T2:T5), as.character) %>%
  # mutate(statistics = "Estimate ± SE")
         #term = str_replace_all(term, "\\Qlspline(time, knots = c(0, 43, 88, 140, 200))\\E", "T")) %>%


Middlesex$casefit <- case_seg_mod$fitted.values
Middlesex$caseresid <- case_seg_mod$residuals

pre_rect <- data.frame(xmin = -Inf, xmax = 43, ymin = -Inf, ymax = Inf)
out_rect <- data.frame(xmin = 43, xmax = 88, ymin = -Inf, ymax = Inf)
dec_rect <- data.frame(xmin = 88, xmax = 140, ymin = -Inf, ymax = Inf)
plat_rect <- data.frame(xmin = 140, xmax = 200, ymin = -Inf, ymax = Inf)
acc_rect <- data.frame(xmin = 200, xmax = Inf, ymin = -Inf, ymax = Inf)
rectobj <- function(df, color, alpha){
  geom_rect(data = df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = color, alpha = alpha)
}


labels <- data.frame("T1", "T2", "T3", "T4", "T5")

ggplot() +
  rectobj(pre_rect, "gold", 0.15) +
  rectobj(out_rect, "blue", 0.45) +
  rectobj(dec_rect, "blue", 0.25) +
  rectobj(plat_rect, "gold", 0.15) +
  rectobj(acc_rect, "blue", 0.15) +
  geom_point(data = Middlesex, aes(x = time, y = dcper100k), col = "red") +
  geom_line(data = Middlesex, aes(x = time, y = casefit), col = "black", size = 1) +
  geom_text(
    aes(x = 20, y = 50, label = "T1"),
    data = labels
  ) +
  geom_text(
    aes(x = 60, y = 50, label = "T2"),
    data = labels
  ) +
  geom_text(
    aes(x = 110, y = 50, label = "T3"),
    data = labels
  ) +
  geom_text(
    aes(x = 170, y = 50, label = "T4"),
    data = labels
  ) +
  geom_text(
    aes(x = 240, y = 50, label = "T5"),
    data = labels
  ) +
  labs(title = "Raw Cases/100k (red) and Fitted Negative Binomial for Middlesex County",
       subtitle = "5 time periods: (T1) pre-outbreak; (T2) acceleration; (T3) decline; (T4) plateau; (T5) and 2nd wave",
       caption = "Time series from 1/22/2020-10/27/2020") +
       ylab("Cases per 100,000 population")

ggplot() +
  rectobj(pre_rect, "gold", 0.15) +
  rectobj(out_rect, "blue", 0.45) +
  rectobj(dec_rect, "blue", 0.25) +
  rectobj(plat_rect, "gold", 0.15) +
  rectobj(acc_rect, "blue", 0.15) +
  geom_text(
    aes(x = 20, y = 1, label = "T1"),
    data = labels
  ) +
  geom_text(
    aes(x = 60, y = 1, label = "T2"),
    data = labels
  ) +
  geom_text(
    aes(x = 110, y = 1, label = "T3"),
    data = labels
  ) +
  geom_text(
    aes(x = 170, y = 1, label = "T4"),
    data = labels
  ) +
  geom_text(
    aes(x = 240, y = 1, label = "T5"),
    data = labels
  ) +
  geom_line(data = Middlesex, aes(x = time, y = caseresid), col = "black") +
  geom_hline(yintercept = 0, col = "red", size = 1) +
  labs(title = "Residuals of Negative Binomial Model on Cases/100k Population") +
       xlab("time") +
       ylab("Residual")
# Find critical points for Middlesex curves for cases ---------------------
##Start curve at time = 0
##Start of outbreak: 43
##Index for peak: 88
##Index for resolution: 140
##Index for new acceleration: 200



# Run Summary Statistics for segment of the curve -------------------------

Case_summ_raw <- Middlesex %>%
  pivot_longer(cols = c("dcper100k", "casefit"),
               names_to = "variables",
               values_to = "values") %>%
  group_by(variables, period, weekday) %>%
  summarise(
    `Mean (Var)` = paste0(round(mean(values), digits = 2), " (", round(var(values), digits = 2), ")")
    #`Median (q1, q3)` = paste0(round(median(values), 2), " (", round(quantile(values, 0.25), 2), ", ", round(quantile(values, 0.75), 2), ")"),
    #`Min, Max` = paste0(round(min(values), digits = 2), ", ", round(max(values), digits = 2))
  ) %>%
  arrange(period, weekday) %>%
  pivot_longer(cols = c("Mean (Var)"),
               names_to = "statistics",
               values_to = "Values") %>%
  pivot_wider(names_from = "period",
              values_from = "Values") %>%
  select(weekday, variables, statistics, pre_out, accel_out, dec_out, plat, accel_new) %>%
  rename(T1 = pre_out,
         T2 = accel_out,
         T3 = dec_out,
         T4 = plat,
         T5 = accel_new) %>%
  mutate(variables = str_replace_all(variables, c("dcper100k" = "Cases/100kPop/Day", "casefit" = "Predicted"))) %>%
  pivot_longer(cols = c("T1", "T2", "T3", "T4", "T5"),
               names_to = "period",
               values_to = "values") %>%
  pivot_wider(names_from = "statistics",
              values_from = "values") %>%
  arrange(weekday, period) %>%
  pivot_wider(names_from = "period",
              values_from = c("Mean (Var)")) %>%
  arrange(variables) %>%
  group_by(weekday)

Case_summ <- Case_summ_ratio %>%
  gt() %>%
  cols_label(
    variables = ""
  ) %>%
  row_group_order(
    groups = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  ) %>%
  #cols_align(
   # align = "right",
    #columns = vars(T1, T2, T3, T4, T5)
  #) %>%
  # tab_style(
  #   style = cell_fill(
  #     color = "lightgrey"
  #   ),
  #   locations = list(
  #     cells_body(
  #       columns = vars(T2, T4)
  #     ),
  #     cells_column_labels(
  #       columns = vars(T2, T4)
  #     )
#   )
# ) %>%
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
    label = "Mean (variance) of case counts and fitted values",
    columns = 3:7
  ) %>%
  tab_header(
    title = md("**Summary Table of Case Counts and NB Predicted Case Counts**"),
    subtitle = html("<em>Middlesex, MA | Data from January 22 - October 28</em>")
  ) %>%
  tab_footnote(
    footnote = "January 22 - March 4",
    locations = cells_column_labels(2)
  ) %>%
  tab_footnote(
    footnote = "March 5 - April 18",
    locations = cells_column_labels(3)
  ) %>%
  tab_footnote(
    footnote = "April 19 -June 9",
    locations = cells_column_labels(4)
  ) %>%
  tab_footnote(
    footnote = "June 10 - August 8",
    locations = cells_column_labels(5)
  ) %>%
  tab_footnote(
    footnote = "August 9 - October 28",
    locations = cells_column_labels(6)
  )



write_csv(Case_summ, "summary tables/NB_case_fit_summary_20201027.csv")


# Do same thing for deaths ------------------------------------------------

#identify critical periods

ggplot(Middlesex, aes(x = time, y = smoothdeath)) +
  geom_line(size = 1) +
  geom_vline(xintercept = c(65, 98, 140, 205), col = "red") +
  labs(title = "Smoothed COVID Cases Over Time in Middlesex County",
       subtitle = "Vertical lines from left to right: Outbreak onset, Outbreak Peak,
       outbreak resolution, Outbreak Acceleration",
       caption = paste0("Time Series from 1/22/2020-", Sys.Date()),
       xlab = "Smoothed Cases/100k Population/Day")

ggplot(Middlesex, aes(x = time, y = logsmoothdeath)) +
  geom_line(size = 1) +
  geom_vline(xintercept = c(65, 98, 140, 205), col = "red") +
  labs(title = "Smoothed Logarithm of COVID Cases Over Time in Middlesex County",
       subtitle = "Vertical lines from left to right: Outbreak onset, Outbreak Peak,
       outbreak resolution, Outbreak Acceleration",
       caption = paste0("Time Series from 1/22/2020-", Sys.Date()),
       xlab = "Smoothed Cases/100k Population/Day")

Middlesex$Date[c(65, 98, 140, 205)]


# Negative binomial model for deaths --------------------------------------

death_seg_mod <- glm.nb(ddper100k ~ weekfac + deathlo + T2D + I(T2D^2) + T3D + 
                          I(T3D^2) + T4D + I(T4D^2) + T5D + I(T5D^2), data = Middlesex)

tab_model(death_seg_mod, show.reflvl = T, show.ci = 0.95, digits = 3)


tidy_deathmod <- tidy(death_seg_mod, exponentiate = T)
  # select(-statistic, -p.value) %>%
  # mutate(term = str_remove_all(term, "weekfac"),
  #        term = str_replace_all(term, c("period" = "")),
  #        std.error = exp(std.error)) %>%
  # mutate_if(is.numeric, round, digits = 3) %>%
  # filter(grepl(":", term)) %>%
  # separate(term, into = c("period", "weekday"), sep = ":") %>%
  # unite(col = "Estimate ± SE", estimate, std.error, sep = " ± ") %>%
  # mutate(period = as.factor(period),
  #        weekday = as.factor(weekday)) %>%
  # pivot_wider(names_from = "period",
  #             values_from = "Estimate ± SE") %>%
  # rename(T2 = T2D,
  #        T3 = T3D,
  #        T4 = T4D, 
  #        T5 = T5D) %>%
  # mutate_at(vars(T2:T5), as.character) %>%
  # mutate(statistics = "Estimate ± SE")

Middlesex$deathfit <- death_seg_mod$fitted.values
Middlesex$deathresid <- death_seg_mod$residuals

pre_rect <- data.frame(xmin = -Inf, xmax = 65, ymin = -Inf, ymax = Inf)
out_rect <- data.frame(xmin = 65, xmax = 98, ymin = -Inf, ymax = Inf)
dec_rect <- data.frame(xmin = 98, xmax = 140, ymin = -Inf, ymax = Inf)
plat_rect <- data.frame(xmin = 140, xmax = 205, ymin = -Inf, ymax = Inf)
acc_rect <- data.frame(xmin = 205, xmax = Inf, ymin = -Inf, ymax = Inf)
rectobj <- function(df, color, alpha){
  geom_rect(data = df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = color, alpha = alpha)
}


labels <- data.frame("T1D", "T2D", "T3D", "T4D", "T5D")

ggplot() +
  rectobj(pre_rect, "gold", 0.15) +
  rectobj(out_rect, "blue", 0.45) +
  rectobj(dec_rect, "blue", 0.25) +
  rectobj(plat_rect, "gold", 0.15) +
  rectobj(acc_rect, "blue", 0.15) +
  geom_point(data = Middlesex, aes(x = time, y = ddper100k), col = "red") +
  geom_line(data = Middlesex, aes(x = time, y = deathfit), col = "black", size = 1) +
  geom_text(
    aes(x = 30, y = 8, label = "T1"),
    data = labels
  ) +
  geom_text(
    aes(x = 80, y = 8, label = "T2"),
    data = labels
  ) +
  geom_text(
    aes(x = 120, y = 8, label = "T3"),
    data = labels
  ) +
  geom_text(
    aes(x = 180, y = 8, label = "T4"),
    data = labels
  ) +
  geom_text(
    aes(x = 250, y = 8, label = "T5"),
    data = labels
  ) +
  labs(title = "Raw Deaths/100k (red) and Fitted Negative Binomial for Middlesex County",
       subtitle = "5 time periods: (T1) pre-outbreak; (T2) acceleration; (T3) decline; (T4) plateau; (T5) and 2nd wave",
       caption = "Time series from 1/22/2020-10/27/2020") +
       ylab("Cases per 100,000 population")


ggplot() +
  rectobj(pre_rect, "gold", 0.15) +
  rectobj(out_rect, "blue", 0.45) +
  rectobj(dec_rect, "blue", 0.25) +
  rectobj(plat_rect, "gold", 0.15) +
  rectobj(acc_rect, "blue", 0.15) +
  geom_text(
    aes(x = 30, y = 5, label = "T1"),
    data = labels
  ) +
  geom_text(
    aes(x = 80, y = 5, label = "T2"),
    data = labels
  ) +
  geom_text(
    aes(x = 120, y = 5, label = "T3"),
    data = labels
  ) +
  geom_text(
    aes(x = 180, y = 5, label = "T4"),
    data = labels
  ) +
  geom_text(
    aes(x = 250, y = 5, label = "T5"),
    data = labels
  ) +
  geom_line(data = Middlesex, aes(x = time, y = deathresid), col = "black") +
  geom_hline(yintercept = 0, col = "red", size = 1) +
  labs(title = "Residuals of Negative Binomial Model on deaths/100k Population",
       caption = "Time series from 1/22/2020-10/27/2020") +
  ylab("Residuals") +
  xlab("time")



# make summary tables for death data --------------------------------------

Death_summ_raw <- Middlesex %>%
  pivot_longer(cols = c("ddper100k", "deathfit"),
               names_to = "variables",
               values_to = "values") %>%
  group_by(variables, period, weekday) %>%
  summarise(
    `Mean (Var)` = paste0(round(mean(values), digits = 2), " (", round(var(values), digits = 2), ")")
    #`Median (q1, q3)` = paste0(round(median(values), 2), " (", round(quantile(values, 0.25), 2), ", ", round(quantile(values, 0.75), 2), ")"),
    #`Min, Max` = paste0(round(min(values), digits = 2), ", ", round(max(values), digits = 2))
  ) %>%
  arrange(period, weekday) %>%
  pivot_longer(cols = c("Mean (Var)"), # "Median (q1, q3)", "Min, Max"),
               names_to = "statistics",
               values_to = "Values") %>%
  pivot_wider(names_from = "period",
              values_from = "Values") %>%
  select(weekday, variables, statistics, pre_out, accel_out, dec_out, plat, accel_new) %>%
  rename(T1D = pre_out,
         T2D = accel_out,
         T3D = dec_out,
         T4D = plat,
         T5D = accel_new) %>%
  mutate(variables = str_replace_all(variables, c("ddper100k" = "Deaths/100kPop/Day", "deathfit" = "Predicted"))) %>%
  pivot_longer(cols = c("T1D", "T2D", "T3D", "T4D", "T5D"),
               names_to = "period",
               values_to = "values") %>%
  pivot_wider(names_from = "statistics",
              values_from = "values") %>%
  arrange(weekday, period) %>%
  pivot_wider(names_from = "period",
              values_from = c("Mean (Var)")) %>%
  group_by(weekday)

Death_summ <- Death_summ_raw %>%
  gt() %>%
  cols_label(
    variables = ""
  ) %>%
  cols_align(
    align = "right",
    columns = vars(T1D, T2D, T3D, T4D, T5D)
  ) %>%
  row_group_order(
    groups = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  ) %>%
  # tab_style(
  #   style = cell_fill(
  #     color = "lightgrey"
  #   ),
  #   locations = list(
  #     cells_body(
  #       columns = vars(T2, T4)
  #     ),
  #     cells_column_labels(
  #       columns = vars(T2, T4)
  #     )
  #   )
  # ) %>%
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
     label = "Mean (Variance) of Death counts and fitted values",
     columns = 3:7
   ) %>%
  tab_header(
    title = md("**Summary Table of Death Counts and NB Predicted Death Counts**"),
    subtitle = html("<em>Middlesex, MA | Data from January 22 - October 28</em>")
  ) %>%
  tab_footnote(
    footnote = "January 22 - March 26",
    locations = cells_column_labels(2)
  ) %>%
  tab_footnote(
    footnote = "March 27 - April 28",
    locations = cells_column_labels(3)
  ) %>%
  tab_footnote(
    footnote = "April 29 -June 9",
    locations = cells_column_labels(4)
  ) %>%
  tab_footnote(
    footnote = "June 10 - August 13",
    locations = cells_column_labels(5)
  ) %>%
  tab_footnote(
    footnote = "August 14 - October 28",
    locations = cells_column_labels(6)
  )
  
  

write_csv(Death_summ, "summary tables/NB_deaths_fit_summary_20201027.csv")


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
  mutate_at(vars(3:9), ~round(`/`(., Mean_Sunday), digits = 2)) %>%
  mutate_at(vars(10:16), ~round(`/`(., Var_Sunday), digits = 2)) %>%
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
  select(weekday, variables, pre_out, accel_out, dec_out, plat, accel_new) %>%
  rename(T1 = pre_out,
         T2 = accel_out,
         T3 = dec_out,
         T4 = plat,
         T5 = accel_new) %>%
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
  mutate_at(vars(3:9), ~round(`/`(., Mean_Sunday), digits = 2)) %>%
  mutate_at(vars(10:16), ~round(`/`(., Var_Sunday), digits = 2)) %>%
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
  select(weekday, variables, pre_out, accel_out, dec_out, plat, accel_new) %>%
  rename(T1 = pre_out,
         T2 = accel_out,
         T3 = dec_out,
         T4 = plat,
         T5 = accel_new) %>%
  mutate(variables = str_replace_all(variables, c("dcper100k" = "Cases/100kPop/Day",
                                                  "casefit" = "Predicted"))) %>%
  ungroup() %>%
  arrange(weekday, variables) %>%
  group_by(weekday)
