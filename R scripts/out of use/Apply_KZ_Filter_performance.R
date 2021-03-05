## Apply_KZ_Filter_peformance.R ##
## Author: Kees Schipper ##
## Date created: 9/20/2020 ##
## Date last edited: 9/20/2020 ##
## Last edited by: Kees Schipper ##
## Notes: The purpose of this script is to apply KZA filters to COVID case data, and test performance
##        of filters with different window widths. Data for this script comes from COVID_JHU_DATA.R
##        Optimization strategies derived from the paper:
##        Applying Kolmogorov-Zurbenko Adaptive R-Software 
##        Igor G Zurbenko1 & Mingzeng Sun1


# Run COVID_JHU_DATA to get data ------------------------------------------
source("R scripts/COVID_JHU_DATA.R")



# create function to apply KZ filter to COVID_states data -----------------

KZA_performance <- function(min_filt, max_filt, state, county){
  COVID_county <- COVID_states %>%
    filter(State == state & County == county)
  
  filter_window <- min_filt:max_filt
  perf_metrs <- data.frame(MSE = vector(mode = "numeric", length = length(filter_window)),
                           MD = vector(mode = "numeric", length = length(filter_window)),
                           SI = vector(mode = "numeric", length = length(filter_window)),
                           m = filter_window)
  
  # create vector of filter windows, and vectors for MSA, MD, SI to store those values for the iteration
  # of each window
  for (i in seq_along(filter_window)){
    COVID_county$KZ <- kz(COVID_county$log_dcases, m = filter_window[i], k = 2)
    perf_metrs$MSE[i] <- sum(abs(COVID_county$log_dcases-COVID_county$KZ)**2)/nrow(COVID_county)
    perf_metrs$MD[i] <- abs((sum(COVID_county$log_dcases)/nrow(COVID_county)) - (sum(COVID_county$KZ)/nrow(COVID_county)))
    perf_metrs$SI[i] <- sum(abs(0-COVID_county$KZ)**2)/(length(!is.na(COVID_county$KZ)))
    
    COVID_county$KZ <- NULL
  }
  
  perf_metrs <- perf_metrs %>%
    pivot_longer(cols = c("MSE", "MD", "SI"),
                 names_to = "indicies",
                 values_to = "ind_vals") %>%
    group_by(indicies) %>%
    mutate(stand_ind_vals = ind_vals/max(ind_vals))
  
ggplot(data = perf_metrs) +
  geom_line(aes(x = m, y = stand_ind_vals, color = indicies), size = 2)
}

KZA_performance(1, 100, "California", "Ventura")


