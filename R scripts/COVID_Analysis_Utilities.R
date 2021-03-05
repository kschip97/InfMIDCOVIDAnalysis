## MDSX_COVID_ANALYSIS_UTILITIES.R ##
## Author: Kees Schipper ##
## Created: 2021-02-18 ##
## Last Updated: 2021-02-18 ##


library(tidyverse)
library(data.table)
library(forecast)
library(kza)
library(lubridate)


# function for cleaning case and death data from JHU github ---------------

CleanCasesDeaths <- function(df, outcome){
  output <- df %>%
    select(-UID, -iso2, -iso3, -code3, -Country_Region, -Lat, -Long_, -Combined_Key) %>%
    rename(county = Admin2,
           state = Province_State,
           fips = FIPS) %>%
    pivot_longer(cols = c('1/22/20':last_col()),
                 names_to = 'date',
                 values_to = outcome) %>%
    mutate(date = as.Date(date, '%m/%d/%y'))
  
  return(output)
}


# create rectangular object for shading background in plots ---------------
# useful for illustrating critical periods

rectobj <- function(df, color, alpha){
  geom_rect(data = df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = color, alpha = alpha)
}


# function for filtering COVID data by county, state, or both -------------

FilterCovid <- function(df, County = NULL, State = NULL){
  
  if (is.null(County) & is.null(State)){
    
    stop('Must have some entry for county, state, or both')
    
  } else if (is.null(County) & !is.null(State)){
    
    output <- df %>%
      dplyr::filter(state == State)
    
    return(output)
    
  } else if (!is.null(County) & is.null(State)){
    warning('There may be multiple counties with the same name in different states')
    
    output <- df %>%
      dplyr::filter(county == County)
    
    return(output)
    
  } else if (!is.null(County) & !is.null(State)){
    
    output <- df %>%
      dplyr::filter(county == County & state == State)
    
    return(output)
  } else
    stop('no valid county or state arguments provided')
  
}


# TODO make sure this function is secure and delete unneccessary demos
# Create filtering function with window and numb of passes ----------------
MultiPassSmoother <- function(x, data, iter, window){
  
    x = as.character(x)
    plot(data[[x]], type = 'l', col = 'grey')
    output <- data
    
    for (pass in 1:iter){
      
      if (pass == 1){
        
      output <- output %>%
        mutate(filter1 = stats::filter(output[[x]], filter = rep(1/window, window)),
               filter1 = ifelse(is.na(filter1), output[[x]], filter1))
      lines(output[['filter1']], col = 'red')
      
      } else if (pass > 1){
        
        output <- output %>%
          mutate(filter1 = stats::filter(filter1, filter = rep(1/window, window)),
                 filter1 = ifelse(is.na(filter1), output[[x]], filter1))
        lines(output[['filter2']], col = 'blue')
      }
    }
    return(output[['filter1']])
}



# find inflection points of acceleration curves for c, d, and t -----------

.FindAccelZeros <- function(df, var, smwindow = 14, smiter = 3, plot_on = NULL){
  
  # create smoothed variable to operate on
  df[["smoothed_var"]] <- kz(df[[var]], m = smwindow, k = smiter)
  df[["accel_var"]] <- c(0, diff(df[["smoothed_var"]]))
  
  posneg <- c()
  negpos <- c()
  for (i in 10:nrow(df)){
    if (df[["accel_var"]][i-1] > 0 & df[["accel_var"]][i] < 0){
      
      posneg <- c(posneg, df[["date"]][i-1])
      
    } else if (df[["accel_var"]][i-1] <= 0 & df[["accel_var"]][i] > 0){
      
      negpos <- c(negpos, df[["date"]][i-1])
      
    }
  }
  output = list(posneg = as_date(posneg, origin = lubridate::origin),
                negpos = as_date(negpos, origin = lubridate::origin),
                smoothed = df[["smoothed_var"]],
                acceleration = df[["accel_var"]])
  
  if (!is.null(plot_on)){
    if (!is.character(plot_on)){
      stop("plot_on must be a string specifying what variable you want to plot
             zeros over. Variable supplied in plot_on must exist in df")
    } else {
      plot(x = df[['date']], y = df[[plot_on]], type = 'l', col = 'black', main = paste0(plot_on, " with inflection points"))
      abline(v = output[[1]], col = 'green')
      abline(v = output[[2]], col = 'red')
    }
  } else if (is.null(plot_on)){
    plot(x = df[['date']], y = df[["smoothed_var"]], type = 'l', col = 'black', main = paste0("Smoothed ", var, " with inflection points"),
         sub = paste0('Smoothing window: ', smwindow, ' obs, with ', smiter, ' iterations'))
    abline(v = output[[1]], col = 'green')
    abline(v = output[[2]], col = 'red')
    }
  
  return(output)
}
