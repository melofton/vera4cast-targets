# Title: DOY models with and without interpolation: FluoroProbe
# Author: Mary Lofton
# Date: 19SEP23

#Purpose: fit DOY model for FluoroProbe from 2014-present

library(mgcv)

#'Function to fit day of year model for chla
#'@param data data frame with columns datetime, site_id, depth_m, observation, variable
#'@param cal_dates list of two dates (yyyy-mm-dd) for start and
#'stop of calibration/fit period
#'
#'ideal output here?
#'would be something like....
#'for each variable, side-by-side plots of the fitted GAM for interpolated
#'and not interpolated, with interpolated points a different color

data <- targets
cal_dates <- c(date(data$datetime[1]),date(last(data$datetime)))

fit_DOY_chla <- function(data, cal_dates){
  
  #assign model fit start and stop dates
  start_cal <- date(cal_dates[1])
  stop_cal <- date(cal_dates[2])
  
  #assign target and predictors
  df <- data %>%
    mutate(Date = date(datetime)) %>%
    filter(Date >= start_cal & Date <= stop_cal) %>%
    mutate(doy = yday(Date)) 
  
  vars <- unique(df$variable)
  
  for(i in 1:length(vars)){
    sub <- df %>%
      filter(variable == vars[i]) %>%
      select(Date, doy, observation)
    
    colnames(sub) <- c("Date","x","y")
    
    #fit GAM following methods in ggplot()
    my.gam <- mgcv::gam(formula = y ~ s(x, bs = "cs"), family = gaussian(),
                        data = sub, method = "REML")
    
    GAM_predicted <- mgcv::predict.gam(my.gam, data.frame(x=sub$x))
    
    sub$pred <- GAM_predicted
    
    GAM_plot <- ggplot()+
      xlab("DOY")+
      ylab(vars[i])+
      geom_point(data = sub, aes(x = x, y = y, fill = "obs"))+
      geom_line(data = sub, aes(x = x, y = pred, color = "DOY model"), linewidth = 1)+
      theme_classic()+
      labs(color = NULL, fill = NULL)
    GAM_plot
    
    # next, need to get df with linear interpolation between all obs and
    # fit GAM and see how it does, maybe write cowplot with both side by side
    # for each variable
    
  }
  
  #get list of calibration dates
  dates <- data %>%
    filter(Date >= start_cal & Date <= stop_cal)
  
  #build output df
  df.out <- data.frame(model_id = "DOY",
                       datetime = dates$Date,
                       variable = "chlorophyll-a",
                       prediction = GAM_predicted)
  
  
  #return output + model with best fit + plot
  return(list(out = df.out, DOY = my.gam, plot = GAM_plot))
}
