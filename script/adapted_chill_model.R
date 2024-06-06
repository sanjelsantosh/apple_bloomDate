# adapted from 
# https://link.springer.com/article/10.1007/s00484-006-0043-x
# modified UTHA version of the model performed best
# GDh = growing degree hours performed 

library(tidyverse)

# Define CU values based on temperature ranges
calculate_cu <- function(temperature) {
  if (temperature < 1.4) {
    return(0)
  } else if (temperature >= 1.5 && temperature <= 2.4) {
    return(0.5)
  } else if (temperature >= 2.5 && temperature <= 9.1) {
    return(1)
  } else if (temperature >= 9.2 && temperature <= 12.4) {
    return(0.5)
  } else if (temperature >= 12.5 && temperature <= 15.9) {
    return(0)
  } else if (temperature >= 16 && temperature <= 18) {
    return(-0.5)
  } else {
    return(-1)
  }
}

# Compute daily CU
compute_daily_cu <- function(df) {
  df %>%
    mutate(CU = sapply(temperature, calculate_cu)) %>%
    group_by(Date = as.Date(datetime)) %>%
    summarize(daily_CU = sum(CU))
}

# Compute GDH for a given day
compute_gdh <- function(hourly_temps, previous_gdh, gdh_in_flow, Tb = 4.4) {
  gdh <- 0
  for (temp in hourly_temps) {
    hourly_gdh <- max(0, (temp - Tb)) * (1 + (previous_gdh / gdh_in_flow) ^ 2)
    gdh <- gdh + hourly_gdh
  }
  return(gdh)
}

# Example main function to run the model
run_model <- function(df, gdh_in_flow = 7850) {
  cu_requirement <- 1075
  gdh_threshold <- gdh_in_flow
  cumulative_cu <- 0
  cumulative_gdh <- 0
  bloom_day <- NULL
  
  daily_cu <- compute_daily_cu(df)
  
  for (i in 1:nrow(daily_cu)) {
    day <- daily_cu$Date[i]
    cu <- daily_cu$daily_CU[i]
    
    cumulative_cu <- cumulative_cu + cu
    hourly_temps <- df %>% filter(as.Date(datetime) == day) %>% pull(temperature)
    daily_gdh <- compute_gdh(hourly_temps, cumulative_gdh, gdh_in_flow)
    cumulative_gdh <- cumulative_gdh + daily_gdh
    
    if (cumulative_gdh >= gdh_threshold) {
      bloom_day <- day
      break
    }
  }
  
  return(bloom_day)
}

# Example usage
# Load your weather data into a DataFrame
df <- read.csv('data/Hourly_RockSprings2024.csv')
# specify the date time format 
df$datetime <- as.POSIXlt(df$date, tz = "UTC", format = "%m/%d/%Y %H:%M")
#change the temperature to celcius
df <- df |> 
  mutate(`Air.Temp..F` = as.numeric(`Air.Temp..F`)) |> 
  mutate(temperature = (`Air.Temp..F` - 32)*5/9)

# run the run_model function 
bloom_date <- run_model(df)
print(paste("Predicted bloom date:", bloom_date))
