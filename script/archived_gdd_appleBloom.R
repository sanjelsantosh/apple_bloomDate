# adapted from  find the right paper information
# 

# Load required packages
library(tidyverse)

# Function to calculate daily GDD
calculate_gdd <- function(tmax, tmin, base_temp) {
  gdd <- ((tmax + tmin) / 2) - base_temp
  gdd[gdd < 0] <- 0
  return(gdd)
}

# Function to extract bloom dates based on GDD thresholds
extract_bloom_dates <- function(daily_weather_data, stage_thresholds) {
  bloom_dates <- list()
  
  for (stage in names(stage_thresholds)) {
    start_gdd <- stage_thresholds[[stage]]$start
    end_gdd <- stage_thresholds[[stage]]$end
    
    # Find the start date
    start_date <- daily_weather_data %>%
      filter(accumulated_gdd >= start_gdd) %>%
      slice(1) %>%
      pull(date)
    
    # Find the end date
    end_date <- daily_weather_data %>%
      filter(accumulated_gdd >= end_gdd) %>%
      slice(1) %>%
      pull(date)
    
    bloom_dates[[stage]] <- list(start_date = start_date, end_date = end_date)
  }
  
  return(bloom_dates)
}


# Load hourly weather data (replace with your data)
library(readr)
weather_data <- read_csv("data/Hourly_RockSprings2024.csv")

# Convert date to datetime
weather_data <- weather_data %>%
  mutate(date = mdy_hm(date))

# Convert Air Temp ˚F to Celsius
weather_data <- weather_data %>%
  mutate(air_temp_c = (`Air Temp ˚F` - 32) * 5 / 9)

# Calculate daily summaries
daily_weather_data <- weather_data %>%
  group_by(date = as_date(date)) %>%
  summarise(
    tmax = max(air_temp_c, na.rm = TRUE),
    tmin = min(air_temp_c, na.rm = TRUE)
  )

# Set parameters
start_date <- as.Date("2022-01-01")  # Start date for GDD accumulation
base_temp <- 6.11 # or 43°F Base temperature 

# Calculate cumulative GDD
daily_weather_data <- daily_weather_data %>%
  mutate(gdd = calculate_gdd(tmax, tmin, base_temp)) %>%
  mutate(accumulated_gdd = cumsum(gdd))

# Define GDD thresholds for stages
stage_thresholds <- list(
  "First Bloom_Cripps Pink" = list(start = 187.42, end = 255.9),
  "Full Bloom_Cripps Pink" = list(start = 216.72, end = 326.19),
  "First Bloom_Gala" = list(start = 220.95, end = 273.88),
  "Full Bloom_Gala" = list(start = 243.19, end = 328.51),
  "First Bloom_Red Delicious" = list(start = 207, end = 266.5),
  "Full Bloom_Red Delicious" = list(start = 234.47, end = 332.74)
)

# Extract bloom dates for the stages
predicted_bloom_dates <- extract_bloom_dates(daily_weather_data, stage_thresholds)

# Print predicted bloom dates
print(predicted_bloom_dates)

#-------------------------------
# data prep for graphs
# create a tibble and make graphs
library(tibble)

# Create a list
predicted_bloom_dates <- list(
  `First Bloom_Cripps Pink` = list(
    start_date = "2024-04-29",
    end_date = "2024-05-05"
  ),
  `Full Bloom_Cripps Pink` = list(
    start_date = "2024-05-01",
    end_date = "2024-05-14"
  ),
  `First Bloom_Gala` = list(
    start_date = "2024-05-01",
    end_date = "2024-05-07"
  ),
  `Full Bloom_Gala` = list(
    start_date = "2024-05-03",
    end_date = "2024-05-14"
  ),
  `First Bloom_Red Delicious` = list(
    start_date = "2024-04-30",
    end_date = "2024-05-06"
  ),
  `Full Bloom_Red Delicious` = list(
    start_date = "2024-05-02",
    end_date = "2024-05-14"
  )
)

# Initialize vectors to store data
variety <- character()
bloom_type <- character()
start_date <- character()
end_date <- character()

# Extract data from the list and store in vectors
for (key in names(predicted_bloom_dates)) {
  split_key <- strsplit(key, "_")[[1]]
  bloom_type <- c(bloom_type, split_key[1])
  variety <- c(variety, split_key[2])
  start_date <- c(start_date, predicted_bloom_dates[[key]]$start_date)
  end_date <- c(end_date, predicted_bloom_dates[[key]]$end_date)
}

# Create a data frame
bloom_dates_df <- tibble(
  Variety = variety,
  BloomType = bloom_type,
  StartDate = start_date,
  EndDate = end_date
)

# Display the data frame
print(bloom_dates_df)

# ------------- plot the predicted bloom dates

# Convert StartDate and EndDate to Date type
bloom_dates_df <- bloom_dates_df  |> 
  mutate(StartDate = as.Date(StartDate),
         EndDate = as.Date(EndDate))

# Create a unique numeric y-position for each variety and bloom type combination
bloom_dates_df <- bloom_dates_df %>%
  mutate(VarietyFactor = as.numeric(as.factor(Variety)),
         y_offset = ifelse(BloomType == "Full Bloom", -0.2, 0),
         y_position = VarietyFactor + y_offset)


# Create the plot with vertical adjustment and points at start and end
p1 <- ggplot(bloom_dates_df, aes(x = StartDate, y = y_position, color = BloomType)) +
  geom_segment(aes(xend = EndDate, yend = y_position), linewidth = 1) +
  geom_point(aes(x = StartDate, y = y_position)) +   # Start points
  geom_point(aes(x = EndDate, y = y_position)) +     # End points
  geom_text(aes(x = StartDate, y = y_position - 0.05, label = StartDate), size = 3) + 
  geom_text(aes(x = EndDate, y = y_position - 0.05, label = EndDate), size = 3) +
  scale_y_continuous(breaks = unique(bloom_dates_df$VarietyFactor), 
                     labels = unique(bloom_dates_df$Variety)) +
  scale_color_manual(values = c("First Bloom" = "blue", "Full Bloom" = "red")) +
  labs(y = "Variety", x = "Date", title = "2024 Apple Bloom Predicted Dates for Rocksprings, PA") +
  theme_minimal() 

p1

# plot 2: with accumulated gdd and predicted dates of bloom

p2 <- ggplot(daily_weather_data, aes(date, accumulated_gdd)) +
  geom_point(color = "red", size =0.5) +
  geom_segment(data = bloom_dates_df, 
               aes(x = StartDate, xend = EndDate, y = -Inf, yend = Inf, 
                   color = BloomType, linetype = BloomType), linewidth = 1) +
  facet_wrap(~ Variety, nrow = 3) +
  scale_color_manual(values = c("First Bloom" = "blue", "Full Bloom" = "green")) +
  scale_linetype_manual(values = c("First Bloom" = "dashed", "Full Bloom" = "solid")) +
  theme_minimal() +
  labs(title = "Accumulated GDD and Bloom Dates",
       x = "Date",
       y = "Accumulated GDD")

# Print the plot
print(p2)
