# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lubridate)

# Create a sample data frame
dta <- data.frame(
  years = c(2024, 2023, 2022, 2021, 2020, 2019),
  # predicted bloom dates for Rock Springs
  bloom_dates = as.Date(c('2024-04-20',	'2023-04-16',	'2022-04-27',	'2021-04-28',	'2020-05-02',	'2019-04-27'))
)

# predicted bloom dates for Harborckreek
#bloom_dates = as.Date(c('2024-04-28',	'2023-05-07',	'2022-05-08',	'2021-04-28',	'2020-05-19',	'2019-05-12'))


# Calculate the day of the year
dta$day_of_year <- yday(dta$bloom_dates)
# Extract month-day format
dta$month_day <- format(dta$bloom_dates, "%b-%d")

# Plot the data
ggplot(dta, aes(x = years, y = day_of_year, group = 1)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = month_day), vjust = -0.5, hjust = 0.5) +
  labs(
    title = "Change of apple bloom dates over years",
    x = "Year",
    y = "Bloom Date (Day of Year)"
  ) 
# Improve the aesthetics of this line graph
p2 <- ggplot(dta, aes(x = years, y = day_of_year, group = 1)) +
  geom_line(color = "blue", linewidth = 1) + # Change line color and size
  geom_point(color = "red", size = 3) + # Change point color and size
  geom_label(aes(label = month_day), vjust = -0.5, hjust = 0.5, size = 3.5) + # Adjust text size
  labs(
    title = "Change of Apple Bloom Dates Over Years at Rock Spring, PA", # adjust based on weather file
    y = "Bloom date (Day of Year)"
  ) +
  ylim(100, 127) +
  theme_minimal()

# better looking plot
print(p2)

# combine the multiple plots
library(ggpubr)
ggarrange(p1, p2, nrow =2)
