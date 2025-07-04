# Load required libraries

# Load pre-bundled cleaned data
weather_data <- readRDS("data/cleaned_weather_data.rds")

# Unpack datasets
hourly_forecast <- weather_data$hourly_forecast
historical_conditions <- weather_data$historical_conditions
forecast_cleaned <- weather_data$forecast_cleaned
results <- weather_data$risk_results
five_day_forecast <- weather_data$five_day_forecast
five_day_forecast$Date <- as.Date(five_day_forecast$Date)


# Optionally extract current temperature from forecast
current_temp <- hourly_forecast$Temp_C[1]

historical_conditions$ObservationTime <- as.POSIXct(
  historical_conditions$ObservationTime,
  format = "%Y-%m-%d %H:%M:%S",
  tz = "Asia/Manila"
)

# 1. Historical Temperature Trend (Line Plot)
ggplot(historical_conditions, aes(x = ObservationTime, y = Temperature_C)) +
  geom_line(color = "firebrick", size = 1) +
  geom_point(color = "darkred", size = 2) +
  labs(
    title = "24-Hour Temperature Trend in Iloilo City",
    subtitle = paste(
      "From", format(min(historical_conditions$ObservationTime, na.rm = TRUE), "%b %d, %H:%M"),
      "to", format(max(historical_conditions$ObservationTime, na.rm = TRUE), "%b %d, %H:%M")
    ),
    x = "Observation Time",
    y = "Temperature (°C)"
  ) +
  theme_minimal() +
  scale_x_datetime(labels = date_format("%H:%M", tz = "Asia/Manila"),
                   breaks = pretty_breaks(n = 8)) +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1))



# 2. Weather Condition Frequency (Bar Plot)
condition_counts <- historical_conditions %>%
  count(Condition) %>%
  arrange(desc(n))

ggplot(condition_counts, aes(x = reorder(Condition, n), y = n, fill = Condition)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Frequency of Weather Conditions (Past 24 Hours)",
       x = "Weather Condition",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14))

# 3. Hourly Forecast (Temperature and Precipitation)
ggplot(hourly_forecast, aes(x = ForecastTime)) +
  geom_line(aes(y = Temp_C, color = "Temperature"), size = 1) +
  geom_col(aes(y = Precip_Probability, fill = "Precipitation"), alpha = 0.5) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Precipitation Probability (%)")) +
  scale_color_manual(values = c("Temperature" = "darkorange")) +
  scale_fill_manual(values = c("Precipitation" = "dodgerblue")) +
  labs(title = "12-Hour Forecast: Temperature and Precipitation Probability",
       x = "Time",
       y = "Temperature (°C)") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1))
# 4. Day vs Night Conditions (Pie Chart)
day_night <- data.frame(
  Period = c("Day", "Night"),
  Precipitation = c(
    as.numeric(forecast_cleaned$Day_Has_Precipitation),
    as.numeric(forecast_cleaned$Night_Has_Precipitation))
)

ggplot(day_night, aes(x = "", y = Precipitation, fill = Period)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = ifelse(Precipitation > 0, "Yes", "No")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Precipitation Expected: Day vs Night") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))

# 5. Temperature Range
temp_summary <- data.frame(
  Time = c("Current", "Forecast Min", "Forecast Max"),
  Temp_C = c(current_temp,
             forecast_cleaned$Min_Temp_C,
             forecast_cleaned$Max_Temp_C)
)


ggplot(temp_summary, aes(x = Time, y = Temp_C, fill = Time)) +
  geom_col() +
  geom_text(aes(label = paste0(Temp_C, "°C")), vjust = -0.5) +
  labs(title = "Temperature Comparison",
       x = "",
       y = "Temperature (°C)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14))

# 6. 5-Day Forecast: Min & Max Temperature Range
forecast_long <- five_day_forecast %>%
  select(Date, Min_Temp_C, Max_Temp_C) %>%
  pivot_longer(cols = c(Min_Temp_C, Max_Temp_C), names_to = "Type", values_to = "Temperature")

ggplot(forecast_long, aes(x = Date, y = Temperature, fill = Type)) +
  geom_col(position = "dodge") +
  labs(title = "5-Day Forecast: Temperature Range",
       x = "Date", y = "Temperature (°C)") +
  scale_fill_manual(values = c("Min_Temp_C" = "skyblue", "Max_Temp_C" = "tomato")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))


# 7. 5-Day Forecast: Precipitation Probability (Day vs Night)
if (!all(is.na(five_day_forecast$Day_Precip_Prob)) &&
    !all(is.na(five_day_forecast$Night_Precip_Prob))) {
  
  precip_long <- five_day_forecast %>%
    select(Date, Day_Precip_Prob, Night_Precip_Prob) %>%
    pivot_longer(cols = c(Day_Precip_Prob, Night_Precip_Prob),
                 names_to = "Period", values_to = "Probability")
  
  ggplot(precip_long, aes(x = Date, y = Probability, fill = Period)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "5-Day Forecast: Precipitation Probability",
         x = "Date", y = "Probability (%)") +
    scale_fill_manual(values = c("Day_Precip_Prob" = "deepskyblue", "Night_Precip_Prob" = "purple")) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14))
  
} else {
  message("❗ No precipitation probability data available for 5-day forecast.")
}

