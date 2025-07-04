raw_data <- readRDS("data/raw_weather_data.rds")

# Unpack variables
loc_data <- raw_data$loc_data
weather_data <- raw_data$weather_data
daily_forecast_data <- raw_data$daily_forecast_data
five_day_data <- raw_data$five_day_data
history_data <- raw_data$history_data
hourly_data <- raw_data$hourly_data


# --- Location Info ---
location_cleaned <- data.frame(
  Location_Key = loc_data$Key[1],
  City = loc_data$LocalizedName[1],
  Province = loc_data$AdministrativeArea$LocalizedName[1],
  Country = loc_data$Country$LocalizedName[1]
)

# --- Current Weather ---
obs_time <- as.POSIXct(weather_data$LocalObservationDateTime, format="%Y-%m-%dT%H:%M:%S", tz="Asia/Manila")
precip_type <- ifelse(is.null(weather_data$PrecipitationType), NA, weather_data$PrecipitationType)

current_weather_cleaned <- data.frame(
  City = location_cleaned$City,
  Observation_Time = obs_time,
  Temperature_C = weather_data$Temperature$Metric$Value,
  Condition = weather_data$WeatherText,
  Has_Precipitation = weather_data$HasPrecipitation,
  Precipitation_Type = precip_type
)

# --- 5-Day Forecast Cleaning ---
five_day_forecast_df <- five_day_data$DailyForecasts

five_day_forecast_cleaned <- data.frame(
  Date = as.Date(five_day_forecast_df$Date),
  Min_Temp_C = round((five_day_forecast_df$Temperature$Minimum$Value - 32) * 5/9, 1),
  Max_Temp_C = round((five_day_forecast_df$Temperature$Maximum$Value - 32) * 5/9, 1),
  Day_Condition = five_day_forecast_df$Day$IconPhrase,
  Night_Condition = five_day_forecast_df$Night$IconPhrase,
  Day_Precip_Prob = if ("PrecipitationProbability" %in% colnames(five_day_forecast_df$Day)) five_day_forecast_df$Day$PrecipitationProbability else NA,
  Night_Precip_Prob = if ("PrecipitationProbability" %in% colnames(five_day_forecast_df$Night)) five_day_forecast_df$Night$PrecipitationProbability else NA,
  stringsAsFactors = FALSE
)




# --- 1-Day Forecast Processing ---
forecast_df <- daily_forecast_data$DailyForecasts

# Only proceed if forecast exists
if (nrow(forecast_df) > 0) {
  forecast_date <- as.Date(safe_extract(forecast_df$Date))
  min_temp_f <- safe_extract(forecast_df$Temperature$Minimum[1]$Value)
  max_temp_f <- safe_extract(forecast_df$Temperature$Maximum[1]$Value)
  min_temp_c <- round((min_temp_f - 32) * 5/9, 1)
  max_temp_c <- round((max_temp_f - 32) * 5/9, 1)
  
  day_info <- forecast_df$Day[1, ]
  night_info <- forecast_df$Night[1, ]
  
  forecast_cleaned <- data.frame(
    City = location_cleaned$City,
    Forecast_Date = forecast_date,
    Min_Temp_C = min_temp_c,
    Max_Temp_C = max_temp_c,
    Day_Condition = safe_extract(day_info$IconPhrase),
    Night_Condition = safe_extract(night_info$IconPhrase),
    Day_Has_Precipitation = safe_extract(day_info$HasPrecipitation),
    Night_Has_Precipitation = safe_extract(night_info$HasPrecipitation),
    Alert = safe_extract(daily_forecast_data$Headline$Text),
    Category = safe_extract(daily_forecast_data$Headline$Category),
    Severity = safe_extract(daily_forecast_data$Headline$Severity),
    Alert_Start = safe_time(daily_forecast_data$Headline$EffectiveDate),
    Alert_End = safe_time(daily_forecast_data$Headline$EndDate)
  )
}

# --- Historical 24-Hour Conditions Cleaning ---
historical_conditions <- data.frame(
  ObservationTime = as.POSIXct(history_data$LocalObservationDateTime, format="%Y-%m-%dT%H:%M:%S", tz="Asia/Manila"),
  Temperature_C = history_data$Temperature$Metric$Value,
  Condition = history_data$WeatherText,
  Has_Precipitation = history_data$HasPrecipitation,
  Precipitation_Type = ifelse(is.null(history_data$PrecipitationType), NA, history_data$PrecipitationType),
  stringsAsFactors = FALSE
)

# --- 12-Hour Hourly Forecast Cleaning ---
hourly_forecast <- data.frame(
  ForecastTime = as.POSIXct(hourly_data$DateTime, format="%Y-%m-%dT%H:%M:%S", tz="Asia/Manila"),
  Temp_C = hourly_data$Temperature$Value,
  Condition = hourly_data$IconPhrase,
  Precip_Probability = hourly_data$PrecipitationProbability,
  Has_Precipitation = hourly_data$HasPrecipitation,
  Is_Daylight = hourly_data$IsDaylight,
  stringsAsFactors = FALSE
)

# new 

current_weather_cleaned <- data.frame(
  City = location_cleaned$City,
  Observation_Time = obs_time,
  Temperature_C = weather_data$Temperature$Metric$Value,
  Condition = weather_data$WeatherText,
  Has_Precipitation = weather_data$HasPrecipitation,
  Precipitation_Type = precip_type,
  Wind_Speed = safe_extract(weather_data$Wind$Speed$Metric$Value),
  Humidity = safe_extract(weather_data$RelativeHumidity)
)


write.csv(hourly_forecast, "data/hourly_forecast.csv", row.names = FALSE)
write.csv(historical_conditions, "data/historical_conditions.csv", row.names = FALSE)
write.csv(forecast_cleaned, "data/forecast_cleaned.csv", row.names = FALSE)
write.csv(five_day_forecast_cleaned, "data/five_day_forecast.csv", row.names = FALSE)


# new
saveRDS(current_weather_cleaned, "data/current_weather_cleaned.rds")

