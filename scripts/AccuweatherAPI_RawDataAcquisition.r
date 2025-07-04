
# --- Helper Functions to Safely Extract Values ---
safe_extract <- function(x) {
  if (length(x) == 0 || is.null(x)) {
    return(NA)
  } else {
    return(x[1])
  }
}

safe_time <- function(x) {
  if (length(x) == 0 || is.null(x)) {
    return(as.POSIXct(NA))
  } else {
    return(as.POSIXct(x, format="%Y-%m-%dT%H:%M:%S", tz="Asia/Manila"))
  }
}


#Raw Data Acquisition
city <- "Iloilo City"
encoded_city <- URLencode(city)
api_key <- "pyRusP3SjBJF6rODjBDjj4UnqTrzNMj5"

# Step 1: Get location key
loc_url <- paste0("https://dataservice.accuweather.com/locations/v1/cities/autocomplete?apikey=", api_key, "&q=", encoded_city)
loc_data <- fromJSON(content(GET(loc_url), "text"))
location_key <- loc_data$Key[1]

# Step 2: Get current weather
weather_url <- paste0("https://dataservice.accuweather.com/currentconditions/v1/", location_key, "?apikey=", api_key)
weather_data <- fromJSON(content(GET(weather_url), "text"))

# Step 3: Get 1-Day of Daily Forecasts
daily_forecast_url <- paste0("http://dataservice.accuweather.com/forecasts/v1/daily/1day/", location_key, "?apikey=", api_key)
daily_forecast_data <- fromJSON(content(GET(daily_forecast_url), "text"))

# Step 3.5: Get 5-Day Daily Forecast
five_day_url <- paste0("http://dataservice.accuweather.com/forecasts/v1/daily/5day/", location_key, "?apikey=", api_key)
five_day_data <- fromJSON(content(GET(five_day_url), "text"))


#Step 4: Historical conditions endpoint (past 24 hours)
history_url <- paste0("https://dataservice.accuweather.com/currentconditions/v1/", location_key,
                      "/historical/24?apikey=", api_key)
res <- GET(history_url)
history_data <- fromJSON(content(res, "text", encoding = "UTF-8"))

# Step: Clean historical data directly (no lapply needed)
historical_conditions <- data.frame(
  ObservationTime = as.POSIXct(history_data$LocalObservationDateTime, format="%Y-%m-%dT%H:%M:%S", tz="Asia/Manila"),
  Temperature_C = history_data$Temperature$Metric$Value,
  Condition = history_data$WeatherText,
  Has_Precipitation = history_data$HasPrecipitation,
  Precipitation_Type = ifelse(is.null(history_data$PrecipitationType), NA, history_data$PrecipitationType),
  stringsAsFactors = FALSE
)

#Step 5: 12-hour forecast endpoint
hourly_url <- paste0("https://dataservice.accuweather.com/forecasts/v1/hourly/12hour/", location_key,
                     "?apikey=", api_key, "&metric=true")
res_hourly <- GET(hourly_url)
hourly_data <- fromJSON(content(res_hourly, "text", encoding = "UTF-8"))

# Step: Convert to clean dataframe
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

raw_data <- list(
  loc_data = loc_data,
  weather_data = weather_data,
  daily_forecast_data = daily_forecast_data,
  five_day_data = five_day_data,        # Added this line
  history_data = history_data,
  hourly_data = hourly_data
)

saveRDS(raw_data, "data/raw_weather_data.rds")
