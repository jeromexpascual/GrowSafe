# GrowSafe Weather Dashboard (UI Layout with bs4Dash)
# Author: Group 3
# =========================
# UI
# =========================
ui <- dashboardPage(
  title = "GrowSafe Dashboard",
  fullscreen = TRUE,
  
  header = dashboardHeader(
    title = dashboardBrand(
      title = tags$div(
        tags$img(
          src = "https://noping.com/_next/image?url=https%3A%2F%2Fblogadmin.noping.com%2Fwp-content%2Fuploads%2F2025%2F05%2Fgrowagarden-1747688567733.jpg&w=1920&q=75",
          height = "30px",
          style = "margin-right: 10px;"
        ),
        span("GrowSafe")
      ),
      color = "primary",
      href = "#"
    )
  ),
  
  sidebar = dashboardSidebar(
    skin = "light",
    status = "primary",
    title = "Menu",
    
    sidebarMenu(
      menuItem("Dashboard Overview", tabName = "dashboard", icon = icon("seedling")),
      menuItem("Current Conditions", tabName = "current_conditions", icon = icon("cloud")),
      menuItem("5-Day Forecast", tabName = "forecast", icon = icon("calendar-alt")),
      menuItem("Crop Advisory", tabName = "crop_advisory", icon = icon("leaf")),
      menuItem("Weather Alerts", tabName = "alerts", icon = icon("exclamation-triangle")),
      menuItem("Farming Insights", tabName = "insights", icon = icon("chart-bar")),
      menuItem("Current Weather", tabName = "current", icon = icon("sun")),
      menuItem("24H Trend", tabName = "trend", icon = icon("chart-line")),
      menuItem("Developers", tabName = "developers", icon = icon("user-cog"))
    )
  ),
  
  body = dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                bs4Card(title = NULL, width = 12, status = "primary", solidHeader = TRUE,
                        div(style = "padding: 20px;",
                            tags$div(style = "font-weight: bold; font-size: 22px; margin-bottom: 5px;",
                                     "Welcome to the Dashboard Overview tab!"
                            ),
                            tags$hr(style = "margin-top: 5px; margin-bottom: 10px;"),
                            p("This section provides important information about dashboard-related insights."),
                            
                            # Insert the value boxes here
                            fluidRow(
                              bs4ValueBoxOutput("currentTempBox", width = 4),
                              bs4ValueBoxOutput("rainProbBox", width = 4),
                              bs4ValueBoxOutput("alertStatusBox", width = 4)
                            )
                        )
                )
              ),
              
              fluidRow(
                bs4Card(title = "📊 Weather Breakdown", width = 6, status = "success", solidHeader = TRUE,
                        plotOutput("breakdownChart", height = "300px")
                ),
                bs4Card(title = "🗺️ Iloilo Weather Map", width = 6, status = "info", solidHeader = TRUE,
                        leafletOutput("iloiloMap", height = "300px")
                )
              ),
              fluidRow(
                bs4Card(title = "📈 Weather Trend (24h)", width = 12, status = "primary", solidHeader = TRUE,
                        plotOutput("dashboardBarChart", height = "250px")
                )
              )
      ),
      
      tabItem(tabName = "current_conditions",
              fluidRow(
                bs4Card(title = NULL, width = 12, status = "info", solidHeader = TRUE,
                        div(style = "padding: 20px;",
                            tags$div(style = "font-weight: bold; font-size: 22px; margin-bottom: 5px;",
                                     "Welcome to the Current Conditions tab!"
                            ),
                            tags$hr(style = "margin-top: 5px; margin-bottom: 10px;"),
                            p("Real-time weather: temperature, humidity, wind, and mini hourly trend."),
                            fluidRow(
                              bs4ValueBoxOutput("currentHumidityBox", width = 4),
                              bs4ValueBoxOutput("currentWindBox", width = 4),
                              bs4ValueBoxOutput("currentConditionBox", width = 4)
                            )
                        )
                )
              ),
              
              fluidRow(
                bs4Card(title = "🌡️ 6-Hour Temperature Trend", width = 12, status = "primary", solidHeader = TRUE,
                        plotOutput("miniTempTrend", height = "250px")
                )
              ),
              fluidRow(
                bs4Card(title = "📋 Hourly Weather Snapshot", width = 12, status = "info", solidHeader = TRUE,
                        DT::dataTableOutput("hourlyTable")
                )
              )
      ),
      
      
      tabItem(tabName = "forecast",
              fluidRow(
                bs4Card(title = NULL, width = 12, status = "success", solidHeader = TRUE,
                        div(style = "padding: 20px;",
                            tags$div(style = "font-weight: bold; font-size: 22px; margin-bottom: 5px;",
                                     "Welcome to the 5-Day Forecast tab!"
                            ),
                            tags$hr(style = "margin-top: 5px; margin-bottom: 10px;"),
                            p("Here you can view weather predictions for the next 5 days including temperature and precipitation trends.")
                        )
                )
              ),
              fluidRow(
                bs4Card(title = "🌡️ Temperature Forecast Range", width = 12, status = "primary", solidHeader = TRUE,
                        plotOutput("forecastTempPlot", height = "300px")
                )
              ),
              fluidRow(
                bs4Card(title = "🌧️ Precipitation Outlook", width = 12, status = "info", solidHeader = TRUE,
                        plotOutput("forecastPrecipPlot", height = "300px")
                )
              )
      ),
      
      tabItem(tabName = "crop_advisory",
              fluidRow(
                bs4Card(title = "🌾 Crop Advisory for Iloilo Farmers", width = 12, status = "success", solidHeader = TRUE,
                        div(style = "padding: 20px;",
                            tags$div(style = "font-weight: bold; font-size: 20px; margin-bottom: 5px;",
                                     "Get weather-based recommendations for rice, corn, and sugarcane farming."
                            ),
                            tags$hr(),
                            p("Advisories are generated based on temperature, rain probability, and classified risk levels.")
                        )
                )
              ),
              fluidRow(
                bs4Card(title = "🌾 Rice", width = 4, status = "info", solidHeader = TRUE,
                        uiOutput("riceAdvice")
                ),
                bs4Card(title = "🌽 Corn", width = 4, status = "warning", solidHeader = TRUE,
                        uiOutput("cornAdvice")
                ),
                bs4Card(title = "🍬 Sugarcane", width = 4, status = "success", solidHeader = TRUE,
                        uiOutput("sugarcaneAdvice")
                )
              )
      ),
      
      tabItem(tabName = "alerts",
              fluidRow(
                bs4Card(title = "🚨 Active Weather Alert", width = 12, status = "danger", solidHeader = TRUE,
                        uiOutput("weatherAlertUI")
                )
              )
              
      ),
      
      tabItem(tabName = "insights",
              fluidRow(
                bs4Card(title = "🌧️ Risk Level Summary (Last 12 Hours)", width = 6, status = "danger", solidHeader = TRUE,
                        plotOutput("riskSummaryChart", height = "300px")
                ),
                bs4Card(title = "🌦️ Condition Frequency (24h)", width = 6, status = "info", solidHeader = TRUE,
                        plotOutput("conditionBar", height = "300px")
                )
              ),
              
              fluidRow(
                bs4Card(title = "🌡️ Temperature Range Forecast", width = 6, status = "success", solidHeader = TRUE,
                        plotOutput("forecastTempRange", height = "300px")
                ),
                bs4Card(title = "☔ Rain Probability: Day vs Night", width = 6, status = "warning", solidHeader = TRUE,
                        plotOutput("precipBar", height = "300px")
                )
              ),
              fluidRow(
                bs4Card(title = "🌦️ Consecutive Rainy/Dry Days", width = 6, status = "info", solidHeader = TRUE,
                        plotOutput("consecutivePatternPlot", height = "300px")
                ),
                bs4Card(title = "📅 Weekday vs Weekend Weather", width = 6, status = "primary", solidHeader = TRUE,
                        plotOutput("weekdayWeekendBox", height = "300px")
                )
              )
              
      ),
      
      tabItem(tabName = "current",
              fluidRow(
                bs4Card(title = NULL, width = 12, status = "info", solidHeader = TRUE,
                        div(style = "padding: 20px;",
                            tags$div(style = "font-weight: bold; font-size: 22px; margin-bottom: 5px;",
                                     "Current Weather Snapshot"),
                            tags$hr(style = "margin-top: 5px; margin-bottom: 10px;"),
                            p("This section shows the most up-to-date weather information for Iloilo City."),
                            
                            fluidRow(
                              bs4ValueBoxOutput("currentTemp", width = 4),
                              bs4ValueBoxOutput("currentCondition", width = 4),
                              bs4ValueBoxOutput("currentHumidity", width = 4)
                            ),
                            
                            fluidRow(
                              bs4ValueBoxOutput("currentWind", width = 4),
                              bs4ValueBoxOutput("precipStatus", width = 4),
                              bs4ValueBoxOutput("observationTime", width = 4)
                            )
                        )
                )
              )
      )
      ,
      
      tabItem(tabName = "trend",
              fluidRow(
                bs4Card(title = NULL, width = 12, status = "info", solidHeader = TRUE,
                        div(style = "padding: 20px;",
                            tags$div(style = "font-weight: bold; font-size: 22px; margin-bottom: 5px;",
                                     "24-Hour Temperature Trend"),
                            tags$hr(style = "margin-top: 5px; margin-bottom: 10px;"),
                            p("Visualize the recent hourly temperature trend based on AccuWeather historical data."),
                            plotOutput("barChart", height = "300px")
                        )
                )
              )
      )
      ,
      
      tabItem(tabName = "developers",
              fluidRow(
                bs4Card(title = NULL, width = 12, status = "purple", solidHeader = TRUE,
                        div(style = "padding: 20px;",
                            tags$div(style = "font-weight: bold; font-size: 22px; margin-bottom: 5px;",
                                     "Welcome to the Developers tab!"
                            ),
                            tags$hr(style = "margin-top: 5px; margin-bottom: 10px;"),
                            p("Meet the amazing team behind GrowSafe Dashboard.")
                        )
                )
              ),
              fluidRow(
                userBox(
                  title = userDescription(
                    title = "Justyn Kurbie P. Carreon",
                    subtitle = "Developer",
                    image = "https://scontent.fmnl34-1.fna.fbcdn.net/v/t39.30808-6/428612391_25800111642921694_2811029730633688125_n.jpg?_nc_cat=111&ccb=1-7&_nc_sid=6ee11a&_nc_eui2=AeHizIdLDUF0mvml8sUmiFuK78ybTbBxrv_vzJtNsHGu_xhP8BjQ-OERDVzp0wcEdJlEmuYV3AeoqVj4iV9i48Jg&_nc_ohc=SZoaCgCqePsQ7kNvwFrXHzp&_nc_oc=AdmJVO5psuPVa_lGvfdnBaF8j7uL4kDj9FaaK9GkN_YLyXLSoeIpwj_ArhpSUETVsNHo9gEYLgZM6YlRpvh6Cg9i&_nc_zt=23&_nc_ht=scontent.fmnl34-1.fna&_nc_gid=872nS2T2H3Xi4v3nBKilhg&oh=00_AfMiIHYGKjPIZYDBYn9nbDZ0tl4Xw26ld1RwP_nZFLZ2HQ&oe=686B3C80",
                    type = 1
                  ),
                  status = "danger",
                  collapsible = FALSE,
                  "Lead Programmer"
                ),
                userBox(
                  title = userDescription(
                    title = "John Mark Fernandez",
                    subtitle = "Developer",
                    image = "https://scontent.fmnl17-7.fna.fbcdn.net/v/t39.30808-6/419046042_3165995693536615_6480884775906712229_n.jpg?_nc_cat=108&ccb=1-7&_nc_sid=a5f93a&_nc_ohc=DbqDnccfs40Q7kNvwENW_Mu&_nc_oc=AdnMzeupd33CbfCskdreBwhrXLlkmxTl4nYEHFFGVblvDfOizxRgOZiC0bpsFZ1xtHI&_nc_zt=23&_nc_ht=scontent.fmnl17-7.fna&_nc_gid=W9eiDkh8cKhPLKN0YH-Psw&oh=00_AfP3TnLJEjDyNauChOabR59hyfwsz9xdvQxsv3WmcE--HQ&oe=686C4CB1",
                    type = 1
                  ),
                  status = "indigo",
                  collapsible = FALSE,
                  "Data analyst and graph integration lead."
                ),
                userBox(
                  title = userDescription(
                    title = "Aean Lee C. Nañez",
                    subtitle = "Developer",
                    image = "https://scontent.fmnl34-1.fna.fbcdn.net/v/t39.30808-6/456647451_3456016461364135_5642100182180259627_n.jpg?_nc_cat=101&ccb=1-7&_nc_sid=6ee11a&_nc_eui2=AeHNeZusz7J6gpLf_CcFyvB_XRzi4qIquLRdHOLioiq4tHNtg5oqmyVk0OPKu4Fa2dcegKOtudTKgiBAXpZX1nXh&_nc_ohc=ZbBls3U2bxUQ7kNvwESMnPA&_nc_oc=AdkyMAFSbu7CgQ384raCfYQWfxZDRP2DtbgH6mdM7lN7-wCrDsWm5ZVDAzIPFU3ypMbWXuSLXljszVSYK23USB2P&_nc_zt=23&_nc_ht=scontent.fmnl34-1.fna&_nc_gid=ESYcCd2pe_TZbbEH0O1K7A&oh=00_AfOwMUyMMHFo4_7SbDwdVYJoyY-KBe9uJqw1mJgtOgSIfg&oe=686B357B",
                    type = 1
                  ),
                  status = "pink",
                  collapsible = FALSE,
                  "Focused on frontend interface and user experience."
                ),
                userBox(
                  title = userDescription(
                    title = "Jerome Cyrus M. Pascual",
                    subtitle = "Developer",
                    image = "https://scontent.fmnl17-4.fna.fbcdn.net/v/t39.30808-1/491700955_3063082370509708_5643731320930943048_n.jpg?stp=dst-jpg_s200x200_tt6&_nc_cat=105&ccb=1-7&_nc_sid=e99d92&_nc_ohc=vWs2aVteIg4Q7kNvwFB-1f-&_nc_oc=AdnqOy3J-GI_lvXlHCZIM6_DPjVeupCkuRHWMKlRzCEQ3ERAtM3o9AMIAbE_wGYBPZA&_nc_zt=24&_nc_ht=scontent.fmnl17-4.fna&_nc_gid=cX20jVtizz21QlB49fJ3Uw&oh=00_AfNOUsFusiX1IVCDzCmbDJtYy5VHS4KRCwgXVCaSGdhBJQ&oe=686B2EC6
",
                    type = 1
                  ),
                  status = "success",
                  collapsible = FALSE,
                  "Handles backend integration and API connectivity."
                ),
                userBox(
                  title = userDescription(
                    title = "Albert John Pelayo",
                    subtitle = "Developer",
                    image = "https://scontent.fmnl17-5.fna.fbcdn.net/v/t39.30808-6/463127284_3104084353076201_1284982293375042436_n.jpg?_nc_cat=102&ccb=1-7&_nc_sid=6ee11a&_nc_ohc=5keSDrNaNCcQ7kNvwFpVTRx&_nc_oc=AdkelS28Avgj1at36SavRi5lm0-AqXFw2IBJMa7uWr1LzVtlOp2IOqeL0mhj1nC_24Q&_nc_zt=23&_nc_ht=scontent.fmnl17-5.fna&_nc_gid=4gVFelv_4clsh85FZdZQ7A&oh=00_AfPyi4PIpS56OPvKZ5txQiwOqEUv_oR5dwnhHNyXspJv0A&oe=686B2BE7",
                    type = 1
                  ),
                  status = "primary",
                  collapsible = FALSE,
                  "Lead Programmer"
                )
              )
      )
    ),
    
    footer = dashboardFooter(left = "GrowSafe Dashboard", right = "2025")
  )
)

# =========================
# SERVER (placeholders only)
# =========================
server <- function(input, output, session) {
  
  # Load cleaned data
  weather_data <- readRDS("../data/cleaned_weather_data.rds")
  
  hourly_forecast <- weather_data$hourly_forecast
  historical_conditions <- weather_data$historical_conditions
  forecast_cleaned <- weather_data$forecast_cleaned
  results <- weather_data$risk_results
  
  # === ValueBox Outputs ===
  current_temp <- hourly_forecast$Temp_C[1]
  rain_prob <- hourly_forecast$Precip_Probability[1]
  alert_status <- ifelse(is.na(forecast_cleaned$Alert), "None", forecast_cleaned$Alert)
  wind_speed <- NA  # Optional placeholder
  
  output$currentTempBox <- renderbs4ValueBox({
    bs4ValueBox(
      value = paste0(current_temp, "°C"),
      subtitle = "Current Temp",
      icon = icon("temperature-high"),
      color = "primary"
    )
  })
  
  output$rainProbBox <- renderbs4ValueBox({
    bs4ValueBox(
      value = paste0(rain_prob, "%"),
      subtitle = "Rain Probability",
      icon = icon("cloud-rain"),
      color = "info"
    )
  })
  
  output$alertStatusBox <- renderbs4ValueBox({
    bs4ValueBox(
      value = HTML(paste0("<span style='font-size: 14px;'>", alert_status, "</span>")),
      subtitle = "Alert Status",
      icon = icon("bolt"),
      color = "danger"
    )
  })
  
  
  # Ensure datetime formatting
  historical_conditions$ObservationTime <- as.POSIXct(
    historical_conditions$ObservationTime,
    format = "%Y-%m-%d %H:%M:%S",
    tz = "Asia/Manila"
  )
  
  hourly_forecast$ForecastTime <- as.POSIXct(hourly_forecast$ForecastTime)
  
  # Output for 24-hour temperature trend
  output$dashboardBarChart <- renderPlot({
    historical_conditions_sorted <- historical_conditions %>%
      arrange(ObservationTime)
    
    ggplot(historical_conditions_sorted, aes(x = ObservationTime, y = Temperature_C)) +
      geom_line(color = "steelblue", size = 1.2) +
      geom_point(color = "navy", size = 2) +
      labs(title = "24-Hour Temperature Trend", x = "Time", y = "Temp (°C)") +
      scale_x_datetime(date_labels = "%H:%M", date_breaks = "2 hours", timezone = "Asia/Manila") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  
  
  
  # Output for weather condition breakdown
  output$breakdownChart <- renderPlot({
    condition_counts <- historical_conditions %>%
      count(Condition) %>%
      arrange(desc(n))
    
    ggplot(condition_counts, aes(x = reorder(Condition, n), y = n, fill = Condition)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Weather Condition Frequency", x = "Condition", y = "Count") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Optional map
  output$iloiloMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 122.55, lat = 10.72, zoom = 10) %>%
      addMarkers(lng = 122.55, lat = 10.72, popup = "Iloilo City")
  })
  
  output$currentHumidityBox <- renderbs4ValueBox({
    # If humidity not in hourly_forecast, replace with NA or dummy
    humidity <- if ("RelativeHumidity" %in% colnames(hourly_forecast)) hourly_forecast$RelativeHumidity[1] else NA
    
    bs4ValueBox(
      value = paste0(humidity, "%"),
      subtitle = "Humidity",
      icon = icon("tint"),
      color = "info"
    )
  })
  
  output$currentWindBox <- renderbs4ValueBox({
    wind <- if ("Wind.Speed.Value" %in% colnames(hourly_forecast)) hourly_forecast$Wind.Speed.Value[1] else NA
    
    bs4ValueBox(
      value = paste0(wind, " km/h"),
      subtitle = "Wind Speed",
      icon = icon("wind"),
      color = "success"
    )
  })
  
  output$currentConditionBox <- renderbs4ValueBox({
    condition <- hourly_forecast$Condition[1]
    
    bs4ValueBox(
      value = condition,
      subtitle = "Condition",
      icon = icon("cloud-sun"),
      color = "warning"
    )
  })
  
  output$miniTempTrend <- renderPlot({
    temp_data <- hourly_forecast[1:6, ]
    
    ggplot(temp_data, aes(x = ForecastTime, y = Temp_C)) +
      geom_line(color = "#0073C2", size = 1.5) +
      geom_point(size = 3, color = "#0073C2") +
      labs(title = "6-Hour Temperature Forecast", x = "Time", y = "Temp (°C)") +
      theme_minimal() +
      scale_x_datetime(date_labels = "%H:%M", date_breaks = "1 hour") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$hourlyTable <- renderDataTable({
    datatable(
      hourly_forecast[, c("ForecastTime", "Temp_C", "Condition", "Precip_Probability")],
      options = list(pageLength = 6, dom = 't'),
      rownames = FALSE,
      class = "cell-border stripe hover dark-mode"
    )
  })
  
  
  
  
  
  output$forecastTempPlot <- renderPlot({
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
  })
  
  output$forecastPrecipPlot <- renderPlot({
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
  })
  
  output$riceAdvice <- renderUI({
    risk_today <- results$Predicted_Risk[1]
    rain_prob <- hourly_forecast$Precip_Probability[1]
    
    if (risk_today == "High Risk" || rain_prob >= 70) {
      HTML("<p><b>⚠️ Caution:</b> Avoid planting or applying fertilizer today. Heavy rainfall expected.</p>")
    } else if (risk_today == "Caution") {
      HTML("<p>🌦️ Monitor conditions. Light irrigation may not be needed. Delay pesticide use if possible.</p>")
    } else {
      HTML("<p>✅ Great day for transplanting or applying fertilizer. Minimal weather risks today.</p>")
    }
  })
  
  output$cornAdvice <- renderUI({
    temp <- hourly_forecast$Temp_C[1]
    
    if (temp >= 35) {
      HTML("<p><b>🔥 High temp warning:</b> Consider mulching to reduce soil evaporation and heat stress.</p>")
    } else {
      HTML("<p>🌽 Ideal growing conditions. Maintain regular irrigation schedule.</p>")
    }
  })
  
  output$sugarcaneAdvice <- renderUI({
    if (!is.na(forecast_cleaned$Alert) && forecast_cleaned$Severity[1] >= 3) {
      HTML("<p><b>🚨 Weather Alert:</b> High-severity alert issued. Avoid fieldwork and secure equipment.</p>")
    } else {
      HTML("<p>🍃 Normal operations. Monitor for pests after recent rain events.</p>")
    }
  })
  
  output$weatherAlertUI <- renderUI({
    alert_text <- forecast_cleaned$Alert[1]
    severity <- forecast_cleaned$Severity[1]
    category <- forecast_cleaned$Category[1]
    
    # Safely convert to POSIXct
    start_time <- as.POSIXct(forecast_cleaned$Alert_Start[1], format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Manila")
    end_time <- as.POSIXct(forecast_cleaned$Alert_End[1], format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Manila")
    
    formatted_start <- if (!is.na(start_time)) format(start_time, "%b %d, %I:%M %p") else "N/A"
    formatted_end <- if (!is.na(end_time)) format(end_time, "%b %d, %I:%M %p") else "N/A"
    
    badge_color <- if (severity >= 4) {
      "danger"
    } else if (severity == 3) {
      "warning"
    } else {
      "success"
    }
    
    tagList(
      tags$div(class = paste0("badge badge-", badge_color),
               paste("Severity:", severity, "-", category)),
      tags$h4(alert_text),
      tags$p(icon("clock"), strong("Effective:"), paste(formatted_start, "to", formatted_end)),
      tags$p(icon("info-circle"), em("Farmers are advised to delay irrigation or outdoor tasks.")),
      if (severity >= 4) {
        tags$p(tags$strong(style = "color: red;", "⚠️ Secure your tools and avoid field work during the alert period."))
      } else if (severity == 3) {
        tags$p(tags$strong("🔸 Monitor conditions and avoid unnecessary field exposure."))
      } else {
        tags$p("✅ No major disruption expected.")
      }
    )
  })
  
  # 1. Risk Summary Pie/Bar Chart
  output$riskSummaryChart <- renderPlot({
    risk_summary <- results %>%
      count(Predicted_Risk) %>%
      arrange(desc(n))
    
    ggplot(risk_summary, aes(x = Predicted_Risk, y = n, fill = Predicted_Risk)) +
      geom_col() +
      geom_text(aes(label = n), vjust = -0.5) +
      labs(title = "Predicted Risk Level Distribution",
           x = "Risk Level", y = "Frequency") +
      scale_fill_manual(values = c("High Risk" = "red", "Caution" = "orange", "Safe" = "green")) +
      theme_minimal()
  })
  
  # 2. Condition Frequency Bar Chart (reuse your earlier logic)
  output$conditionBar <- renderPlot({
    condition_counts <- historical_conditions %>%
      count(Condition) %>%
      arrange(desc(n))
    
    ggplot(condition_counts, aes(x = reorder(Condition, n), y = n, fill = Condition)) +
      geom_col() +
      coord_flip() +
      labs(title = "Weather Condition Frequency (Past 24 Hours)", x = "Condition", y = "Count") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # 3. Forecast Temperature Range (from 5-day)
  output$forecastTempRange <- renderPlot({
    forecast_long <- five_day_forecast %>%
      select(Date, Min_Temp_C, Max_Temp_C) %>%
      tidyr::pivot_longer(cols = c(Min_Temp_C, Max_Temp_C),
                          names_to = "Type", values_to = "Temperature")
    
    ggplot(forecast_long, aes(x = Date, y = Temperature, fill = Type)) +
      geom_col(position = "dodge") +
      labs(title = "5-Day Forecast: Temperature Range",
           x = "Date", y = "Temp (°C)") +
      scale_fill_manual(values = c("Min_Temp_C" = "skyblue", "Max_Temp_C" = "tomato")) +
      theme_minimal()
  })
  
  # 4. Precipitation Probability Day vs Night
  output$precipBar <- renderPlot({
    precip_long <- five_day_forecast %>%
      select(Date, Day_Precip_Prob, Night_Precip_Prob) %>%
      tidyr::pivot_longer(cols = c(Day_Precip_Prob, Night_Precip_Prob),
                          names_to = "Period", values_to = "Probability")
    
    ggplot(precip_long, aes(x = Date, y = Probability, fill = Period)) +
      geom_col(position = "dodge") +
      labs(title = "5-Day Forecast: Precipitation (Day vs Night)",
           x = "Date", y = "Probability (%)") +
      scale_fill_manual(values = c("Day_Precip_Prob" = "dodgerblue", "Night_Precip_Prob" = "purple")) +
      theme_minimal()
  })
  
  current_weather <- readRDS("../data/current_weather_cleaned.rds")
  
  output$currentTemp <- renderbs4ValueBox({
    bs4ValueBox(
      value = paste0(current_weather$Temperature_C, "°C"),
      subtitle = "Temperature",
      icon = icon("temperature-low"),
      color = "primary"
    )
  })
  output$currentCondition <- renderbs4ValueBox({
    bs4ValueBox(
      value = current_weather$Condition,
      subtitle = "Condition",
      icon = icon("cloud-sun"),
      color = "warning"
    )
  })
  output$currentHumidity <- renderbs4ValueBox({
    bs4ValueBox(
      value = paste0(current_weather$Humidity, "%"),
      subtitle = "Humidity",
      icon = icon("tint"),
      color = "info"
    )
  })
  output$currentWind <- renderbs4ValueBox({
    bs4ValueBox(
      value = paste0(current_weather$Wind_Speed, " km/h"),
      subtitle = "Wind Speed",
      icon = icon("wind"),
      color = "success"
    )
  })
  output$precipStatus <- renderbs4ValueBox({
    precip_status <- ifelse(current_weather$Has_Precipitation, "Yes", "No")
    
    bs4ValueBox(
      value = precip_status,
      subtitle = "Precipitation?",
      icon = icon("umbrella"),
      color = ifelse(precip_status == "Yes", "danger", "secondary")
    )
  })
  output$observationTime <- renderbs4ValueBox({
    obs_time <- format(current_weather$Observation_Time, "%b %d %H:%M")
    
    bs4ValueBox(
      value = obs_time,
      subtitle = "Observed At",
      icon = icon("clock"),
      color = "secondary"
    )
  })
  
  output$barChart <- renderPlot({
    historical_conditions$ObservationTime <- as.POSIXct(
      historical_conditions$ObservationTime,
      format = "%Y-%m-%d %H:%M:%S",
      tz = "Asia/Manila"
    )
    
    historical_conditions_sorted <- historical_conditions %>%
      arrange(ObservationTime)
    
    ggplot(historical_conditions_sorted, aes(x = ObservationTime, y = Temperature_C)) +
      geom_line(color = "firebrick", size = 1) +
      geom_point(color = "darkred", size = 2) +
      labs(title = "24-Hour Temperature Trend in Iloilo City",
           x = "Time",
           y = "Temperature (°C)") +
      scale_x_datetime(
        date_labels = "%H:%M",
        date_breaks = "2 hours",
        timezone = "Asia/Manila"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 14)
      )
  })
  
  # Consecutive Rainy/Dry Days Summary
  output$consecutivePatternPlot <- renderPlot({
    # Use the latest hourly forecast with precipitation
    results %>%
      mutate(
        Day = as.Date(ForecastTime),
        Status = case_when(
          Precip_Probability >= 50 | Has_Precipitation == TRUE ~ "Rainy",
          TRUE ~ "Dry"
        )
      ) %>%
      group_by(Day, Status) %>%
      summarise(Hours = n(), .groups = "drop") %>%
      ggplot(aes(x = Day, y = Hours, fill = Status)) +
      geom_col(position = "stack") +
      labs(title = "Consecutive Rainy vs Dry Hours",
           x = "Date", y = "Number of Hours") +
      scale_fill_manual(values = c("Rainy" = "dodgerblue", "Dry" = "orange")) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 14))
  })
  
  
  output$weekdayWeekendBox <- renderPlot({
    weather_daytype <- historical_conditions %>%
      dplyr::mutate(
        DayType = ifelse(weekdays(as.Date(ObservationTime)) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
      )
    
    ggplot(weather_daytype, aes(x = DayType, y = Temperature_C, fill = DayType)) +
      geom_boxplot() +
      labs(title = "Temperature: Weekdays vs Weekends", y = "Temp (°C)", x = NULL) +
      scale_fill_manual(values = c("Weekend" = "tomato", "Weekday" = "steelblue")) +
      theme_minimal()
  })
  
}


# =========================
# RUN APP
# =========================
shinyApp(ui, server)
