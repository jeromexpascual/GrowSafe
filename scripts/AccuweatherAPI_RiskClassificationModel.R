if (!exists("hourly_forecast")) {
  hourly_forecast <- read.csv("data/hourly_forecast.csv")
}

# Step 1: Load required libraries

# Step 2: Prepare your training dataset
# Simulated labeled data from your cleaned hourly_forecast
# Add a new column 'Risk_Level' manually or through domain rules

set.seed(123)

labeled_data <- hourly_forecast %>%
  mutate(
    Risk_Level = case_when(
      Temp_C >= 35 & Precip_Probability >= 70 ~ "High Risk",
      Temp_C >= 34 & Has_Precipitation == TRUE ~ "High Risk",
      Temp_C >= 34 & Precip_Probability >= 40 ~ "Caution",
      Temp_C >= 33 & Has_Precipitation == TRUE ~ "Caution",
      Precip_Probability >= 50 ~ "Caution",
      Has_Precipitation == TRUE ~ "Caution",
      TRUE ~ "Safe"
    )
  )


# Convert Risk_Level to factor (required by classification models)
labeled_data$Risk_Level <- factor(labeled_data$Risk_Level)

# Step 3: Train a decision tree classification model
model <- rpart(Risk_Level ~ Temp_C + Precip_Probability + Has_Precipitation,
               data = labeled_data,
               method = "class",
               control = rpart.control(cp = 0.01))

# Step 4: Visualize the decision tree
rpart.plot(model, main = "GrowSafe Risk Classification Tree", type = 4, extra = 104)

# Step 5: Make predictions on new/unseen weather
new_data <- hourly_forecast %>%
  select(Temp_C, Precip_Probability, Has_Precipitation)

predicted_risks <- predict(model, newdata = new_data, type = "class")

# Combine predictions with original data for interpretation
results <- hourly_forecast %>%
  select(ForecastTime, Temp_C, Precip_Probability, Has_Precipitation) %>%
  mutate(Predicted_Risk = predicted_risks)

# View results
print(head(results, 12))

# Step 6: Evaluate the model
# Split labeled_data into training and test sets (80/20)
set.seed(42)
split_index <- sample(1:nrow(labeled_data), size = 0.8 * nrow(labeled_data))
train_set <- labeled_data[split_index, ]
test_set <- labeled_data[-split_index, ]

# Train model on training set
model_eval <- rpart(Risk_Level ~ Temp_C + Precip_Probability + Has_Precipitation,
                    data = train_set,
                    method = "class",
                    control = rpart.control(cp = 0.01))

# Predict on test set
test_predictions <- predict(model_eval, newdata = test_set, type = "class")

# Confusion Matrix
conf_matrix <- confusionMatrix(test_predictions, test_set$Risk_Level)
print(conf_matrix)

# Save predicted risk levels
write.csv(results, "data/predicted_risk_levels.csv", row.names = FALSE)

# Save decision tree plot (optional but useful)
png("dashboard/tree_plot.png", width = 800, height = 600)
rpart.plot(model, main = "GrowSafe Risk Classification Tree", type = 4, extra = 104)
dev.off()

# Create cleaned data bundle for Shiny app
historical_conditions <- read.csv("data/historical_conditions.csv")
forecast_cleaned <- read.csv("data/forecast_cleaned.csv")
five_day_forecast_cleaned <- read.csv("data/five_day_forecast.csv")

cleaned_data <- list(
  hourly_forecast = hourly_forecast,
  historical_conditions = historical_conditions,
  forecast_cleaned = forecast_cleaned,
  risk_results = results,
  five_day_forecast = five_day_forecast_cleaned  # Add this
)

saveRDS(cleaned_data, "data/cleaned_weather_data.rds")


# new

# Save summarized risk distribution:
risk_summary <- labeled_data %>%
  count(Risk_Level) %>%
  arrange(desc(n))
write.csv(risk_summary, "data/risk_distribution_summary.csv", row.names = FALSE)

# OPTIONAL:
# Save the confusion matrix stats as text for optional reporting:
writeLines(capture.output(print(conf_matrix)), "output/confusion_matrix.txt")

