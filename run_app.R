# ===============================
# GrowSafe: Main Runner Script
# ===============================

# Step 0: Install and load required packages
required_packages <- c(
  "httr", "jsonlite", "shiny", "bs4Dash", "leaflet", "DT",
  "rpart", "rpart.plot", "dplyr", "caret", "ggplot2", "scales", "tidyr"
)

installed <- rownames(installed.packages())
to_install <- setdiff(required_packages, installed)
if (length(to_install) > 0) {
  install.packages(to_install)
}

lapply(required_packages, library, character.only = TRUE)

# Step 1: Set working directory
cat("📁 Setting working directory...\n")
project_dir <- file.path(Sys.getenv("USERPROFILE"), "Documents", "GrowSafe")
setwd(project_dir)

# Step 2: Run acquisition and transformation
cat("📡 Running data acquisition...\n")
source("scripts/AccuweatherAPI_RawDataAcquisition.r")

cat("🧹 Running data cleaning and transformation...\n")
source("scripts/AccuweatherAPI_DataCleaningTransformation.r")

# Step 3: Run risk model
cat("🌾 Running risk classification model...\n")
source("scripts/AccuweatherAPI_RiskClassificationModel.r")

# Optional: Run EDA/visuals for local testing
# cat("📊 Running EDA visualizations...\n")
source("scripts/AccuWeatherAPI_EDA_Visualizations.r")

# Step 4: Launch the dashboard
cat("🚀 Launching GrowSafe Shiny Dashboard...\n")
shiny::runApp(appDir = file.path(project_dir, "dashboard"), launch.browser = TRUE)
