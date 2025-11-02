# ==============================================================================
# Statistics using R Mini Project: CO2 Emissions and GDP Per Capita Analysis
# Project Focus: Exploratory Data Analysis (EDA) using Core Packages
# ==============================================================================

# 1. SETUP AND PACKAGE INSTALLATION

# Load the packages separately
library(dplyr)
library(ggplot2)
library(readxl) 
library(tidyr) # LOADED

# ==============================================================================
# 2. DATA LOADING AND INITIAL CLEANING
# ==============================================================================

# FINALIZED FILE PATH: Using forward slashes (/) to handle Windows paths correctly 
# and explicitly pointing to the .xlsx extension.
file_path_excel <- "D:\EDA of GDP and CO2\dataset\World_GDP_Population_CO2_Emissions_Dataset.xlsx"

# --- XLSX LOADING (PRIMARY WORKING VERSION) ---
# Reads the data from the Excel file path. Assuming the data is on the first sheet (Sheet 1).
data_raw <- read_excel(file_path_excel, sheet = 1)


# Clean Column Names and select core variables. 
# We rename columns to R-friendly names (snake_case) for easy use later.
co2_data <- data_raw %>%
  rename(
    # Column names match the raw data headers.
    gdp_real = "GDP Real (USD)", 
    gdp_per_capita = "Per Capita", 
    co2_emissions_tons = "Fossil CO2 Emissions (tons)", 
    co2_per_capita = "CO2 emissions per capita",
    # ADDED: Population Density (P/Km²) for Figure 2 revision
    population_density = "Population Density (P/Km²)"
  ) %>%
  # Select the core columns needed for analysis
  select(
    Year, 
    gdp_per_capita, 
    co2_emissions_tons, 
    co2_per_capita,
    population_density # Included for new Figure 2
  ) %>%
  # Convert raw CO2 to Gigatons for better readability in plots
  mutate(
    co2_emissions_gigatons = co2_emissions_tons / 10^9
  )

# Display the structure for checking data types
print("--- Data Structure and First Rows ---")
print(str(co2_data))
print(head(co2_data))

# ==============================================================================
# 3. EDA: DISTRIBUTION AND RELATIONSHIP VISUALS (Syllabus Requirement)
# ==============================================================================

# --- Figure 1 (NEW): Distribution Check - Time Series Plot for GDP Per Capita ---
# Goal: Visualize the temporal distribution (trend over time) and easily identify 
# periods of stagnation or drops. (Replaces Boxplot)
plot_gdp_time_series <- co2_data %>%
  ggplot(aes(x = Year, y = gdp_per_capita)) +
  geom_line(color = "#388E3C", linewidth = 1.5) +
  geom_point(color = "#1B5E20", size = 3) +
  labs(
    title = "Trend of Global GDP Per Capita (1990-2022)",
    x = "Year",
    y = "GDP Per Capita (USD)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Save and Display the plot
ggsave("plot_gdp_time_series.png", plot_gdp_time_series, width = 10, height = 6)
print("Saved plot_gdp_time_series.png")
print(plot_gdp_time_series)


# --- Figure 2: Time Series Co-Movement - Population Density vs. CO2 Per Capita ---
# Goal: Visualize the change of two metrics over time to show their relationship/co-movement.
plot_env_time_series_facet <- co2_data %>%
  # Reshape data to long format to plot two metrics against Year using facets
  select(Year, co2_per_capita, population_density) %>%
  tidyr::pivot_longer(cols = -Year, names_to = "Metric", values_to = "Value") %>%
  ggplot(aes(x = Year, y = Value, color = Metric)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~ Metric, scales = "free_y", ncol = 1, 
             labeller = labeller(Metric = c(co2_per_capita = "CO2 Emissions Per Capita (Tons)", 
                                            population_density = "Population Density (P/Km²)"))) +
  labs(
    title = "(Population Density & CO2 Emissions) vs Year",
    y = "Metric Value",
    x = "Year"
  ) +
  scale_color_manual(values = c("co2_per_capita" = "#00BFA5", "population_density" = "#FF7043")) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "none",
        strip.text = element_text(face = "bold")) # Make facet labels bold

# Save and Display the time series plot
ggsave("plot_env_time_series_facet.png", plot_env_time_series_facet, width = 10, height = 8)
print("Saved plot_env_time_series_facet.png")
print(plot_env_time_series_facet)


# ==============================================================================
# 4. EDA: VISUALIZING RELATIONSHIPS
# ==============================================================================

# --- Figure 3: Relationship Plot - Scatter Plot of GDP vs. CO2 ---
# Goal: Visualize the key relationship and add a regression line.

plot_correlation <- co2_data %>%
  ggplot(aes(x = gdp_per_capita, y = co2_per_capita)) +
  geom_point(aes(size = Year), color = "#3F51B5", alpha = 0.8) +
  # Add Linear Model (LM) trend line
  geom_smooth(method = "lm", color = "#FF9800", se = TRUE, linewidth = 1) + 
  labs(
    title = "Scatter Plot of GDP vs. CO2 Per Capita",
    x = "GDP Per Capita (USD)",
    y = "CO2 Emissions Per Capita (Tons)",
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Save and Display the correlation plot
ggsave("plot_correlation.png", plot_correlation, width = 10, height = 6)
print("Saved plot_correlation.png")
print(plot_correlation)


# ==============================================================================
# 5. STATISTICAL ANALYSIS (Simple Correlation)
# ==============================================================================

# Run a simple linear regression model on the raw data.
# This quantifies the relationship observed in the scatter plot (Figure 3).
model_raw <- lm(co2_per_capita ~ gdp_per_capita, data = co2_data)

print("--- Statistical Analysis Output (Raw Linear Model Summary) ---")
summary_raw <- summary(model_raw)
print(summary_raw)

# Extract key correlation statistics
correlation_check <- cor(co2_data$gdp_per_capita, co2_data$co2_per_capita, method = "pearson")
print(paste("Pearson Correlation (R):", round(correlation_check, 4)))

# ==============================================================================
# 6. SUMMARY OUTPUTS
# ==============================================================================

print("R Script Execution Complete. Three plots and statistical output generated.")
