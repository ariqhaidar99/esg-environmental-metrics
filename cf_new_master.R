# ==============================================================================
# Project: Scotland Carbon Footprint Trend Analysis (1998 - 2021)
# Description: Exploratory Data Analysis (EDA) of Scottish greenhouse gas 
#              emissions by source using strictly base R.
# ==============================================================================

# 1. Setup & Data Import -------------------------------------------------------
getwd()
list.files()
# Disable scientific notation for cleaner axis labels on plots
options(scipen = 9999)
# Import the raw dataset
# Ensure your working directory is set to the location of this CSV file
cf <- read.csv("scottish carbon footprint.csv", stringsAsFactors = T)
colnames(cf) <- c("feature_code", "feature_name", "feature_type", 
                      "year", "measurement", "units", "value", "source")
# Remove rows with missing values in 'value' or 'source' to ensure clean plotting
cf.clean <- cf[!is.na(cf$value) & !is.na(cf$source), ]

# 3. Subsetting the Data -------------------------------------------------------
# Isolate specific emission sources based on the exact labels in the dataset
cf.direct <- subset(cf.clean, source == "Scottish emissions generated directly by households")
cf.imports <- subset(cf.clean, source == "Emissions embedded in imports direct to Scottish final consumption")
cf.uk.prod <- subset(cf.clean, source == "UK production emissions attributable to Scottish final consumption")

# 4. Sort the subsets chronologically by year so the lines draw left to right
cf.direct <- cf.direct[order(cf.direct$year), ]
cf.imports <- cf.imports[order(cf.imports$year), ]
cf.uk.prod <- cf.uk.prod[order(cf.uk.prod$year), ]

# 5. Exploratory Data Analysis & Visualization ---------------------------------

# PLOT 1: A focused look at Direct Household Emissions
# Set plotting area to a single pane
par(mfrow = c(1, 1))
plot(cf.direct$year, cf.direct$value, 
     pch = 19, col = "red", 
     xlab = "Year", ylab = expression("Carbon Footprint (Mt CO"[2]*"e)"), 
     main = "Scottish Household Direct Emissions (1998-2021)",
     ylim = c(10, 16))
# Add a smooth trend line using base R 'lowess'
lines(lowess(cf.direct$year, cf.direct$value), col = "darkred", lwd = 2)

# PLOT 2: Comparative Grid 
# Set plotting area to a 2x2 grid for multi-panel comparison
par(mfrow = c(2, 2))
# Panel A: Imported Goods
plot(cf.imports$year, cf.imports$value, 
     pch = 19, col = "blue", 
     xlab = "Year", ylab = "Mt CO2e", 
     main = "Emissions From Imported Goods", 
     ylim = c(8, 25))
lines(lowess(cf.imports$year, cf.imports$value), col = "darkblue", lwd = 2)
# Panel B: Direct Household
plot(cf.direct$year, cf.direct$value, 
     pch = 19, col = "red", 
     xlab = "Year", ylab = "Mt CO2e", 
     main = "Direct Household Emissions", 
     ylim = c(10, 16))
lines(lowess(cf.direct$year, cf.direct$value), col = "darkred", lwd = 2)
# Panel C: UK Produced Goods
plot(cf.uk.prod$year, cf.uk.prod$value, 
     pch = 19, col = "purple", 
     xlab = "Year", ylab = "Mt CO2e", 
     main = "Emissions From UK-Produced Goods", 
     ylim = c(15, 35))
lines(lowess(cf.uk.prod$year, cf.uk.prod$value), col = "darkmagenta", lwd = 2)

# ==============================================================================
# Feature 1: Forecasting the Path to Net Zero 2045
# ==============================================================================

# 1. Build the Predictive Model
# We fit a simple linear regression model tracking household emissions over time
model.direct <- lm(value ~ year, data = cf.direct)

# 2. Generate Future Data Points
# Create a sequence of years from the start of our data up to the 2045 target
future.years <- data.frame(year = 1998:2045)

# 3. Calculate Predictions and Confidence Intervals
# Predict future values and calculate the 95% confidence bounds
forecast.direct <- predict(model.direct, newdata = future.years, interval = "confidence")

# 4. Visualization: Plotting the Forecast
par(mfrow = c(1, 1)) # Ensure we are using a single plot pane
# Set up the base plot area, expanding the X-axis to 2045 and Y-axis down to 0
plot(cf.direct$year, cf.direct$value, 
     xlim = c(1998, 2045), ylim = c(0, 18),
     pch = 19, col = "red",
     xlab = "Year", ylab = expression("Carbon Footprint (Mt CO"[2]*"e)"),
     main = "Forecast: Scottish Household Emissions vs. 2045 Net Zero Target")
# Drawing shaded polygon for the 95% confidence interval
polygon(c(future.years$year, rev(future.years$year)), 
        c(forecast.direct[, "lwr"], rev(forecast.direct[, "upr"])), 
        col = rgb(1, 0, 0, 0.15), border = NA) # rgb() with 0.15 adds transparency
# Drawing projected trend line (dashed)
lines(future.years$year, forecast.direct[, "fit"], col = "darkred", lwd = 2, lty = 2)
# Re-draw the historical data points on top of the shaded polygon
points(cf.direct$year, cf.direct$value, pch = 19, col = "red")

# 5. Scottish Net-Zero Target
# Mark the 2045 target with a large green cross
points(2045, 0, pch = 4, col = "darkgreen", cex = 2, lwd = 3)
text(2040, 1.5, "2045 Net Zero Target", col = "darkgreen", font = 2)
# Add a horizontal baseline at zero
abline(h = 0, col = "black", lwd = 1, lty = 3)
# Takeaway: needs aggressive emission-reducing policies to achieve net-zero

# ==============================================================================
# Feature 2: Indexing Emissions to 1998 Baseline
# *please keep cf.clean and its subsets loaded
# ==============================================================================

# 1. Calculate the Baseline Value (1998) for each source
base.direct <- cf.direct$value[cf.direct$year == 1998]
base.imports <- cf.imports$value[cf.imports$year == 1998]
base.uk.prod <- cf.uk.prod$value[cf.uk.prod$year == 1998]

# 2. Create Indexed Values ((Current Year / Base Year) * 100)
cf.direct$indexed <- (cf.direct$value / base.direct) * 100
cf.imports$indexed <- (cf.imports$value / base.imports) * 100
cf.uk.prod$indexed <- (cf.uk.prod$value / base.uk.prod) * 100

# 3. Visualization: Indexed Comparison Plot
par(mfrow = c(1, 1)) # Ensure single plot layout

# Determine Y-axis limits dynamically based on the indexed data
y.min <- min(c(cf.direct$indexed, cf.imports$indexed, cf.uk.prod$indexed))
y.max <- max(c(cf.direct$indexed, cf.imports$indexed, cf.uk.prod$indexed))

# Create an empty base plot to set up axes and labels
plot(1, type = "n", 
     xlim = c(1998, max(cf.clean$year)), ylim = c(y.min, y.max),
     xlab = "Year", ylab = "Indexed Emissions (1998 = 100%)",
     main = "Scotland Relative Decarbonization Rates (1998 Baseline)")

# Add a horizontal baseline reference at 100%
abline(h = 100, col = "gray50", lty = 2, lwd = 2)

# Add line paths and points for each source
lines(cf.direct$year, cf.direct$indexed, col = "red", lwd = 2, type = "b", pch = 19)
lines(cf.imports$year, cf.imports$indexed, col = "blue", lwd = 2, type = "b", pch = 17)
lines(cf.uk.prod$year, cf.uk.prod$indexed, col = "purple", lwd = 2, type = "b", pch = 15)

# Add a legend so viewers know which color is which
legend("topleft", 
       legend = c("Direct Household", "Imported Goods", "UK-Produced Goods"),
       col = c("red", "blue", "purple"),
       pch = c(19, 17, 15),
       lwd = 2, bty = "n")

# ==============================================================================
# Feature 3: Adding Macro-Economic Context (The "So What?")
# ==============================================================================

# 1. Annotate the 2008 Global Financial Crisis
# Draw a vertical dashed line at the year 2008
abline(v = 2008, col = "darkorange", lwd = 2, lty = 3)

# Add rotated text right next to the line
text(x = 2008.5, y = 135, labels = "2008 Financial Crisis", 
     col = "darkorange", font = 2, srt = 90, cex = 0.9)

# 2. Annotate the 2020 COVID-19 Pandemic Lockdowns
# Draw a vertical dashed line at the year 2020
abline(v = 2020, col = "darkorange", lwd = 2, lty = 3)

# Add rotated text right next to the line
text(x = 2019.5, y = 135, labels = "COVID-19 Lockdowns", 
     col = "darkorange", font = 2, srt = 90, cex = 0.9)

# 3. Highlight the insight: The 2020 Supply Chain Crash
# We can draw an arrow pointing to the massive crash in UK-Produced Goods in 2020
arrows(x0 = 2017, y0 = 65, x1 = 2019.8, y1 = 48, 
       col = "orange", lwd = 2, length = 0.1)
text(x = 2016.5, y = 68, labels = "Supply Chain\nHalt", 
     col = "orange", font = 3, cex = 0.8)

# ==============================================================================
# Feature 4: Advanced Statistical Testing (Linear Regression & ANOVA)
# ==============================================================================

# ------------------------------------------------------------------------------
# Part A: Linear Regression (Quantifying the Decay Rate)
# ------------------------------------------------------------------------------
# We want to know exactly how fast Household Emissions are dropping per year.
model.direct.decay <- lm(value ~ year, data = cf.direct)

# Print the summary to view the slope (coefficient for 'year') and the p-value
cat("\n--- Linear Regression: Direct Household Emissions ---\n")
summary(model.direct.decay)

# ------------------------------------------------------------------------------
# Part B: ANOVA (Testing Variance Between Sources)
# ------------------------------------------------------------------------------
# Is there a statistically significant difference in the absolute emissions
# produced by Households vs. Imports vs. UK-Produced Goods?

# First, subset the clean data to ONLY include our three target sources
target.sources <- c("Scottish emissions generated directly by households", 
                    "Emissions embedded in imports direct to Scottish final consumption", 
                    "UK production emissions attributable to Scottish final consumption")

cf.anova.data <- subset(cf.clean, source %in% target.sources)

# Convert 'source' to a factor, as ANOVA requires a categorical independent variable
cf.anova.data$source <- as.factor(cf.anova.data$source)

# Run the One-Way ANOVA: Does the emission 'source' significantly affect the 'value'?
model.anova <- aov(value ~ source, data = cf.anova.data)
par(mfrow = c(2, 2))
plot(model.anova)
cat("\n--- ANOVA: Variance Between Emission Sources ---\n")
summary(model.anova)
# Post-hoc testing: If the ANOVA is significant, which specific pairs are different?
cat("\n--- Tukey HSD Post-Hoc Test: Pairwise Comparisons ---\n")
TukeyHSD(model.anova)

# Improving the first model with square-root transformation
# Use this if your data is "moderately" skewed (e.g., count data)
model.anova.sqrt <- aov(sqrt(value) ~ source, data = cf.anova.data)
plot(model.anova.sqrt)
cat("\n--- ANOVA: Sqrt Transform Summary ---\n")
summary(model.anova.sqrt)

# Improving the sqrt model with log transform
# Use this if your data has high variance or is heavily right-skewed 
# Note: We use log(value + 1) to handle any potential zeros in the data safely
model.anova.log <- aov(log(value + 1) ~ source, data = cf.anova.data)
plot(model.anova.log)
cat("\n--- ANOVA: Log Transform Summary ---\n")
summary(model.anova.log)
