# Load the airquality dataset
data(airquality)

# Display the structure of the dataset
str(airquality)

# Summary statistics for Ozone concentration
ozone_stats <- summary(airquality$Ozone)
ozone_sd <- sd(airquality$Ozone, na.rm=TRUE)

par(mfrow=c(1,1))
print(ozone_stats)
print(paste("Standard Deviation:", ozone_sd))

# Histogram for Ozone concentration
hist(airquality$Ozone, main="Histogram of Ozone Concentration", xlab="Ozone Concentration (ppb)", col="lightblue", border="black")

# Boxplot for Ozone concentration
boxplot(airquality$Ozone, main="Boxplot of Ozone Concentration", ylab="Ozone Concentration (ppb)")

# Boxplot for Ozone by Month
boxplot(Ozone ~ Month, data=airquality, main="Boxplot of Ozone Concentration by Month", xlab="Month", ylab="Ozone Concentration (ppb)")

# Pearson correlation between Ozone and Wind
cor_ozone_wind <- cor(airquality$Ozone, airquality$Wind, use="complete.obs")
print(paste("Pearson Correlation between Ozone and Wind:", cor_ozone_wind))

# Scatter plot with trend line
plot(airquality$Ozone, airquality$Wind, main="Scatter Plot of Ozone vs Wind", xlab="Ozone Concentration (ppb)", ylab="Wind Speed (mph)")
abline(lm(Wind ~ Ozone, data=airquality), col="red")  # Add trend line

# Fit a linear regression model
par(mfrow=c(2,2))
model_airquality <- lm(Ozone ~ Solar.R + Wind, data=airquality)

# Display the summary of the model
summary(model_airquality)

# Plot residuals
plot(model_airquality)


# Perform PCA
pca_airquality <- prcomp(na.omit(airquality[, c("Ozone", "Solar.R", "Wind", "Temp")]), scale = TRUE)


# Plot the explained variance
par(mfrow=c(1,1))
summary(pca_airquality)
plot(pca_airquality, main="PCA: Explained Variance")

# Biplot of the first two principal components
biplot(pca_airquality, main="PCA Biplot")
