# Load the dataset
data(USArrests)

# Display the structure of the dataset
str(USArrests)

# Summary statistics for Murder arrests
murder_stats <- summary(USArrests$Murder)
murder_sd <- sd(USArrests$Murder)

print(murder_stats)
print(paste("Standard Deviation:", murder_sd))

# Histogram for Murder arrests
hist(USArrests$Murder, main="Histogram of Murder Arrests", xlab="Murder Arrests per 100,000", col="lightblue", border="black")

# Boxplot for Murder arrests
boxplot(USArrests$Murder, main="Boxplot of Murder Arrests", ylab="Murder Arrests per 100,000")

# Categorize Murder arrests into bins
USArrests$MurderCategory <- cut(USArrests$Murder, breaks=c(0, 3, 6, 9, 15), labels=c("Low", "Medium", "High", "Very High"))

# Bar plot for Murder categories
barplot(table(USArrests$MurderCategory), main="Bar Plot of Murder Categories", xlab="Category", ylab="Frequency", col="lightgreen")


# Pearson correlation between Murder and Assault
cor_murder_assault <- cor(USArrests$Murder, USArrests$Assault)
print(paste("Pearson Correlation between Murder and Assault:", cor_murder_assault))

# Scatter plot with trend line
plot(USArrests$Murder, USArrests$Assault, main="Scatter Plot of Murder vs Assault", xlab="Murder Arrests per 100,000", ylab="Assault Arrests per 100,000")
abline(lm(USArrests$Assault ~ USArrests$Murder), col="red")  # Add trend line

# Fit a linear regression model
model <- lm(Murder ~ Assault + Rape, data=USArrests)

# Display the summary of the model
summary(model)

# Plot residuals
par(mfrow=c(2,2))
plot(model)

# Perform PCA
pca <- prcomp(USArrests[, 1:4], scale=TRUE)

# Plot the explained variance
summary(pca)
par(mfrow=c(1,1))
plot(pca, main="PCA: Explained Variance")

# Biplot of the first two principal components
biplot(pca, main="PCA Biplot")



