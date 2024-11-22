# Load the dataset
data(mtcars)

# Display the structure of the dataset
str(mtcars)

# Summary statistics for mpg
mpg_stats <- summary(mtcars$mpg)
mpg_sd <- sd(mtcars$mpg)

print(mpg_stats)
print(paste("Standard Deviation:", mpg_sd))

# Histogram
hist(mtcars$mpg, main="Histogram of MPG", xlab="Miles per Gallon", col="lightblue", border="black")

# Boxplot
boxplot(mtcars$mpg, main="Boxplot of MPG", ylab="Miles per Gallon")

# Bar plot for cylinders
barplot(table(mtcars$cyl), main="Bar Plot of Cylinders", xlab="Number of Cylinders", ylab="Frequency", col="lightgreen")

# Pearson correlation between mpg and hp
cor_mpg_hp <- cor(mtcars$mpg, mtcars$hp)
print(paste("Pearson Correlation between mpg and hp:", cor_mpg_hp))

# Scatter plot with trend line
plot(mtcars$mpg, mtcars$hp, main="Scatter Plot of MPG vs HP", xlab="Miles per Gallon", ylab="Horsepower")
abline(lm(mtcars$hp ~ mtcars$mpg), col="red")  # Add trend line

# Fit a linear regression model
model <- lm(mpg ~ hp + wt, data=mtcars)

# Display the summary of the model
summary(model)

# Plot residuals
par(mfrow=c(2,2))
plot(model)

# Perform PCA on the numerical columns
pca <- prcomp(mtcars[, sapply(mtcars, is.numeric)], scale=TRUE)

# Plot the explained variance
par(mfrow=c(1,1))
summary(pca)
plot(pca, main="PCA: Explained Variance")

# Biplot of the first two principal components
biplot(pca, main="PCA Biplot", cex=0.5)