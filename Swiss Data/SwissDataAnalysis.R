# Load the swiss dataset
data(swiss)

# Display the structure of the dataset
str(swiss)

# Summary statistics for Fertility
fertility_stats <- summary(swiss$Fertility)
fertility_sd <- sd(swiss$Fertility)

print(fertility_stats)
print(paste("Standard Deviation:", fertility_sd))

# Histogram for Fertility
hist(swiss$Fertility, main="Histogram of Fertility", xlab="Fertility (Children per woman)", col="lightblue", border="black")

# Boxplot for Fertility
boxplot(swiss$Fertility, main="Boxplot of Fertility", ylab="Fertility (Children per woman)")

# Create a binary categorization of Catholic population (above or below 50%)
swiss$CatholicCategory <- ifelse(swiss$Catholic > 50, "Above 50%", "Below 50%")

# Bar plot for Catholic categories
barplot(table(swiss$CatholicCategory), main="Bar Plot of Catholic Population Categories", xlab="Category", ylab="Frequency", col="lightgreen")

# Pearson correlation between Fertility and Education
cor_fertility_education <- cor(swiss$Fertility, swiss$Education)
print(paste("Pearson Correlation between Fertility and Education:", cor_fertility_education))

# Scatter plot with trend line
plot(swiss$Fertility, swiss$Education, main="Scatter Plot of Fertility vs Education", xlab="Fertility (Children per woman)", ylab="Education Expenditure (%)")
abline(lm(Education ~ Fertility, data=swiss), col="red")  # Add trend line

# Fit a linear regression model
model_swiss <- lm(Fertility ~ Agriculture + Examination, data=swiss)

# Display the summary of the model
summary(model_swiss)

# Plot residuals
par(mfrow=c(2,2))
plot(model_swiss)

# Perform PCA on selected numerical variables
pca_swiss <- prcomp(swiss[, c("Fertility", "Agriculture", "Examination", "Education", "Catholic", "Infant.Mortality")], scale=TRUE)

# Plot the explained variance
summary(pca_swiss)
plot(pca_swiss, main="PCA: Explained Variance")

# Biplot of the first two principal components
biplot(pca_swiss, main="PCA Biplot")

'''
CONCLUSION:

Univariate Analysis: The Fertility variable has a fairly uniform distribution, with a few outliers. The bar plot for Catholic population categories reveals that most provinces have a Catholic population under 50%.
Multivariate Analysis: There is a weak negative correlation between Fertility and Education. The multiple regression model shows how Agriculture and Examination contribute to explaining fertility rates. Residual diagnostics suggest that some assumptions of linear regression are met.
Advanced PCA: PCA reveals the dimensional structure of the data, and the biplot helps us interpret how variables like Fertility and Infant.Mortality relate to the principal components.
'''
