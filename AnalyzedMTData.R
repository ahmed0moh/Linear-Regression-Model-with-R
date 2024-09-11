library(MASS)
library(ggplot2)
library(corrplot)

data(mtcars)

mtcars_matrix <- as.matrix(mtcars)
rownames(mtcars_matrix) <- rownames(mtcars)

# the questions of the assignment:
# Transpose of the matrix
mtcars_transpose <- t(mtcars_matrix)

# Product of the matrix and its transpose
product_matrix <- mtcars_matrix %*% mtcars_transpose

# Select 3 continuous variables and create a 3x3 matrix
selected_matrix <- mtcars_matrix[1:3, c("mpg", "disp", "hp")]

# Check if the matrix is invertible by finding its determinant
det_selected_matrix <- det(selected_matrix)
if(det_selected_matrix != 0) {
  # Compute its inverse
  inverse_matrix <- solve(selected_matrix)
} else {
  print("the matrix isn't invertible")
}

# Calculate the column means using matrix operations
column_means <- colMeans(mtcars_matrix)

# Identify the car with the highest horsepower (hp)
max_hp_car <- rownames(mtcars)[which.max(mtcars$hp)]

# Create a correlation matrix from the original matrix
correlation_matrix <- cor(mtcars)

# Find the variable that has the highest average correlation coefficient with all other variables
avg_correlation <- colMeans(correlation_matrix)
max_correlation_variable <- names(which.max(avg_correlation))

# Is an automatic or manual transmission better for MPG
# Quantify the MPG difference between automatic and manual transmissions
transmission_mpg <- aggregate(mpg ~ am, data = mtcars, FUN = mean)
mpg_difference <- diff(transmission_mpg$mpg)
t_test_result = t.test(mpg~am, data = mtcars)

# Try to looking at relationships between multiple variables
# Analyze at least one dependent variable and three independent variables
linear_model <- lm(mpg ~ wt + hp + qsec, data = mtcars)

# Summary of the linear model to check the relationships
summary(linear_model)

# Stepwise regression for model fitting
full_model <- lm(mpg ~ ., data = mtcars)
stepwise_model <- stepAIC(full_model, direction = "both")

# Summary of the stepwise regression model
summary(stepwise_model)

# Visualize the results of the stepwise regression
# Plotting MPG against the most significant variable from the stepwise model
significant_variable <- names(which.min(summary(stepwise_model)$coefficients[, "Pr(>|t|)"]))
significant_variable_sym <- rlang::sym(significant_variable)
ggplot(mtcars, aes(x = !!significant_variable_sym, y = "mpg")) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = paste("MPG vs", significant_variable), x = significant_variable, y = "Miles per Gallon") +
  theme_minimal()

# Visualize the correlation matrix using corrplot
corrplot(correlation_matrix, method = "circle")



