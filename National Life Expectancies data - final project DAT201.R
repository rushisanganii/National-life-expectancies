library(readxl)
UNLifeExpectancy_1_ <- read_excel("C:/Users/jasmi/Desktop/MCE DATA SCIENCE/DATA 201 DATA ANALYTICS AND MODELLING/final project/UNLifeExpectancy (1).xlsx")

url<- "C:/Users/jasmi/Desktop/MCE DATA SCIENCE/DATA 201 DATA ANALYTICS AND MODELLING/final project/UNLifeExpectancy (1).xlsx"
data <- read_excel(url)

head(data)
summary(data)


install.packages("ggplot2")
library(ggplot2)

###1

ggplot(data, aes(x = FERTILITY, y = LIFEEXP)) +
  geom_point() +
  labs(title = "Scatter Plot of LIFEEXP vs. FERTILITY",
       x = "Fertility",
       y = "Life Expectancy")

data$FERTILITY <- as.numeric(as.character(data$FERTILITY))
data$LIFEEXP <- as.numeric(as.character(data$LIFEEXP))
data <- na.omit(data)


cor(data$FERTILITY, data$LIFEEXP)

###2

model <- lm(LIFEEXP ~ FERTILITY, data = data)
summary(model)

###3

new_data <- data.frame(FERTILITY = 2.0)

predicted_life_expectancy <- predict(model, newdata = new_data)

print(predicted_life_expectancy)

###4

data$FERTILITY <- as.numeric(as.character(data$FERTILITY))
data$PUBLICEDUCATION <- as.numeric(as.character(data$PUBLICEDUCATION))
data$PRIVATEHEALTH <- as.numeric(as.character(data$PRIVATEHEALTH))

data <- na.omit(data)

data$lnHEALTH <- log(data$PRIVATEHEALTH)

model_multiple <- lm(LIFEEXP ~ FERTILITY + PUBLICEDUCATION + lnHEALTH, data = data)
summary(model_multiple)


###c
# Assuming 'model_multiple' is your multiple regression model
summary(model_multiple)

summary_output <- summary(model_multiple)


# Extract the p-value for PUBLICEDUCATION
p_value_public_education <- summary_output$coefficients["PUBLICEDUCATION", "Pr(>|t|)"]

significance_level <- 0.05

if (p_value_public_education < significance_level) {
  cat("Reject the null hypothesis. PUBLICEDUCATION is statistically significant.\n")
} else {
  cat("Fail to reject the null hypothesis. PUBLICEDUCATION is not statistically significant.\n")
}

 ###7

# Assuming 'model_multiple' is your multiple regression model

# View the summary of the model
summary_output <- summary(model_multiple)

# Extract relevant information
p_value_public_education <- summary_output$coefficients["PUBLICEDUCATION", "Pr(>|t|)"]
coeff_public_education <- summary_output$coefficients["PUBLICEDUCATION", "Estimate"]
r_squared <- summary_output$r.squared

# Set significance level
significance_level <- 0.05

# Make an overall comment
cat("Overall Comment for Non-Statistician Audience:\n")

# Comment on Public Education
cat("\n1. Public Education:\n")
if (p_value_public_education < significance_level) {
  cat("   The level of public education is statistically significant in predicting life expectancy.\n")
  cat("   For every one-unit increase in public education, we expect a", round(coeff_public_education, 3), "unit increase in life expectancy, on average.\n")
} else {
  cat("   The level of public education does not show a statistically significant impact on life expectancy in this analysis.\n")
  
}

# Comment on R-squared
cat("\n2. Model Fit:\n")
cat("   The model explains approximately", round(r_squared * 100, 2), "% of the variability in life expectancy based on the included variables.\n")




