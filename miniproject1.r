# Dataset 3: Mtcars: Written by Mustaeen Ahmed
library(readxl)
cars <- read_excel("M:/Users/Mustaeen/Downloads/Cars.xlsx")
# read_excel is used to use dataset in visual studio code

dataset <- data.frame(cars)

######## PART A ########

# Shapiro test for normality
print("Normality test for manual cars:")
print(shapiro.test(dataset$mpg[dataset$am == 0]))
print("Normality test for automatic cars:")
print(shapiro.test(dataset$mpg[dataset$am == 1]))

# Variance test, fuel efficiency in manual/automatic cars
var_res <- var.test(mpg ~ as.factor(am), data = dataset)
print(var_res)

# Step 0:
# P-Values for both Shapiro tests are greater than 0.05, making
# both datasets normally distributed. It can reasonably
# be assumed that data was collected through a random sample.

# Step 1/2:
# H0: The population variance of automatic and manual cars are equal.
# HA: The population variance of automatic and manual cars are not equal.

# Step 3/4:
# After conducting a variance test, we can see that the ratio of variances
# is 0.3865615, with a RR of >0.1243721 and <1.0703429. As the ratio
# doesn't fall in this interval, we fail to reject the HO.

# Step 5:
# At α = 0.05, we fail to reject the HO (ratio doesn't fall in interval).
# We conclude that there is not enough evidence to reject the claim
# of variance between automatic and manual cars being equal.

######## PART B ########

# One-factor ANOVA, avg fuel efficiency based on num cylinders
cylinder_results <- aov(mpg ~ as.factor(cyl), data = dataset)

print("One-factor ANOVA, fuel efficiency and num of cylinders")
print(summary(cylinder_results))

######## PART C ########

######## PART D ########

# One-factor ANOVA, avg fuel efficiency based on engine shape
engine_results <- aov(mpg ~ vs, data = dataset)

print("One-factor ANOVA, fuel efficiency and engine shape")
print(summary(engine_results))

######## PART E ########

# Two-factor ANOVA, num cylinders, transmission, avg fuel efficiency
two_factor_results <- aov(mpg ~ as.factor(vs) + as.factor(am)
                          + (as.factor(vs) * as.factor(am)), data = dataset)

print("Two-factor ANOVA")
print(summary(two_factor_results))
