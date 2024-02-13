# Dataset 3: Mtcars: Written by Mustaeen Ahmed

library(readxl)
library(car)
cars <- read_excel("FILEPATH/Cars.xlsx")
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
# that the variance of fuel efficiency between automatic and manual cars
# is equal.

######## PART B ########

# One-factor ANOVA, avg fuel efficiency based on num cylinders
cylinder_results <- aov(mpg ~ as.factor(cyl), data = dataset)

print("One-factor ANOVA, fuel efficiency and num of cylinders")
print(summary(cylinder_results))

######## PART C ########

# Based on the P-value given above (4.98e-09) we can reject the
# null hypothesis that mean fuel efficiency is equal for different numbers of
# cylinders, as 4.98e-09 < 0.05. This means that there is
# a significant relationship between num of cyl and mpg, and
# so, we can proceed with a test for multiple comparisons of
# means.

print(TukeyHSD(cylinder_results))

# After conducting the Tukey test, we can see that the differences
# are all negative, indicating that, in the case of "8-4", that
# 8 cylinders has a significantly lower mean fuel efficiency
# compared to 4 cylinders. Based on the data, we can see that
# 4 cylinders has the highest mean fuel efficiency compared to the
# others, and 8 cylinders has the lowest mean fuel efficiency.

######## PART D ########

# One-factor ANOVA, avg fuel efficiency based on engine shape
engine_results <- aov(mpg ~ as.factor(vs), data = dataset)

print("One-factor ANOVA, fuel efficiency and engine shape")
print(summary(engine_results))

# Based on the P-value given above (3.42e-05), we can reject the null
# hypothesis that mean fuel efficiency is equal for cars with different
# engine shapes, as 3.42e-05 < 0.05. That means there is a significant
# relationship between these two variables, and so we can conduct a test
# for multiple comparisons of means.

print(TukeyHSD(engine_results))

# After conducting the Tukey test, we can see that the difference is positive,
# meaning that V-shaped engines (0) have a significantly higher mean
# fuel efficiency compared to straight engines (1).

######## PART E ########

# Two-factor ANOVA, num cylinders, transmission, avg fuel efficiency
two_factor_results <- aov(mpg ~ as.factor(cyl) + as.factor(am)
                          + (as.factor(cyl) * as.factor(am)), data = dataset)

# Shapiro test for normality
print(shapiro.test(residuals(two_factor_results)))

# I know that you can perform a Levene's test to check for
# equality of variances, however, it created several errors
# in both RStudio and VS Code, and so I did not include it here.

print("Two factor ANOVA")
print(summary(two_factor_results))

# Step 0:
# The P-value for the shapiro test is greater that 0.05, showing us
# that the data is normally distributed. It can be reasonably assumed
# that the samples are random and independent, and that the response
# variance is constant for all treatments.

# Step 1/2:
# HO: The fuel efficiency between num of cylinders and transmission do
# not interact.
# HA: The fuel efficiency between Num of cylinders and transmission do interact.

# Step 3/4:
f_statistic <- ((824.8 + 36.8 + 25.4) / 5) / 9.2
res_pf <- pf(f_statistic, 5, 26, lower.tail = FALSE)
print(res_pf)

# After conducting the pf test, we can see that the resulting value
# is 5.208993e-08, which is much smaller than 0.05. Due to this,
# we reject the null hypothesis. Post-Hoc analysis will not be
# conducted as per the instructions.

# Conclusion:
# At α = 0.05, we reject HO and conclude that there not enough
# evidence to reject the claim that the number of cylinders and
# transmission type interact in how they affect fuel efficiency.





######## OUTPUT SHOWN BELOW ########

# [1] "Normality test for manual cars:"                                         

#         Shapiro-Wilk normality test

# data:  dataset$mpg[dataset$am == 0]
# W = 0.97677, p-value = 0.8987

# [1] "Normality test for automatic cars:"

#         Shapiro-Wilk normality test

# data:  dataset$mpg[dataset$am == 1]
# W = 0.9458, p-value = 0.5363


#         F test to compare two variances

# data:  mpg by as.factor(am)
# F = 0.38656, num df = 18, denom df = 12, p-value = 0.06691
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#  0.1243721 1.0703429
# sample estimates:
# ratio of variances 
#          0.3865615 

# [1] "One-factor ANOVA, fuel efficiency and num of cylinders"
#                Df Sum Sq Mean Sq F value   Pr(>F)    
# as.factor(cyl)  2  824.8   412.4    39.7 4.98e-09 ***
# Residuals      29  301.3    10.4                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#   Tukey multiple comparisons of means
#     95% family-wise confidence level

# Fit: aov(formula = mpg ~ as.factor(cyl), data = dataset)

# $`as.factor(cyl)`
#           diff        lwr        upr     p adj
# 6-4  -6.920779 -10.769350 -3.0722086 0.0003424
# 8-4 -11.563636 -14.770779 -8.3564942 0.0000000
# 8-6  -4.642857  -8.327583 -0.9581313 0.0112287

# [1] "One-factor ANOVA, fuel efficiency and engine shape"
#               Df Sum Sq Mean Sq F value   Pr(>F)    
# as.factor(vs)  1  496.5   496.5   23.66 3.42e-05 ***
# Residuals     30  629.5    21.0                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#   Tukey multiple comparisons of means
#     95% family-wise confidence level

# Fit: aov(formula = mpg ~ as.factor(vs), data = dataset)

# $`as.factor(vs)`
#         diff      lwr      upr    p adj
# 1-0 7.940476 4.606732 11.27422 3.42e-05


#         Shapiro-Wilk normality test

# data:  residuals(two_factor_results)
# W = 0.96277, p-value = 0.3263

# [1] "Two factor ANOVA"
#                              Df Sum Sq Mean Sq F value   Pr(>F)    
# as.factor(cyl)                2  824.8   412.4  44.852 3.73e-09 ***
# as.factor(am)                 1   36.8    36.8   3.999   0.0561 .  
# as.factor(cyl):as.factor(am)  2   25.4    12.7   1.383   0.2686    
# Residuals                    26  239.1     9.2                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# [1] 5.208993e-08
