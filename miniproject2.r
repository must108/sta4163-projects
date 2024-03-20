# Mini-Project 2: Written by Mustaeen Ahmed

library(readxl)
cars <- read_excel("./Cars.xlsx")
# read excel is used to import the dataset to VSCode

dataset <- data.frame(cars)
attach(dataset)

# Question 1
print("Question 1:")

# ANOVA that compares mpg based on number of cylinders
anova_results <- aov(mpg ~ factor(cyl), data = dataset)
print(summary(anova_results))

# Validate normal assumption
normal_results <- shapiro.test(mpg)
print(normal_results)

# Non-Parametric test
np_results <- kruskal.test(mpg ~ factor(cyl), data = dataset)
print(np_results)

# Question 2
print("Question 2:")

counts <- c(17, 16, 67)
exp_probs <- c(1 / 3, 1 / 3, 1 / 3)
chisq_results <- chisq.test(counts, p = exp_probs)

print(chisq_results)
print(chisq_results$expected)

# Question 3
print("Question 3")

vaccine <- matrix(c(77, 19634, 833, 18908), byrow = TRUE, nrow = 2)

dimnames(vaccine) <- list(c("Vaccine", "Placebo"),
                          c("Infection", "No Infection"))

print(vaccine)

ind_results <- chisq.test(vaccine)
print(ind_results)
print(ind_results$expected)
