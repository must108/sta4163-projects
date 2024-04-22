library(readxl)

bank <- read_excel("./bank.xlsx")

dataset <- data.frame(bank)
# print(dataset) # nolint
attach(dataset)

# Question 1:
plot(Income, Rating, xlab = "Income ($10,000s)",
     ylab = "Credit Score (in points)",
     main = "Income v.s. Credit Score Scatterplot")
# this answers part b

bank_model <- lm(Rating ~ Income, data = dataset)
print(summary(bank_model))
print(anova(bank_model))
# parts c, e, j

print(confint(bank_model, level = 0.95))
# for part h

print(cor(Income, Rating))
# for part i

plot(bank_model)
print(shapiro.test(bank_model$residuals))
# for part k

new_val <- data.frame(Income = 56000)
pred_val <- predict(bank_model, newdata = new_val)
conf_pred_val <- predict(bank_model, newdata = new_val, interval = "confidence",
                         level = 0.95)
pred_pred_val <- predict(bank_model, newdata = new_val, interval = "prediction",
                         level = 0.95)

print(pred_val)
print(conf_pred_val)
print(pred_pred_val)
# for part n

multi_model <- lm(Rating ~ Income + Age + Education, data = dataset)
print(summary(multi_model))
print(anova(multi_model))
# for question 2