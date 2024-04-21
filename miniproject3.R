library(readxl)

bank <- read_excel("./bank.xlsx")

dataset <- data.frame(bank)
print(dataset)
attach(dataset)

# Question 1:
plot(Income, Rating)