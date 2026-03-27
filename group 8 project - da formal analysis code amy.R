data <- read.csv("DAProject8.csv")

head(data)
str(data)
summary(data)

data$AgeGroup <- factor(data$AgeGroup)
data$Sex <- factor(data$Sex)
data$Employment <- factor(data$Employment)
data$Veg <- factor(data$Veg)
data$Fruit <- factor(data$Fruit)
data$Year <- factor(data$Year)

library(ggplot2)

# bmi by sex
tapply(data$BMI, data$Year, sd)
# smallest is 5.164536 and largest is 5.361751, not double so equal variance

par(mfrow = c(1,2))
qqnorm(data$BMI[data$Sex == "Male"]); qqline(data$BMI[data$Sex == "Male"])
qqnorm(data$BMI[data$Sex == "Female"]); qqline(data$BMI[data$Sex == "Female"])
par(mfrow = c(1,1))

ttest_sex <- t.test(BMI ~ Sex, data = data, var.equal = TRUE)
ttest_sex
# no sigificant difference in bmi between males and females, p-value > 0.05, CI includes 0

# bmi by veg
var.test(BMI ~ Veg, data = data)
# p value is greater than 0.05 so variances assumed equal 

par(mfrow = c(1, 2))
qqnorm(data$BMI[data$Veg == "Yes"]); qqline(data$BMI[data$Veg == "Yes"])
qqnorm(data$BMI[data$Veg == "No"]); qqline(data$BMI[data$Veg == "No"])
par(mfrow = c(1, 1))

ttest_veg <- t.test(BMI ~ Veg, data = data, var.equal = FALSE)
ttest_veg

# BMI differs significantly between people who do and don't meet the veg intake 
# p-value <0.05 so statistically significant, CI is (0.07,0.4)

# BMI by fruit 

var.test(BMI ~ Fruit, data = data)
# p-value < 0.05 so variances not assumed equal 

par(mfrow = c(1, 2))
qqnorm(data$BMI[data$Fruit == "Yes"]); qqline(data$BMI[data$Fruit == "Yes"])
qqnorm(data$BMI[data$Fruit == "No"]); qqline(data$BMI[data$Fruit == "No"])
par(mfrow = c(1, 1))

ttest_fruit <- t.test(BMI ~ Fruit, data = data, var.equal = FALSE)
ttest_fruit
# BMI differs significantly between people who do and don't meet fruit intake 
# p-value < 0.05 so statistically significant, CI is (-0.41, -0.12)


# multiple linear regression

levels(data$Employment)
data$Employment <- relevel(data$Employment, ref = "In paid employment, self-employed or on gov't training")

full_model <- lm(BMI ~ Year + Sex + AgeGroup + Employment + Veg + Fruit, data = data)
summary(full_model)
confint(full_model)

par(mfrow = c(1, 2))
plot(full_model, which = 1)  
plot(full_model, which = 2)  
par(mfrow = c(1, 1))

model_reduced <- lm(BMI ~ AgeGroup + Employment + Veg, data = data)
summary(model_reduced)
confint(model_reduced)

par(mfrow = c(1, 2))
plot(model_reduced, which = 1)  
plot(model_reduced, which = 2)  
par(mfrow = c(1, 1))





levels(data$Employment)
data$Employment <- relevel(data$Employment, ref = "In paid employment, self-employed or on gov't training")



tapply(data$BMI, data$Sex, mean)
tapply(data$BMI, data$Sex, sd)


# Year not significant p-value 0.376756 > 0.05 





