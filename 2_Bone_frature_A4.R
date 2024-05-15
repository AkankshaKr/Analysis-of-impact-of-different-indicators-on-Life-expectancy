library(ggplot2)
library(readxl)
library(dplyr)
library(car)
library(pscl)
library(lmtest)
my_data <- read_excel("C:/Users/Acer/Downloads/Bone Fracture.xlsx")  #Upload the data
#filtered_data
my_data<-na.omit(my_data)
#Response variable/dependent variable - fracture
#Explanatory variable - age, sex, weight_kg, height_cm, bmd
#dummy variables for the categorical variables:
#In sex we coded female as 0 and male as 1(considered as reference)
#Similarly for fracture we coded  no fracture as 0 and fracture as 1(considered as reference)

my_data$Dummy_sex <- ifelse(my_data$sex > F,1,0)
my_data$Dummy_fracture <- ifelse(my_data$fracture == "no fracture",0,1)
View(my_data)

#Correlation between age and bone fracture
cor.test(my_data$age, my_data$Dummy_fracture )
#The correlation coefficient (cor) is 0.3230301.There is positive correlation between age and bone fracture
#It implies that as age increases,  the risk of bone fracture increases.The strength of the correlation is moderate, which is not extremely close to 0.
#The t-statistic is 4.4109 resulting in a very small p-value of 1.839e-05.The small p-value suggests that the correlation is statistically significant, which means model is relatively good fit.
#the positive correlation implies that on average, as age increases, the likelihood of bone fracture (coded as 1 for males) tends to increase.
#Boxplot of age versus fracture

ggplot(data = my_data, aes(x = Dummy_fracture, y = age, group = Dummy_fracture)) +
  geom_boxplot() +
  labs(
    x = "Dummy_fracture",
    y = "Age",
    title = "Distribution of Age across Patients with and without Fractures"
  )
#In the box plot tracking ages of patients and their fracture and no fracture status, we see that patients with a fracture seem to have a slightly higher median age than patients without a fracture.

#Plot  age versus fracture
plot(my_data$'age',my_data$'Dummy_fracture' , main = "Scatter Plot of age vs Dummy_fracture", xlab = "age", ylab = "Dummy_fracture")
fit <- lm(Dummy_fracture ~ age, data = my_data)
abline(fit, col = "red")
#no change of dependent variable as the independent variable changes (like a horizontal straight line, which has zero slope).

#Boxplot of between BMD and bone fracture
ggplot(data = my_data, aes(x = Dummy_fracture, y = bmd, group = Dummy_fracture)) +
  geom_boxplot() +
  labs(
    x = "Dummy_fracture",
    y = "bmd",
    title = "Distribution of bmd across Patients with and without Fractures"
  )
#In the box plot tracking bone mass density of patients and their fracture status, we see that patients with a fracture seem to have a much lower bone mass density than patients without a fracture. This corresponds to the findings of the correlation test done earlier.

#Correlation between BMD and bone fracture
cor.test(my_data$bmd, my_data$Dummy_fracture )
#As value we get is -0.6238454, which show a positive relationship between BMD and bone fracture
#The negative correlation implies that as BMD increases, the presence of bone fracture also tends to decrease.

#Plot  bmd versus fracture
plot(my_data$'bmd',my_data$'Dummy_fracture' , main = "Scatter Plot of bmd vs Dummy_fracture", xlab = "bmd", ylab = "Dummy_fracture")
fit <- lm(Dummy_fracture ~ bmd, data = my_data)
abline(fit, col = "blue")
#no change of dependent variable as the independent variable changes (like a horizontal straight line, which has zero slope).


### logistic regression model that predicts the probability of fracture

#ggplot(my_data, aes(x=bmd,
#                   y=Dummy_fracture))+
#  geom_jitter(height=0.05,
#             alpha=.1)

model <- glm(Dummy_fracture~bmd+age+Dummy_sex+weight_kg+height_cm,
             data=my_data ,
             family="binomial")
summary(model)
# The intercept is -0.19132. This is the log-odds of the event when bmd is 0.
#bmd: The coefficient for bmd is -16.24991. This represents the change in the log-odds of the event for a one-unit increase in bmd.
#bmd have very small p-values.bmd is only statistically significant
#the algorithm went through 6 iterations to  estimate the coefficients

#Checking for multicollinearity

vif(model)
#The variance inflation factor observed is less than 10 for the explanatory variables.

#for Pseudo R Square
pR2(model)
#Higher values of McFadden's pseudo R-squared indicate a better fit, with 0.2 often considered as a reasonable fit and values above 0.4 suggesting a good fit.As we got value as 0.4672401, since greater than 0.4 hence a good fit


ggplot(my_data,aes(x=bmd,
                   y=Dummy_fracture))+
  geom_point()+
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se=FALSE)

# Fit two models
model1 <- glm(Dummy_fracture ~ bmd, data = my_data, family = "binomial")
model2 <- glm(Dummy_fracture ~ bmd + age + Dummy_sex + weight_kg + height_cm, data = my_data, family = "binomial")

# Calculate AIC values
aic_model1 <- AIC(model1)
aic_model2 <- AIC(model2)

# Print AIC values
cat("AIC for Model 1:", aic_model1, "\n")
cat("AIC for Model 2:", aic_model2, "\n")

# Compare AIC values
if (aic_model1 < aic_model2) {
  cat("Model 1 is preferred.\n")
} else {
  cat("Model 2 is preferred.\n")
}

### Predict the probability of bone fracture  of a Female of 60 years of age, with a weight of 70 Kg, and height of 160 cms, and a BMD of 0.8 
my_data_female <- data.frame(age = 60, weight_kg = 70, height_cm = 160, bmd = 0.8, Dummy_sex = 0)
predict_prob_female <- predict(model, newdata = my_data_female, type = "response")
print(predict_prob_female)

### Predict the probability of bone fracture  of a Male, keeping all the other values same as in the previous case ###
my_data_male <- data.frame(age = 60, weight_kg = 70, height_cm = 160, bmd = 0.8, Dummy_sex = 1)
predict_prob_male <- predict(model, newdata = my_data_male, type = "response")
print(predict_prob_male)

cat("Female Probability:", predict_prob_female, "\n")
cat("Male Probability:", predict_prob_male, "\n")

#As we can see male probability is higher hence we can conclude that  A higher probability indicates a higher estimated risk. Therefore, according to model, males with the specified characteristics are predicted to have a higher risk of bone fracture compared to females with the same characteristics.


