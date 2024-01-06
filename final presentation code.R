install.packages("plm")
library(forecast)
library(ggplot2)
library(readxl)
library(dplyr)
library(car)
library(lmtest)
library(plm)
my_data <- read_excel("C:/Users/Acer/Downloads/Qdapp dataset (1).xlsx")  #Upload the data
#filtered_data
my_data<-na.omit(my_data)
sum(is.na(my_data))







# Build the model
colnames(my_data)[colnames(my_data)=="Life expectancy"] <- "Life_expectancy"
colnames(my_data)[colnames(my_data)=="Hepatitis B"] <- "Hepatitis_B"
colnames(my_data)[colnames(my_data)=="HIV/AIDS"] <- "HIVAIDS"
colnames(my_data)[colnames(my_data)=="Adult Mortality"] <- "Adult_Mortality"
colnames(my_data)[colnames(my_data)=="percentage expenditure"] <- "percentage_expenditure"
colnames(my_data)[colnames(my_data)=="Total expenditure"] <- "Total_expenditure"
colnames(my_data)[colnames(my_data)=="under-five deaths"] <- "under-five_deaths"

arima_model <- auto.arima(my_data$Life_expectancy)
summary(arima_model)





fixed <- plm(Life_expectancy ~ percentage_expenditure+Adult_Mortality+Diphtheria+BMI+HIVAIDS+Schooling, data=my_data, index=c("Country", "Year"), model="within")  #fixed model
summary(fixed)

random <- plm(Life_expectancy~percentage_expenditure+Adult_Mortality+Diphtheria+BMI+HIVAIDS+Schooling ,data=my_data, index=c("Country", "Year"), model="random")  #random model
summary(random)
phtest(fixed,random) #Hausman test

library(plm)
# Assuming you have loaded the necessary libraries and defined your my_data dataframe

# Load required libraries
library(plm)
library(lmtest)

# Define your random effects model
random <- plm(Life_expectancy ~ percentage_expenditure + Adult_Mortality + Diphtheria + BMI + HIVAIDS + Schooling,
              data = my_data, index = c("Country", "Year"), model = "random")

# Summary of the random effects model
summary(random)

# Breusch-Pagan test for heteroscedasticity
bp_test_random <- bptest(random)
print(bp_test_random)

#Pesaran CD test for correlation 
fe_data <- pdata.frame(my_data, index = c("Year"))
pool<-plm(Life_expectancy ~  percentage_expenditure+Adult_Mortality+Diphtheria+BMI+HIVAIDS+Schooling,data= fe_data, model ="random")

pcdtest(pool, test = c("cd"))

#Breusch-Godfrey/Wooldridge test for serial correlation in panel models
pbgtest(random)

# Re-run the random effects model with robust standard errors
random_robust <- plm(Life_expectancy ~ percentage_expenditure + Adult_Mortality + Diphtheria + BMI + HIVAIDS + Schooling,
                     data = my_data, index = c("Year"), model = "random", effect = "individual", random.method = "swar")

# Summary of the random effects model with robust standard errors
summary(random_robust)
# Re-run the random effects model with robust standard errors
random_robust <- plm(Life_expectancy ~ percentage_expenditure + Adult_Mortality + Diphtheria + BMI + HIVAIDS + Schooling,
                     data = my_data, index = c("Country"), model = "random", effect = "individual", random.method = "swar")

# Summary of the random effects model with robust standard errors
summary(random_robust)