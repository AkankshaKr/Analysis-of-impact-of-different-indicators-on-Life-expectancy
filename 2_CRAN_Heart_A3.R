# Libraries
library(ggplot2)

library(readxl)
my_data <- read_excel("C:/Users/Acer/Downloads/CRAN_Heart.xlsx")  #Upload the data

options(scipen=100)
filtered_data<-na.omit(my_data)
#We are applying model fit by taking MaxHR AS RESPONSE VARIABLE and we will check with which explanatory numerical variable it gives significant p value.
model_hr <- lm(MaxHR~RestBP+Chol+Age+Oldpeak, data = filtered_data) 
summary(model_hr) #Review the results

#Chol is not significant
model_hr <- lm(MaxHR~RestBP+Age+Oldpeak, data = filtered_data)  
summary(model_hr) #Review the results
#RestBP has the highest p-value in the model,so we remove it


#Response variable/dependent variable - MaxHR
#Explanatory variable - Age, Oldpeak
#Here MaxHR depicts Maximum Heart rate and Oldpeak is ST depression induced by exercise relative to rest
#MaxHR is taken because it is an indicator of cardiovascular health and exercise capacity.As we age, our bodies change. One of the changes that occurs is a decrease in our maximum heart rate.Maximal heart rate (HRmax)-prediction equations based on a person's age are frequently used in prescribing exercise intensity and other clinical applications.
#The relation between MaxHR and Oldpeak is that during exercise heart rate increases which induces ST-segment depression. Exercise-induced ST-segment depression is a reliable electrocardiographic (ECG) finding for diagnosis of CAD
#We choose MaxHR, Age and Oldpeak because they are numeric variables, there are other numeric variables as well but we took this because after applying model fit they gave us significant p values.


##Null Hypothesis:there is no significant relationship between the maximum heart rate (max HR rate) and the combination of Age and Oldpeak, the null hypothesis can be written as 
#H0: β1 and β2 =0
#Here β1 and β2  are the coefficients associated with the Age and Oldpeak variables, respectively, in the regression model. This null hypothesis states that the coefficients for both slope and ExAng are equal to zero, indicating no effect or relationship between these variables and the maximum heart rate.


#Alternate Hypothesis: The alternative hypothesis :At least one of these coefficients is not equal to zero, indicating a significant linear relationship between the maximum heart rate (max HR rate) and the combination of Age and Oldpeak:
#H1: There is a significant linear relationship between the maximum heart rate and the combination of Age and Oldpeak.
#In terms of regression coefficients, this can be expressed as: H1: βAge is not equal to Zero or βOldpeak not equal to zero
#βAge and βOldpeak represent the regression coefficients for the Age and Oldpeak variables, respectively.


model_hr2 <- lm(MaxHR~Age+Oldpeak, data = filtered_data) 
summary(model_hr2)


## A negative coefficient (-0.8552 for age and -5.4025 for old peak) suggests that as the independent variable increases, the dependent variable tends to decrease.The negative sign of the coefficient for the age and old peak variable in the regression model indicates the direction of the relationship

##The coefficient value signifies how much the mean of the dependent variable changes given a one-unit shift in the independent variable while holding other variables in the model constant.
##The value for the intercept term in this model is 201.7801. This would mean the MaxHR of a person is 201.7801 when the Age and the Oldpeak both are equal to zero.
##However, we still need to keep the intercept term in the model in order to use it to make predictions. The intercept just doesn’t have any meaningful interpretation for this model.
#This does not make sense to interpret since it’s not possible for MaxHR to have zero Age and zero Oldpeak.
#both the explanatory variable gave the significant p value that is less than 0.05 so both of them are significant.

#The low p-value (< 0.05) indicates that the model is statistically significant, the F-statistic is also significant as by the rule of thumb F value > 4.0 is usually statistically significant, indicating that the model as a whole is statistically significant and therefore we can reject the null hypothesis.
##The F-statistic is a measure of overall significance of the model. In this case:



summary(model_hr2)$fstatistic



#F-statistic: 44.09 , Degrees of freedom for the F-statistic: 2 and 300, p-value: < 2.2e-16
# the F-statistic is also significant as by the rule of thumb F value > 4.0 is usually statistically significant, indicating that the model as a whole is statistically significant and therefore we can reject the null hypothesis.





ggplot(data = filtered_data, aes(x = Age, y = MaxHR))+
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)
#the regression line of the data has a negative slope, the data has a negative linear relationship.
ggplot(data = filtered_data, aes(x = Oldpeak, y = MaxHR))+
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

#the regression line of the data has a negative slope, the data has a negative linear relationship.
#This would suggest that individuals with higher maximum heart rates tend to have lower values of Oldpeak or Age, depending on which is on the y-axis.



lm(formula = MaxHR ~ Age, data = filtered_data)
#calculate 95% confidence interval for each coefficient in model
ci <-confint(model_hr2, level=0.95)
print(ci)
#To determine whether to accept or reject the null hypothesis based on confidence intervals,we look whether the interval includes the null hypothesis value that is considered zero in our case.
##Null Hypothesis: The intercept is equal to  zero.
# Confidence Interval: (187.809624, 215.7505984)
# Decision: Since this confidence interval doesn't include zero, we would likely reject the null hypothesis that the intercept is zero.
#Null Hypothesis: The Oldpeak coefficient is equal to  zero.
#  Confidence Interval: (-1.113406, -0.5970026)
# Decision: Similar to the intercept, since this confidence interval doesn't include zero, we would likely reject the null hypothesis that the Oldpeak coefficient is zero.
##Null Hypothesis: The Age coefficient is equal to  zero.
#  Confidence Interval: (-7.412509, -3.3924479)
# Decision: Similar to the Oldpeak, since this confidence interval doesn't include zero, we would likely reject the null hypothesis that the Age coefficient is zero.
# Yes the conclusions are similar to hypothesis testing as there also we rejected the null hypothesis and hee also we came to the same conclusion.


print(model_hr2$coefficients)
f2<- summary(model_hr2)$fstatistic
f_value <- anova(model_hr2)$'Pr(>F)' [1]
cat("F-value:", f_value, "\n")
#if F value is less than 0.05 we can reject the null hypothesis and the f value coming is 0.000000000000135.


#view only regression coefficients of model
#view regression coefficients with standard errors, t-statistics, and p-values
summary(model_hr2)$coefficients
#The regression model, incorporating Age and Oldpeak as explanatory variables for predicting Max HR, yields an R^2 value of 0.2272. This model therefore has relatively low R^2 value. This implies that approximately 22.72% of the variability observed in Max HR can be explained by the linear relationship with Age and Oldpeak. 
#Value of R^2 < 0.3 is weak, Value between 0.3 and 0.5 is moderate and value greater than 0.7 means strong effect on dependent variable.




#Yes both the explanatory variables are significant 

# Make predictions


# Plotting
plot(x = model_hr2$fitted.values, y = filtered_data$MaxHR,
     xlab = 'Predicted Values',
     ylab = 'Actual Values',
     main = 'Predicted vs. Actual Values',
     col = c('blue', 'red'),  # Blue for predicted, red for actual
     pch = 16  # Use solid circles as points
)

#add diagonal line for estimated regression line
abline(a=0, b=1)  


#It is showing scatter plot of Predicted vs actual values 
#The x-axis shows the model’s predicted values, while the y-axis shows the dataset’s actual values. The estimated regression line is the diagonal line in the center of the plot.
#some data points are  quite close to the projected regression line, but most can be seen scattered as well and more scattered to below the line, the outliers are also visible that indicates data points that disproportionately affect the model
#The lower the R Square, the weaker the Goodness of fit of your model, the more foggy or dispersed the points are that is away from this diagonal line which can be seen in the graph.



#get list of residuals 
res <- resid(model_hr2)
#produce residual vs. fitted plot
plot(fitted(model_hr2), res)

#add a horizontal line at 0 
abline(0,0)

#This plot of residual vs fittedmodel shows that the data points are not evenly distributed around zero that indicates that the variability of the residuals is not consistent across all levels of the independent variable and the outliers are quite visible which can have a significant impact on the assumptions of linear regression and may indicate influential data points that disproportionately affect the model.
# Therefore it could be said that the too many errors can be seen

anova <- aov(model_hr2)
summary(anova)
#the p value comes to be less than 0.5  which suggests that the null hypothesis can be rejected.

