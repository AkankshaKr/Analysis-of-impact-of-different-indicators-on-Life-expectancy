# Analysis-of-impact-of-different-indicators-on-Life-expectancy
 We aim to assess the impact of different indicators on life expectancy,  finding insights into which factors play a more significant role. 
Aim-We aim to assess the  impact of different indicators on life expectancy, finding insights into which factors play a more significant role.
Data was taken from kaggle and it was from year 2000-2015 for 193 countries.
Missing data was handled using na.omit function in R.
I did panel regression at the dataset was both cross-sectional as well as time series
Through pooled ols, i haved removed the variables which had insignificant p value
Next i did vif test for multicollinearity, value less than 2 indicates a very low level of multicollinearity and the predictors are likely not highly correlated.
Then i tested for Fixed and Random model effect and the results were- For Fixed model effect 5 out of 6 independent variables in the fixed effect models are significant and we can see that p value is much smaller than 0.05 with adjusted R2 value of 0.36. And for Random model, the p values obtained for all independent variables are significant with overall p value of much smaller than 0.05 and adjusted R2 value of 0.6106.
Through Hausman test  I have compared between fixed effect and random effect model and here the p value obtained is more than 0.05 and thus we Fail to reject the null hypothesis , further conclude that there is not enough evidence to suggest that the random effects model is inconsistent and less efficient than the fixed effects model.
Through Breusch-Pagan test I tested for heteroscedasticity. The null hypothesis for this test assumes homoscedasticity in the data. But the result has p value less than 0.05 which makes us reject the null hypothesis and infers that heteroscedasticity exists in the dataset.
Through Pesaran CD test I tested for cross-sectional Dependence. The null hypothesis for the test is that there is no cross sectional dependence , Since the p-value obtained is less than 0.05 we reject the null hypothesis and conclude that there is cross sectional dependence.
Through Breusch-Godfrey/Wooldridge test I checked for serial correlation in panel models. The null hypothesis for the test is that there is no serial correlation, Since the p-value obtained is less than 0.05 we reject the null hypothesis and thus we conclude that there is serial correlation that exists.
The final result I obtained says as compared to other independent variables, the percentage expenditure, Adult Mortality, Diphtheria, BMI, HIV/AIDS, Schooling have significant effects on Life expectancy with Adult Mortality and HIV/AIDS having negative coefficients.
