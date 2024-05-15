library(readxl)
library(dplyr)
library(ggplot2)

my_data <- read_excel("C:/Users/Acer/Downloads/CRAN_Heart.xlsx")

# TODO: Subsetting the data: one categorical and one numerical
filtered_data <-my_data %>%
  dplyr::select(ChestPain, RestBP) 
  
# Summary
summary(filtered_data)
filtered_data %>% group_by(ChestPain) %>%
  summarise(mean(RestBP),median(RestBP))

# Calling quantile() Function 
quantile(my_data$RestBP) 

# Histogram
ggplot(filtered_data, aes(x=RestBP)) + geom_histogram()
ggplot(filtered_data, aes(x=RestBP, fill="red ")) + geom_histogram(binwidth= 5.0)
 

#Box and Whisker
library("ggplot2")
View(my_data)
# Basic box plot
p <- ggplot(my_data , aes(x=ChestPain, y=RestBP)) + 
  geom_boxplot()
p

geom_boxplot(outlier.colour="black", outlier.shape=16,
             outlier.size=2, notch=FALSE)

# Change box plot colors by groups
p<-ggplot(my_data , aes(x=ChestPain, y=RestBP, fill=ChestPain)) +
  geom_boxplot()
p


#Add median line to boxplot

ggp +
  stat_summary(fun = median,
               geom = "line",
               aes(group = 1),
               col="red")

median = median(my_data$RestBP)
print(median)