library(tidyverse)
school <- read_csv('/Users/Codyxu/R/ICE1_Data.csv')

# two numerical variables
graduationCollege <- school %>% 
  select(`graduation 2010-11`, `college enroll 2010-11`)

plot(graduationCollege)
model <- lm(`college enroll 2010-11`~`graduation 2010-11`, data = graduationCollege)
summary(model)
plot(graduationCollege)

abline(a = coef(model)[1], b = coef(model)[2], col = "red")
videoData = read_csv('/Users/Codyxu/R/ICE3_data.csv')
videoData
summary(videoData)
# using plot to create matrix
videoDataRegression <- videoData %>% select(participation, watch.time, confusion.points, key.points)
plot(videoDataRegression)
library(psych)
pairs.panels(videoDataRegression, hist.col = 'blue', ellipses= FALSE)
videoModel <- lm(watch.time ~ participation + confusion.points + key.points, data = videoDataRegression)
summary(videoModel)        
summary(model)             
