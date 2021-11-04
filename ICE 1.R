# loading pack
library("beepr")
beep(5)
beepr::beep(5)
?beepr
?beep()
(.packages())
# Understand data type and sturcture in R
X<-1:100
print(X)
typeof(X)
length(X)
# ordinal
CharacterData <- c("East","West","East","North","North","East","West","West","West","East","North")
typeof(CharacterData)
print(CharacterData)
facto_data <- factor(CharacterData)
typeof(facto_data)
print(facto_data)
#####Null based values: NULL, NA, NaN,Inf
V <- c(TRUE,TRUE,FALSE)
typeof(V)
print(V)
V <-c("1", 1, TRUE)
typeof(V)
print(V)
l <- list("1", 1, TRUE)
typeof(l)
print(l)
#Matrix and data frame
m <- matrix(nrow = 3, ncol = 3)
FOURS <- matrix(c(4,4,4,4),nrow = 2, ncol = 2)
FOURS
orderedM <- matrix( c(1, 2, 3, 4),   nrow = 2,   ncol = 2)
dim(FOURS)
class(FOURS)
typeof(FOURS)
# data frame
id <- letters[1:10]
score <- 1:10
enrolled <- logical(10)
df <- data.frame(id, score, enrolled)
df
# data frame funcation
data("mtcars")
head(mtcars)
data("iris")
iris
tail(mtcars)
ncol(5)
str(5)
library(tidyverse)
ICEdata <- read_csv("ICE1_Data.csv")
ICEdata
getwd()
summary(ICEdata)
ICEdata_factor <- ICEdata %>%
  mutate_if(sapply(ICEdata, is.character), as.factor)
summary(ICEdata_factor)
# limited number of variables
studentProgressGraduation <-
  ICEdata_factor %>%  
  select(`Progress_Rpt_10-11`, `graduation 2010-11`)
studentProgressGraduation
graduation201011 <- ICEdata$`graduation 2010-11`
head(graduation201011)
length(graduation201011)
#plot something
plot(studentProgressGraduation)
hist(graduation201011)
getwd()
