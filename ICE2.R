getwd()
setwd("/Users/Codyxu/R")
##Indexing with numbers and names
v <- c(1,4,2,3,2,3)
print(v)
print(v[3])

print(v[c(2,3,4)])
print(v[2:4])
print(v[c(2,5,3)])
#create data frame
df <- read.table(header=T, text='
 subject sex size
       1   M    7
       2   F    6
       3   F    9
       4   M   11')
print(df)
print(df[1,3])
print(df[1:2, ])
# Get rows 1 and 2, and only column 2print(df[1:2, ])
print(df[1:2, 2])
#  Get rows 1 and 2, and only the columns named "sex" and "size"
df[1:2, c("sex","size")]
df$sex
# index with a boolean vector
# assigning the result vector to a variable called f
f <- df$size >= 9
f
# only select the rows where size is greater than 9
df[f, ]
# integrated way
df[df$size >= 9, ]

#Negative indexing
print(v)
# Drop the first element
print(v[-1])
# Keep dropping the first three
print(v[-1:-3])
#position of the last element 
print(v)
print(length(v))
# Index it in the numerical way
print(v[length(v)])
print(tail(v,1))
#indexing with tidyverse 
library(tidyverse)
icedata <- read_csv("ICE1_Data.csv")
icedata
new_ice <- select(icedata, 'DBN','Quality_Review_Score', `Progress_Rpt_10-11`)
new_ice
#adding a column with mutate 
collegeGraduation <- mutate(icedata, colllegeGraduationRate = `college enroll 2010-11` / `graduation 2010-11`)
collegeGraduation
# filtering
filter(icedata,`graduation 2010-11` > 0.8)
filter(icedata, `graduation 2010-11` > 0.8 & `Quality_Review_Score` == "Proficient")
#the pipe
new_ICE <- select(icedata, `Quality_Review_Score`, `Student_Progress_10-11`, `graduation 2010-11`)

filter(new_ICE, `graduation 2010-11` > 0.8)
icedata %>% 
  select(`Quality_Review_Score`, `Student_Progress_10-11`, `graduation 2010-11`) %>% 
  filter(`graduation 2010-11` > 0.8)


16 %>% sqrt()
16 %>% sqrt() %>% log2()
#log2(sqrt(16)).
icedata %>% select(`Quality_Review_Score`, `Student_Progress_10-11`, `graduation 2010-11`) %>% filter(`graduation 2010-11` > 0.8)
# combine datasets
v <- c(1,4,4,3,2,2,3)
v
c(v, 9)
#append() to put the new variable at the very end.
append(v, 9)
library(nycflights13)
data(flights)
data(weather)
flights <- flights %>% select(carrier, flight,
                              origin, dest, time_hour)
weather <- weather %>% select(temp, wind_speed, precip, origin, time_hour)
mergeCols <- c("origin", "time_hour")
left_dplyr  <- left_join(flights,  weather, by = mergeCols)
right_dplyr <- right_join(flights, weather, by = mergeCols)
inner_dplyr <- inner_join(flights, weather, by = mergeCols)
full_dplyr  <- full_join(flights,  weather, by = mergeCols)
#summarize
g <- icedata %>% 
  filter(`Quality_Review_Score` == "Proficient") %>% 
  summarise(average = mean(`graduation 2010-11`, na.rm = TRUE),
            standardDeviation = sd(`graduation 2010-11`, na.rm = TRUE))
g$average

#group_by
icedata %>% group_by(Quality_Review_Score)
icedata %>%
  group_by(Quality_Review_Score) %>% 
  summarize(GraduationAverage = mean(`graduation 2010-11`, na.rm = TRUE),
            CollegeEnrollAverage = mean(`college enroll 2010-11`, na.rm = TRUE))
icedata %>% 
  group_by(`Quality_Review_Score`, `Progress_Rpt_10-11`) %>%
  summarize(count = n())
