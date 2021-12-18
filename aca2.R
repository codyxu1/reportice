library(tidyverse)
library(ggplot2)
library(reshape2)
library(party)
# import data set
aca2_train <- read.csv('/Users/codyxu/R/aca2_dataset_training.csv')
head(aca2_train)
# import validation data 
aca2_validation <- read.csv('/Users/codyxu/R/aca2_dataset_validation.csv')
head(aca2_validation)
##The names of variables in the data
names(aca2_train)#There are 18 different variables in the data

#The structure of the data
str(aca2_train)#This is a mixed data with integer variables and character variables.
#The variable of interest, 'ONTASK' is a character variable with two responses N for on task, and Y for off-task.

#Checking for the missing values/observations
sum(is.na(aca2_train))#The sum of missing values is 0, implying that the data has no missing values.

#Summary statistics for the numerical/continuous variables
num_vars<-data.frame(aca2_train$OBSNUM,aca2_train$totalobs.forsession,aca2_train$TRANSITIONS,
                     aca2_train$NumACTIVITIES,aca2_train$NumFORMATS,
                     aca2_train$Obsv.act,aca2_train$Transitions.Durations)
names(num_vars)=c('Osnum','totalobs','Transitions','NumActivities',
                  'NumFormats','Obs','Duration')
summary(num_vars)

#Correlations for the numerical/continuous features/variables
Corrs<-round(cor(num_vars),2)
Corrs #Pearson's Correlation coefficients
melted_cormat<- melt(Corrs)
melted_cormat
#Generating a correlation heatmap for the Pearson's Correlation Coefficients.
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

#Frequency distribution of the character variables of interest 
table(aca2_train$ONTASK,aca2_train$Gender)#0=Female, 1=Male. The result reveals that more females (3813) were on task than males (3433).

table(aca2_train$ONTASK,aca2_train$GRADE)#majority of the students on task were from the fourth grade (2362)

table(aca2_train$ONTASK,aca2_train$Activity)#Majority of those on task were on wholedesks activities, while the least on task were those on daancing activities

#Plotting or Graphical Representation of the variables
num_vars<-data.frame(aca2_train$ONTASK,num_vars)
names(num_vars)=c('Ontask','Osnum','totalobs','Transitions','NumActivities',
                  'NumFormats','Obs','Duration')
#Histogram of Duration based on On and Off task
ggplot(data = num_vars) +
  geom_histogram(mapping = aes(x = Duration, fill = Ontask),binwidth = 20)


#Part 2: Training Classification Models
#The variable of interest is Ontask. On task is a binary variable with 2 outcomes, N for on task, and Y for off task.
#The two classication algorithims suitable to classify the variable are logistic regression and the decision tree model algorithms 

#Creating a classification sample 
Ontask<-as.numeric(factor(aca2_train$ONTASK))
Gender<-aca2_train$Gender
Time<-aca2_train$Total.Time
Duration<-aca2_train$Transitions.Durations
train_df<-data.frame(Ontask,Gender,Time,Duration)

#1. Logistics Regression model
logit_model <- glm(Ontask ~.,data = train_df)
summary(logit_model)

#2. Decision Tree Model
D_Tree <- ctree(Ontask ~Gender+Time+Duration,data = train_df)
summary(D_Tree)
#visualizing the tree
print(D_Tree)

plot(D_Tree)

#Part 3: Model Evaluation on Validation set
Ontask.v<-as.numeric(factor(aca2_validation$ONTASK))
Gender.v<-aca2_validation$Gender
Time.v<-aca2_validation$Total.Time
Duration.v<-aca2_validation$Transitions.Durations
validation_df<-data.frame(Ontask.v,Gender.v,Time.v,Duration.v)

#Logistics Regression Confusion Matrix
logits_model <- glm(Ontask.v ~Gender.v+Time.v+Duration.v,data = validation_df)
summary(logits_model)
predicted <- predict(logits_model, validation_df, type="response")
head(predicted)
#create confusion matrix
#predictions
probabilities <- predict(logits_model, validation_df[,2:3], type = "response")
preds_logit <- ifelse(probabilities > 0.5, "N", "Y")

#The confusion table
logit_CM <- table(validation_df$Ontask.v,preds_logit)
logit_CM

#Decision tree confusion matrix
preds_tree <- predict(D_Tree, validation_df[,2:3])
preds_tree <- ifelse(probabilities > 0.5, "N", "Y")

#The confusion table
tree_CM <- table(validation_df$Ontask.v,preds_tree)
tree_CM

#Part 4: Interpretation of Results
#Overall, there were no cases of missing data. 
#The project uses gender, time, and duration to classify the students into either on task or off task.
#The data manipulation process included extracting the variables of interest from the given data to form a sample data for both training and validation
#The logistics regression algorithm was used to classify the students into either task or off task.
#The summary statistics of the logistics regression model demonstrated that gender, time and duration are important in classifying the students into either on task or off task.
#The confusion matrices reveal that both models perform better, with similar predictions (N=1849, Y=3698)
#Therefore, both logistics regression and decision tree are relevant in predicting whether the student is on task or off task.

