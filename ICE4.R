library(tidyverse)
mooc <- read_csv('/Users/codyxu/R/ICE4_Data.csv')
mooc
table(mooc$certified)
summary(mooc)
library(GGally)
ggpairs(mooc, columns = 2:4, ggplot2::aes(colour=certified)) 
moocD <- mooc %>% mutate(certified_yes = as_factor(certified)) %>% 
  select(certified_yes, forum.posts, grade, assignment)
moocD
logitModel <- glm(certified_yes ~ forum.posts + grade + assignment, data = moocD, family = "binomial")
summary(logitModel)
## decison tree
library(party)
moocTree <- ctree(certified_yes ~ forum.posts + grade + assignment,data = moocD)
plot(moocTree)
# navie bayes
library(e1071)
moocNB <- naiveBayes(
  certified_yes ~ forum.posts + grade + assignment,
  data = moocD)
certified_pred_NB <- predict(moocNB, moocD[,2:4])
performance = moocD$certified_yes == certified_pred_NB
cat('The accuracy is', sum(performance)/length(performance)*100, '%')
#model evaluation
set.seed(123)

sample_size <- floor(0.8*nrow(moocD))

picked <- sample(seq_len(nrow(moocD)),size = sample_size)

training_moocD <- moocD[picked,]

#Step 2. Re-train the model with just the training data. By this time you should be able to build your own models. Give it a try.
testing_moocD <- moocD[-picked,]
moocLogit <- glm(certified_yes ~ forum.posts + grade + assignment, data = training_moocD, family = "binomial")
moocTree <- ctree(certified_yes ~ forum.posts + grade + assignment, data = moocD)
#Step 3. Feed the Xs in the testing dataset and obtain the predicted Ys.
probabilities <- predict(moocLogit, testing_moocD[,2:4], type = "response")
certified_pred_logit <- ifelse(probabilities > 0.5, "yes", "no")
certified_pred_tree <- predict(moocTree, testing_moocD[,2:4])
#Step 4. Compare the predicted Ys with what is actually in the testing dataset (the ground truth) and obtain the confusion matrix. In R, confusion matrix can be print out in a fairly easy way by using table. It simple builds a cross-table to summarize the data.
logitCM <- table(testing_moocD$certified_yes,certified_pred_logit)
logitCM
library(caret) 
logitAccuracy <- confusionMatrix(logitCM)$overall["Accuracy"]
cat('The accuracy for the logistic regression model is', logitAccuracy*100, '%')
