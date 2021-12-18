#The process begins by setting a working directory and loading the required packages.The following chunk of code was used to load these packages:
library(tidyverse)
library(cluster)
library(factoextra)
library(VIM)
data=read.csv('/Users/codyxu/R/CollegeScorecard1.csv')
#The required dataset is then imported into the R console and five variables selected for exploration. The variables were selected because they are a good measure of student performance and per-student expenditure. The code finally shows the first six values of each of these variables as shown below:
#importing the datas
data1=cbind(data$UNITID,data$ACTWRMID,data$SAT_AVG,data$PCIP13,data$NPT4_PUB)
head(data1)
aggr(data1)
#Data Preparation
#We then graph the 'aggr' plot which shows the amount of missing/imputed values in each column.
#Based on the aggr plot above, UNITID does not contain any missing values. On the other hand, ACTWRMID, SAT_AVG, PCIP13, and NPT4_PUB have missing values which have to be deleted before proceeding with the analysis.
#dealing with NAs
df<-data1
df1<-na.omit(df)
head(df1)
#After removing the NAs, we then scale column 2 to column 5 data is then scaled. Column 1 is not scaled because it acts the identity column for the universities.
#scaling with datas
df2<- scale(df1)
head(df2)
#We then use the Elbow method for kmeans to determine the optimal number of clusters.
#Elbow method 
fviz_nbclust(df2,kmeans,method = "wss")+geom_vline(xintercept = 3, linetype = 2)
#Based on the graph, the optimal number of clusters is 3. We then compute k-means with k = 3 
#compute k-means with k=3
set.seed(123)
km.res <- kmeans(df2[,2:5], 3, nstart = 25)
#print the result
print(km.res)
#The output shows that there as 3 clusters of sizes 51, 59, and 30. The results also show the cluster means and the clustering vectors.
#We then visualize kmeans clustering by using repel = TRUE to avoid over plotting. The plot is shown below:
aggregate(df2[,2:5], by=list(cluster=km.res$cluster), mean)
fviz_cluster(km.res, df2[,-1], ellipse.type = "norm")
#The final task involves obtaining an array of cluster labels corresponding to UNITID. The code below does this and shows the first 6 values.
#list of cluster assignments
o=order(km.res$cluster)
df3=data.frame(df1[,1][o],km.res$cluster[o])
head(df3)
#In terms of performance, universities which belonged to cluster 1 have the poorest results followed by cluster 2 and lastly cluster 3 which encompassed the best performing universities. It is also clear that best performing institutions were also expensive. In other words, they had high per-student expenditure.
library(dlstats) 
library(pkgsearch) 
rocPkg <-  pkg_search(query="ROC",size=200)
rocPkgShort <- rocPkg %>% 
  filter(maintainer_name != "ORPHANED", score > 190) %>%
  select(score, package, downloads_last_month) %>%
  arrange(desc(downloads_last_month))
head(rocPkgShort)
library(dlstats)
shortList <- c("pROC","precrec","ROCit", "PRROC","ROCR","plotROC")
downloads <- cran_stats(shortList)
ggplot(downloads, aes(end, downloads, group=package, color=package)) +
  geom_line() + geom_point(aes(shape=package)) +
  scale_y_continuous(trans = 'log2')
#ROCR - 2005
library(ROCR)
data(ROCR.simple)
df <- data.frame(ROCR.simple)
pred <- prediction(df$predictions, df$labels)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)
