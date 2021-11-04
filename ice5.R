#Clustering and k-Means
twoDData <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2),
                  matrix(rnorm(100, mean = 2, sd = 0.2), ncol = 2),
                  matrix(rnorm(100, mean = 4, sd = 0.5), ncol = 2),
                  matrix(rnorm(100, mean = 6, sd = 0.2), ncol = 2))

colnames(twoDData) <- c("x", "y")
plot(twoDData)
cl <- kmeans(twoDData, centers = 4)
plot(twoDData, col = cl$cluster)
#Silhoutte Coefficient and Silhoutte Plot
library(cluster)
dis = dist(twoDData)^2
sil = silhouette(cl$cluster, dis)
plot(sil, col = cl$cluster)
library(factoextra)
fviz_silhouette(sil)
#Clustering with k-Means: Self-Reported Motivation
library(tidyverse)
motivation <- read_csv('/Users/codyxu/R/ICE5_Data.csv')
motivationClean <- motivation %>% na.omit() %>% 
  select(-id)
motivationClean
#Let’s fit a two-cluster model first and check out the silhouette plot 
#as well as the coefficient.
motivation2CL <- kmeans(motivationClean, centers = 2)
dis2CL = dist(motivationClean)^2
sil2CL = silhouette(motivation2CL$cluster, dis2CL)
fviz_silhouette(sil2CL)
motivation3CL <- kmeans(motivationClean, centers = 3)
dis3CL = dist(motivationClean)^2
sil3CL = silhouette(motivation3CL$cluster, dis3CL)
fviz_silhouette(sil3CL)

motivation4CL <- kmeans(motivationClean, centers = 4)
dis4CL = dist(motivationClean)^2
sil4CL = silhouette(motivation4CL$cluster, dis4CL)
fviz_silhouette(sil4CL)
#contain the clustering result.
motivationClean %>% mutate(cluster = motivation3CL$cluster)
fviz_nbclust(motivationClean, kmeans, method = "wss")
