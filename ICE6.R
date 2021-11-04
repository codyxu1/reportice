library(tidyverse)
set.seed(123)

# A list of 200 values of x with means of 1 and sd of 1.5
X <- rnorm(200, mean = 1, sd = 1.5)
# A list of 200 residuals with sd of 0.5
res <- rnorm(200, mean = 0, sd = 0.5)
y <- 1 + 2 * X + res

twoDData <- tibble(X = X, y = y)
plot(twoDData)
pca <- prcomp(twoDData, scale. = TRUE)
# scale. = TRUE is a logical value indicating whether the variables should be scaled to have unit variance before the analysis takes place. The default is FALSE for consistency with the dataset, but in general scaling is advisable.
summary(pca)
pca$rotation
#PCA for Dimension Reducation
pc1 <- pca$x[,1]
rotation1 <- pca$rotation[,1]
plot(scale(twoDData), col = "blue") # the original data is scaled because PCA is also scaled
points(pc1 %*% t(rotation1), col = "orange")
ICEdata <- read_csv('/Users/codyxu/R/ICE6_Data.csv')
ICEdata
ICEdata_noid <- ICEdata %>% select(-id)
icepca <- prcomp(ICEdata_noid, scale. = FALSE) # Here let's see a unscaled example
summary(icepca)
icepca2c <- icepca$x[,1:2]
plot(icepca2c)
#From the scatter plot, we can clearly see three clusters.
cl <- kmeans(icepca2c, centers = 3)
plot(icepca2c, col = cl$cluster)
biplot(icepca, cex=.7)
