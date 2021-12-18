#Social Network Analysis

#SNA Data Preparation
library(igraph)
library(tidyverse)
Visualizing the network
SNAdata <- read.csv("/Users/codyxu/R/ICE8_Data.csv", row.names = 1)

g <- graph_from_adjacency_matrix(as.matrix(SNAdata), weighted=TRUE, mode="undirected")
plot(g)                                 
#Network Statistics
#Density
d <- edge_density(g)
d
#Degree centrality
degree(g)
#Closeness centrality
betweenness(g, normalized = TRUE)
#Betweenness centrality
betweenness(g, directed = FALSE, normalized = TRUE)
#Community detection
fc <- cluster_fast_greedy(g)
membership(fc)
sizes(fc)
V(g)$color <- fc$membership
plot(g)

#Make Your Network Prettier
V(g)$degree <- degree(g)

plot(g,
     vertex.size = V(g)$degree*2)
plot(g,
     vertex.size = V(g)$degree*2,
     edge.width = 5^(E(g)$weight)/5)
set.seed(123)

plot(g,
     vertex.size = V(g)$degree*2,
     edge.width = 5^(E(g)$weight)/5,
     layout=layout.fruchterman.reingold)
