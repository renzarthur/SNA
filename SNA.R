library(igraph)
library(statnet)

edges <- read.csv(file.choose(), header = T)
head(edges)
nodes <- read.csv(file.choose(), header = T, check.names = FALSE, row.names = 1)
head(nodes)

#edges
eth <- graph_from_data_frame(edges, directed = FALSE, vertices = NULL)
#nodes
g <- graph_from_adjacency_matrix(as.matrix(nodes),weighted = TRUE, mode = "directed")

#Coloring based on Gender

V(g)$color <- ifelse(E(eth)$Gender == "Male", "dodgerblue", "white")
V(eth)$color <- ifelse(E(eth)$Gender == "Male", "dodgerblue", "white")
plot(g, vertex.col = 0.5, displaylabels = TRUE, vertex.size = 10, edge.arrow.size = 0.3, 
    layout=layout.random)

E(eth)$weight <- E(eth)$HasTie # Assigning edge attribute to each edge
eth

#Nodes
V(g)
#Edges
E(eth)

#1. Eigenvector centrality
eig <- evcent(eth)$vector
V(g)$Eigen<-eig
V(g)$Eigen
which.max(eig)

#2. Betweenness centrality
bw<-betweenness(g, directed = FALSE)
V(g)$betweenness<-bw
V(g)$betweenness
which.max(bw)

#DATA FRAME TEXT VERSION
DF <- as_long_data_frame(eth)
DF


edge_density(eth) # Global density
A1<-induced_subgraph(eth, E(eth)$Gender=="Male", impl=c("auto")) # Subgraphing into each class
edge_density(A1) # Male gender density

#Plotting based on Ethnicity and Weight
par(mar=c(0,0,3,0),mfrow=c(1,1))
V(eth)$size <- strength(eth) #this line matches node size according to its weight
par(mar=c(0,0,0,0)); plot(eth,edge.arrow.size = 0.1,)
V(eth)$size <- log(strength(eth)) * 3+2 #this line cleans the graph reducing node sizes
par(mar=c(0,0,0,0)); plot(eth,edge.arrow.size = 0.1,)
V(eth)$label <- ifelse( strength(eth)>=5, V(eth)$name, NA ) #removed some labels on the nodes for a cleaner graph
par(mar=c(0,0,0,0)); plot(eth,edge.arrow.size = 0.1,)


# FINAL GRAPH
set.seed(1000)
fr <- layout_with_fr(eth, niter=1000)
plot(eth, layout=fr,edge.arrow.size = 0.1,)

legend(
  "topleft",
  legend = c("Male","Female"),
  pt.bg  = c("dodgerblue", "white"),
  pch    = 21,
  cex    = 1.5,
  bty    = "n",
  title  = "School"
)
