# Social Network Analysis
install.packages("igraph") # igraph - It is use in the analysis of graphs or networks
library(igraph)
# directed = False means it will not point out to any vertex and the values which are given
# it is for joining each vertex through one another with the help of node,
# n = 7 means that 4 vertex which are connected and 3 more vertex which are not connected
g <- graph(c(1,2,2,3,3,4,4,1),
           directed = FALSE,
           n = 7)
plot(g,
     vertex.color = "green",
     vertex.size = 40,
     edge.color = "red") # for plotting the graph in the plots section
g[] # 7 x 7 sparse Matrix of class "dgCMatrix"

g1 <- graph(c("Amy","Ram","Ram","Li","Li","Amy","Amy"
              ,"Li","Kate","Li"))
plot(g1,
     vertex.color = "green",
     vertex.size = 50,
     edge.color = "red")
g1



# Network measure
degree(g1) # Number of Connections
degree(g1, mode = 'in') # Number of node coming in
degree(g1, mode = 'out') # Number of node going out

diameter(g1)
edge_density(g1, loops = F) # The density of a graph is the ratio of the actual number of edges and the largest possible number of edges in the graph, assuming that no multi-edges are present.
ecount(g1) # counting edges
vcount(g1) # counting vertices
closeness(g1, mode = 'all', weights = NA) # Closeness centrality measures how many steps is required to access every other vertex from a given vertex.
betweenness(g1, directed = T) # The vertex and edge betweenness are (roughly) defined by the number of geodesics (shortest paths) going through a vertex or an edge.



# Read data file
data <- read.csv(file.choose(), header = T)
y <- data.frame(data$first, data$second)


# Creating network
net <- graph.data.frame(y, directed = T)
V(net) # Vertices
E(net) # Edges
V(net)$label <- V(net)$name
V(net)$degree <- degree(net)



# Histogram of Node degree
hist(V(net)$degree,
     col = 'green',
     main = 'histogram of Node Degree',
     ylab = 'Frequency',
     xlab = 'Degree of Vertices')


# Network Diagram
set.seed(222)
plot(net,
     vertex.color = 'orange',
     vertext.size = 2,
     edge.arrow.size = 0.1,
     vertex.label.cex = 0.8)



# Highlighting degrees and layouts
plot(net,
     vertex.color = rainbow(52),# changing every color of vertex
     vertex.size = V(net)$degree*0.4,# If the degree is higher the circle will be bigger and vice-versa
     edge.arrow.size = 0.1,
     layout = layout.fruchterman.reingold)
plot(net,
     vertex.color = rainbow(52),# changing every color of vertex
     vertex.size = V(net)$degree*0.4,# If the degree is higher the circle will be bigger and vice-versa
     edge.arrow.size = 0.1,
     layout = layout.graphopt) # change in layout
plot(net,
     vertex.color = rainbow(52),# changing every color of vertex
     vertex.size = V(net)$degree*0.4,# If the degree is higher the circle will be bigger and vice-versa
     edge.arrow.size = 0.1,
     layout = layout.kamada.kawai) # change in layout



# Hubs and authorities
# Hubs has outgoing links and authorities has incoming links from the hubs
hs <- hub_score(net)$vector # The hub scores of the vertices are defined as the principal eigenvector of A A^T, where A is the adjacency matrix of the graph.
as <- authority.score(net)$vector
par(mfrow=c(1,2))
set.seed(123)
plot(net,
     vertex.size = hs*30,#if node has more outgoing link will appear bigger
     main = 'Hubs',
     vertex.color = rainbow(52),
     edge.arrow.size = 0.1,
     layout = layout.kamada.kawai)
set.seed(123)
plot(net,
     vertex.size = as*30,#if node has more incoming link will appear bigger
     main = 'Authorities',
     vertex.color = rainbow(52),
     edge.arrow.size = 0.1,
     layout = layout.kamada.kawai)
par(mfrow = c(1,1))


# Community Detection
net <- graph.data.frame(y, directed = F)
cnet <- cluster_edge_betweenness(net) # Many networks consist of modules which are densely connected themselves but sparsely connected to other modules.
plot(cnet,
     net,
     vertex.size = 10,
     vertex.label.cex = 0.8) # Different groups with dense connections and between the groups there are sparse connections 
