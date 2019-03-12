# This is my R-code for learning network analsysis in R. 
# I took code from this link: http://kateto.net and book by Kolaczyk 

# The collection of code is the company to my network analysis notes.

# Step I install essential packages 
# Install the essential packages 
install.packages("igraph") 
install.packages("network") 
install.packages("sna") 
install.packages("ndtv")

#Step II - Creating the network graphs

library(igraph)
g <- graph.formula(1-2, 2-3, 2-4, 3-4)
V(g) # gives the sequences of nodes 
E(g) # gives the sequences of edge 

s <- graph.formula(A-B, B-C, C-D, B-D) # we can use alphabet to represent nodes
str(g) # shows the structure of network 
str(s)

plot(g)
plot(s)

# we can change the color and size of nodes (vertes)

V(g)$size <- 16
V(g)$color <- "#8ABCDD"
plot(g)

E(s)$width <- 1.6
E(g)$width <- 1.6
E(s)$color <- "black"
E(g)$color <- "black"
# plot two figures side by sdie 
par(mfrow = c(1,2))

plot(g, layout=layout.auto, main = "Figure 2.11")
plot(s, layout = layout.auto, main = "Figure 2.22")

# add one plot with directions 

dg <-graph.formula( A -+ B, B -+ C, C -+ D, D -+ B)
plot(dg)

# change the arrow size 
E(dg)$arrow.size <- 0.3
E(dg)$color <- "black"
V(dg)$color <- "white"
plot(dg)

# plot the network 
par(mfrow = c(1,3))
plot(g, layout=layout.auto)
title("Figure 2.11", line = -5)
plot(s, layout = layout.auto)
title("Figure 2.12", line = -5)
plot(dg, layout = layout.auto)
title("Figure 2.13", line = -5)

# transfer matrix into the network 

g <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), nrow = 3, byrow = T)
gg <- graph_from_adjacency_matrix(g, mode = "undirected" ) # model can be directed 
V(gg)$color <- "#78C8A5"
plot(gg)

g2 <- matrix(c(0, 0, 0, 1, 0, 1, 0, 1, 0), nrow = 3, byrow = T)
gg2 <- graph_from_adjacency_matrix(g2) # default mode = undirected
par(bg = "#EBEBEB") # change the background color 
plot(gg2)


g3 = matrix(c(0, 1 , 1, 0, 1, 0 , 0, 1, 1, 0, 0, 1, 0, 1, 1, 0), nrow = 4, byrow = T)
gg3 <- graph_from_adjacency_matrix(g3, mode = "undirected")
plot(gg3)

# Degree and network density 
par(mfrow=c(1,2))
g4 <- matrix(c(0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1,
        0, 1, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 1,0),nrow = 6, byrow = T)
gg4 <- graph_from_adjacency_matrix(g4, mode = "undirected")
E(gg4)$width = 1.6
V(gg4)$color = "#8ABCDD"
plot(gg4, layout = layout_as_star)

g5 <- matrix(c(0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1,
        0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1,0),nrow = 6, byrow = T)
gg5 <- graph_from_adjacency_matrix(g5, mode = "directed")
E(gg5)$width = 1.6
E(gg5)$arrow.size = 0.3
V(gg5)$color = "#8ABCDD"
plot(gg5, layout = layout_as_star)

# We check the structure of graph 
print(gg4)
print(gg5)
is.simple(gg4) # checked weighted or unweighted graph 
vcount(gg4) # count the number of nodes

igraph::degree(gg4) # put igraph before degree, as this one is not compatiable with sna
igraph::degree(gg4, mode = 'in')
igraph::degree(gg4, mode = 'out')

igraph::degree(gg5)
igraph::degree(gg5, mode = 'in')
igraph::degree(gg5, mode = 'out')

# degree distribution plot
degree_distribution(gg4)
degree_distribution(gg4, cumulative = T)

deg <- igraph::degree(gg4, mode = 'all')
degdis <- degree_distribution(gg4, cumulative = T)
par(mfrow= c(1,2))
hist(deg, breaks=1:vcount(gg4)-1, main="Histogram of node degree", xlab = 'degree')
plot(x = 0:max(deg), y = 1-degdis, type = 'b', pch = 19, cex=1.2, col = '#1976D2',
        xlab = 'degree', ylab = 'Frequency', main = 'Distribution of degree')
noncumdegdis <- degree_distribution(gg4)
points(x = 0:max(deg), y = noncumdegdis, type = 'b', pch = 15, 
        cex=1.2, col = '#1BB941')
legend('topleft', legend = c('Cumulative distribution', 'noncumulative distribution'),
        col = c('#1976D2', '#1BB941'), pch = c(19,15),text.col = 'black', cex = 0.6)

# calculate combination and permutations

install.packages("combinat")
library(combinat)
combn(4,2)
length(combn(4,2))/2
combn(4,3)
combn(3,2)








