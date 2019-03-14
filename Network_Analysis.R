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



# bipartite graph example 

m = matrix(c(1 , 0 , 0 , 0,
        1 , 1 , 0 , 0 ,
        1 , 1 , 1 , 0 ,
        0 , 1 , 0 , 1 ,
        0 , 0 , 1 , 0 ,
        0 , 0 , 0 , 1), nrow = 6, byrow = T)

bg <- graph.incidence(m) # indicence for ploting bipartite graph 
plot(bg, layout = layout_as_bipartite) # put the layout 

V(bg)$type  # it gives two sets with FALSE and TRUE

# now change the color 

V(bg)$color <- ifelse(V(bg)$type == FALSE, "#649EFC", "#E75854")
plot(bg, layout = layout_as_bipartite, vertex.size = 22, 
        vertex.label.cex=1.6, edge.width = 1.6)


# plot with the specific size 

# set working directory with copied path (command+option+c)
setwd('/Users/Michael/Library/Mobile Documents/com~apple~CloudDocs/Economics/Network/Notes')
png(filename = "figure31.png",width = 400, height = 360)
plot(bg, layout = layout_as_bipartite, vertex.size = 22, 
        vertex.label.cex=1.6, edge.width = 1.6,
        main = "A Bipartite Graph")
dev.off()

# try pdf 
pdf(file = "figure31.pdf",width = 4, height = 3.6)
plot(bg, layout = layout_as_bipartite, vertex.size = 26, 
        vertex.label.cex=1, edge.width = 1.6, 
        main = "A Bipartite Graph")
dev.off()

# try to change margins
pdf(file = "figure311.pdf",width = 4, height = 3.6)
par(mar = c(1, 0, 2, 1))  # use par after pdf file 
plot(bg, layout = layout_as_bipartite, vertex.size = 26, 
        vertex.label.cex=1, edge.width = 1.6, 
        main = "A Bipartite Graph")
dev.off()

# Bipartite projection 

pro <- bipartite_projection(bg)
pdf(file = "figure32.pdf", width = 5.8, height = 3.6)
par(mfrow = c(1,2), mar = c(1, 1, 2, 1), cex.main = 1, font.main = 1)
plot(pro$proj1, layout = layout_in_circle, main = "Blue projecion on Red")
plot(pro$proj2, layout = layout_in_circle, main = "Red projection on Blue")
dev.off()

# don't use layout_on_grid that ofthen, as it gives wrong connection. 
plot(pro$proj1, layout = layout_on_grid(pro$proj1, dim = 2))  

# get adjacency matrix 

get.adjacency(pro$proj1, sparse = F, attr = "weight")

# project matrix 

cr = matrix(c(1 , 0 , 0 , 0 ,
        1 , 1 , 0 , 0 ,
        1 , 1 , 1 , 0 ,
        0 , 1 , 0 , 1 ,
        0 , 0 , 1 , 0 ,
        0 , 0 , 0 , 1 ), nrow = 6, byrow = T)
cb = matrix(c(1 , 1 , 1 , 0 , 0 , 0 ,
        0 , 1 , 1 , 1 , 0 , 0 ,
        0 , 0 , 1 , 0 , 1 , 0 ,
        0 , 0 , 0 , 1 , 0 , 1 ), nrow = 4, byrow = T)

adjmatrx = cr%*%cb
diag(adjmatrx) = 0 
adjmatrx  # it is equal to the one through equation get.adjacency 

prmatrix(adjmatrx, rowlab = rep("", 6), collab = rep("", 6)) # print matrix without indices

# adjacency matrix for red project 
get.adjacency(pro$proj2, sparse = F, attr = "weight")
adjmatrx2 = cb%*%cr
diag(adjmatrx2) = 0 
adjmatrx2

prmatrix(adjmatrx2, rowlab = rep("", 4), collab = rep("", 4))  # print out matrix 


# row sums shows the importance or partication rate of each member 
rowSums(adjmatrx)
rowSums(adjmatrx2)


# nested split graph, where igraph does not support this kind of analysis
# need install 

# check the degree first 
degree(bg)
plot(bg, layout = layout_in_circle) # different plot layout 

# creat a stepwise matrix 

swm = matrix(c(0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
               1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 
               1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 
               1, 1, 1, 0, 1, 1, 0, 0, 0, 0,
               1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
               1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
               1, 1, 1, 0, 0, 0, 0, 0, 0, 0,
               1, 1, 1, 0, 0, 0, 0, 0, 0, 0,
               1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
               1, 1, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 10, byrow = T)
nwswm <- graph_from_adjacency_matrix(swm, mode = "undirected")
V(nwswm)$color = "#8ABCDD"
plot(nwswm, layout = layout_in_circle)

bipgraph <- graph.incidence(swm)
V(bipgraph)$color <- ifelse(V(bipgraph)$type == FALSE, "#78C8A5", "#E75854")
plot(bipgraph, layout = layout_as_bipartite)

# I will put more code on nested split graph later 

## Measuring and Visualizing network 
# install.packages("sand") # which includes igraphdata , statistical analysis network with R
install.packages("igraphdata")
library(igraphdata)
data("UKfaculty")
plot(UKfaculty, layout = layout_in_circle)
















# 
















