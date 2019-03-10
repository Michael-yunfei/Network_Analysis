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


par(mfrow = c(1,3))
plot(g, layout=layout.auto)
title("Figure 2.11", line = -5)
plot(s, layout = layout.auto)
title("Figure 2.12", line = -5)
plot(dg, layout = layout.auto)
title("Figure 2.13", line = -5)

















