# This is my R-code for learning network analsysis in R.
# I took code from this link: http://kateto.net and book by Kolaczyk

# The collection of code is the company to my network analysis notes.

setwd("/Users/Michael/Documents/Network_Analysis")

# Step I install essential packages
# Install the essential packages
install.packages("igraph")
install.packages("network")
install.packages("sna")
install.packages("ndtv")
install.packages("crop")

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
plot(gg4, layout = layout_in_circle)

g5 <- matrix(c(0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1,
        0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1,0),nrow = 6, byrow = T)
gg5 <- graph_from_adjacency_matrix(g5, mode = "directed")
E(gg5)$width = 1.6
E(gg5)$arrow.size = 0.3
V(gg5)$color = "#8ABCDD"
plot(gg5, layout = layout_in_circle)

# We check the structure of graph
print(gg4)
print(gg5)
is.simple(gg4) # checked weighted or unweighted graph
vcount(gg4) # count the number of nodes

igraph::degree(gg4) # put igraph before degree, as this one is not compatiable with sna
mean(igraph::degree(gg4))
igraph::degree(gg4, mode = 'in')
igraph::degree(gg4, mode = 'out')

distances(gg4, mode="all")  # distances
mean_distance(gg4, directed=FALSE)


igraph::degree(gg5)
igraph::degree(gg5, mode = 'in')
igraph::degree(gg5, mode = 'out')

distances(gg5, mode="all")
mean_distance(gg5, directe=TRUE)

gg6 <- erdos.renyi.game(6, 0.2)
E(gg6)$width = 1.6
E(gg6)$arrow.size = 0.3
V(gg6)$color = "#8ABCDD"
plot(gg6, layout=layout_in_circle)

igraph::degree(gg6)
mean(igraph::degree(gg6))

distances(gg6, mode="all")
mean_distance(gg6, directed=FALSE)

# plot gg4 and gg6 together

png(file="geodesic1.png", width=5.6, height=3.8, units="in", res=569)
par(mfrow=c(1,2), mar=c(1,1,1,1))
plot(gg4, layout=layout_in_circle)
plot(gg6, layout=layout_in_circle)
dev.off()

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

# generate a regular graph
rg <- sample_k_regular(10, 5)
plot(rg, layout = layout_in_circle)
distances(rg)
mean_distance(rg, directed = F)
degree(rg)

# use g4 (gg4) and g5(gg5) to go through all measurement of graph
# in case the commada gives wrong answers

eigen_centrality(gg4)

# get eigenvector of measuring importance
eigen(g4)
eigen(g4)$vector[,1]*-2  # please compare this one with g4eiv
g4eiv <- eigen_centrality(gg4)$vector  # eigenvector from graph, which normalized the element
# with largest eigenvalue into one and make sure it is positive.
pfeiva <- max(eigen(g4)$values)  # maximum of eigenvalues
g4 %*% g4eiv  # the vector in r has no dimension, you have to be careful on this part
round(g4 %*% g4eiv, 6)  == round(pfeiva * g4eiv, 6) # without round, it will be false
# You should clear how to calcuate eigenvector centrality



## Generate a random work (game-theory or dynamic model)

gb <- barabasi.game(10, directed = F)
plot(gb)

pdf(file = "random_graph.pdf",width = 8.2, height = 3.6)
par(mfrow = c(1,3), mar = c(1, 1, 2, 1), cex.main = 1, font.main = 1)
nn = 10  # number of nodes
gb1 <- erdos.renyi.game(nn, 0.05, type = "gnp")
V(gb1)$color = "#8ABCDD"
plot(gb1, vertex.size = 26, vertex.label.cex=1.5, edge.width = 1.8, main =
                "Ga: Random Graph with Edge p = 0.05")
box()  # you can change it to "figure" or "inner" or "outer"

gb2 <- erdos.renyi.game(nn, 0.2, type = "gnp")
V(gb2)$color = "#8ABCDD"
plot(gb2, vertex.size = 26, vertex.label.cex=1.5, edge.width = 1.8, main =
                "Gb: Random Graph with Edge p = 0.2")
box()

gb3 <- barabasi.game(10, directed = F)
V(gb3)$color = "#8ABCDD"
plot(gb3, vertex.size = 26, vertex.label.cex=1.5, edge.width = 1.8, main =
                "Gc: Random Graph based on Barabasi Model")
box(which = "plot")
dev.off()


# Poisson distribution

# the normal curve we have seen almost every day is a density function or a distribution
# function !!

# dpois gives density
# ppois gives distribution
ppois(16, lambda = 12)
xseq <- seq(0, 20, 1)
dx1 <- dpois(xseq, lambda = 1)
dx2 <- dpois(xseq, lambda = 4)
dx3 <- dpois(xseq, lambda = 10)

poisdata <- data.frame("k" = xseq, "dl1" = dx1, "dl2" = dx2, "dl3" = dx3)
library(ggplot2)
pp <- ggplot(poisdata, aes(x = k)) +
                geom_point(aes(y = dl1), colour = "black", fill= "#FDA428",
                        shape = 21, size = 3.6) +
        geom_line(aes(y = dl1), color = "#808080") +
        geom_point(aes(y = dl2), colour = "black", fill= "#7F0F7E",
                shape = 21, size = 3.6) + geom_line(aes(y = dl2), color = "#808080") +
        geom_point(aes(y = dl3), colour = "black", fill= "#AED8E5",
                shape = 21, size = 3.6) + geom_line(aes(y = dl3), color = "#808080") +
        ylab("P(x = k)") + theme(axis.text = element_text(size=12),
                axis.title = element_text(size = 14)) +
        ggtitle("Probability mass function for the Poisson distribution") +
        theme(plot.title = element_text(hjust = 0.5))

pp

# effeicient version

pp1 <- ggplot(poisdata, aes(x = k)) +
        geom_point(aes(y = dl1), colour = "black", fill= "#FDA428",
        shape = 21, size = 3.6) +
        geom_line(aes(y = dl1), color = "#808080") +
        geom_point(aes(y = dl2), colour = "black", fill= "#7F0F7E",
                shape = 21, size = 3.6) + geom_line(aes(y = dl2), color = "#808080") +
        geom_point(aes(y = dl3), colour = "black", fill= "#AED8E5",
                shape = 21, size = 3.6) + geom_line(aes(y = dl3), color = "#808080") +
        labs(x = "k", y = "P(x = k)",
                title = "Probability mass function for the Poisson distribution") +
        theme(plot.title = element_text(hjust = 0.5),
                axis.text = element_text(size=12),
                axis.title = element_text(size = 14)) # change the theme in the end

pp1

# to add the lagend we have to reshapte the data
install.packages("latex2exp")
library(latex2exp)
library(tidyverse)
lambda = c(1, 4, 10)
poisdata %>%
        gather(lambda, massdensity, dl1:dl3) %>%
        ggplot(aes(x = k, y = massdensity,fill = lambda)) +
        geom_point(shape =21, size = 3.6) + geom_line(color = "#808080") +
        labs(x = "k", y = "P(x = k)",
                title = "Probability mass function for the Poisson distribution",
                fill = "Lambda value") +
        theme(plot.title = element_text(hjust = 0.5),
                axis.text = element_text(size=12),
                axis.title = element_text(size = 14),
                legend.position = c(0.95, 0.95),
                legend.justification = c("right", "top")) +
        scale_fill_discrete(labels = lapply(sprintf('$\\lambda = %d$', lambda), TeX)) -> p3
# please use scale option in the end
ggsave("poisson_den.pdf", plot = p3, width = 5.8, height = 3.6)


# static power law

gplaw <- static.power.law.game(2000, 5560, 2.3, 2.3)  # out/in-degree exponent must be >= 2
E(gplaw)$arrow.size = 0.3
plot(gplaw, layout = layout_on_sphere)
deg <- degree(gplaw, mode = "out")
degdis <- degree.distribution(gplaw, cumulative = T, mode = "out")
plot(degdis)  # please make sure you understand log scale
plot(log10(degdis))
cindx <- c(1:21)
plot(x = log10(cindx), y = degdis)
plot(x = log10(cindx), y = log10(degdis))

# one figure to understand the log scare graph
par(mfrow = c(2,3), pch = 19)
plot(degdis, log = "x", main = "x-axis log-scale")
plot(degdis, log = 'y', main = "y-axis log-scale")
plot(degdis, log = "xy", main = "x and y axises log-scale")
plot(x = log10(cindx), y = degdis, main = "take log on x-variable")
plot(x = cindx, y = log10(degdis), main = "take log on y-variable")
plot(x = log10(cindx), y = log10(degdis), main = "take log on x and y variabls")


g <- sample_fitness_pl(1000, 3000, 2.2, 2.3)
plot(degree_distribution(g, cumulative=TRUE, mode="out"), log="xy")

# Poisson vs. Power-law Distributions

poisson_g1 <- erdos.renyi.game(50, 0.079)
mean(igraph::degree(poisson_g1))  # 3.88
mean_distance(poisson_g1, directed=FALSE)
pg1_d <- get.diameter(poisson_g1)
diameter(poisson_g1)
E(poisson_g1)$color = "grey"
E(poisson_g1)$width = 1.6
E(poisson_g1, path=pg1_d)$color = "#F67770"
E(poisson_g1, path=pg1_d)$width = 2
V(poisson_g1)$color = "#8ABCDD"
V(poisson_g1)[pg1_d]$color = "#F67770"
dev.print(png, file = "poissong1.png", width = 1024, height = 768)
png(file="poissong1.png", bg="transparent" )# width=5.6, height=3.8, units='in', res=596)
par(mar=c(1,0.5,1,0.5))
plot(poisson_g1, vertex.size=13, layout=layout_nicely)
dev.off()
save(poisson_g1, file="poissong1.Rdata")
distances(poisson_g1)





barabasi_g1 <- barabasi.game(50, power=2.1, m=2, out.pref=FALSE, directed=FALSE, zero.appeal=1)
mean(igraph::degree(barabasi_g1))  # 3.88
mean_distance(barabasi_g1, directed=FALSE)
diameter(barabasi_g1)
bg1_d <- get.diameter(barabasi_g1)
E(barabasi_g1)$color = "grey"
E(barabasi_g1)$width = 1.6
E(barabasi_g1, path=bg1_d)$color = "#F67770"
E(barabasi_g1, path=bg1_d)$width = 2
V(barabasi_g1)$color = "#8ABCDD"
V(barabasi_g1)[bg1_d]$color = "#F67770"
dev.print(png, file = "barabasig1.png", width = 1024, height = 768)
png(file="barabasig1.png", bg="transparent" )# width=5.6, height=3.8, units='in', res=596)
par(mar=c(1,0.5,1,0.5))
plot(barabasi_g1, vertex.size=13, layout=layout_nicely)
dev.off()

png(file="histdist1.png", bg="transparent", width=1024, height=768)
par(mfrow=c(1,2), cex.main=1, cex.axis=1.5, font.lab=2, cex.lab=1.5)
hist(distances(poisson_g1), breaks=5, main="Random Network", xlab="geodesic distance", ylab="percentage of geodesics", col="#28C2C6")
hist(distances(barabasi_g1), breaks=5, main="Scale-Free Network", xlab="geodesic distance", ylab=NULL, xlim=c(0,6), col="#AED8E5")
dev.off()
