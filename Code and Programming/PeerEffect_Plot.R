## This is the companion code to my paper - Structural identification and estimation of 
## peer effects using network data 
## @ Fei (Michael) Wang 
## Student ID: 942870 

## All the code in this file produce the figures in the paper
## This code has to be run with the main scripts - PeerEffect_main.R, 
## PeerEffect_model(1, 2mle, 2sls, 3) etc.

# Figure 1.1 
# generate three networks based on Erdos-Renyi and Barabasi-Albert model

set.seed(165)
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


## Figure 4.1 

pdf(file = "random_fit.pdf",width = 6.8, height = 5.6) 
par(mfrow = c(2,2))
plot(net2check, cex.lab = 1.3, cex.axis = 1.3, plotlogodds = T)
dev.off()

## Figure 4.2 
pdf(file = "network_exm.pdf", width = 7.6, height = 3.6)
par(mfrow = c(1,2), mar = c(1, 1, 1, 1), cex.main = 1, font.main = 1)
plot(graph.adjacency(kgpcleanmat$sch3class1, mode = "directed"), 
        edge.arrow.size = 0.3, edge.width = 1.8, 
        vertex.color = "#8ABCDD", 
        main = "Directed network from School3 Class1") # check out one example
plot(graph.adjacency(kgpcleanmat_undirt$sch3class1, mode = "undirected"),
        edge.width = 1.8, vertex.color = "#8ABCDD",
        main = "Undirected network from School3 Class1")  # check out one example
dev.off()

## Figure C.2 
pdf(file = "degree_net.pdf", width = 8.9, height = 3)
par(mfrow = c(1,3), mar = c(2.6, 2.5, 2, 1.8), cex.main = 1, font.main = 1)
hist(degree(graph.adjacency(kgpcleanblock, mode = "directed"),
        mode = "in"), main = "Directed: in-degree distribtuon")
hist(degree(graph.adjacency(kgpcleanblock, mode = "directed"),
        mode = "out"), main = "Directed: out-degree distribtuon")
hist(degree(graph.adjacency(kgpcleanblock_undirt, mode = "undirected")),
        main = "Undirected: degree distribtuon")
dev.off()


## Figure C.3 
pdf(file = 'exp1fit.pdf', width = 8.9, height = 5.6)
par(mfrow = c(2,3), cex.main = 1)  # 2 by 3 graphs 
plot(kgpexp1fit, cex.lab = 1.6, cex.axis = 1.6, plotlogodds = T)
dev.off()

# Figure C.4
pdf(file = 'exp2fit.pdf', width = 8.9, height = 5.6)
par(mfrow = c(2,3))  # 2 by 3 graphs 
plot(kgpexp2fit, cex.lab = 1.6, cex.axis = 1.6, plotlogodds = T)
dev.off()

## Figure C.5 
pdf(file = "mode12_resid.pdf", width = 6.8, height = 4.6)
par(mfrow = c(2,2), mar = c(2.5, 2.5, 2, 2), cex.main = 1, font.main = 1)
qn1a <- qqnorm(sar_lim_mode1a$second_step)
plot(x = qn1a$panel.args[[1]]$x, y = qn1a$panel.args[[1]]$y, 
        xlab = "Standardized residuals", ylab = "Quantiles of strandard normal",
        main = "QQplot for Mode 1a")
abline(0, 1)

qn1b <- qqnorm(sar_lim_mode1b$second_step)
plot(x = qn1b$panel.args[[1]]$x, y = qn1b$panel.args[[1]]$y, 
        xlab = "Standardized residuals", ylab = "Quantiles of strandard normal",
        main = "QQplot for Mode 1b")
abline(0, 1)

qn2b <- qqnorm(sar_lim_mode2b$second_step)
plot(x = qn2b$panel.args[[1]]$x, y = qn2b$panel.args[[1]]$y, 
        xlab = "Standardized residuals", ylab = "Quantiles of strandard normal",
        main = "QQplot for Mode 2")
abline(0, 1)
dev.off()


# Figure C.6 
pdf(file = "mode2SLS_resid.pdf", width = 6.8, height = 4.6)
par(mfrow = c(2,2), mar = c(2.5, 2.5, 2, 2), cex.main = 1, font.main = 1)

qqnorm(twosls1$residuals, main = "QQplot for model (2) 2SLS G")
qqline(twosls1$residuals)
plot(x = twosls1$yhat, y = twosls1$residuals, main = "Residual plot for model (2) 2SLS G")
abline(h = 0, col = 'red', lwd = 2, lty = 2)
qqnorm(twosls2$residuals, main = "QQplot for model (2) 2SLS Ghat")
qqline(twosls2$residuals)
plot(x = twosls2$yhat, y = twosls2$residuals, main = "Residual plot for model (2) 2SLS Ghat")
abline(h = 0, col = 'red', lwd = 2, lty = 2)
dev.off()

# Figure C.7
pdf(file = "mode3a_resid.pdf", width = 6.8, height = 4.6)
plot(mode3, main = "Residual Plot for Model (3a)")
dev.off()

# Figure C.8
pdf(file = "mode3b_resid.pdf", width = 6.8, height = 4.6)
plot(mode3b, main = "Residual Plot for Model (3b)")
dev.off()
