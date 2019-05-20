## This is the companion code to my paper - Structural identification and estimation of 
## peer effects using network data 
## @ Fei (Michael) Wang 
## Student ID: 942870 


## simulation on model 2:  2SLS 

# it takes around 10 minutes to run the whole simuations 

# The simulation part will only use two models:
# model (2) 2SLS G 
# model (2) 2SLS Ghat 

# dependent varaible:
        # simulaton will only focus on average gpa 
# independent variable: 
        # previous year gpa, continuous 
        # gender (dummy )
        # age continuous 
        # IQ
        # prestige, continous 
        # mother education university or not  (dummy)
        # father education university or not (dummy)
        # GX 

##################       Part 0      ###################
## load essential pakcages and dataset
rm(list = ls())  # clear the working memoery 
# Please set the working directory which includes the dataset - data_network2.csv
setwd("/Users/Michael/Library/Mobile Documents/com~apple~CloudDocs/Economics/Network/Code and Programming")

pkgs <- c("igraph", "tidyverse", "magic", "network", "ergm", "econet",
        "sna", "Hmisc", "stargazer", "xtable", "stringr")
for (i in 1:length(pkgs)){
        if (pkgs[i] %in% row.names(installed.packages()) == F) {
                install.packages(pkgs[i])
        }
}  # check packages and intall all 
lapply(pkgs, require, character.only = TRUE)  # load all packages    

source("PeerEffect_fun.R")  # Load all functions, please read instruction on functions
# Load all essential data, please read the instrcution on those datasets and matrix
load("kgpclean.Rdata")
load("kgpcleanblock.Rdata")
load("kgpcleanblock_undirt.Rdata")
load("kgpcleanmat.Rdata")
load("kgpcleanmat_undirt.Rdata")


##################       Part I  Simulation    ###################

# simulate 100 times 
set.seed(56897)  # set seed to make sure the simulation can be replicated.


# create one matrix to store all simulated values, one matirx to stroe the density of network 
simresults <- matrix(0,400, 6)  # 400 (4 x 100) by 6, 4 coefficents will be selected
simdensity <- matrix(0, 100, 4)  # 100 by 4, 2-for density, 2-for mean_shortest_distance

n = 0 

for (si in 1:100) {
        n = n+1 
        
        ##  prepare the data: Y, G, X 
        samplenetwork <- sample(1:101, 50, replace = F)  # generate 50 random numbers from 101
        # subnetworks; we will run 100 simulations 
        
        sublist <- kgpcleanmat_undirt[samplenetwork] # get sublist of all network 
        
        # do block 
        G <- sublist[[1]]  # initial the block matrix by assigning the first one 
        for (i in 2:length(sublist)) {
                G <- adiag(G, sublist[[i]])  # use adiag function from magic package
                
        }

        # get corresponding dataset 
        listname <- names(sublist)
        schclassid <- matrix(0, length(listname), 2)
        for (i in 1:length(listname)){
                chr <- as.numeric(str_extract_all(listname[i], "\\d+")[[1]])
                schclassid[i, 1:2] <- chr
                
        }
        cdataset <- data.frame()
        for (i in 1:length(listname)){
                kgpclean %>%
                        filter(school == schclassid[i, 1]) %>%
                        filter(class == schclassid[i, 2]) -> sc 
                cdataset <- rbind(cdataset, sc)
                
        }
        
      
        cdataset %>%
                dplyr::select(sch_gpa_68, fem, age , iq , prestige, 
                        edu_motherd2, edu_fatherd2) ->simX

        # Mode (2) 2SLS G 
        msls2sim <- netw2sls(cdataset,cdataset$sch_gpa_10,G,simX)
        
        # extract the results 
        select_coe <- rbind(msls2sim$results[c(1, 2, 5), ],msls2sim$alpha)
        
        # store the values
        simresults[(4*(si-1)+1):(4*si), 1:3] <- select_coe  # be careful on the order of operator 
       
        
        # Mode (2) 2SLS Ghat 
        simnet <- network(G, directed = F)
        simgraph <- graph.adjacency(G, mode = "undirected")
        simdensity[si, 1] <- network.density(simnet)  # store the density 
        simdensity[si, 3] <- mean_distance(simgraph, directed = F, unconnected = T)
        
        cdataset %>%
                dplyr::select(fem, sch_gpa_68, age, prestige, iq) -> attr
        
        for (i in 1:dim(attr)[2]){  # first column is ID, so start it from 2 
                set.vertex.attribute(simnet,names(attr)[i], attr[[i]])  # use [[]] to extract value
        }
        
        # fit ergm model
        sim_ergmfit <- ergm(simnet ~ edges + nodematch("fem") +
                        nodematch("sch_gpa_68") + nodematch("age") 
                + nodematch("prestige")
                + nodematch("iq"))
        
        ghat_ergmsim <- simulate(sim_ergmfit, nsim = 30)
        indt <- sample(1:30, 1, replace=F)
        simdensity[si, 2] <- network.density(ghat_ergmsim[[indt]])  # store the density 
        
        # get Ghat 
        Ghat <- as.matrix.network(ghat_ergmsim[[indt]], matrix.type = "adjacency")
        
        simgraphhat <- graph.adjacency(Ghat, mode = "undirected")
        simdensity[si, 4] <- mean_distance(simgraphhat, directed = F, unconnected = T)
        
        # 2SLS estimation 
        msls2sim_ghat <- netw2sls(cdataset, cdataset$sch_gpa_10, Ghat, simX)
        
        # extract the results 
        select_coe_ghat <- rbind(msls2sim_ghat$results[c(1, 2, 5), ],msls2sim_ghat$alpha)
        
        # store the values
        simresults[(4*(si-1)+1):(4*si), 4:6] <- select_coe_ghat
}


# end the simulation 

##################       Part II  Simulation Analysis    ###################

dim(simresults)
dim(simdensity)
simresults <- as.data.frame(simresults)
simdensity <- as.data.frame(simdensity)
colnames(simresults) <- c("Estimation(G)", "Std.Error(G)", "t-value(G)",
        "Estimation(Ghat)", "Std.Error(Ghat)", "t-value(Ghat)")
colnames(simdensity) <- c("Density(G)", "Density(Ghat)", "Mean_distance(G)", 
        "Mean_distance(Ghat)")

# for the row of simrults are:
        # phi
        # previous gpa (68)
        # iq
        # alpha 

coefnames <- rep(c("phi", "pre_gpa", "iq", "alpha"), dim(simdensity)[1])
simresults$coefnames <- coefnames

head(simresults)
simresults <- simresults[, c(7, 1:6)]  # change the order of columns 
head(simresults)
tail(simresults)

# save data
save(simresults, file = "simresults.Rdata")
save(simdensity, file = "simdensity.Rdata")

## Now calculate the rejection rate, use 1.645 as benche mark 

simresults %>%
        mutate(Grj = if_else(abs(`t-value(G)`) >=1.645, 1, 0)) %>%
        mutate(Ghatrj = if_else(abs(`t-value(Ghat)`) >= 1.645, 1, 0)) ->simresults2 

simresults2 %>%
        group_by(coefnames) %>% 
        summarise(Grej_rate = mean(Grj), Ghatrej_rate = mean(Ghatrj)) ->rejection_rate

stargazer(as.matrix(rejection_rate))  # print it out as latex format 


## plot all estimates based on degree and distance. , it gives figure 5.2

pdf(file = "sim_densityhist.pdf", width = 5.6, height = 3.8)
par(cex.main = 1, font.main = 1)
hist(simdensity$`Density(G)`, col = "lightblue", xlab = "Density",
        main = "Histogram of Network Density from Simulation")
hist(simdensity$`Density(Ghat)`,col = rgb(0,0,0.3,0.2), add = T)
legend(0.00226, 28, legend = c("Original adjacency matrix - G",
        "Estimated adjacency matrix - Ghat"), pch=15, bty = "n",y.intersp = 2,
        cex = 0.5, col = c("lightblue",rgb(0,0,0.3,0.2)), pt.cex = 1)
dev.off()


pdf(file = "sim_distancehist.pdf", width = 5.6, height = 3.8)
par(cex.main = 1, font.main = 1)
hist(simdensity$`Mean_distance(G)`, xlim = c(2,9),breaks = 10,
        col = "lightblue", xlab = "Average Shortest Distance among all nodes",
        main = "Histogram of Network Average Shortest Distance from Simulation")
hist(simdensity$`Mean_distance(Ghat)`, xlim = c(2,9), breaks = 20, 
        col = rgb(0,0,0.3,0.2), add = T)
legend("topright", legend = c("Original adjacency matrix - G",
        "Estimated adjacency matrix - Ghat"), pch=15, bty = "n",y.intersp = 1.3,
        cex = 0.6, col = c("lightblue",rgb(0,0,0.3,0.2)), pt.cex = 1)
dev.off()


## exam the power size with degree and distance  


simresults %>%
        dplyr::filter(coefnames == "phi") -> simphi 

# plot of phi, gives figure 5.1 
pdf(file = "sim_phi.pdf", width = 5.6, height = 3.8)
par(cex.main = 1, font.main = 1)
hist(simphi$`Estimation(G)`,  col = "#AED8E5", xlim = c(-0.1, 0.3),
        xlab = "Estimations of Peer Effect", 
        main = "Histogram of Peer Effect Estimation from Simulation")
hist(simphi$`Estimation(Ghat)`, col = rgb(0,0,0.3,0.2), xlim = c(-0.1, 0.3), add = T)
legend(-0.1, 26, legend = c("Original adjacency matrix - G",
        "Estimated adjacency matrix - Ghat"), pch=15, bty = "n",y.intersp = 1.3,
        cex = 0.6, col = c("#AED8E5",rgb(0,0,0.3,0.2)), pt.cex = 1)
dev.off()

pdf(file = "sim_iq.pdf", width = 5.6, height = 3.8)
par(cex.main = 1, font.main = 1)
hist(simiq$`Estimation(G)`, col = "#AED8E5", xlab = "Estimation of IQ",
        main = "Histogram of Estimatiosn of IQ from Simulation")
hist(simiq$`Estimation(Ghat)`, col = rgb(0,0,0.3,0.2), add = T)
legend(-0.0045, 24, legend = c("Original adjacency matrix - G",
        "Estimated adjacency matrix - Ghat"), pch=15, bty = "n",y.intersp = 1.3,
        cex = 0.6, col = c("#AED8E5",rgb(0,0,0.3,0.2)), pt.cex = 1)
dev.off()

simphiall <- cbind(simphi, simdensity)


### correlation matrix analysis 

cordataphi <- cor(simphiall[, c(4, 7, 8:11)])

# corrplot(cordataphi, method = "pie")  # plot the correlation for fun, need corrplot package 
# 
        
tvaluecorphi <-  rcorr(as.matrix(simphiall[, c(4, 7, 8:11)]))

stargazer(round(tvaluecorphi$r, 4))  # print out as table 

stargazer(round(tvaluecorphi$P, 4)) 


simresults %>%
        dplyr::filter(coefnames == "iq") -> simiq 

simiqall <- cbind(simiq, simdensity)

# corrplot(cor(simiqall[, c(4, 7, 8:11)]))  # plot the correlation for fun, need corrplot package 

tvaluecoriq <-  rcorr(as.matrix(simiqall[, c(4, 7, 8:11)]))

stargazer(round(tvaluecoriq$r, 4))  # print out as table 

stargazer(round(tvaluecoriq$P, 4)) 





















