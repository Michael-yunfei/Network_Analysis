## This is the companion code to my paper - Structural identification and estimation of 
## peer effects using network data 
## @ Fei (Michael) Wang 
## Student ID: 942870 

### Clean and tidy the data, it construct the main dataset and matrix will be used later 

# structure of coe
        # part 0 load packages
        # get clean dataset, block matrix (direct and undirect), save them as Rdata
        # package citate 

##################       Part 0      ###################
# During the data analysis and model estimation process, we will load many packages
# Please install those packages if you don't have it in your R environment

rm(list = ls())  # clear the working memoery 
# Please set the working directory which includes the dataset - data_network2.csv
setwd("/Users/Michael/Library/Mobile Documents/com~apple~CloudDocs/Economics/Network/Code and Programming")

pkgs <- c("igraph", "tidyverse", "magic", "network", "ergm", "econet",
        "sna", "Hmisc", "stargazer", "xtable")
for (i in 1:length(pkgs)){
        if (pkgs[i] %in% row.names(installed.packages()) == F) {
                install.packages(pkgs[i])
        }
}  # check packages and intall all 
lapply(pkgs, require, character.only = TRUE)    
                
# load all packages, 
# if it is false, please install, like install.packages("magic")

source("PeerEffect_fun.R")  # make sure PeerEffect_fun.R is in the same working file with
# the current transcript and dataset 

# adjmatrixfun.R includes 6 written functions 


##################       Part I.1     ###################
## Clean Data by removing NAs 
## give descriptive statistics 
## Primay tests: test the network components are Erdos-Renyi or Barabasi-Albert model 


kgp <- read_csv(file = "data_network2.csv")
is.data.frame(kgp)  # make sure it is dataframe format 
dim(kgp)  # dimensions of dataset
names(kgp)  # check the variable names

## only focus on the network data, don't remove NAs for covariate variables 

# remove NA based on school
kgp1 <- filter(kgp, !is.na(school))  # filter out NA from school 
length(unique(kgp1$school))  # 68 schools 
unique(kgp1$school)  #  list all indicator there is no NA
summary(kgp1[, 33:81])  # list the summary of all covariate variables

# the summary shows there is 795 NA in sch_gap_68, there are many NAs in edu_fater
# we will come back to deal with those NAs when we do estimation 
# there is no NAs in school and class, we can begin to construct the adjacency matrix 
# use the function written by myself to build up the adjacency matrix
# the function is called datatolist 


kgpmatrix <- datatolist(kgp1)  # use function to get the matrix list 
length(kgpmatrix)  # we got 121 matries 
# check the empty matrix and all zero matrics 
checkmatrix(kgpmatrix)
# no empty matrix, but it doesn't mean no empty network. It might be all 0 in matrix 

# Block all matrices 
kgpblock <- kgpmatrix[[1]]  # initial the block matrix by assigning the first one 
for (i in 2:length(kgpmatrix)) {
        kgpblock <- adiag(kgpblock, kgpmatrix[[i]])  # use adiag function from magic package
        
}
dim(kgpblock)  # check the dimension, which is same with the number of observations 


# tidy the adjacency matrix into symmetric one, which means treat the network more strictly
# use the function wirtten by me called dir_to_undir(), which is in the same function file 


kgpmatrix_undirt <- dir_to_undir(kgpmatrix)  # use function to change direct to undirect
# check empty and zero matrices again 
checkmatrix(kgpmatrix_undirt)
# block undirected (mutual friendship) matrix 
kgpblock_undirt <- kgpmatrix_undirt[[1]]  # initial the block matrix by assigning the first one 
for (i in 2:length(kgpmatrix_undirt)) {
        kgpblock_undirt <- adiag(kgpblock_undirt, kgpmatrix_undirt[[i]])  # use adiag function from magic package
        
}
dim(kgpblock_undirt)  # check the dimension, which is same with the number of observations 


### Now with block matrix, it's very easy to get the descriptive statistics of network

## The following code gives table 4.1 
bignet1 <- graph.adjacency(kgpblock, mode = "directed")  # direct one 
net1degreein <- degree(bignet1, mode = "in")  # in - degree
net1degreeout <- degree(bignet1, mode = "out")  # out - degree 
summary(net1degreein)
hist(net1degreein)
summary(net1degreeout)
hist(net1degreeout)

bignet2 <- graph.adjacency(kgpblock_undirt, mode = "undirected")
net2degree <- degree(bignet2)
summary(net2degree)
hist(net2degree)

# library(network)  # pakcage to calcuate the denstiy
network.density(network(kgpblock, directed = T))
network.density(network(kgpblock_undirt, directed = F))

# Fit with Bernoulli and Erdos-Reny models (check whether it is random network or not)
# logic: use average density to simulate 100 networks with same size and compare 
# library(ergm)  # need ergm function 
# the following code gives figure 4.1 
net2fit <- ergm(network(kgpblock_undirt, directed = F) ~ edges)  # fit with only edges 
summary(net2fit)
expit(net2fit$coef)  # should be same with average density 
net2check <- gof(net2fit, GOF = ~distance + espartners + triadcensus,
        verbose = T, 
        interval = 5e+4)  # running simulation takes time 
# plot(net2check) # check PeerEffect_Plot.R 

# saveRDS(net2check, file = "ergmfit.rds")  # save simulated results to use it later 



##################       Part I.2    ###################
##### Clean data again and remove all NAs 
### 

colSums(is.na(kgp1[, 33:81]))  # covariate variables NA check 
max(colSums(is.na(kgp1[, 33:81])))  # maximum is 875
which(colSums(is.na(kgp1[, 33:81])) == max(colSums(is.na(kgp1[, 33:81]))))  # it is sch_score_70_8, german grade 

# we will try to remove all NAs from covariate variables (kgp1[, 33:81])

kgpclean <- kgp1[complete.cases(kgp1[, 33:81]), ]
dim(kgpclean)  # 2320 by 81 
max(colSums(is.na(kgpclean[, 33:81])))  # 0 NAs 

## gives the summary of interested variables 

kgpclean %>%  # select 
        dplyr::select(studentid, school, class, sch_gpa_10, sch_score_70_3,
                sch_score_70_8, sch_score_70_13,sch_gpa_68, sch_score_68_3,
                sch_score_68_8, sch_score_68_13, fem, age,
                iq, prestige, edu_motherd2, edu_fatherd2) %>%  # create new variable
        mutate(gpa_diff = sch_gpa_10 - sch_gpa_68) -> modedata  
dim(modedata)
describe(modedata[, 4:17])  # print out all descriptive statistics 
stargazer(as.data.frame(modedata[, 4:18]))  # use package stargazer to generate latex table


## Now, we will use kgpclean to construct the matrix again 

kgpcleanmat <- datatolist(kgpclean)  # direct one 
length(kgpcleanmat)
checkmatrix(kgpcleanmat)  # no empty matrics and no all zero matrics

kgpcleanmat_undirt <- dir_to_undir(kgpcleanmat)  # convert it to undirected 
length(kgpcleanmat_undirt)
checkmatrix(kgpcleanmat_undirt)


# do block 
kgpcleanblock <- kgpcleanmat[[1]]  # initial the block matrix by assigning the first one 
for (i in 2:length(kgpcleanmat)) {
        kgpcleanblock <- adiag(kgpcleanblock, kgpcleanmat[[i]])  # use adiag function from magic package
        
}
dim(kgpcleanblock)  # check the dimension, which is same with the number of observations 

kgpcleanblock_undirt <- kgpcleanmat_undirt[[1]]  # initial the block matrix by assigning the first one 
for (i in 2:length(kgpcleanmat_undirt)) {
        kgpcleanblock_undirt <- adiag(kgpcleanblock_undirt, kgpcleanmat_undirt[[i]])  # use adiag function from magic package
        
}
dim(kgpcleanblock_undirt)  # check the dimension, which is same with the number of observations 

# check PeerEffect_Plot.R for Figure C.2 


### Save cleaned data and matrix into Rdata, so we can call them later 
save(kgpclean, file = "kgpclean.Rdata")
save(kgpcleanblock, file = "kgpcleanblock.Rdata")
save(kgpcleanblock_undirt, file = "kgpcleanblock_undirt.Rdata")
save(kgpcleanmat, file = "kgpcleanmat.Rdata")
save(kgpcleanmat_undirt, file = "kgpcleanmat_undirt.Rdata")

########  Fit ERGM models with covariates again

## undirected first

kgpnetwork1 <- network(kgpcleanblock_undirt, directed = F)  # creat the network from blcok matrix 

# with adjacency matrix and covariate variables, we can fit expoential familay model again
# first, we need assign attributes to network
# we will select age, gender, gpa68, gap10, iq and prestige as covariates 

kgpclean %>%
        dplyr::select(studentid, age, fem, sch_gpa_68, sch_gpa_10, iq, prestige) -> kgpattributes
dim(kgpattributes)  # 2320 by 7 
# until now, there is no function from network package to set multiple attributes
# so we use for loop to do this 

for (i in 2:dim(kgpattributes)[2]){  # first column is ID, so start it from 2 
        set.vertex.attribute(kgpnetwork1,names(kgpattributes)[i], kgpattributes[[i]])  # use [[]] to extract value
}
summary(kgpnetwork1) # check out attributes
# commet: when extracting values, please be careful, if you do [i], it will give a list 
# not a vector, the vertext attributes will become different 
# to check your setting is right or not, always use summary() to glipse the structure 

# undirect network fit 
kgpexp1 <- ergm(kgpnetwork1 ~ edges + nodematch("age") + nodematch("fem")
        + nodematch("prestige") + nodematch("sch_gpa_68")
        + nodematch("iq")  + nodematch("sch_gpa_10"))
summary(kgpexp1)  # age, gender and prstiage is significant on the probability of tie 

## only two variabes are significant, edges and gpa68 
expit(kgpexp1$coef)  ## calculate the probability 

# check the fitness 
kgpexp1fit <- gof(kgpexp1, GOF = ~distance + espartners + triadcensus + degree,
        verbose = T, 
        interval = 5e+4)

# saveRDS(kgpexp1fit, file = "kgpexp1fit.rds")  # you don't have to save if you don't need it later 
# check PeerEffect_Plot.R for Figure C.3

## use directed one to check is there any differences 

kgpnetwork2 <- network(kgpcleanblock, directed = T)  # directed one
# assign attributes 
for (i in 2:dim(kgpattributes)[2]){  # first column is ID, so start it from 2 
        set.vertex.attribute(kgpnetwork2,names(kgpattributes)[i], kgpattributes[[i]])  # use [[]] to extract value
}
summary(kgpnetwork2) # check out attributes

kgpexp2 <- ergm(kgpnetwork2 ~ edges  +  nodematch("age") + 
                nodematch("fem")
        + nodematch("prestige") + nodematch("sch_gpa_68")
        + nodematch("iq")  + nodematch("sch_gpa_10"))
summary(kgpexp2) 

expit(kgpexp2$coef)

kgpexp2fit <- gof(kgpexp2, GOF = ~distance + espartners + triadcensus + odegree,
        verbose = T, 
        interval = 5e+4)

# check PeerEffect_Plot.R for Figure C.3

##################       Part II      ###################

## Package ciatations 

lapply(pkgs, citation)
