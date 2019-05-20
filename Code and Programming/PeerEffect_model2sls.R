## This is the companion code to my paper - Structural identification and estimation of 
## peer effects using network data 
## @ Fei (Michael) Wang 
## Student ID: 942870 

# mode 2  - 2SLS 

# dependent varaible:
        #gpa , german, math, english, in 1970
# independent variable: 
        # previous year gpa, german, math, english in 1968 
        # gender (dummy )
        # age continuous 
        # IQ
        # prestige, continous 
        # mother education university or not  (dummy)
        # father education university or not (dummy)
        # GX (mose model here no GX)

####### 2SLS estimation 
## Network fixed effects, formula is from Lecture 2 by Konig, M(2019)
## use the function written by myself: netw2sls 

##################       Part 0      ###################
## load essential pakcages and dataset
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

source("PeerEffect_fun.R")  # Load all functions, please read instruction on functions
# Load all essential data, please read the instrcution on those datasets and matrix
load("kgpclean.Rdata")
load("kgpcleanblock.Rdata")
load("kgpcleanblock_undirt.Rdata")
load("kgpcleanmat.Rdata")
load("kgpcleanmat_undirt.Rdata")


##################       Part I     ###################
# fit with ERGM and get 60 simulated adjacency network matrix 

net <- network(kgpcleanblock_undirt, directed = F) 

kgpclean %>%
        dplyr::select(studentid,  fem, sch_gpa_68, age, prestige, iq) -> attr
dim(attr)  # 2320 by 7 

for (i in 2:dim(attr)[2]){  # first column is ID, so start it from 2 
        set.vertex.attribute(net,names(attr)[i], attr[[i]])  # use [[]] to extract value
}

# summary(two_sls_net) 

# fit ergm model
ergmfit <- ergm(net ~ edges + nodematch("fem") +
                nodematch("sch_gpa_68") + nodematch("age") + nodematch("prestige")
        + nodematch("iq"))

ergmsim <- simulate(ergmfit, nsim = 60)
# summary(two_sls_ergmsim)

##################       Part II   Estimate   ###################

### Depedent Variable: averge gpa

# select the covariate variables 
kgpclean %>%
         dplyr::select(sch_gpa_68, fem, age, iq , prestige, 
                edu_motherd2, edu_fatherd2) -> twoslsX

# run two stage least squre, it takes less then 30 seconds, much faster than MLE 
twosls1 <- netw2sls(kgpclean,kgpclean$sch_gpa_10, kgpcleanblock_undirt, twoslsX)
# to use netw2sls, the first input has to be the full dataset !

twosls1$results
twosls1$alpha
par(xpd=FALSE)  # in case qqline go ousite of graph
qqnorm(twosls1$residuals)
qqline(twosls1$residuals)
plot(x = twosls1$yhat, y = twosls1$residuals)
abline(h = 0, col = 'red', lwd = 2, lty = 2)

#### use predicted matrix as IVs, generate a random number to select from simulated matrix 
randm <- sample(1:60, 1, replace = F)
Ghat <- as.matrix.network(ergmsim[[randm]], matrix.type = "adjacency")
all((Ghat - kgpcleanblock_undirt) == 0)  # not exactly same 

# different Ghat from simulation will give different results, but for most esimations 
# the results are very close to results from other models.
twosls2 <- netw2sls(kgpclean,kgpclean$sch_gpa_10, Ghat, twoslsX)

twosls2$results
twosls2$alpha
qqnorm(twosls2$residuals)
qqline(twosls2$residuals)
plot(x = twosls2$yhat, y = twosls2$residuals)
abline(h = 0, col = 'red', lwd = 2, lty = 2)

# Check PeerEffect_Plot.R for figure C.6


# use difference of gpa as dependent variables, for robustness check 
kgpclean %>%
        dplyr::select(fem, age , iq , prestige ,edu_motherd2, edu_fatherd2) -> twoslsXdiff

twosls3 <- netw2sls(kgpclean,(kgpclean$sch_gpa_10 - kgpclean$sch_gpa_68),
        kgpcleanblock_undirt, twoslsXdiff)

twosls3$results
twosls3$alpha
qqnorm(twosls3$residuals)
qqline(twosls3$residuals)
plot(x = twosls3$yhat, y = twosls3$residuals)
abline(h = 0, col = 'red', lwd = 2, lty = 2)

twosls4 <- netw2sls(kgpclean,(kgpclean$sch_gpa_10 - kgpclean$sch_gpa_68),
        Ghat, twoslsXdiff)

twosls4$results
twosls4$alpha
plot(x = twosls4$yhat, y = twosls4$residuals)
abline(h = 0, col = 'red', lwd = 2, lty = 2)


### Depedent Variable: Math 

# select the covariate variables 
kgpclean %>%
        dplyr::select(sch_score_68_3, fem, age , iq , prestige ,edu_motherd2, edu_fatherd2) -> twoslsXm

# run two stage least squre, it takes less then 30 seconds, much faster than MLE 
twoslsm <- netw2sls(kgpclean,kgpclean$sch_score_70_3, kgpcleanblock_undirt, twoslsXm)
# to use netw2sls, the first input has to be the full dataset !

twoslsm$results
twoslsm$alpha
stargazer(rbind(twoslsm$results, twoslsm$alpha), digits = 4)
qqnorm(twoslsm$residuals)
qqline(twoslsm$residuals)
plot(x = twoslsm$yhat, y = twoslsm$residuals)
abline(h = 0, col = 'red', lwd = 2, lty = 2)

#### use predicted matrix as IVs, generate a random number to select from simulated matrix 
randm <- sample(1:60, 1, replace = F)
Ghatm <- as.matrix.network(ergmsim[[randm]], matrix.type = "adjacency")
all((Ghatm - kgpcleanblock_undirt) == 0)  # not exactly same 

# different Ghat from simulation will give different results, but for most esimations 
# the results are very close to results from other models.
twoslsm2 <- netw2sls(kgpclean,kgpclean$sch_score_70_3, Ghatm, twoslsXm)

twoslsm2$results
twoslsm2$alpha
stargazer(rbind(twoslsm2$results, twoslsm2$alpha), digits = 4)
qqnorm(twoslsm2$residuals)
qqline(twoslsm2$residuals)
plot(x = twoslsm2$yhat, y = twoslsm2$residuals)
abline(h = 0, col = 'red', lwd = 2, lty = 2)


### Depedent Variable: English

# select the covariate variables 
kgpclean %>%
        dplyr::select(sch_score_68_13, fem, age , iq , prestige ,edu_motherd2, edu_fatherd2) -> twoslsXe

# run two stage least squre, it takes less then 30 seconds, much faster than MLE 
twoslse <- netw2sls(kgpclean,kgpclean$sch_score_70_13, kgpcleanblock_undirt, twoslsXe)
# to use netw2sls, the first input has to be the full dataset !

twoslse$results
twoslse$alpha
stargazer(rbind(twoslse$results, twoslse$alpha), digits = 4)
qqnorm(twoslse$residuals)
qqline(twoslse$residuals)
plot(x = twoslse$yhat, y = twoslse$residuals)
abline(h = 0, col = 'red', lwd = 2, lty = 2)

#### use predicted matrix as IVs, generate a random number to select from simulated matrix 
randm <- sample(1:60, 1, replace = F)
Ghate <- as.matrix.network(ergmsim[[randm]], matrix.type = "adjacency")
all((Ghate - kgpcleanblock_undirt) == 0)  # not exactly same 

# different Ghat from simulation will give different results, but for most esimations 
# the results are very close to results from other models.
twoslse2 <- netw2sls(kgpclean,kgpclean$sch_score_70_13, Ghate, twoslsXe)

twoslse2$results
twoslse2$alpha
stargazer(rbind(twoslse2$results, twoslse2$alpha), digits = 4)
qqnorm(twoslsm2$residuals)
qqline(twoslsm2$residuals)
plot(x = twoslsm2$yhat, y = twoslsm2$residuals)
abline(h = 0, col = 'red', lwd = 2, lty = 2)


### Depedent Variable: German

# select the covariate variables 
kgpclean %>%
        dplyr::select(sch_score_68_8, fem, age , iq , prestige ,edu_motherd2, edu_fatherd2) -> twoslsXg

# run two stage least squre, it takes less then 30 seconds, much faster than MLE 
twoslsg <- netw2sls(kgpclean,kgpclean$sch_score_70_8, kgpcleanblock_undirt, twoslsXg)
# to use netw2sls, the first input has to be the full dataset !

twoslsg$results
twoslsg$alpha
stargazer(rbind(twoslsg$results, twoslsg$alpha), digits = 4)
qqnorm(twoslsg$residuals)
qqline(twoslsg$residuals)
plot(x = twoslsg$yhat, y = twoslsg$residuals)
abline(h = 0, col = 'red', lwd = 2, lty = 2)

#### use predicted matrix as IVs, generate a random number to select from simulated matrix 
randm <- sample(1:60, 1, replace = F)
Ghatg <- as.matrix.network(ergmsim[[randm]], matrix.type = "adjacency")
all((Ghatg - kgpcleanblock_undirt) == 0)  # not exactly same 

# different Ghat from simulation will give different results, but for most esimations 
# the results are very close to results from other models.
twoslsg2 <- netw2sls(kgpclean,kgpclean$sch_score_70_8, Ghatg, twoslsXg)

twoslsg2$results
twoslsg2$alpha
stargazer(rbind(twoslsg2$results, twoslsg2$alpha), digits = 4)
qqnorm(twoslsg2$residuals)
qqline(twoslsg2$residuals)
plot(x = twoslsg2$yhat, y = twoslsg2$residuals)
abline(h = 0, col = 'red', lwd = 2, lty = 2)

## finish all 2sls estimation 
