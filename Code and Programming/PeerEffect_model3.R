## This is the companion code to my paper - Structural identification and estimation of 
## peer effects using network data 
## @ Fei (Michael) Wang 
## Student ID: 942870 

# mode 3 - spartial autocorrelation model 

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


##################       Part I Estimate     ###################

# Dependent Variable: average gpa 
kgpclean %>%
        dplyr::select(sch_gpa_68, fem, age , iq , prestige ,edu_motherd2, edu_fatherd2) -> ac3X

ac3X$fix_effect <- matrix(1, dim(kgpclean)[1])  # add vector u 

# as autocorrelation really takes time to run, so we just use first half observations
mode3 <- lnam(kgpclean$sch_gpa_10[1:600], data.matrix(ac3X)[1:600, ], 
        kgpcleanblock_undirt[1:600, 1:600],
        kgpcleanblock_undirt[1:600, 1:600], tol = 1e-8)  #Caution: it takes time 
# once number of observation is greater than 600, it takes half an hour to get results
# saveRDS(mode3, file = "sarmode3a.rds")
summary(mode3)
plot(mode3)
# check PeerEffect_Plot.R for saving plot, figure c.7  

# mode 3b with GX
mode3bGx <- kgpcleanblock_undirt %*% data.matrix(ac3X[, 1:7])

mode3bX <- cbind(data.matrix(ac3X), mode3bGx)

mode3b <- lnam(kgpclean$sch_gpa_10[1:300], mode3bX[1:300, ], 
        kgpcleanblock_undirt[1:300, 1:300],
        kgpcleanblock_undirt[1:300, 1:300], tol = 1e-8)  #Caution: it takes time 

summary(mode3b)
plot(mode3b)
#saveRDS(mode3b, file = "sarmode3b.rds")
# check PeerEffect_Plot.R for saving plot, figure c.8 


##  Dependent Variable: math 
kgpclean %>%
        dplyr::select(sch_score_68_3, fem, age , iq , prestige ,edu_motherd2, edu_fatherd2) -> ac3Xm

ac3Xm$fix_effect <- matrix(1, dim(kgpclean)[1])  # add vector u 

# as autocorrelation really takes time to run, so we just use first half observations
mode3m <- lnam(kgpclean$sch_score_70_3[1:500], data.matrix(ac3Xm)[1:500, ], 
        kgpcleanblock_undirt[1:500, 1:500],
        kgpcleanblock_undirt[1:500, 1:500], tol = 1e-8)  #Caution: it takes time 
# once number of observation is greater than 600, it takes half an hour to get results
# saveRDS(mode3, file = "sarmode3a.rds")
summary(mode3m)
plot(mode3m)

mode3mresults <- cbind(mode3m$beta, mode3m$beta.se, mode3m$beta/mode3m$beta.se)
colnames(mode3mresults) <- c("Estimate", "Std.Error", "Z value")
mode3mrho1 <- cbind(mode3m$rho1, mode3m$rho1.se, mode3m$rho1/mode3m$rho1.se)
mode3mrho2 <- cbind(mode3m$rho2, mode3m$rho2.se, mode3m$rho2/mode3m$rho2.se)
mode3mresults <- rbind(mode3mresults, mode3mrho1 , mode3mrho2)

stargazer(mode3mresults, digits = 4)


##  Dependent Variable: English
kgpclean %>%
        dplyr::select(sch_score_68_13, fem, age , iq , prestige ,edu_motherd2, edu_fatherd2) -> ac3Xe

ac3Xe$fix_effect <- matrix(1, dim(kgpclean)[1])  # add vector u 

# as autocorrelation really takes time to run, so we just use first half observations
mode3e <- lnam(kgpclean$sch_score_70_13[1:500], data.matrix(ac3Xe)[1:500, ], 
        kgpcleanblock_undirt[1:500, 1:500],
        kgpcleanblock_undirt[1:500, 1:500], tol = 1e-8)  #Caution: it takes time 
# once number of observation is greater than 600, it takes half an hour to get results
# saveRDS(mode3, file = "sarmode3a.rds")
summary(mode3e)
plot(mode3e)

mode3eresults <- cbind(mode3e$beta, mode3e$beta.se, mode3e$beta/mode3e$beta.se)
colnames(mode3eresults) <- c("Estimate", "Std.Error", "Z value")
mode3erho1 <- cbind(mode3e$rho1, mode3e$rho1.se, mode3e$rho1/mode3e$rho1.se)
mode3erho2 <- cbind(mode3e$rho2, mode3e$rho2.se, mode3e$rho2/mode3e$rho2.se)
mode3eresults <- rbind(mode3eresults, mode3erho1 , mode3erho2)

stargazer(mode3eresults, digits = 4)

##  Dependent Variable: German
kgpclean %>%
        dplyr::select(sch_score_68_8, fem, age , iq , prestige ,edu_motherd2, edu_fatherd2) -> ac3Xg

ac3Xg$fix_effect <- matrix(1, dim(kgpclean)[1])  # add vector u 

# as autocorrelation really takes time to run, so we just use first half observations
mode3g <- lnam(kgpclean$sch_score_70_8[1:500], data.matrix(ac3Xg)[1:500, ], 
        kgpcleanblock_undirt[1:500, 1:500],
        kgpcleanblock_undirt[1:500, 1:500], tol = 1e-8)  #Caution: it takes time 
# once number of observation is greater than 600, it takes half an hour to get results
# saveRDS(mode3, file = "sarmode3a.rds")
summary(mode3g)
plot(mode3g)

mode3gresults <- cbind(mode3g$beta, mode3g$beta.se, mode3g$beta/mode3g$beta.se)
colnames(mode3gresults) <- c("Estimate", "Std.Error", "Z value")
mode3grho1 <- cbind(mode3g$rho1, mode3g$rho1.se, mode3g$rho1/mode3g$rho1.se)
mode3grho2 <- cbind(mode3g$rho2, mode3g$rho2.se, mode3g$rho2/mode3g$rho2.se)
mode3gresults <- rbind(mode3gresults, mode3grho1 , mode3grho2)

stargazer(mode3gresults, digits = 4)

## finish all estimations 
