## This is the companion code to my paper - Structural identification and estimation of 
## peer effects using network data 
## @ Fei (Michael) Wang 
## Student ID: 942870 

# mode 1  - MLE 

# dependent varaible:
        #gpa , german, math, english, in 1970
# independent variable: 
        # previous year gpa, continuous 
        # gender (dummy )
        # age continuous 
        # IQ
        # prestige, continous 
        # mother education university or not  (dummy)
        # father education university or not (dummy)
 
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


##################       Part I      ###################
##  prepare dataset 

# whole dataset for model 
kgpclean %>%  # select 
        dplyr::select(studentid, school, class, sch_gpa_10, sch_score_70_3,
                sch_score_70_8, sch_score_70_13,sch_gpa_68, sch_score_68_3,
                sch_score_68_8, sch_score_68_13, fem, age,
                iq, prestige, edu_motherd2, edu_fatherd2) %>%  # create new variable
        mutate(gpa_diff = sch_gpa_10 - sch_gpa_68) -> modedata  

### Mode 1a 

# covariates will be selected automatically based on the name 

# specify the formula first, names have to be same with the dataset 
sar_mode1a <- formula("sch_gpa_10 ~ sch_gpa_68 + fem + age + iq + prestige + edu_motherd2 +
        edu_fatherd2")

sarG1a <- kgpcleanblock_undirt  # adjacency matrix for model 1, undirect one 
# find the initial phi value 
phi_initiala <- 1/max(eigen(sarG1a)$values)  # the reciprocial of largest eigenvalue 

starting1a <- c(alpha = 0.47352, beta_sch_gpa_68 = 0.3268, beta_fem = 0.0316, 
        beta_age = 0.0289, beta_iq = 0.0465, beta_prestige = 0.1289, 
        beta_edu_motherd2 = 0.0356, beta_edu_fatherd2 = 0.054, 
        phi = phi_initiala)

sar_lim_mode1a <- net_dep(formula = sar_mode1a, data = modedata, 
        G = sarG1a, model = "model_B", estimation = "NLLS", 
        hypothesis = "lim", start.val = starting1a)  #Caution: it takes time

summary(sar_lim_mode1a)
plot(sar_lim_mode1a$second_step, main = "Mode 1 without considering endogenous issuse")
qqnorm(sar_lim_mode1a$second_step)  ## qqnormal



# Mode 1b
sar_mode1b <- formula("sch_gpa_10 ~ sch_gpa_68 + fem + age + iq + prestige + edu_motherd2 +
        edu_fatherd2")
sarG1b <- kgpcleanblock_undirt 
rownames(sarG1b) <- rownames(modedata)  # need assign row id to match matrix 
phi_initialb <- 1/max(eigen(sarG1b)$values)

# use kgpcleanblock (direct adjacency matrix) as exclusion restriction 

starting1b <- c(alpha = 0.7352, beta_sch_gpa_68 = 0.6268, beta_fem = 0.0316, 
        beta_age = 0.0089, beta_iq = 0.0465, beta_prestige = 0.1289, 
        beta_edu_motherd2 = 0.0356, beta_edu_fatherd2 = 0.054, 
        phi = phi_initialb, unobservables = 0.0709)

sar_lim_mode1b <- net_dep(formula = sar_mode1b, data = modedata, 
        G = sarG1b, model = "model_B", estimation = "NLLS", endogeneity = T, 
        correction = "heckman", first_step = "standard", 
        exclusion_restriction = kgpcleanblock,
        hypothesis = "lim", start.val = starting1b)  #Caution: it takes time

# saveRDS(sar_lim_mode1b, file = "sarmode1b.rds")
summary(sar_lim_mode1b)
plot(sar_lim_mode1b$second_step)  # plot residuals 
qqnorm(sar_lim_mode1b$second_step)  # qqplot of residuals 
summary(sar_lim_mode1b$first_step)

# print out coefficents, std, p-values in Latex code 
cef1b1 <- summary(sar_lim_mode1b$first_step)
coef1b2 <- summary(sar_lim_mode1b$second_step)
stargazer(cef1b1$coefficients, digits = 4) 
stargazer(coef1b2$coefficients, digits = 4)

## finish all estimations of model 1 in table 5.2 