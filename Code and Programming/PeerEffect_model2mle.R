## This is the companion code to my paper - Structural identification and estimation of 
## peer effects using network data 
## @ Fei (Michael) Wang 
## Student ID: 942870 

# mode 2  - MLE 

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
        # GX 


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


##################       Part II   Estimate   ###################

### Depedent Variable: averge gpa

# covariate
kgpclean %>%
        dplyr::select(sch_gpa_68, fem, age , iq , prestige ,edu_motherd2, 
                edu_fatherd2) ->sarX

for (i in 1:length(names(sarX))){
        names(sarX)[i] <- paste('G', names(sarX)[i], sep = "")
}
# names(sarX)
sarG <- kgpcleanblock_undirt # use undirected matrix 
sarGX <- sarG %*% data.matrix(sarX) #only multiply the covariates 
modedata2 <- as.data.frame(cbind(modedata, sarGX)) 

sar_mode2b <- formula("sch_gpa_10 ~ sch_gpa_68 + fem + age + iq + 
        prestige + edu_motherd2 + edu_fatherd2 + Gsch_gpa_68 + Gfem +
        + Gage + Giq + Gprestige + Gedu_motherd2 + Gedu_fatherd2")
rownames(sarG) <- rownames(modedata)  # need assign row id to match matrix 
phi_initial2b <- 0.038 # or you can use phi_initiala <- 1/max(eigen(sarG)$values) 

# satring values 
starting2b <- c(alpha = 0.87352, beta_sch_gpa_68 = 0.6268, beta_fem = 0.0116, 
        beta_age = 0.0029, beta_iq = 0.0035, beta_prestige = 0.0059, 
        beta_edu_motherd2 = 0.0356, beta_edu_fatherd2 = 0.054, 
        beta_Gsch_gpa_68 = 0.0068, beta_Gfem = 0.0036, 
        beta_Gage = 0.0029, beta_Giq = 0.0015, beta_Gprestige = 0.0039, 
        beta_Gedu_motherd2 = 0.0036, beta_Gedu_fatherd2 = 0.0054, 
        phi = phi_initial2b, unobservables = 0.0129)

# fit the model, use kgpcleanblock (direct adjacency matrix) as exclusion restriction 
sar_lim_mode2b <- net_dep(formula = sar_mode2b, data = modedata2, 
        G = sarG, model = "model_B", estimation = "NLLS", endogeneity = T, 
        correction = "heckman", first_step = "standard", 
        exclusion_restriction = kgpcleanblock,
        hypothesis = "lim", start.val = starting2b)  #Caution: it takes time

summary(sar_lim_mode2b)  # check PeerEffec_plot.R for figure C.5 

###  Depedent Variable: math 

# prepare data

# covariate
kgpclean %>%
        dplyr::select(sch_score_68_3, fem, age , iq , prestige ,edu_motherd2, 
                edu_fatherd2) ->sarXm

for (i in 1:length(names(sarXm))){
        names(sarXm)[i] <- paste('G', names(sarXm)[i], sep = "")
}
# names(sarX)
sarGm <- kgpcleanblock_undirt # use undirected matrix 
sarGXm <- sarGm %*% data.matrix(sarXm) #only multiply the covariates 
modedata2m <- as.data.frame(cbind(modedata, sarGXm)) 

# formula
sar_mode2bm <- formula("sch_score_70_3 ~ sch_score_68_3 + fem + age + iq + 
        prestige + edu_motherd2 + edu_fatherd2 + Gsch_score_68_3 + Gfem +
        + Gage + Giq + Gprestige + Gedu_motherd2 + Gedu_fatherd2")
rownames(sarGm) <- rownames(modedata2m)  # need assign row id to match matrix 


# use kgpcleanblock (direct adjacency matrix) as exclusion restriction 

starting2bm <- c(alpha = 0.87352, beta_sch_score_68_3 = 0.6268, beta_fem = 0.0116, 
        beta_age = 0.0029, beta_iq = 0.0035, beta_prestige = 0.0059, 
        beta_edu_motherd2 = 0.0356, beta_edu_fatherd2 = 0.054, 
        beta_Gsch_score_68_3 = 0.0068, beta_Gfem = 0.0036, 
        beta_Gage = 0.0029, beta_Giq = 0.0015, beta_Gprestige = 0.0039, 
        beta_Gedu_motherd2 = 0.0036, beta_Gedu_fatherd2 = 0.0054, 
        phi = 0.036, unobservables = 0.0129)

sar_lim_mode2bm <- net_dep(formula = sar_mode2bm, data = modedata2m, 
        G = sarGm, model = "model_B", estimation = "NLLS", endogeneity = T, 
        correction = "heckman", first_step = "standard", 
        exclusion_restriction = kgpcleanblock,
        hypothesis = "lim", start.val = starting2bm)  #Caution: it takes time (around 10 minutes)

summary(sar_lim_mode2bm)
plot(sar_lim_mode2bm$second_step)
qqnorm(sar_lim_mode2bm$second_step)


###  Depedent Variable: English 

# prepare data

# covariate
kgpclean %>%
        dplyr::select(sch_score_68_13, fem, age , iq , prestige ,edu_motherd2, 
                edu_fatherd2) ->sarXe

for (i in 1:length(names(sarXe))){
        names(sarXe)[i] <- paste('G', names(sarXe)[i], sep = "")
}
# names(sarX)
sarGe <- kgpcleanblock_undirt # use undirected matrix 
sarGXe <- sarGe %*% data.matrix(sarXe) #only multiply the covariates 
modedata2e <- as.data.frame(cbind(modedata, sarGXe)) 

# formula
sar_mode2be <- formula("sch_score_70_13 ~ sch_score_68_13 + fem + age + iq + 
        prestige + edu_motherd2 + edu_fatherd2 + Gsch_score_68_13 + Gfem +
        + Gage + Giq + Gprestige + Gedu_motherd2 + Gedu_fatherd2")
rownames(sarGe) <- rownames(modedata2e)  # need assign row id to match matrix 

# use kgpcleanblock (direct adjacency matrix) as exclusion restriction 
starting2be <- c(alpha = 1.789, beta_sch_score_68_13 = 0.6268, beta_fem = 0.0116, 
        beta_age = 0.0029, beta_iq = 0.0035, beta_prestige = 0.0059, 
        beta_edu_motherd2 = 0.0356, beta_edu_fatherd2 = 0.054, 
        beta_Gsch_score_68_13 = 0.0068, beta_Gfem = 0.0036, 
        beta_Gage = 0.0029, beta_Giq = 0.0015, beta_Gprestige = 0.0039, 
        beta_Gedu_motherd2 = 0.0036, beta_Gedu_fatherd2 = 0.0054, 
        phi = 0.036, unobservables = 0.0129)

sar_lim_mode2be <- net_dep(formula = sar_mode2be, data = modedata2e, 
        G = sarGe, model = "model_B", estimation = "NLLS", endogeneity = T, 
        correction = "heckman", first_step = "standard", 
        exclusion_restriction = kgpcleanblock,
        hypothesis = "lim", start.val = starting2be)  #Caution: it takes time (around 10 minutes)

# warning: this one sometime is not converging 

summary(sar_lim_mode2be)
plot(sar_lim_mode2be$second_step)
qqnorm(sar_lim_mode2be$second_step)
stargazer(summary(sar_lim_mode2be$second_step)$coefficients, digits = 4)

###  Depedent Variable: German

# prepare data

# covariate
kgpclean %>%
        dplyr::select(sch_score_68_8, fem, age , iq , prestige ,edu_motherd2, 
                edu_fatherd2) ->sarXg

for (i in 1:length(names(sarXg))){
        names(sarXg)[i] <- paste('G', names(sarXg)[i], sep = "")
}
# names(sarX)
sarGg <- kgpcleanblock_undirt # use undirected matrix 
sarGXg <- sarGg %*% data.matrix(sarXg) #only multiply the covariates 
modedata2g <- as.data.frame(cbind(modedata, sarGXg)) 

# formula
sar_mode2bg <- formula("sch_score_70_8 ~ sch_score_68_8 + fem + age + iq + 
        prestige + edu_motherd2 + edu_fatherd2 + Gsch_score_68_8 + Gfem +
        + Gage + Giq + Gprestige + Gedu_motherd2 + Gedu_fatherd2")
rownames(sarGg) <- rownames(modedata2g)  # need assign row id to match matrix 

# use kgpcleanblock (direct adjacency matrix) as exclusion restriction 
starting2bg <- c(alpha = 2.13, beta_sch_score_68_8 = 0.6268, beta_fem = 0.0116, 
        beta_age = 0.0029, beta_iq = 0.0035, beta_prestige = 0.0059, 
        beta_edu_motherd2 = 0.0356, beta_edu_fatherd2 = 0.054, 
        beta_Gsch_score_68_8 = 0.0068, beta_Gfem = 0.0036, 
        beta_Gage = 0.0029, beta_Giq = 0.0015, beta_Gprestige = 0.0039, 
        beta_Gedu_motherd2 = 0.0036, beta_Gedu_fatherd2 = 0.0054, 
        phi = 0.036, unobservables = 0.0129)

sar_lim_mode2bg <- net_dep(formula = sar_mode2bg, data = modedata2g, 
        G = sarGg, model = "model_B", estimation = "NLLS", endogeneity = T, 
        correction = "heckman", first_step = "standard", 
        exclusion_restriction = kgpcleanblock,
        hypothesis = "lim", start.val = starting2bg)  #Caution: it takes time (around 10 minutes)

summary(sar_lim_mode2bg)
plot(sar_lim_mode2bg$second_step)
qqnorm(sar_lim_mode2bg$second_step)
stargazer(summary(sar_lim_mode2bg$second_step)$coefficients, digits = 4)


## Finish all estimation for Model 2