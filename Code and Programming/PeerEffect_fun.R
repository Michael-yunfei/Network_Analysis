## This is the function code to my paper - Structural identification and estimation of
## peer effects using network data
## @ Fei (Michael) Wang
## Student ID: 942870

## this is the main function file I have written,
## in some cases, it relies on the packages you should load in Part 0.
## it includes the following 6 functions:
        # datatolist - a function takes the full dataset as input
                       # and then gives the list which includes all
                       # adjacency matrix

        # datatomatrix - a function converent network relationship into
                         # adjacency matrix

        # dir_to_undir - a function convert directed adjacency matrix into
                         # undirected adjacency matrix

        # checkmatrix - a function that checks the final block matrix has empty or
                         # all zero matrix

        # expit - a funciont that calcuates the probability from
                  # exponential model log odds ratio

        # netw2sls - a function that estimating the model with 2SLS method


datatolist <- function(m) {
        ## input m is a dataframe, this function
        ## output is a list which includes all matrix
        ## this function only works for this dataset, it's not general one
        kgpmatrix <- list()  # create a list to store all matrix
        ni <- 0  # global counter
        schindt <- as.numeric(unique(m$school))  # get vector to indicating school ID
        for (i in 1:length(schindt)) {
                schid <- schindt[i]
                schdata <- dplyr::filter(m, school == schid)  # filter out each school
                classnumber <- unique(schdata$class)  # check how many classes there in each shcool
                if (length(classnumber) == 1){
                        ni = ni+1
                        classdata1 <- dplyr::filter(schdata, class == classnumber)  # filter out each class
                        adjmatrix1 <- datatomatrix(classdata1)  # convernt network part to matrix
                        kgpmatrix[[ni]] <- adjmatrix1
                        names(kgpmatrix)[ni] <- paste("sch",schid, "class",classnumber, sep = "")
                } else {
                        ni = ni+1
                        # do fist class
                        classdata1 <- dplyr::filter(schdata, class == 1)  # filter out each class
                        adjmatrix1 <- datatomatrix(classdata1)
                        kgpmatrix[[ni]] <- adjmatrix1
                        names(kgpmatrix)[ni] <- paste("sch",schid, "class1", sep = "")
                        # do second class
                        ni = ni+1 # count twice
                        classdata2 <- dplyr::filter(schdata, class == 2)
                        adjmatrix2 <- datatomatrix(classdata2)
                        kgpmatrix[[ni]] <- adjmatrix2
                        names(kgpmatrix)[ni] <- paste("sch",schid, "class2", sep = "")
                }
                # mare sure adjmatrix 1 and adjmatrix 2 can be stored globally

        }
        return(kgpmatrix)

}


datatomatrix <- function(d){
        ## a function converent network relationship into adjacency matrix
        ## input d should be data.frame
        ## we use full dataset
        if (is.data.frame(d)){
                datadim <- dim(d)  # get the dimension
                studentid <- as.numeric(d$studentid)
                adjmatrix <- matrix(0, length(studentid)+1, length(studentid)+1)  # create a matrix with length of studentid + 1
                adjmatrix[1, 2:ncol(adjmatrix)] <- studentid  # assign student id into the fist row and column
                adjmatrix[2:nrow(adjmatrix), 1] <- studentid
                for (i in 2:dim(adjmatrix)[1]) {
                        friends <- as.numeric(d[i-1,1:32])  # convert it to numerical
                        idtofriends <- match(d[i-1, 1:32], adjmatrix[1, ], nomatch = 0)  # match friends
                        for (j in 1:length(idtofriends)) {
                                if (idtofriends[j] != 0){
                                        adjmatrix[i, idtofriends[j]] <- 1
                                }
                        }
                }
                adjmatrix <- adjmatrix[2:nrow(adjmatrix), 2:ncol(adjmatrix)]
                diag(adjmatrix) <- 0  ## make sure diagonal part is zero
                return(adjmatrix)
        } else{
                print("input is not dataframe")
        }
}


dir_to_undir <- function(dg) {
        ## input is directed adjacency matrix list
        ## output is a undirected adjacency matrix list
        library(igraph)
        kgpmatrix_undirt <- dg  # initialize undirected list by assign the directed list
        for (i in 1:length(kgpmatrix_undirt)){
                gr <- graph.adjacency(dg[[i]])  ## convert it to graph
                grun <- as.undirected(gr, mode = "mutual")  # only mutual friendship is kept
                grun_matrix <- as.matrix(get.adjacency(grun, type = "both"))  # concert s4 class to matrix
                kgpmatrix_undirt[[i]] <-unname(grun_matrix)
                # only change matirx (without attributes), not the name
        }
        return(kgpmatrix_undirt)
}


checkmatrix <- function(matlst) {
        nz = 0
        zeromat <- c()  # store the indices
        for (i in 1:length(matlst)) {
                if (length(matlst[[i]]) == 0) {
                        print(names(matlst[i]))
                }
                if (all(matlst[[i]] == 0)){
                        nz = nz+1
                        zeromat[nz] <- i
                }
        }
        print(nz)
        print(zeromat)
}


expit = function(x) 1/(1+exp(-x))  # write a function for calculating probability


# 2sls estimation function
netw2sls <- function(dataset, y, G, X){
        ## input
        # dataset: cleaned full dataset (data.frame) that is used to consctruct ublock and jblock
        # y: dependent variable vector,
        # G: adjacency matrix
        # X: covariate variables (data.frame),

        ## output
        # a table includes all estimations except for fixed effect alpha
        # residuals
        # fitted values
        # a vector inlucdes, fixed effect - alpha, standar error and t ratio


        # construct block unit vector model  and diagonal J block
        matrixlist <- datatolist(dataset)
        firstmatrix_len <- dim(matrixlist[[1]])[1]
        ublock <- matrix(1, firstmatrix_len)  # initialize the unit block by using the first submatrix of kgpcleanmat
        jblock <- diag(firstmatrix_len) - 1/firstmatrix_len*ublock%*%t(ublock)  # initialize the J block
        for (i in 2:length(matrixlist)){
                matlen <- dim(matrixlist[[i]])[1]
                m1 <- matrix(1,matlen)
                j1 <- diag(matlen) - 1/matlen*m1%*%t(m1)
                ublock <- adiag(ublock, m1)
                jblock <- adiag(jblock, j1)
        }
        testju <- jblock %*% ublock
        # check wheter JU = 0 or not
        if (! all(round(testju, 6) == 0)){
                print("Warning: wrong ublock and jblcok matrix")
        }

        #create variables
        A <- G
        y <- data.matrix(y)  # convert it into matrix format
        X <- data.matrix(X)
        ay <- A%*%y
        ax <- A%*%X
        a2x <- A%*%ax
        Z <- cbind(ay,X, ax)

                # create IV
                au <- A%*%ublock
                hsub <- cbind(X, ax, a2x, au)
                H <- jblock %*% hsub
                P <- H %*% solve(t(H)%*%H) %*% t(H)

        # estimate coefficients
        estimations <- solve(t(Z) %*% P %*% Z) %*% t(Z) %*% P %*% y
        colnames(estimations) <- "Estimate"
        fixeffect <- y - Z %*% estimations  # get fixed effects (or constant one)
        alpha = mean(fixeffect)  # get fixed effects
        alphatratio <- alpha/sd(fixeffect)
        coefull <-  rbind(alpha, estimations)
        zfull <- cbind(alpha * matrix(1, dim(y)[1]), Z)
        resids <- y - zfull %*% coefull  # get residuals
        df <- dim(X)[1] - dim(Z)[2]  # degree of freedom, omit alpha at this moment

        cov <- t(resids) %*% resids/ df
        covmatrix <- cov[[1]] * solve(t(Z) %*% P %*% t(P)%*% Z)
        stderror <- sqrt(diag(covmatrix))
        names(stderror) <- "Standard Error"
        tratio <- estimations/stderror
        colnames(tratio) <- "t ratio"
        results <- cbind(estimations, stderror, tratio)

        # return a list which includes the results, residuals, and fitted values
        list(results = round(results, 4),
                residuals = resids ,
                yhat = zfull %*% coefull,
                alpha = round(c(alpha, sd(fixeffect), alphatratio), 4))


}

# End of Code
