# This program tests out the features of the causalTree package


#install.packages("devtools")
library(devtools)
#install.packages("rpart", dependencies=TRUE, repos='http://cran.us.r-project.org')
#install.packages("rpart.plot", dependencies=TRUE, repos='http://cran.us.r-project.org')
#install.packages("reshape2", dependencies=TRUE, repos='http://cran.us.r-project.org')
#install.packages("plyr", dependencies=TRUE, repos='http://cran.us.r-project.org')
library(rpart)
library(rpart.plot)
# install_github("susanathey/causalTree",force=TRUE)
library(causalTree)
library(reshape2)
library(plyr)

library(pcalg)
library(Rgraphviz)


synthesize_data <- function(p, pt, py, n) {
  # Generate data
  # parameters for data generating
  p <- p # number of total covariants
  pt <- pt # number of covariants affecting treatment effects
  py <- py # number of covariants affecting outcomes but not treatment effects
  asym <- .5 # whether treatment effects are distributed asymmetrically across treated and control
  n <- n # total size of the dataset ### change made
  propens <- .5 #treatment probability
  sig = .01
  treatsize <- .5 # treatment effect size
  levsize <- 1
  
  # draw W
  w <- rbinom(n, 1, propens)
  w_ <- 1-w
  
  # draw X
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  
  # assign column names
  column_names <- c()
  for (i in 1:p) {
    name_char <- paste('x',as.character(i),sep='')
    column_names <- append(column_names, name_char)
  }
  colnames(X) <- column_names

  # generate treatment effects as function of X
  if (p<pt+py) print("error: p>=pt+py required")
  tau <- 0
  for (iii in 1:pt) {
    tau <- tau + treatsize*pmax(X[,iii],array(0,n))*(2*X[,iii])
  }
  
  # generate average value of outcomes
  mu <- treatsize*rowSums(X[,1:pt])+levsize*rowSums(X[,(pt+1):(pt+py)])
  
  # generate outcomes as function of treatment status, mu, tau, and noise
  y <- mu +  asym*w*tau + (asym-1)*(1-w)*tau + rnorm(n,0,sig)
  y_ <- mu + asym*w_*tau + (asym-1)*(1-w_)*tau + rnorm(n,0,sig)
  
  
  # Shisheng start from here ####
  # concatenate X, w, y ####
  continous_data <- cbind(X, w, y)
  continous_data_cf <- cbind(X, w_, y_)
  
  # binarised data ####
  binary_data <- ifelse(continous_data>0, 1, 0)
  binary_data_cf <- ifelse(continous_data_cf>0, 1, 0)
  
  # set alpha and trial number ####
  alpha = 0.01
  trial = 1
  
  # draw DAG for continuous_data ####
  # Load predefined data
  n <- nrow (continous_data)
  V <- colnames(continous_data) # labels aka node names
  # estimate Skeleton
  skel_1.fit <- skeleton(suffStat = list(C=cor(continous_data), n=n),
                       indepTest = gaussCItest,
                       alpha = alpha, 
                       labels = V, 
                       verbose = TRUE)

  # draw DAG for binary_data
  # Load predefined data
  n <- nrow (binary_data)
  V <- colnames(binary_data) # labels aka node names
  # estimate Skeleton
  skel_2.fit <- skeleton(suffStat = list(dm = binary_data, adaptDF=FALSE),
                       indepTest = binCItest,
                       alpha = alpha, 
                       labels = V, 
                       verbose = TRUE)

  # draw DAG for continuous_data_cf
  # Load predefined data
  n <- nrow (continous_data_cf)
  V <- colnames(continous_data_cf) # labels aka node names
  # estimate Skeleton
  skel_3.fit <- skeleton(suffStat = list(C=cor(continous_data_cf), n=n),
                       indepTest = gaussCItest,
                       alpha = alpha, 
                       labels = V, 
                       verbose = TRUE)

  # draw DAG for binary_data_cf
  # Load predefined data
  n <- nrow (binary_data_cf)
  V <- colnames(binary_data_cf) # labels aka node names
  # estimate Skeleton
  skel_4.fit <- skeleton(suffStat = list(dm = binary_data_cf, adaptDF=FALSE),
                       indepTest = binCItest,
                       alpha = alpha, 
                       labels = V, 
                       verbose = TRUE)

  print('Finished synthesizing data!')
  print(paste('number of total covariates:', p))
  print(paste('number of covariates affecting treatment effects:', pt))
  print(paste('number of covariates affecting outcomes but not treatment effects:', py))
  
  return(list(continous_data, binary_data, continous_data_cf, binary_data_cf, 
              p, pt, py, 
              skel_1.fit, skel_2.fit, skel_3.fit, skel_4.fit))
}


