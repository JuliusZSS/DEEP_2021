# Input: NA
# Output: causal trees result
# https://github.com/susanathey/causalTree
# https://ml-in-econ.appspot.com/lab3.html

rm(list = ls())

# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("Rgraphviz")

# set hyper-parameters ####
p <- 40    # number of total covariants
pt <- 4    # number of covariants affecting treatment effects
py <- 4    # number of covariants affecting outcomes but not treatment effects
n <- 80000    # total size of the dataset ### change made

# set number of loops here ####
number_of_batches <- 10

# run self training causal trees ####
source("sz101_causal_trees.R")
get_causal_effect_using_causal_trees(p, pt, py, n, number_of_batches)

# run self training causal tree-based decile plot ####
# source('sz103_causal_trees_decile_plot.R')
# get_decile_plot_from_causal_trees(p)

# set times of cross validation here ####
cross_validation_times = 1
cross_validation_folds = 10

# use Susan Athey's package to build causal trees ####
# install.packages("devtools")
library(devtools)
# writeLines('PATH="${RTOOLS40_HOME}D:\\rtools40\\usr\\bin;${PATH}"', con = "~/.Renviron")
# Sys.which("make")
# install_github("susanathey/causalTree")
library(causalTree)
library(caret)

# to save mean and std of PEHE and MAPE ####
PEHE_MAPE = data.frame(matrix(nrow = number_of_batches, ncol = 6))
names(PEHE_MAPE) = c('PEHE','PEHE_mean','PEHE_std','MAPE','MAPE_mean','MAPE_std')

# number of causal trees self training batches ####
causal_tree_batch = 10

# loop through the batches ####
PEHE_MAPE_cross_validation_times = data.frame(matrix(nrow = causal_tree_batch, ncol = 2))
names(PEHE_MAPE_cross_validation_times) = c('PEHE','MAPE')

start_time <- Sys.time()

dir.create(paste('data/causal_trees_cross_validation',sep=''))

for (current_causal_tree_batch in 1:causal_tree_batch) {

  # read csv file for running time ####
  # data = read.csv(paste('data_', p, '_4_4/causal_trees_self_training/causal_tree_self_training_batch_',current_causal_tree_batch,'.csv',sep=''))[,-1]
  # data = data[,-ncol(data)]
  
  # read csv file for running time ####
  data = read.csv(paste('data/causal_trees_self_training/causal_tree_self_training_batch_',current_causal_tree_batch,'.csv',sep=''))[,-1]

  
  # set random seed ####
  set.seed(234 + current_causal_tree_batch)
  
  # 10-fold 1 time cross validation ####
  data_folds = createFolds(as.numeric(rownames(data)), k=cross_validation_folds)
  
  # to save CATEs ####
  CATE_of_folds = data.frame()
  
  for (fold_number in 1:cross_validation_folds) {
    
    train = data[-data_folds[[fold_number]],]
    test = data[data_folds[[fold_number]],]
    
    # save train and test data for DEEP purpose ####
    write.csv(train, paste('data/causal_trees_cross_validation/causal_tree_cross_validation_batch_',
                           current_causal_tree_batch,'_fold_',fold_number,'_train.csv',sep=''))
    write.csv(test, paste('data/causal_trees_cross_validation/causal_tree_cross_validation_batch_',
                          current_causal_tree_batch,'_fold_',fold_number,'_test.csv',sep=''))
    
    # create formula ####
    formula_ct = formula(paste('y',paste(names(data)[1:p],collapse='+'),sep='~'))
    
    # build tree (using default parameters) ####
    tree <- causalTree(formula_ct, data = train,
                       treatment = train$w, split.Rule = "CT",
                       cv.option = "CT", split.Honest = T, cv.Honest = T,
                       split.Bucket = F, xval = 5, cp = 0, minsize = 20,
                       propensity = 0.5, x = TRUE, y = TRUE)
    
    # tree$cptable
    
    # plot causal tree and save it ####
    jpeg(paste('data/causal_trees_cross_validation/causal_tree_cross_validation_batch_',
               current_causal_tree_batch,'_fold_',fold_number,'.png',sep=''))
    rpart.plot(tree)
    dev.off()

    # use causal trees to predict all rows ####
    tree_results <- data.frame(predict(tree, data.frame(test$y, test[,1:p]),
                                       type='vector'))
    colnames(tree_results) <- c('CATE')
    
    # save causal trees result in this fold ####
    tree_results_output <- data.frame(test, tree_results)
    CATE_of_folds = rbind(CATE_of_folds, tree_results_output)
    
    end_time <- Sys.time()
    print(end_time - start_time)
    print(end_time - start_time)
    
  }
  
  # save 
  write.csv(CATE_of_folds, paste('data/causal_trees_cross_validation/causal_tree_cross_validation_batch_',current_causal_tree_batch,'.csv',sep=''))
  
  tau_difference = data.frame(matrix(nrow = nrow(CATE_of_folds), ncol = 1))
  tau_difference_percentage = data.frame(matrix(nrow = nrow(CATE_of_folds), ncol = 1))
  
  for (j in 1:nrow(CATE_of_folds)) {
    # get pattern
    pattern = paste(CATE_of_folds[j,1:(pt+py)],collapse=' ')
    
    # get pattern corresponding ground truth phi
    ground_truth = read.csv(paste('data/ground_truth/binary_parents_of_y_',(pt+py),'.csv',sep=''))
    ground_truth_phi = ground_truth[which(ground_truth['keys']==pattern),'phi']
    
    # difference in tau
    tau_hat_minus_tau = CATE_of_folds[j,'CATE'] - ground_truth_phi
    tau_hat_minus_tau_percentage = tau_hat_minus_tau / ground_truth_phi
    tau_difference[j,1] = tau_hat_minus_tau
    tau_difference_percentage[j,1] = abs(tau_hat_minus_tau_percentage)
  }
  
  # PEHE
  PEHE_output = sqrt((sum(tau_difference^2))/(nrow(tau_difference)*ncol(tau_difference)))
  PEHE_MAPE_cross_validation_times[current_causal_tree_batch,'PEHE'] = PEHE_output
  
  
  # MAPE
  tau_difference_percentage_copy = tau_difference_percentage*is.finite(unlist(tau_difference_percentage))
  MAPE_output = rowMeans(t(tau_difference_percentage_copy), na.rm=TRUE)
  MAPE_output = rowMeans(t(MAPE_output))
  PEHE_MAPE_cross_validation_times[current_causal_tree_batch,'MAPE'] = MAPE_output

}

PEHE_MAPE[,'PEHE'] = PEHE_MAPE_cross_validation_times[,'PEHE']
PEHE_MAPE[,'MAPE'] = PEHE_MAPE_cross_validation_times[,'MAPE']

PEHE_MAPE[1,'PEHE_mean'] = rowMeans(as.matrix(t(PEHE_MAPE_cross_validation_times['PEHE'])))
PEHE_MAPE[1,'MAPE_mean'] = rowMeans(as.matrix(t(PEHE_MAPE_cross_validation_times['MAPE'])))

PEHE_MAPE[1,'PEHE_std'] = rowSds(as.matrix(t(PEHE_MAPE_cross_validation_times['PEHE'])))
PEHE_MAPE[1,'MAPE_std'] = rowSds(as.matrix(t(PEHE_MAPE_cross_validation_times['MAPE'])))

write.csv(PEHE_MAPE,'data/causal_trees_cross_validation_summary.csv')


end_time <- Sys.time()
end_time - start_time


