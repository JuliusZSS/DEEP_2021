### Input: NA
### Output: causal trees result
### https://github.com/susanathey/causalTree
### https://ml-in-econ.appspot.com/lab3.html

get_causal_effect_using_causal_trees <- function(p, pt, py, n=100, number_of_batches=10) {
  
  ### set hyper-parameters
  p <- p # number of total covariants
  pt <- pt # number of covariants affecting treatment effects
  py <- py # number of covariants affecting outcomes but not treatment effects
  n <- n # total size of the dataset ### change made
  
  ### set loops here
  number_of_batches <- number_of_batches
  
  ### import datasets
  source('sz003_generate_synthetic_data_batches.R')
  data_for_causal_trees_output = generate_synthetic_data_batches(p, pt, py ,n, number_of_batches)
  
  ### use Susan Athey's package to build causal trees
  # install.packages("devtools")
  library(devtools) 
  # writeLines('PATH="${RTOOLS40_HOME}D:\\rtools40\\usr\\bin;${PATH}"', con = "~/.Renviron")
  # Sys.which("make")
  # install_github("susanathey/causalTree")
  library(causalTree)
  
  ### save final PEHE and MAPE
  PEHE_MAPE = data.frame(matrix(nrow = number_of_batches, ncol = 6))
  names(PEHE_MAPE) = c('PEHE','PEHE_mean','PEHE_std','MAPE','MAPE_mean','MAPE_std')
  
  ### get each dataset
  binary_binary_cf = data_for_causal_trees_output[[1]]
  
  dir.create(paste('data/causal_trees_self_training',sep=''))
  
  ### start loop here (flip a coin 1=train 0=test)
  for (i in 1:number_of_batches) {
    
    set.seed(123 + i)
    all_rows = nrow(binary_binary_cf) / 2
    coin_decision = sample(c(0,1), all_rows, replace=TRUE)
    
    train = data.frame(matrix(nrow=all_rows, ncol=1))
    test = data.frame(matrix(nrow=all_rows, ncol=1))
    
    for (j in 1:length(coin_decision)) {
      if (coin_decision[j]) {
        train[j,1] = j
        test[j,1] = j + n
      } else {
        train[j,1] = j + n
        test[j,1] = j 
      }
    }
    
    train_data = binary_binary_cf[unlist(train),]
    test_data = binary_binary_cf[unlist(test),] # never use because cannot be observed in real world
    
    write.csv(train_data, paste('data/causal_trees_self_training','/causal_tree_self_training_batch_',i,'.csv',sep=''))
    
    
    ############################################################################
    # 
    # ### create formula
    # formula_ct = formula(paste('y',paste(names(binary_binary_cf)[1:p],collapse='+'),sep='~'))
    # 
    # ### build tree (using default parameters)
    # tree <- causalTree(formula_ct, data = train_data, 
    #                    treatment = train_data$w, split.Rule = "CT", 
    #                    cv.option = "CT", split.Honest = T, cv.Honest = T, 
    #                    split.Bucket = F, xval = 5, cp = 0, minsize = 20, 
    #                    propensity = 0.5, x = TRUE, y = TRUE)
    # 
    # # tree$cptable
    # 
    # ### plot causal tree and save it
    # jpeg(paste('data/causal_trees_self_training/causal_tree_self_training_batch_',i,'.png',sep=''))
    # rpart.plot(tree)
    # dev.off()
    # 
    # ### use causal trees to predict all rows
    # tree_results <- data.frame(predict(tree, data.frame(train_data$y, train_data[,1:p]),
    #                                    type='vector'))
    # colnames(tree_results) <- c('CATE')
    # 
    # ### causal trees result output
    # binary_binary_cf_ct_output <- data.frame(train_data, tree_results)
    # 
    #   
    #   write.csv(binary_binary_cf_ct_output, paste('data/causal_trees_self_training','/causal_tree_self_training_batch_',i,'.csv',sep=''))
    #   
    #   
    #   ### calculate PEHE and MAPE
    #   tau_difference = data.frame(matrix(ncol = 1, nrow = nrow(binary_binary_cf_ct_output)))
    #   tau_difference_percentage = data.frame(matrix(ncol = 1, nrow = nrow(binary_binary_cf_ct_output)))
    #   
    #   for (j in 1:nrow(binary_binary_cf_ct_output)) {
    #     
    #     ### get pattern
    #     pattern = paste(binary_binary_cf_ct_output[j,1:(pt+py)],collapse=' ')
    #     
    #     ### get pattern corresponding ground truth phi
    #     ground_truth = read.csv(paste('data/ground_truth','/binary_parents_of_y_',(pt+py),'.csv',sep=''))
    #     ground_truth_phi = ground_truth[which(ground_truth['keys']==pattern),'phi']
    #     
    #     ### difference in tau
    #     tau_hat_minus_tau = binary_binary_cf_ct_output[j,'CATE'] - ground_truth_phi
    #     tau_hat_minus_tau_percentage = tau_hat_minus_tau / ground_truth_phi
    #     tau_difference[j,1] = tau_hat_minus_tau
    #     tau_difference_percentage[j,1] = abs(tau_hat_minus_tau_percentage)
    #   }
    #   
    #   ### PEHE
    #   PEHE_output = sqrt((sum(tau_difference^2))/(nrow(tau_difference)))
    #   PEHE_MAPE[i,'PEHE'] = PEHE_output
    #   
    #   ### MAPE
    #   tau_difference_percentage = tau_difference_percentage*is.finite(unlist(tau_difference_percentage))
    #   MAPE_output = rowMeans(t(tau_difference_percentage), na.rm=TRUE)
    #   PEHE_MAPE[i,'MAPE'] = MAPE_output
    #   
  }
  # 
  # PEHE_MAPE[1,'PEHE_mean'] = rowMeans(as.matrix(t(PEHE_MAPE['PEHE'])))
  # PEHE_MAPE[1,'MAPE_mean'] = rowMeans(as.matrix(t(PEHE_MAPE['MAPE'])))
  # 
  # PEHE_MAPE[1,'PEHE_std'] = rowSds(as.matrix(t(PEHE_MAPE['PEHE'])))
  # PEHE_MAPE[1,'MAPE_std'] = rowSds(as.matrix(t(PEHE_MAPE['MAPE'])))
  # 
  # write.csv(PEHE_MAPE,'data/causal_trees_self_training_summary.csv')
  # 
  ############################################################################
  
  
}

