### Input: CT results
### OUtput: decile plot

data_name = 'US_census'

get_decile_plot_from_causal_trees <- function(p) {
  
  library(causalTree)
  library(caret)
  library(dplyr)
  library(matrixStats)
  library(ggplot2)
  library(devtools) 
  
  source('get_contingency_table.R')
  source('get_causal_effect_from_contingency_table.R')
  
  ### set hyper-parameters
  p <- p # number of total covariants
  
  dir.create(paste('data/decile',data_name,sep=''))
  
  data = read.csv('data/causal_trees_self_training/causal_tree_self_training_batch_1.csv')[,-1]
  data[,'CATE'] = NULL
  
  ### set times of cross validation here
  cross_validation_times = 10
  cross_validation_folds = 2
  
  decile_phi = data.frame(matrix(nrow = 10, ncol = cross_validation_times))
  
  for (current_cross_validation_times in 1:cross_validation_times) {
    ### start loop here
    # current_cross_validation_times = 1
    
    ### set random seed
    set.seed(345 + current_cross_validation_times)
    
    ### 2-fold 10 times cross validation
    data_folds = createFolds(as.numeric(rownames(data)), k=cross_validation_folds)
    
    ### to save CATEs
    CATE_of_folds = data.frame()
    
    for (current_fold in 1:cross_validation_folds) {
      ### get train and test data
      train = data[-data_folds[[current_fold]],]
      test = data[data_folds[[current_fold]],]
      
      ### save train and test data for DEEP purpose
      write.csv(train, paste('data/causal_trees_self_training_decile/causal_tree_cross_validation_time_',
                             current_cross_validation_times,'_fold_',current_fold,'_train.csv',sep=''))
      write.csv(test, paste('data/causal_trees_self_training_decile/causal_tree_cross_validation_time_',
                            current_cross_validation_times,'_fold_',current_fold,'_test.csv',sep=''))
      
      ### create formula
      formula_ct = formula(paste('y',paste(names(data)[1:p],collapse='+'),sep='~'))
      
      ### build tree (using default parameters)
      tree <- causalTree(formula_ct, data = train,
                         treatment = train$w, split.Rule = "CT",
                         cv.option = "CT", split.Honest = T, cv.Honest = T,
                         split.Bucket = F, xval = 5, cp = 0, minsize = 20,
                         propensity = 0.5, x = TRUE, y = TRUE)
      
      # tree$cptable
      
      ### plot causal tree and save it
      jpeg(paste('data/causal_trees_self_training_decile/causal_tree_cross_validation_time_',
                 current_cross_validation_times,'_fold_',current_fold,'.png',sep=''))
      rpart.plot(tree)
      dev.off()
      
      ### use causal trees to predict all rows
      tree_results <- data.frame(predict(tree, data.frame(test$y, test[,1:p]),
                                         type='vector'))
      colnames(tree_results) <- c('CATE')
      
      ### save causal trees result in this fold
      tree_results_output <- data.frame(test, tree_results)
      CATE_of_folds = rbind(CATE_of_folds, tree_results_output)
      
    }
    
    ### decile the data by CATE
    CATE_of_folds$quartile <- ntile(-CATE_of_folds$CATE, 10)  
    current_ntile_data_phi = data.frame(matrix(nrow = 10, ncol = 1))
    
    for (current_ntile in 1:10) {
      ### get data
      current_ntile_data = CATE_of_folds[which(CATE_of_folds$quartile==current_ntile),]
      
      ### get n11 n12 n21 n22
      current_ntile_data_contingency_table = get_contingency_table(current_ntile_data)
      
      ### get pi_1 pi_2 phi
      current_ntile_data_phi[current_ntile, 1] = get_causal_effect_from_contingency_table(current_ntile_data_contingency_table)
    }
    
    decile_phi[,current_cross_validation_times] = current_ntile_data_phi
    
  }
  
  ### aggregate phi and calculate its mean and std
  decile_phi['phi_mean'] = rowMeans(decile_phi[,1:10])
  decile_phi['phi_std'] = rowSds(as.matrix(decile_phi[,1:10]))
  
  # create plot data
  plot_data <- data.frame(percentile=format(round(0:9), nsmall=1),value=decile_phi['phi_mean'],sd=decile_phi['phi_std'])
  
  ### decile plot and save it
  jpeg(paste('data/causal_trees_self_training_decile/causal_tree_2_fold_10_times_decile_plot.png',sep=''))
  decile_plot <- ggplot(plot_data) +
    geom_bar(aes(x=percentile, y=phi_mean), stat="identity", fill="skyblue", alpha=0.7) +
    geom_errorbar(aes(x=percentile, ymin=phi_mean-phi_std, ymax=phi_mean+phi_std), width=0.4, colour="orange", alpha=0.9, size=1.3)
  print(decile_plot)
  dev.off()

}