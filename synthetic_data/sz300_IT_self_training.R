# Input: IT self training results
# Output: PEHE and MAPE


rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(matrixStats)


# folder_names = c('data_20_4_4','data_40_4_4','data_60_4_4','data_80_4_4','data_100_4_4')
folder_names = 'data'

pt = 4
py = 4

################################################################################

for (folder_name in folder_names) {
  
  ### save final PEHE and MAPE
  PEHE_MAPE = data.frame(matrix(nrow = 10, ncol = 6))
  names(PEHE_MAPE) = c('PEHE','PEHE_mean','PEHE_std','MAPE','MAPE_mean','MAPE_std')
  
  for (n_batch in 1:10) {
    
    file_1 = paste(folder_name, '/interaction_trees_self_training/interaction_trees_self_training_batch_',n_batch,'.csv',sep='')
    data_1 = read.csv(file_1)
    data_1 = data_1[, -1]
    
    ### ground truth
    ground_truth_csv = read.csv(paste(folder_name,'/ground_truth_summary.csv',sep=''))
    
    ### calculate PEHE and MAPE
    tau_difference = data.frame(matrix(ncol = 1, nrow = nrow(data_1)))
    tau_difference_percentage = data.frame(matrix(ncol = 1, nrow = nrow(data_1)))
    
    for (j in 1:nrow(data_1)) {
      
      ### get pattern
      pattern = paste(data_1[j,1:(pt+py)],collapse=' ')
      
      ### get pattern corresponding ground truth phi
      ground_truth_phi = ground_truth_csv[which(ground_truth_csv['keys']==pattern),'phi']
      
      ### difference in tau
      tau_hat_minus_tau = data_1[j,'CATE'] - ground_truth_phi
      tau_hat_minus_tau_percentage = tau_hat_minus_tau / ground_truth_phi
      tau_difference[j,1] = tau_hat_minus_tau
      tau_difference_percentage[j,1] = abs(tau_hat_minus_tau_percentage)
    }
    
    ### PEHE
    PEHE_output = sqrt((sum(tau_difference^2))/(nrow(tau_difference)))
    PEHE_MAPE[n_batch,'PEHE'] = PEHE_output
    
    ### MAPE
    tau_difference_percentage = tau_difference_percentage*is.finite(unlist(tau_difference_percentage))
    MAPE_output = rowMeans(t(tau_difference_percentage), na.rm=TRUE)
    PEHE_MAPE[n_batch,'MAPE'] = MAPE_output
    
  }
  
  PEHE_MAPE[1,'PEHE_mean'] = rowMeans(as.matrix(t(PEHE_MAPE['PEHE'])))
  PEHE_MAPE[1,'MAPE_mean'] = rowMeans(as.matrix(t(PEHE_MAPE['MAPE'])))
  
  PEHE_MAPE[1,'PEHE_std'] = rowSds(as.matrix(t(PEHE_MAPE['PEHE'])))
  PEHE_MAPE[1,'MAPE_std'] = rowSds(as.matrix(t(PEHE_MAPE['MAPE'])))
  
  write.csv(PEHE_MAPE,paste(folder_name,'/IT_self_training_summary.csv',sep=''))
  
}







