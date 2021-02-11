# Input: uplift model output self-training
# Output: summary of uplift performance

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(matrixStats)

pt = 4
py = 4

number_of_batches = 10

# folder_names = c('data_20_4_4','data_40_4_4','data_60_4_4','data_80_4_4','data_100_4_4')
folder_names = 'data'

for (folder_name in folder_names) {

  # folder_name = folder_names[folder_index]
  
  training_method = 'uplift_tree_self_training'
  training_method_patterns = 'causal_trees_self_training'
  ground_truth = 'ground_truth_summary.csv'
  
  ### save final PEHE and MAPE
  PEHE_MAPE = data.frame(matrix(nrow = number_of_batches, ncol = 6))
  names(PEHE_MAPE) = c('PEHE','PEHE_mean','PEHE_std','MAPE','MAPE_mean','MAPE_std')
  
  ### start loop here
  for (batch_number in 1: number_of_batches) {
    
    ### uplift output
    uplift_result = read.csv(paste(folder_name,'/',training_method,
                                   '/uplift_tree_self_training_batch_',batch_number,'.csv',sep=''))
    colnames(uplift_result) = c('index','uplift_CATE')
    
    ### patterns of uplift output
    uplift_result_patterns = read.csv(paste(folder_name,'/',training_method_patterns,
                                            '/causal_tree_self_training_batch_',batch_number,'.csv',sep=''))
    uplift_result_patterns = uplift_result_patterns[,-1]
    
    ### ground truth
    ground_truth_csv = read.csv(paste(folder_name,'/',ground_truth,sep=''))
    
    ### calculate PEHE and MAPE
    tau_difference = data.frame(matrix(ncol = 1, nrow = nrow(uplift_result)))
    tau_difference_percentage = data.frame(matrix(ncol = 1, nrow = nrow(uplift_result)))
    
    for (j in 1:nrow(uplift_result)) {
      
      ### get pattern
      pattern = paste(uplift_result_patterns[j,1:(pt+py)],collapse=' ')
      
      ### get pattern corresponding ground truth phi
      ground_truth_phi = ground_truth_csv[which(ground_truth_csv['keys']==pattern),'phi']
      
      ### difference in tau
      tau_hat_minus_tau = uplift_result[j,'uplift_CATE'] - ground_truth_phi
      tau_hat_minus_tau_percentage = tau_hat_minus_tau / ground_truth_phi
      tau_difference[j,1] = tau_hat_minus_tau
      tau_difference_percentage[j,1] = abs(tau_hat_minus_tau_percentage)
    }
    
    ### PEHE
    PEHE_output = sqrt((sum(tau_difference^2))/(nrow(tau_difference)))
    PEHE_MAPE[batch_number,'PEHE'] = PEHE_output
    
    ### MAPE
    tau_difference_percentage = tau_difference_percentage*is.finite(unlist(tau_difference_percentage))
    MAPE_output = rowMeans(t(tau_difference_percentage), na.rm=TRUE)
    PEHE_MAPE[batch_number,'MAPE'] = MAPE_output
    
  }
  
  PEHE_MAPE[1,'PEHE_mean'] = rowMeans(as.matrix(t(PEHE_MAPE['PEHE'])))
  PEHE_MAPE[1,'MAPE_mean'] = rowMeans(as.matrix(t(PEHE_MAPE['MAPE'])))
  
  PEHE_MAPE[1,'PEHE_std'] = rowSds(as.matrix(t(PEHE_MAPE['PEHE'])))
  PEHE_MAPE[1,'MAPE_std'] = rowSds(as.matrix(t(PEHE_MAPE['MAPE'])))
  
  write.csv(PEHE_MAPE,paste(folder_name,'/uplift_tree_self_training_summary.csv',sep=''))

}



