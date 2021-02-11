# Input: DEEP test results
# Output: 


rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(matrixStats)


# empty matrix
output_csv = data.frame(matrix(nrow = 10, ncol = 6))
colnames(output_csv) = c('PEHE', 'PEHE_mean', 'PEHE_std', 'MAPE', 'MAPE_mean', 'MAPE_std')

# data_names = c('data_20_4_4','data_40_4_4','data_60_4_4','data_80_4_4','data_100_4_4')
data_names = 'data'

for (data_name in data_names) {
  for (n_batch in 1:10) {
    
    result_1 = data.frame()
    
    for (n_fold in 1:10) {
      
      # import data
      result_folds = read.csv(paste(data_name, '/deep_cross_validation/results/deep_cross_validation_batch_',n_batch,'_fold_',n_fold,'_results.csv',sep = ''))
      
      result_1 = rbind(result_1, result_folds)
      
    }
    
    ### calculate PEHE
    tau = result_1$CATE - result_1$ground_truth
    PEHE = sqrt(sum(tau^2)/length(tau))
    output_csv[n_batch, 'PEHE'] = PEHE
    
    ### calculate MAPE
    MAPE_1 = tau/result_1$ground_truth
    MAPE_1 = MAPE_1[is.finite(MAPE_1)]
    MAPE = sum(abs(MAPE_1)) / length(tau)
    output_csv[n_batch, 'MAPE'] = MAPE
    
  }
  
  ### calculate std and mean
  output_csv[1,'PEHE_mean'] = rowMeans(as.matrix(t(output_csv['PEHE'])))
  output_csv[1,'MAPE_mean'] = rowMeans(as.matrix(t(output_csv['MAPE'])))
  
  output_csv[1,'PEHE_std'] = rowSds(as.matrix(t(output_csv['PEHE'])))
  output_csv[1,'MAPE_std'] = rowSds(as.matrix(t(output_csv['MAPE'])))
  
  ### export data
  output_csv_filename = paste(data_name, '/deep_cross_validation_summary.csv', sep = '')
  write.csv(output_csv, output_csv_filename)
  
}

