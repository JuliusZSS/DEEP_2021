### Input: DEEP CATE
### OUtput: DEEP CATE and ground truth

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(matrixStats)

data_name = "data_40_4_4"

output_summary_csv = paste(data_name,"/deep_cross_validation_summary.csv",sep = "")
output_summary = data.frame(matrix(ncol = 6))
colnames(output_summary) = c('PEHE', 'PEHE_mean', 'PEHE_std', 'MAPE', 'MAPE_mean', 'MAPE_std')

for (batch_number in 1:10) {
  output_csv = paste(data_name,"/deep_cross_validation/deep_cross_validation_batch_",batch_number,"_CATE_ground_truth.csv",sep="")
  
  ### import data
  ground_truth_data = read.csv(paste(data_name,"/ground_truth/binary_parents_of_y_8.csv",sep = ""))
  ground_truth_data = ground_truth_data[,-1]
  
  DEEP_CATE_concatenated = data.frame()
  
  for (fold in 1:10) {
    DEEP_CATE = read.csv(paste(data_name,"/deep_cross_validation/deep_cross_validation_batch_",batch_number,"_fold_",fold,"_test_CATE.csv",sep=""))
    DEEP_CATE = DEEP_CATE[,-1]
    
    ### get pattern
    DEEP_CATE['pattern'] = paste(DEEP_CATE$x1, DEEP_CATE$x2, DEEP_CATE$x3, DEEP_CATE$x4, 
                                 DEEP_CATE$x5, DEEP_CATE$x6, DEEP_CATE$x7, DEEP_CATE$x8)
    DEEP_CATE_concatenated = rbind(DEEP_CATE_concatenated, DEEP_CATE)
  }
  
  ### match pattern
  DEEP_CATE_big = merge(DEEP_CATE_concatenated, ground_truth_data, by.x = 'pattern', by.y = 'keys')
  
  ### export data
  write.csv(DEEP_CATE_big, output_csv)
  
  ### calculate PEHE
  tau = DEEP_CATE_big$CATE - DEEP_CATE_big$phi
  PEHE = sqrt(sum(tau^2)/length(tau))
  output_summary[batch_number, 'PEHE'] = PEHE
  
  ### calculate MAPE
  MAPE_1 = tau/DEEP_CATE_big$phi
  MAPE_1 = MAPE_1[is.finite(MAPE_1)]
  MAPE = sum(abs(MAPE_1)) / length(tau)
  output_summary[batch_number, 'MAPE'] = MAPE
}

### calculate std and mean
output_summary[1,'PEHE_mean'] = rowMeans(as.matrix(t(output_summary['PEHE'])))
output_summary[1,'MAPE_mean'] = rowMeans(as.matrix(t(output_summary['MAPE'])))

output_summary[1,'PEHE_std'] = rowSds(as.matrix(t(output_summary['PEHE'])))
output_summary[1,'MAPE_std'] = rowSds(as.matrix(t(output_summary['MAPE'])))

### export data
write.csv(output_summary, output_summary_csv)
