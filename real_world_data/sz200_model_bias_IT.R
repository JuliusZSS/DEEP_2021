# Input: model_vs_contingency_table
# Output: bias

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(matrixStats)

# US census
data_name = 'US_census'

# # marketing campaign
# data_name = 'marketing_campaign'

# # email analytics
# data_name = 'email_analytics'

# # email analytics women
# data_name = 'email_analytics_women'

# # email analytics men
# data_name = 'email_analytics_men'

# # adult binary
# data_name = 'adult_binary'

# # twins
# data_name = 'twins'

# # criteo uplift
# data_name = 'criteo_uplift'


################################################################################
save_bias = data.frame(matrix(nrow = 20, ncol = 3))

for (n_batch in 1:10) {
  for (n_fold in 1:2) {
    
    file_1 = paste('data/model_vs_contingency_table_',data_name,'_IT/model_vs_contingency_table_cv_',n_batch,'_fold_',n_fold,'.csv',sep='')
    data_1 = read.csv(file_1)
    
    # weighted average bias
    weighted_avg_bias = sum(data_1['bias'] * data_1['N']) / sum(data_1['N'])
    save_bias[(2*n_batch) - n_fold%%2, 1] = weighted_avg_bias
    
  }
}


# std and mean of bias and save it
save_bias[1, 2] = mean(save_bias[,1])
save_bias[1, 3] = rowSds(t(save_bias[,1]))
colnames(save_bias) = c('bias', 'mean_bias', 'std_bias')

write.csv(save_bias, paste('data/model_vs_contingency_table_',data_name,'_final_weighted_average_bias_table_IT.csv', sep=""))













