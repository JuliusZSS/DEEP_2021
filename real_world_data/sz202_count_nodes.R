# Input: interaction trees results
# Output: average number of nodes

library(matrixStats)

# data_name = 'US_census'
# data_name = 'marketing_campaign'
# data_name = 'email_analytics'
# data_name = 'email_analytics_women'
# data_name = 'email_analytics_men'
# data_name = 'adult_binary'
# data_name = 'criteo_uplift'
# data_name = 'twins'

data_names = c('US_census', 'marketing_campaign', 
               'email_analytics', 'email_analytics_women', 'email_analytics_men',
               'adult_binary', 'criteo_uplift', 'twins')

################################################################################

for (data_name in data_names) {
  
  n_nodes_matrix = data.frame(matrix(nrow = 20, ncol = 1))
  colnames(n_nodes_matrix) = 'n_nodes'
  
  # start loop here
  for (n_cv in 1:10) {
    
    for (n_fold in 1:2) {
      
      file_1 = paste('data_',data_name,'/model_vs_contingency_table_',data_name,'_IT/model_vs_contingency_table_cv_',n_cv,'_fold_',n_fold,'.csv', sep = "")
      
      data_1 = read.csv(file_1)
      
      n_nodes_matrix[(2*n_cv - n_fold %% 2), 1] = nrow(data_1)
      
    }
    
  }
  
  print(mean(n_nodes_matrix[, 1]))
  print(rowSds(t(n_nodes_matrix[, 1])))
  
}




