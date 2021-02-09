### Input: sz002_files_into_hashable_table.R
### Input: number of data batches
### Output: data batches

generate_synthetic_data_batches <- function(p, pt, py, n=10000, number_of_batches=1) {
  
  library(matrixStats)
  
  source('sz002_files_into_hashable_table.R')
  source('save_DAG.R')
  
  ### set hyper-parameters
  p <- p # number of total covariates
  pt <- pt # number of covariates affecting treatment effects
  py <- py # number of covariates affecting outcomes but not treatment effects
  n <- n # total size of the dataset ### change made
  
  ### set loops here
  number_of_batches = number_of_batches
  
  ### summary of results
  copied = FALSE
  
  ### save combined binary and binary_cf for causal trees
  data_for_causal_trees_copied = FALSE

  ### make new folder
  dir.create(paste('data/ground_truth',sep=''))
  
  binary_and_binary_cf_and_output_table <- files_into_hashable_table(p, pt, py, n)
  
  ### get output table
  binary_and_binary_cf <- binary_and_binary_cf_and_output_table[1][[1]]
  output_table <- binary_and_binary_cf_and_output_table[2][[1]]
  
  ### copy phi to summary
  summary_results = data.frame(output_table['keys'], output_table['phi'])
  copied = TRUE

  ### get parents of y and number of covariates
  parents_of_y = as.integer(binary_and_binary_cf[6]) + as.integer(binary_and_binary_cf[7])
  number_of_covariants = as.integer(binary_and_binary_cf[5])
  
  ### save output table
  write.csv(output_table, paste('data/ground_truth','/binary_parents_of_y_',parents_of_y,'.csv',sep=''))
  
  ### get data
  continous_data = binary_and_binary_cf[1][[1]]
  binary_data = binary_and_binary_cf[2][[1]]
  continous_data_cf = binary_and_binary_cf[3][[1]]
  binary_data_cf = binary_and_binary_cf[4][[1]]

  ### save data 
  n = nrow(continous_data)
  write.csv(continous_data,file=paste('data/ground_truth','/continous_',n,'.csv',sep=''))
  write.csv(binary_data,file=paste('data/ground_truth','/binary_',n,'.csv',sep=''))
  write.csv(continous_data_cf,file=paste('data/ground_truth','/continous_cf_',n,'.csv',sep=''))
  write.csv(binary_data_cf,file=paste('data/ground_truth','/binary_cf_',n,'.csv',sep=''))
  
  ### get DAG
  continous_data_DAG = binary_and_binary_cf[8][[1]]
  binary_data_DAG = binary_and_binary_cf[9][[1]]
  continous_data_cf_DAG = binary_and_binary_cf[10][[1]]
  binary_data_cf_DAG = binary_and_binary_cf[11][[1]]
  
  ### save DAG
  save_DAG(continous_data_DAG, paste('data/ground_truth','/continous_data_DAG','.png',sep=''))
  save_DAG(binary_data_DAG, paste('data/ground_truth','/binary_data_DAG','.png',sep=''))
  save_DAG(continous_data_cf_DAG, paste('data/ground_truth','/continous_data_cf_DAG','.png',sep=''))
  save_DAG(binary_data_cf_DAG, paste('data/ground_truth','/binary_data_cf_DAG','.png',sep=''))
  
  ### append data
  save_col_names = colnames(binary_data)
  colnames(binary_data) = c()
  colnames(binary_data_cf) = c()
  data_for_causal_trees = rbind.data.frame(binary_data, binary_data_cf)
  colnames(data_for_causal_trees) = save_col_names
  
  data_for_causal_trees_output = list(data_for_causal_trees)
  data_for_causal_trees_copied = TRUE

  # calculate mean and std of phi
  summary_results_output = data.frame(summary_results['keys'], 0) 
  colnames(summary_results_output) = c('keys', 'phi')
  
  summary_results_output['phi'] = summary_results[,-1]

  write.csv(summary_results_output, paste('data/ground_truth_summary.csv'))
  
  # combine binary and binary_cf
  write.csv(data_for_causal_trees, paste('data/ground_truth/binary_binary_cf_',n,'.csv',sep=''))

  return(data_for_causal_trees_output)
  
}












