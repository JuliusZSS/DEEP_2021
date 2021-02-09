# Input: number of features, input_data, 
# Output: first_hash_table

source('get_pattern_table.R')
source('get_contingency_table.R')


stratification <- function(n_features, input_data) {
  
  # first hash table
  first_hash_table = get_pattern_table(n_features)
  first_hash_table['n11'] = 0
  first_hash_table['n12'] = 0
  first_hash_table['n21'] = 0
  first_hash_table['n22'] = 0
  first_hash_table['pi_1'] = 0
  first_hash_table['pi_2'] = 0
  first_hash_table['phi'] = 0
  
  # add pattern column in input_data data frame
  input_data$pattern = apply(input_data[,1:n_features], 1, function(x) {return(paste(x, collapse = ""))})
  
  # loop over first hash table to find counts
  for (i in 1:nrow(first_hash_table)) {
    current_row = first_hash_table[i,1:n_features]
    current_row_p = paste(current_row, collapse = "")
    # get correct subset
    input_data_subset = subset(input_data, input_data$pattern == current_row_p)
    
    # calculate contingency table
    n11_n12_n21_n22 = get_contingency_table(input_data_subset, treatment = 'W', outcome = 'Y')
    first_hash_table[i,c('n11','n12','n21','n22')] = n11_n12_n21_n22
  }
  
  # key step: remove unused rows
  if (length(which(first_hash_table$n11+first_hash_table$n12+first_hash_table$n21+first_hash_table$n22==0))) {
    first_hash_table = first_hash_table[-which(first_hash_table$n11+first_hash_table$n12+first_hash_table$n21+first_hash_table$n22==0),]
  }
  
  return(first_hash_table)
  
}



