### Input: sz001_synthetic_data_sz.R
### Output: hashable CATE

files_into_hashable_table <- function(p, pt, py, n) {
  
  ### import from sz001_synthetic_data_sz.R
  source('sz001_synthetic_data_sz.R')
  binary_and_binary_cf = synthesize_data(p, pt, py, n)
  
  ### hyper-parameters
  parents_of_y = as.integer(binary_and_binary_cf[6]) + as.integer(binary_and_binary_cf[7])
  number_of_covariants = as.integer(binary_and_binary_cf[5])
  
  ### read files
  file_1 = binary_and_binary_cf[2][[1]]
  file_2 = binary_and_binary_cf[4][[1]]
  
  ### combined dataframe
  file_all = cbind(file_1[,], file_2[,-1:-(number_of_covariants)])
  
  ### hashable keys
  keys = file_all[,1:parents_of_y]
  keys_chars = apply(keys, 2, as.character)
  # keys_chars = cbind(keys_chars, c(''))
  
  keys_one_char = data.frame(matrix(ncol = 1, nrow = nrow(keys_chars)))

  for (i in 1:nrow(keys_chars)) {
    keys_one_char[i,1] = paste(keys_chars[i,], collapse=' ')
  }
  
  ### hashable keys, w, w_, y, y_
  keys_w_cfw_y_cfy = cbind(keys_one_char, file_all[,c('w','w_','y','y_')])
  colnames(keys_w_cfw_y_cfy) = c('keys','w','w_','y','y_')
  
  ### initialise output table
  keys_one_char_no_repeat = unique(keys_one_char)
  output_table <- data.frame(keys_one_char_no_repeat, 0, 0, 0)
  colnames(output_table) = c('keys', 'case_number', 'accumulated_causal_effect', 'phi')
  
  ### loop and assign each row
  for(i in 1:nrow(keys_w_cfw_y_cfy)) {
    row <- keys_w_cfw_y_cfy[i,]
    
    # do stuff with row
    if (row['w'] == 1) {
      output_table[which(output_table['keys'] == c(row['keys'])),'case_number'] = 
        output_table[which(output_table['keys'] == c(row['keys'])),'case_number'] + 1
      
      output_table[which(output_table['keys'] == c(row['keys'])),'accumulated_causal_effect'] = 
        output_table[which(output_table['keys'] == c(row['keys'])),'accumulated_causal_effect'] +
        row['y'] - row['y_'] 
      
    } else {
      output_table[which(output_table['keys'] == c(row['keys'])),'case_number'] = 
        output_table[which(output_table['keys'] == c(row['keys'])),'case_number'] + 1
      
      output_table[which(output_table['keys'] == c(row['keys'])),'accumulated_causal_effect'] = 
        output_table[which(output_table['keys'] == c(row['keys'])),'accumulated_causal_effect'] +
        row['y_'] - row['y'] 
    }

  }
  
  output_table['phi'] = output_table['accumulated_causal_effect'] / output_table['case_number']
  
  return(list(binary_and_binary_cf, output_table))
  
}








