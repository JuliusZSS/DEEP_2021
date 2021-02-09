# Input: DEEP patterns, test csv, ground truth csv
# Output: test results


rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data_names = c('data_20_4_4', 'data_40_4_4', 'data_60_4_4', 'data_80_4_4', 'data_100_4_4')
# data_names = c('data_80_4_4')

for (data_name in data_names) {
  
  for (n_batch in 1:10) {
    
    for (n_fold in 1:10) {
      
      # import data
      DEEP_patterns_csv = paste(data_name,'/deep_cross_validation/deep_cross_validation_batch_',n_batch,'_fold_',n_fold,'_patterns.csv',sep="")
      DEEP_patterns = read.csv(DEEP_patterns_csv)
      DEEP_patterns = DEEP_patterns[,-which(colnames(DEEP_patterns) == 'X')]
      
      test_csv = paste(data_name,'/causal_trees_cross_validation/causal_tree_cross_validation_batch_',n_batch,'_fold_',n_fold,'_test.csv',sep = "")
      test = read.csv(test_csv)
      test = test[,-which(colnames(test) == 'X')]
      colnames(test) = toupper(colnames(test))
      
      to_match = colnames(DEEP_patterns)[1:(which(colnames(DEEP_patterns) == 'n11')-1)]
      
      # match pattern
      for (row_index_1 in 1:nrow(test)) {
        
        print(row_index_1)
        
        for (row_index_2 in 1:nrow(DEEP_patterns)) {
          
          # get the test row
          row_from_test = test[row_index_1, to_match]
          
          # get pattern
          pattern = DEEP_patterns[row_index_2,to_match]
          n_NA = is.na(pattern)
          
          # remove NA places
          if (any(n_NA)) {
            row_from_test = row_from_test[-which(n_NA)]
            pattern = pattern[-which(n_NA)]
          } 
          
          # stop searching if it is found
          if (all(row_from_test == pattern)) {
            test[row_index_1, 'CATE'] = DEEP_patterns[row_index_2,'phi']
            break
          }
          
        }
        
      }
      
      test_2 = test
      
      # match ground truth
      ground_truth = read.csv(paste(data_name,'/ground_truth/binary_parents_of_y_8.csv',sep = ""))
      ground_truth = ground_truth[,which(colnames(ground_truth) %in% c('keys','phi'))]
      
      test_2['pattern'] = apply(test_2[,1:8], 1, paste, collapse = " ")
      
      # merge test_2 and ground_truth
      test_3 = merge(test_2, ground_truth, by.x = 'pattern', by.y = 'keys')
      
      test_3 = test_3[,-which(colnames(test_3) == 'pattern')]
      colnames(test_3)[ncol(test_3)] = 'ground_truth'
      
      output_csv = write.csv(test_3, paste(data_name,'/deep_cross_validation/results/deep_cross_validation_batch_',n_batch,'_fold_',n_fold,'_results.csv',sep = ""))
      
    }
    
  }
  
}




