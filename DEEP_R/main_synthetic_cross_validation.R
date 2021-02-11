### Input: synthetic data file representing features, treatment and outcome
### Output: DEEP results

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# start_time <- Sys.time()
library(pcalg)
library(dplyr)
source('get_pattern_table.R')
source('get_contingency_table.R')
source('get_edit_distance.R')
source('merge_patterns.R')
source('get_z_significant.R')
source('calculate_edit_distance.R')
source('adjust_CATE.R')
source('stratification.R')


# significance level in PC-select algorithm
alpha = 0.01

# group name
# data_names = c('data_20_4_4', 'data_40_4_4', 'data_60_4_4', 'data_80_4_4', 'data_100_4_4')
data_names = 'data'


for (data_name in data_names) {
  
  dir.create(paste("../synthetic_data/",data_name,"/deep_cross_validation", sep=""))
  
  for (batch in 1:10) {
    
    for (n_fold in 1:10) {
      
      start_time <- Sys.time()

      # data input
      data_file = paste("../synthetic_data/",data_name,"/causal_trees_cross_validation/causal_tree_cross_validation_batch_",batch,"_fold_",n_fold,"_train.csv", sep = "")
      output_csv = paste("../synthetic_data/",data_name,"/deep_cross_validation/deep_cross_validation_batch_",batch,"_fold_",n_fold,"_patterns.csv", sep="")
      
      input_data = read.csv(data_file)
      colnames(input_data) = toupper(colnames(input_data)) 
      input_data['X'] = NULL
      # input_data['CATE'] = NULL
      input_data_outcome = input_data['Y']
      input_data_variables = input_data[,!(names(input_data) %in% c('Y'))]
      
      # call PC-simple from pcalg
      pc1 = pcSelect(input_data_outcome, input_data_variables, alpha,
                     corMethod = "standard", verbose = FALSE, directed = TRUE)
      # save results
      Z = pc1$G[pc1$G]
      
      # whether variable(x) is independent of treatment(w)
      input_data_variables_gsq_test = input_data_variables[, which(pc1$G)]
      input_data_variables_gsq_test_matrix = matrix(unlist(input_data_variables_gsq_test),
                                                    ncol = ncol(input_data_variables_gsq_test), nrow = 10000)
      # gSquareBin(x=1, y=ncol(input_data_variables_gsq_test_matrix), S=NULL, 
      #            dm=input_data_variables_gsq_test_matrix, verbose = FALSE, adaptDF = FALSE)
      x_w_pvalue = sapply(1:(ncol(input_data_variables_gsq_test)-1), gSquareBin, y = ncol(input_data_variables_gsq_test),
                          S=NULL, dm=input_data_variables_gsq_test_matrix)
      x_w_pvalue_independent = ifelse(x_w_pvalue>0.05, TRUE, FALSE)
      names(x_w_pvalue_independent) = names(input_data_variables_gsq_test)[1:(ncol(input_data_variables_gsq_test)-1)]
      
      # number of features
      n_features = length(x_w_pvalue_independent)
      
      # stratification
      first_hash_table = stratification(n_features, input_data)
      
      # calculate pi_1, pi_2, phi
      first_hash_table$pi_1 = first_hash_table$n11 / (first_hash_table$n11 + first_hash_table$n12)
      first_hash_table$pi_2 = first_hash_table$n21 / (first_hash_table$n21 + first_hash_table$n22)
      first_hash_table$phi = first_hash_table$pi_1 - first_hash_table$pi_2
      first_hash_table$z_significant = get_z_significant(first_hash_table)
      first_hash_table$index = rownames(first_hash_table)
      
      # save significant patterns
      z_significant_threshold = 1.96  # gamma = 95%
      S = first_hash_table[which(first_hash_table$z_significant > z_significant_threshold),]
      S_bar = first_hash_table[which(first_hash_table$z_significant <= z_significant_threshold),]
      
      # calculate pairwise pattern distances
      # 8 - 16s
      # 10 - 5min
      # test_table = get_pattern_table(10)
      # edit_input = test_table
      edit_input = first_hash_table[,1:n_features]
      distance_matrix = data.frame(matrix(nrow = nrow(edit_input), ncol = nrow(edit_input)))
      distance_matrix = get_edit_distance(edit_input, distance_matrix)
      
      # get pairs' closest distance 
      closest_edit_distance = min(distance_matrix, na.rm = TRUE)
      
      # working table
      second_hash_table = first_hash_table
      
      # start loop here
      for (closest_edit_distance in 1:n_features) {
        
        found_pairs = TRUE
        has_insignificant = TRUE
        
        while (found_pairs & has_insignificant) {
          
          # get pairs that have the smallest distance
          closest_edit_distance_pairs = data.frame(which(distance_matrix == closest_edit_distance, arr.ind=TRUE))
          if (nrow(closest_edit_distance_pairs) == 0) {
            found_pairs = FALSE
            break
          }
          
          # check if pairs are all significant pairs
          if (all(second_hash_table[closest_edit_distance_pairs$row, 'z_significant'] > z_significant_threshold) && 
              all(second_hash_table[closest_edit_distance_pairs$col, 'z_significant'] > z_significant_threshold)) {
            has_insignificant = FALSE
            break
          }
          
          # remove significant pairs
          significant_pairs_row = second_hash_table[closest_edit_distance_pairs$row, 'z_significant'] > z_significant_threshold
          significant_pairs_col = second_hash_table[closest_edit_distance_pairs$col, 'z_significant'] > z_significant_threshold
          significant_pairs_row_col = t(rbind.data.frame(significant_pairs_row, significant_pairs_col))
          rownames(significant_pairs_row_col) = 1:nrow(significant_pairs_row_col)
          significant_pairs = which((significant_pairs_row_col[,1]==TRUE) & (significant_pairs_row_col[,2]==TRUE))
          closest_edit_distance_pairs = closest_edit_distance_pairs[-c(significant_pairs),]
          rownames(closest_edit_distance_pairs) = 1:nrow(closest_edit_distance_pairs)
          
          # remove merged but still appearing pairs
          index_row = strsplit(second_hash_table[closest_edit_distance_pairs$row, 'index'], split=" ") 
          index_col = strsplit(second_hash_table[closest_edit_distance_pairs$col, 'index'], split=" ") 
          index_flag = c()
          
          for (index in 1:length(index_row)) {
            if ((all(index_row[[index]] %in% index_col[[index]])) | (all(index_col[[index]] %in% index_row[[index]]))) {
              index_flag = c(index_flag, TRUE)
            } else {
              index_flag = c(index_flag, FALSE)
            }
          }
          
          if (sum(index_flag == TRUE) == length(index_row)) {
            break
          } else if (sum(index_flag == TRUE) > 0) {
            closest_edit_distance_pairs = closest_edit_distance_pairs[-which(index_flag),]
          }
          
          # calculate heterogeneity
          closest_edit_distance_pairs_heterogeneity = data.frame(abs(second_hash_table[closest_edit_distance_pairs$row, 'phi'] - 
                                                                       second_hash_table[closest_edit_distance_pairs$col, 'phi']))
          
          # get the pair with smallest heterogeneity
          pair_index = which(closest_edit_distance_pairs_heterogeneity == 
                               min(closest_edit_distance_pairs_heterogeneity, na.rm = TRUE))
          if (min(closest_edit_distance_pairs_heterogeneity, na.rm = TRUE) == Inf) {break}
          
          # reporting
          print(paste("Now working distance is", closest_edit_distance, "Heterogeneity is", min(closest_edit_distance_pairs_heterogeneity, na.rm = TRUE)))
          
          # record patterns to delete and to add
          patterns_to_delete = c()
          patterns_to_add = 0
          
          # start merging
          for (i in 1:length(pair_index)) {
            # get two patterns
            first_pattern_index = closest_edit_distance_pairs[pair_index[i], 'row']
            second_pattern_index = closest_edit_distance_pairs[pair_index[i], 'col']
            first_pattern = second_hash_table[first_pattern_index,]
            second_pattern = second_hash_table[second_pattern_index,]
            
            print(first_pattern)
            print(second_pattern)
            
            # determine whether is significant pattern
            if (first_pattern$z_significant < z_significant_threshold && 
                second_pattern$z_significant < z_significant_threshold) {
              # both are not significant
              new_pattern = merge_patterns(first_hash_table, first_pattern, second_pattern, feature_length=n_features)
              # delete old patterns
              patterns_to_delete = c(patterns_to_delete, first_pattern_index, second_pattern_index)
              
            } else if (first_pattern$z_significant >= z_significant_threshold && 
                       second_pattern$z_significant < z_significant_threshold) {
              # first is significant, second is not significant
              new_pattern = merge_patterns(first_hash_table, first_pattern, second_pattern, feature_length=n_features)
              # delete old patterns
              patterns_to_delete = c(patterns_to_delete, second_pattern_index)
              
            } else if (first_pattern$z_significant < z_significant_threshold && 
                       second_pattern$z_significant >= z_significant_threshold) {
              # first is not significant, second is significant
              new_pattern = merge_patterns(first_hash_table, first_pattern, second_pattern, feature_length=n_features)
              # delete old patterns
              patterns_to_delete = c(patterns_to_delete, first_pattern_index)
              
            } else {
              print('else next')
              next
            }
            # whether new pattern exists in second hash table (create by another path)
            pattern_exist = nrow(merge(new_pattern[1,-ncol(new_pattern)],second_hash_table[,-ncol(new_pattern)]))  
            if (!pattern_exist) {
              # whether new pattern is significant
              if (new_pattern$z_significant > z_significant_threshold) {
                S = rbind(S, new_pattern)
                rownames(S) <- 1:nrow(S)
              } 
              # append new pattern to hash table
              second_hash_table = rbind(second_hash_table, new_pattern)
              # print(new_pattern)
              patterns_to_add = patterns_to_add + 1
            }
          }
          
          # delete old patterns
          patterns_to_delete = unique(patterns_to_delete)
          second_hash_table = second_hash_table[-c(patterns_to_delete),]
          
          # re-index data frame
          rownames(second_hash_table) <- 1:nrow(second_hash_table)
          
          # # calculate new distance matrix
          # edit_input = second_hash_table[,1:n_features]
          # distance_matrix = data.frame(matrix(nrow = nrow(edit_input), ncol = nrow(edit_input)))
          # distance_matrix = get_edit_distance(edit_input, distance_matrix)
          
          # delete distances from old patterns 
          distance_matrix = distance_matrix[-patterns_to_delete,]
          distance_matrix = distance_matrix[,-patterns_to_delete]
          
          # add distances from new patterns
          if (patterns_to_add > 0) {
            rows_distance_matrix = nrow(distance_matrix)
            cols_distance_matrix = ncol(distance_matrix)
            
            for (new_col in (cols_distance_matrix + 1):(cols_distance_matrix + patterns_to_add)) {
              for (new_row in 1:(rows_distance_matrix + new_col - cols_distance_matrix - 1)) {
                distance_matrix[new_row, new_col] = calculate_edit_distance(second_hash_table[new_row,1:n_features], second_hash_table[new_col,1:n_features])
              }
            }
            distance_matrix[new_row + 1, new_col] = NA
            
          }
          
          rownames(distance_matrix) = 1:nrow(distance_matrix)
          
        }
      }
      
      print('Merging finished!')
      # end_time <- Sys.time()
      # end_time - start_time
      
      
      # adjustment for CATE
      third_hash_table = second_hash_table
      if (any(!x_w_pvalue_independent)) {
        print('Start adjusting...')
        # select rows that need adjustment
        col_adjustment = names(which(x_w_pvalue_independent==FALSE))
        select_by_col = is.na(third_hash_table[,col_adjustment])
        # select rows have NA in selected columns
        if (length(col_adjustment) > 1) {
          select_by_col_bool = apply(select_by_col, 1, any)
        } else {
          select_by_col_bool = select_by_col
        }
        # select rows that need adjustment and remove them from third hash table
        need_adjustment = third_hash_table[which(select_by_col_bool),]
        third_hash_table = third_hash_table[-which(select_by_col_bool),]
        
        # loop over rows in need_adjustment
        for (i in 1:nrow(need_adjustment)) {
          adjusted_CATE = adjust_CATE(first_hash_table, need_adjustment[i,], col_adjustment)
          need_adjustment[i, 'phi'] = adjusted_CATE
        }
        
        # feed new table to third_hash_table
        third_hash_table = rbind(third_hash_table, need_adjustment)
        rownames(third_hash_table) = 1:nrow(third_hash_table)
      }
      
      end_time <- Sys.time()
      print(end_time - start_time)
      print(end_time - start_time)
      
      # save results
      write.csv(third_hash_table, output_csv)
      
    }
    
  }
  
}






