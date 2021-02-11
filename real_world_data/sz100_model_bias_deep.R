# Input: ground truth, DEEP patterns
# Output: bias

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(matrixStats)
source('get_contingency_table.R')
source('get_causal_effect_from_contingency_table.R')

# import data
data_name = 'US_census'
data_treatment = 'educ.12'
data_outcome = 'income.50K'

# # email analytics
# data_name = 'email_analytics'
# data_treatment = 'segment'
# data_outcome = 'visit'

# # email analytics men
# data_name = 'email_analytics_men'
# data_treatment = 'segment'
# data_outcome = 'visit'

# # email analytics women
# data_name = 'email_analytics_women'
# data_treatment = 'segment'
# data_outcome = 'visit'

# # marketing campaign
# data_name = 'marketing_campaign'
# data_treatment = 'TREATMENT'
# data_outcome = 'PURCHASE'

# # adult binary
# data_name = 'adult_binary'
# data_treatment = 'education.num.12'
# data_outcome = 'class'

# # twins
# data_name = 'twins'
# data_treatment = 'T'
# data_outcome = 'target'

# # twins
# data_name = 'criteo_uplift'
# data_treatment = 'treatment'
# data_outcome = 'visit'

################################################################################
dir.create(paste('../real_world_data/data/model_vs_contingency_table_', data_name, '_deep/results', sep=''))

save_bias = data.frame(matrix(nrow = 20, ncol = 3))

for (n_batch in 1:10) {
  for (n_fold in 1:2) {
    
    ground_truth_file = paste('data/model_vs_contingency_table_',data_name,'/cross_validation_time_',n_batch,'_fold_',n_fold,'_as_ground_truth.csv',sep = "")
    patterns_file = paste('data/model_vs_contingency_table_',data_name,'_deep/cross_validation_batch_',n_batch,'_fold_',n_fold,'_patterns.csv',sep="")
    output_file = paste('data/model_vs_contingency_table_',data_name,'_deep/results/cross_validation_batch_',n_batch,'_fold_',n_fold,'_results.csv',sep="")
    
    # clean patterns
    patterns = read.csv(patterns_file)
    patterns = patterns[,-which(colnames(patterns) == 'X')]
    pattern_names = colnames(patterns)[1:(which(colnames(patterns) == 'n11')-1)]
    # clean ground truth
    ground_truth = read.csv(ground_truth_file)
    ground_truth = ground_truth[,-which(colnames(ground_truth) %in% c('X','CATE'))]
    ground_truth = ground_truth %>% relocate(all_of(data_treatment), .after = last_col())
    ground_truth = ground_truth %>% relocate(all_of(data_outcome), .after = last_col())
    colnames(ground_truth)[(ncol(ground_truth)-1):ncol(ground_truth)] = c('W', 'Y')
    ground_truth = ground_truth[,c(pattern_names, 'W', 'Y')]
    
    # loop over patterns
    for (row_1 in 1:nrow(patterns)) {
      
      working_ground_truth = ground_truth
      p = patterns[row_1, pattern_names]
      
      # remove NA places
      n_NA = is.na(p)
      if (any(n_NA)) {
        working_ground_truth = working_ground_truth[-which(n_NA)]
        p = p[-which(n_NA)]
      } 
      
      # summarise pattern in ground truth data frame
      p_paste = paste(p, collapse = " ")
      if (length(p) > 1) {
        working_ground_truth$pattern = apply(working_ground_truth[,1:(which(colnames(working_ground_truth) == 'W')-1)],
                                             1, paste, collapse=" ")
        # get correct subset
        subset_ground_truth = subset(working_ground_truth, working_ground_truth$pattern == p_paste)
      } 
      else if (length(p) == 1) {
        # get correct subset
        subset_ground_truth = subset(working_ground_truth, working_ground_truth[,colnames(p)] == p_paste)
      }
      else {
        subset_ground_truth = working_ground_truth
      }
      
      # get contingency table using ground truth
      table_ground_truth = get_contingency_table(subset_ground_truth, 'W', 'Y')
      patterns[row_1, 'CATE_n11'] = table_ground_truth[[1]]
      patterns[row_1, 'CATE_n12'] = table_ground_truth[[2]]
      patterns[row_1, 'CATE_n21'] = table_ground_truth[[3]]
      patterns[row_1, 'CATE_n22'] = table_ground_truth[[4]]
      
    }
    
    # calculate CATE
    patterns$CATE_n11 = patterns$CATE_n11 + 0.5
    patterns$CATE_n12 = patterns$CATE_n12 + 0.5
    patterns$CATE_n21 = patterns$CATE_n21 + 0.5
    patterns$CATE_n22 = patterns$CATE_n22 + 0.5
    patterns[, 'CATE'] = get_causal_effect_from_contingency_table(list(patterns$CATE_n11,
                                                                       patterns$CATE_n12,
                                                                       patterns$CATE_n21,
                                                                       patterns$CATE_n22))
    # calculate bias and N
    patterns['bias'] = abs(patterns$CATE - patterns$phi)
    patterns['N'] = patterns$CATE_n11 + patterns$CATE_n12 + patterns$CATE_n21 + patterns$CATE_n22
    
    # save patterns
    write.csv(patterns, output_file)
    
    # weighted average bias
    weighted_avg_bias = sum(patterns['bias'] * patterns['N']) / sum(patterns['N'])
    save_bias[(2*n_batch) - n_fold%%2, 1] = weighted_avg_bias
    

  }
  
}

# std and mean of bias and save it
save_bias[1, 2] = mean(save_bias[,1])
save_bias[1, 3] = rowSds(t(save_bias[,1]))
colnames(save_bias) = c('bias', 'mean_bias', 'std_bias')

write.csv(save_bias, paste('data/model_vs_contingency_table_',data_name,'_final_weighted_average_bias_table_deep.csv', sep=""))






