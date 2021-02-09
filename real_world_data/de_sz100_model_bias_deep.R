### Input: DEEP results, split data from sz001_model_bias.R
### Output: weighted average bias

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(stringr)
library(matrixStats)
source('get_contingency_table.R')
source("get_causal_effect_from_contingency_table.R")


### define dataset name
data_name = 'US_census'
treatment = 'educ.12'
outcome = 'income.50K'

### email analytics
data_name = 'email_analytics'
treatment = 'segment'
outcome = 'visit'

### marketing campaign
data_name = 'marketing_campaign'
treatment = 'TREATMENT'
outcome = 'PURCHASE'

### adult binary
data_name = 'adult_binary'
treatment = 'education.num.12'
outcome = 'class'


################################################################################
### 10 times 2-fold cross validation
cross_validation_times = 10
cross_validation_folds = 2

### to save final bias
final_weighted_average_bias_table = data.frame(matrix(nrow = cross_validation_times*cross_validation_folds, 
                                                      ncol = 1))

### define cv time and number of folds
for (current_cross_validation_time in 1:10) {
  for (current_fold in 1:2) {
    ### import data csv
    output_csv = paste("data_",data_name,"/model_vs_contingency_table_",
                       data_name,"_deep/model_vs_contingency_table_cv_",
                       current_cross_validation_time,"_fold_",current_fold,".csv",sep="")
    
    pattern_to_match_csv = paste("data_",data_name,"/model_vs_contingency_table_",
                                 data_name,"_deep/output/cross_validation_time_",
                                 current_cross_validation_time,"_fold_",
                                 current_fold,"_for_model_DEEPOutput.csv",sep="")
    
    ground_truth_csv = paste("data_",data_name,"/model_vs_contingency_table_",
                             data_name,"_deep/cross_validation_time_",
                             current_cross_validation_time,"_fold_",
                             current_fold,"_as_ground_truth.csv",sep="")
    ground_truth_csv_CATE = paste("data_",data_name,"/model_vs_contingency_table_",
                                  data_name,"_deep/cross_validation_time_",
                                  current_cross_validation_time,"_fold_",
                                  current_fold,"_as_ground_truth_CATE.csv",sep="")
    ### import data
    pattern_to_match_import = read.csv(pattern_to_match_csv)
    ground_truth = read.csv(ground_truth_csv)
    
    ### remove columns
    pattern_to_match = pattern_to_match_import[,1:(which(colnames(pattern_to_match_import)=="n11")-1)]
    pattern_to_match["causal_effect"] = pattern_to_match_import$causalEffect
    
    ### remove brackets
    pattern_column = colnames(pattern_to_match)
    for (i in 1:length(pattern_column)) {
      # remove potential other characters
      if (grepl("X.", pattern_column[i], fixed=TRUE)) {
        changed_name = str_split(pattern_column[i], "X.")[[1]]
        changed_name = changed_name[2]
        changed_name = substr(changed_name, 1, nchar(changed_name)-1)
        colnames(pattern_to_match)[i] = changed_name
      }
    }
    
    ### create pattern
    pattern_column_to_match = colnames(pattern_to_match)[1:(ncol(pattern_to_match)-1)]
    
    merged_table = merge(ground_truth, pattern_to_match, 
                         by.x = pattern_column_to_match,
                         by.y = pattern_column_to_match)
    write.csv(merged_table, ground_truth_csv_CATE)
    causal_effect = unique(merged_table$causal_effect)
    
    ### create empty contingency table
    contingency_table = data.frame(matrix(nrow = length(causal_effect), ncol = ncol(merged_table)))
    colnames(contingency_table) = colnames(merged_table)
    contingency_table[treatment] = NULL
    contingency_table[outcome] = NULL
    contingency_table['n11'] = NA
    contingency_table['n12'] = NA
    contingency_table['n21'] = NA
    contingency_table['n22'] = NA
    contingency_table['pi_1'] = NA
    contingency_table['pi_2'] = NA
    contingency_table['phi'] = NA
    contingency_table['bias'] = NA
    contingency_table['N'] = NA
    
    ### loop through subset
    for (i in 1:length(causal_effect)) {
      merged_table_subset = merged_table[which(merged_table$causal_effect==causal_effect[i]),]
      ### get n11 n12 n21 n22
      n11_n12_n21_n22 = get_contingency_table(merged_table_subset, treatment, outcome)
      ### get pattern
      merged_table_subset_pattern = merged_table_subset
      merged_table_subset_pattern[treatment] = NULL
      merged_table_subset_pattern[outcome] = NULL
      merged_table_subset_pattern['causal_effect'] = NULL
      
      one_pattern = merged_table_subset_pattern[1,]
      
      for (j in 1:ncol(merged_table_subset_pattern)) {
        if_unique = length(unique(merged_table_subset_pattern[,j]))
        # print(if_unique)
        if (if_unique != 1) {
          one_pattern[1,j] = NA
        }
      }
      ### fill in contingency table
      contingency_table[i,colnames(one_pattern)] = one_pattern
      contingency_table[i,'causal_effect'] = merged_table_subset[1,'causal_effect']
      contingency_table[i,c('n11','n12','n21','n22')] = n11_n12_n21_n22
      
    }
    
    ### create contingency table
    contingency_table$pi_1 = contingency_table$n11 / (contingency_table$n11 + contingency_table$n12)
    contingency_table$pi_2 = contingency_table$n21 / (contingency_table$n21 + contingency_table$n22)
    contingency_table$phi = contingency_table$pi_1 - contingency_table$pi_2
    contingency_table$bias = abs(contingency_table$phi - contingency_table$causal_effect)
    contingency_table$N = contingency_table$n11 + contingency_table$n12 + contingency_table$n21 + contingency_table$n22
    
    ### remove NA in phi column
    if (any(is.na(contingency_table$phi))) {
      contingency_table = contingency_table[-which(is.na(contingency_table$phi)),]
    }

    write.csv(contingency_table, output_csv)
    
    final_weighted_average_bias = sum(contingency_table$bias * contingency_table$N) / sum(contingency_table$N)
    
    final_weighted_average_bias_table[2*current_cross_validation_time-current_fold%%2,1] = 
      final_weighted_average_bias
  }
}

### calculate mean and std of final_weighted_average_bias
final_weighted_average_bias_table$mean = NA
final_weighted_average_bias_table$std = NA
final_weighted_average_bias_table$mean[1] = mean(final_weighted_average_bias_table[,1])
final_weighted_average_bias_table$std[1] = rowSds(t(final_weighted_average_bias_table[,1]))

write.csv(final_weighted_average_bias_table,paste('data_',data_name,'/model_vs_contingency_table_',
                                                  data_name,'_final_weighted_average_bias_table_deep.csv',sep=''))

### decile plot
source('sz101_decile_plot_deep.R')
sz101_decile_plot_deep(data_name,treatment,outcome,cross_validation_times,cross_validation_folds)




