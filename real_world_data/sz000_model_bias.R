### Input: real world data
### Output: weighted average bias

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(causalTree)
library(caret)
library(dplyr)
library(matrixStats)
library(ggplot2)
library(devtools) 


`%nin%` = Negate(`%in%`)

# read real world data ####
# US census
data_name = 'US_census'
file_1 = read.csv('input_data/adult_binary.csv')
file_2 = read.csv('input_data/census-income_binary.csv')
# define treatment and outcome
treatment = 'educ.12'
outcome = 'income.50K'
# combine files
data = rbind(file_1, file_2)

# # marketing campaign
# data_name = 'marketing_campaign'
# file_1 = read.csv('input_data/marketing_campaign_train.csv')
# file_2 = read.csv('input_data/marketing_campaign_valid.csv')
# # define treatment and outcome
# treatment = 'TREATMENT'
# outcome = 'PURCHASE'
# # combine files
# data = rbind(file_1, file_2)

# # email analytics
# data_name = 'email_analytics'
# file = read.csv('input_data/emailAnalytics.csv')
# # define treatment and outcome
# treatment = 'segment'
# outcome = 'visit'
# # combine files
# data = file

# # email analytics women
# data_name = 'email_analytics_women'
# file = read.csv('input_data/emailAnalytics_women_SZ.csv')
# # define treatment and outcome
# treatment = 'segment'
# outcome = 'visit'
# # combine files
# data = file

# # email analytics men
# data_name = 'email_analytics_men'
# file = read.csv('input_data/emailAnalytics_men_SZ.csv')
# # define treatment and outcome
# treatment = 'segment'
# outcome = 'visit'
# # combine files
# data = file

# # data from causal decision tree
# data_name = 'adult_binary'
# file = read.csv('input_data/adultAllBinary_Simple_1.csv')
# # define treatment and outcome
# treatment = 'education.num.12'
# outcome = 'class'
# # combine files
# data = file

# # criteo_uplift
# data_name = 'criteo_uplift'
# file = read.csv('input_data/criteo-uplift-v2.1_binarySZ.csv')
# # define treatment and outcome
# treatment = 'treatment'
# outcome = 'visit'
# # combine files
# data = file

# # twins
# data_name = 'twins'
# file = read.csv('input_data/Twins_bydebo_binarySZ.csv')
# # define treatment and outcome
# treatment = 'T'
# outcome = 'target'
# # combine files
# data = file

################################################################################
dir.create('real_world_data/data')
dir.create(paste('real_world_data/data/model_vs_contingency_table_',data_name,sep=''))

### 10 times 2-fold cross validation
cross_validation_times = 10
cross_validation_folds = 2

### to save final bias
final_weighted_average_bias_table = data.frame(matrix(nrow = cross_validation_times*cross_validation_folds, 
                                                      ncol = 1))
  
### start loop here
# current_cross_validation_time = 1
for (current_cross_validation_time in 1:cross_validation_times) {
  
  ### set random seed
  set.seed(234 + current_cross_validation_time)
  
  ### 2-fold 10 times cross validation
  data_folds = createFolds(as.numeric(rownames(data)), k=cross_validation_folds)
  
  # current_fold = 1
  for (current_fold in 1:cross_validation_folds) {
    
    data_for_causal_tree = data[data_folds[current_fold][[1]],]
    data_as_ground_truth = data[data_folds[-current_fold][[1]],]
    
    ### train causal tree
    ### create formula
    formula_ct = formula(paste(outcome,paste(names(data)[which(colnames(data)!=outcome & colnames(data)!=treatment)],collapse='+'),sep='~'))
    
    ### build tree (using default parameters)
    tree <- causalTree(formula_ct, data = data_for_causal_tree,
                       treatment = array(c(unlist(data_for_causal_tree[treatment]))), 
                       split.Rule = "CT",cv.option = "CT", 
                       split.Honest = T, cv.Honest = T,
                       split.Bucket = F, xval = 5, cp = 0, minsize = 20,
                       propensity = 0.5, x = TRUE, y = TRUE)
    
    # tree$cptable
    
    ### use causal tree to predict
    ground_truth_predictions = data.frame(predict(tree,data.frame(data_as_ground_truth[,outcome], 
       data_as_ground_truth[,which(colnames(data_as_ground_truth)!=outcome)]),type='vector'))
    colnames(ground_truth_predictions) = c('CATE')
    
    ### plot causal tree and save it
    jpeg(paste('data/model_vs_contingency_table_',data_name,'/cross_validation_time_',
               current_cross_validation_time,'_fold_',current_fold,'.png',sep=''))
    rpart.plot(tree)
    dev.off()
    
    ### get leaves patterns and their causal effect
    leaf_nodes_index_causal_effect = data.frame(matrix(nrow = length(tree$frame$var), ncol = 2))
    colnames(leaf_nodes_index_causal_effect) = c('nodes','causal_effect')
    
    leaf_nodes_index_causal_effect$nodes = tree$frame$var
    leaf_nodes_index_causal_effect$causal_effect = tree$frame$yval
    
    leaf_nodes_index_causal_effect = leaf_nodes_index_causal_effect[
      which(leaf_nodes_index_causal_effect$nodes== '<leaf>'),]
    
    ### find corresponding causal effect in test dataset
    data_for_causal_tree_output = data.frame(tree$where)
    colnames(data_for_causal_tree_output) = c('leaf_index')
    
    ### data + leaf index
    data_for_causal_tree = data.frame(data_for_causal_tree, data_for_causal_tree_output) 
    
    ### blank for found patterns
    found_common_patterns = data.frame(matrix(nrow = nrow(leaf_nodes_index_causal_effect), 
                                              ncol = ncol(data_as_ground_truth)))
    colnames(found_common_patterns) = colnames(data_as_ground_truth)
    
    ### loop over to find common pattern
    for (i in 1:nrow(leaf_nodes_index_causal_effect)) {
      index = rownames(leaf_nodes_index_causal_effect[i,])
      ### select all rows belonging to the leaf_index
      to_find_common_pattern = data_for_causal_tree[which(data_for_causal_tree['leaf_index']==index),]
      ### filter over columns
      found_common_pattern_cols = which(apply(to_find_common_pattern, 2, 
                                              function(x) length(unique(x))) == 1)
      ### get pattern
      found_common_pattern = to_find_common_pattern[1, found_common_pattern_cols]
      ### save pattern
      found_common_patterns[i,(colnames(found_common_pattern))] = found_common_pattern
    }
    
    ### clean unnecessary columns
    found_common_patterns[,which(colnames(found_common_patterns) %nin% 
                                   c(row.names(tree$splits),'leaf_index'))] = NA
    
    ### start on the other half data
    ### record number of cases
    contingency_table = data.frame(0,0,0,0,0,0,0)
    colnames(contingency_table) = c('n11','n12','n21','n22','pi_1','pi_2','phi')
    found_common_patterns_contingency_table = data.frame(found_common_patterns, contingency_table)
    
    ### loop through ground truth table
    for (i in 1:nrow(data_as_ground_truth)) {
      row = data_as_ground_truth[i,]
      
      for (j in 1:nrow(found_common_patterns)) {
        pattern = found_common_patterns[j,which(!is.na(found_common_patterns[j,]))]
        
        if (all(row[,which(colnames(row) %in% colnames(pattern))] == pattern[,-(ncol(pattern))])) {
          
          found_common_patterns_contingency_table_row = j
          
          if (row[,treatment] == 1 && row[,outcome] == 1) {
            found_common_patterns_contingency_table[found_common_patterns_contingency_table_row,'n11'] = 
              found_common_patterns_contingency_table[found_common_patterns_contingency_table_row,'n11'] + 1
          }
          if (row[,treatment] == 1 && row[,outcome] == 0) {
            found_common_patterns_contingency_table[found_common_patterns_contingency_table_row,'n12'] = 
              found_common_patterns_contingency_table[found_common_patterns_contingency_table_row,'n12'] + 1
          }
          if (row[,treatment] == 0 && row[,outcome] == 1) {
            found_common_patterns_contingency_table[found_common_patterns_contingency_table_row,'n21'] = 
              found_common_patterns_contingency_table[found_common_patterns_contingency_table_row,'n21'] + 1
          }
          if (row[,treatment] == 0 && row[,outcome] == 0) {
            found_common_patterns_contingency_table[found_common_patterns_contingency_table_row,'n22'] = 
              found_common_patterns_contingency_table[found_common_patterns_contingency_table_row,'n22'] + 1
          }
        }
      }
    }

    ### calculate pi_1, pi_2, phi
    found_common_patterns_contingency_table$pi_1 = found_common_patterns_contingency_table$n11 / 
      (found_common_patterns_contingency_table$n11 + found_common_patterns_contingency_table$n12)
    
    found_common_patterns_contingency_table$pi_2 = found_common_patterns_contingency_table$n21 / 
      (found_common_patterns_contingency_table$n21 + found_common_patterns_contingency_table$n22)
    
    found_common_patterns_contingency_table$phi = found_common_patterns_contingency_table$pi_1 - 
      found_common_patterns_contingency_table$pi_2
    
    ### append causal effect from causal tree
    found_common_patterns_contingency_table$causal_effect_model = leaf_nodes_index_causal_effect$causal_effect
    
    ### calculate bias
    found_common_patterns_contingency_table$bias = abs(found_common_patterns_contingency_table$phi-
                                                    found_common_patterns_contingency_table$causal_effect_model)
    found_common_patterns_contingency_table$N = found_common_patterns_contingency_table$n11 +
      found_common_patterns_contingency_table$n12 + found_common_patterns_contingency_table$n21 + 
      found_common_patterns_contingency_table$n22
    
    final_weighted_average_bias = sum(found_common_patterns_contingency_table$bias * 
                                        found_common_patterns_contingency_table$N) / 
      sum(found_common_patterns_contingency_table$N)
    
    write.csv(found_common_patterns_contingency_table, 
              paste('data/model_vs_contingency_table_',data_name,'/model_vs_contingency_table_',
                                                             'cv_',current_cross_validation_time,
                                                             '_fold_',current_fold,'.csv',sep=''))
    
    final_weighted_average_bias_table[2*current_cross_validation_time-current_fold%%2,1] = 
      final_weighted_average_bias
    
    ### causal tree output on ground truth 
    data_as_ground_truth = data.frame(data_as_ground_truth, ground_truth_predictions)

    ### save train and test data 
    write.csv(data_for_causal_tree, paste('data/model_vs_contingency_table_',data_name,'/cross_validation_time_',
                                          current_cross_validation_time,'_fold_',current_fold,
                                          '_for_model.csv',sep=''))
    write.csv(data_as_ground_truth, paste('data/model_vs_contingency_table_',data_name,'/cross_validation_time_',
                                          current_cross_validation_time,'_fold_',current_fold,
                                          '_as_ground_truth.csv',sep=''))
  }
}

### calculate mean and std of final_weighted_average_bias
final_weighted_average_bias_table$mean = NA
final_weighted_average_bias_table$std = NA
final_weighted_average_bias_table$mean[1] = mean(final_weighted_average_bias_table[,1])
final_weighted_average_bias_table$std[1] = rowSds(t(final_weighted_average_bias_table[,1]))

write.csv(final_weighted_average_bias_table,paste('data/model_vs_contingency_table_',
                                                  data_name,'_final_weighted_average_bias_table.csv',sep=''))

################################################################################

### decile plot
source('sz001_decile_plot.R')
sz001_decile_plot(data_name,treatment,outcome,cross_validation_times,cross_validation_folds)








