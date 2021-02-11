# Input: uplift results
# Output: average bias


rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(causalTree)
library(caret)
library(dplyr)
library(matrixStats)
library(ggplot2)
library(devtools) 


`%nin%` = Negate(`%in%`)

# US census
data_name = 'US_census'
data_folder = 'data_US_census/model_vs_contingency_table_US_census_uplift'
output_name = 'data_US_census/model_vs_contingency_table_US_census_final_weighted_average_bias_table_uplift.csv'
treatment = 'educ.12'
outcome = 'income.50K'

# # marketing campaign
# data_name = 'marketing_campaign'
# data_folder = 'data_marketing_campaign/model_vs_contingency_table_marketing_campaign_uplift'
# output_name = 'data_marketing_campaign/model_vs_contingency_table_marketing_campaign_final_weighted_average_bias_table_uplift.csv'
# treatment = 'TREATMENT'
# outcome = 'PURCHASE'

# # email analytics
# data_name = 'email_analytics'
# data_folder = 'data_email_analytics/model_vs_contingency_table_email_analytics_uplift'
# output_name = 'data_email_analytics/model_vs_contingency_table_email_analytics_final_weighted_average_bias_table_uplift.csv'
# treatment = 'segment'
# outcome = 'visit'

# # email analytics women
# data_name = 'email_analytics_women'
# data_folder = 'data_email_analytics_women/model_vs_contingency_table_email_analytics_women_uplift'
# output_name = 'data_email_analytics_women/model_vs_contingency_table_email_analytics_women_final_weighted_average_bias_table_uplift.csv'
# treatment = 'segment'
# outcome = 'visit'

# # email analytics men
# data_name = 'email_analytics_men'
# data_folder = 'data_email_analytics_men/model_vs_contingency_table_email_analytics_men_uplift'
# output_name = 'data_email_analytics_men/model_vs_contingency_table_email_analytics_men_final_weighted_average_bias_table_uplift.csv'
# treatment = 'segment'
# outcome = 'visit'

# # adult binary
# data_name = 'adult_binary'
# data_folder = 'data_adult_binary/model_vs_contingency_table_adult_binary_uplift'
# output_name = 'data_adult_binary/model_vs_contingency_table_adult_binary_final_weighted_average_bias_table_uplift.csv'
# treatment = 'education.num.12'
# outcome = 'class'

# # criteo uplift
# data_name = 'criteo_uplift'
# data_folder = 'data_criteo_uplift/model_vs_contingency_table_criteo_uplift_uplift'
# output_name = 'data_criteo_uplift/model_vs_contingency_table_criteo_uplift_final_weighted_average_bias_table_uplift.csv'
# treatment = 'treatment'
# outcome = 'visit'

# # twins
# data_name = 'twins'
# data_folder = 'data_twins/model_vs_contingency_table_twins_uplift'
# output_name = 'data_twins/model_vs_contingency_table_twins_final_weighted_average_bias_table_uplift.csv'
# treatment = 'T'
# outcome = 'target'

################################################################################
### 10 times 2-fold cross validation
cross_validation_times = 10
cross_validation_folds = 2

### to save final bias
final_weighted_average_bias_table = data.frame(matrix(nrow = cross_validation_times*cross_validation_folds, 
                                                      ncol = 1))

### start loop here
for (current_cross_validation_time in 1:cross_validation_times) {
  for (current_cross_validation_fold in 1:cross_validation_folds) {
    
    # current_cross_validation_time = 1
    # current_cross_validation_fold = 1
    
    file_name = paste('/model_vs_contingency_table_cv_',current_cross_validation_time,
                      '_fold_',current_cross_validation_fold,'.csv',sep='')
    data = read.csv(paste(data_folder, file_name, sep=''))
    
    current_bias = sum(data$bias * data$N / sum(data$N))
    
    final_weighted_average_bias_table[2*current_cross_validation_time-current_cross_validation_fold%%2,1] = current_bias
    
  }
}

### calculate mean and std of final_weighted_average_bias
final_weighted_average_bias_table$mean = NA
final_weighted_average_bias_table$std = NA
final_weighted_average_bias_table$mean[1] = mean(final_weighted_average_bias_table[,1])
final_weighted_average_bias_table$std[1] = rowSds(t(final_weighted_average_bias_table[,1]))

write.csv(final_weighted_average_bias_table,output_name)

### decile plot
source('sz301_decile_plot_uplift.R')
sz301_decile_plot_uplift(data_name,treatment,outcome,cross_validation_times,cross_validation_folds)

