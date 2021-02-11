# Input: ground truth, DEEP patterns
# Output: decile plot

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(matrixStats)
library(ggplot2)
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

decile_phi = data.frame(matrix(nrow = 10, ncol = 20))

for (n_batch in 1:10) {
  for (n_fold in 1:2) {
    
    print(paste(n_batch, n_fold))
    
    ground_truth_file = paste('data/model_vs_contingency_table_',data_name,'/cross_validation_time_',n_batch,'_fold_',n_fold,'_as_ground_truth.csv',sep = "")
    patterns_file = paste('data/model_vs_contingency_table_',data_name,'_deep/cross_validation_batch_',n_batch,'_fold_',n_fold,'_patterns.csv',sep="")
    
    
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
    ground_truth[,'CATE'] = NA
    
    # merge to each pattern
    for (i in 1:nrow(patterns)) {
      
      # current pattern and working ground truth
      working_ground_truth = ground_truth
      p = patterns[i, 1:(which(colnames(patterns) == 'n11')-1)]
      
      # remove NA places
      n_NA = is.na(p)
      if (any(n_NA)) {
        working_ground_truth = working_ground_truth[,-which(n_NA)]
        p = p[-which(n_NA)]
      } 
      
      # add pattern to working ground truth data frame
      p_paste = paste(p, collapse = " ")
      if (length(p) > 1) {
        working_ground_truth['pattern'] = apply(working_ground_truth[,colnames(p)], 1, paste, collapse=" ")
        # fill in CATE in the blank slots in ground truth data frame
        to_update_rows = which(working_ground_truth$pattern == p_paste & is.na(ground_truth$CATE))
        ground_truth[to_update_rows, 'CATE'] = patterns[i, 'phi']
      }
      else if (length(p) == 1)  {
        working_ground_truth['pattern'] = p
        # fill in CATE in the blank slots in ground truth data frame
        to_update_rows = which(working_ground_truth$pattern == p_paste & is.na(ground_truth$CATE))
        ground_truth[to_update_rows, 'CATE'] = patterns[i, 'phi']
      }
      else {
        # fill in CATE in the blank slots in ground truth data frame
        to_update_rows = which(is.na(ground_truth$CATE))
        ground_truth[to_update_rows, 'CATE'] = patterns[i, 'phi']
      }
        
    }
    
    # decile part
    ground_truth$quartile <- ntile(-(ground_truth$CATE), 10)  
    current_ntile_data_phi = data.frame(matrix(nrow = 10, ncol = 1))
    
    for (current_ntile in 1:10) {
      ### get data
      current_ntile_data = ground_truth[which(ground_truth$quartile==current_ntile),]
      
      ### get n11 n12 n21 n22
      current_ntile_data_contingency_table = get_contingency_table(current_ntile_data,'W','Y')
      current_ntile_data_contingency_table = lapply(current_ntile_data_contingency_table, function(x) {return(x+0.5)})
      
      ### get pi_1 pi_2 phi
      current_ntile_data_phi[current_ntile, 1] = get_causal_effect_from_contingency_table(current_ntile_data_contingency_table)
    }
    
    decile_phi[,2*n_batch-n_fold%%2] = current_ntile_data_phi
    
  }

}


### aggregate phi and calculate its mean and std
decile_phi['phi_mean'] = rowMeans(decile_phi[,1:10])
decile_phi['phi_std'] = rowSds(as.matrix(decile_phi[,1:10]))

# create plot data
plot_data <- data.frame(percentile=format(round(0:9), nsmall=1),value=decile_phi['phi_mean'],sd=decile_phi['phi_std'])

### decile plot and save it
jpeg(paste('../real_world_data/data/deep_2_fold_10_times_decile_plot_',data_name,'.png',sep=''))
decile_plot <- ggplot(plot_data) +
  geom_bar(aes(x=percentile, y=phi_mean), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar(aes(x=percentile, ymin=phi_mean-phi_std, ymax=phi_mean+phi_std), width=0.4, colour="orange", alpha=0.9, size=1.3)
print(decile_plot)
dev.off()










