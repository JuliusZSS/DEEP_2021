### Input: output from sz003_model_bias_uplift.R 
### Output: decile plot

### use as function parameter
# data_name = 'US_census' 
# treatment = 'educ.12'
# outcome = 'income.50K'
# cross_validation_times = 10
# cross_validation_folds = 2

sz301_decile_plot_uplift <- function(data_name,treatment,outcome,cross_validation_times,cross_validation_folds) {
  
  ################################################################################
  source('get_contingency_table.R')
  source('get_causal_effect_from_contingency_table.R')
  library(dplyr)
  library(matrixStats)
  library(ggplot2)
  
  
  ### table for decile plot
  decile_phi = data.frame(matrix(nrow = cross_validation_times, ncol = 10*cross_validation_folds))
  
  for (current_cross_validation_time in 1:cross_validation_times) {
    # current_cross_validation_time = 1
    for (current_fold in 1:cross_validation_folds) {
      # current_fold = 1
      
      read_in_data = read.csv(paste('data_',data_name,'/model_vs_contingency_table_',data_name,'_uplift/cross_validation_time_',
                                    current_cross_validation_time,'_fold_',current_fold,'_as_ground_truth.csv',
                                    sep=''))
      
      read_in_data$quartile <- ntile(-read_in_data$CATE, 10)  
      current_ntile_data_phi = data.frame(matrix(nrow = 10, ncol = 1))
      
      for (current_ntile in 1:10) {
        ### get data
        current_ntile_data = read_in_data[which(read_in_data$quartile==current_ntile),]
        
        ### get n11 n12 n21 n22
        current_ntile_data_contingency_table = get_contingency_table(current_ntile_data,treatment,outcome)
        
        ### get pi_1 pi_2 phi
        current_ntile_data_phi[current_ntile, 1] = get_causal_effect_from_contingency_table(current_ntile_data_contingency_table)
      }
      
      decile_phi[,2*current_cross_validation_time-current_fold%%2] = current_ntile_data_phi
    }
    
  }
  
  ### aggregate phi and calculate its mean and std
  decile_phi['phi_mean'] = rowMeans(decile_phi[,1:10])
  decile_phi['phi_std'] = rowSds(as.matrix(decile_phi[,1:10]))
  
  # create plot data
  plot_data <- data.frame(percentile=format(round(1:10), nsmall=0),value=decile_phi['phi_mean'],sd=decile_phi['phi_std'])
  
  ### decile plot and save it
  jpeg(paste('data_',data_name,'/uplift_2_fold_10_times_decile_plot_',data_name,'.png',sep=''))
  decile_plot <- ggplot(plot_data) +
    geom_bar(aes(x=percentile, y=phi_mean), stat="identity", fill="skyblue", alpha=0.7) +
    geom_errorbar(aes(x=percentile, ymin=phi_mean-phi_std, ymax=phi_mean+phi_std), width=0.4, colour="orange", alpha=0.9, size=1.3) +
    ylab('Probability difference in test data') + 
    xlab('Deciles')
  print(decile_plot)
  dev.off()
  
}





