# Input: IT results
# Output: decile plot

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(matrixStats)
library(dplyr)
library(ggplot2)
source('get_contingency_table.R')
source('get_causal_effect_from_contingency_table.R')

# # US census
data_name = 'US_census'

# # marketing campaign
# data_name = 'marketing_campaign'

# # email analytics
# data_name = 'email_analytics'

# # email analytics women
# data_name = 'email_analytics_women'

# # email analytics men
# data_name = 'email_analytics_men'

# # adult binary
# data_name = 'adult_binary'

# # criteo uplift
# data_name = 'criteo_uplift'

# # twins
# data_name = 'twins'


################################################################################
decile_phi = data.frame(matrix(nrow = 10, ncol = 20))

for (n_batch in 1:10) {
  for (n_fold in 1:2) {
    
    file_1 = paste('data/model_vs_contingency_table_',data_name,'_IT/cross_validation_batch_',n_batch,'_fold_',n_fold,'_results.csv',sep='')
    data_1 = read.csv(file_1)
    
    # decile part
    data_1$quartile <- ntile(-(data_1$trt.effect), 10)  
    current_ntile_data_phi = data.frame(matrix(nrow = 10, ncol = 1))
    
    for (current_ntile in 1:10) {
      ### get data
      current_ntile_data = data_1[which(data_1$quartile==current_ntile),]
      
      ### get n11 n12 n21 n22
      current_ntile_data_contingency_table = get_contingency_table(current_ntile_data,'trt','y')
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
plot_data <- data.frame(percentile=format(round(1:10), nsmall=0),value=decile_phi['phi_mean'],sd=decile_phi['phi_std'])

### decile plot and save it
jpeg(paste('data/IT_2_fold_10_times_decile_plot_',data_name,'.png',sep=''))
decile_plot <- ggplot(plot_data) +
  geom_bar(aes(x=percentile, y=phi_mean), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar(aes(x=percentile, ymin=phi_mean-phi_std, ymax=phi_mean+phi_std), width=0.4, colour="orange", alpha=0.9, size=1.3) +
  ylab('Probability difference in test data') + 
  xlab('Deciles')

print(decile_plot)
dev.off()






