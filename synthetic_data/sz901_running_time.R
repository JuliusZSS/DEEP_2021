# Input: running time
# Output: line chart showing running time

remove(list = ls())
library(ggplot2)
library(dplyr)
library(reshape)

causal_tree = c(12.2, 2.13 * 60, 21 * 60, 2 * 60 * 60, 2 * 60 * 60, 2 * 60 * 60)
DEEP = c(3.3 * 60, 47, 47, 35, 33.9, 30.8)
interaction_tree = c(11.6, 56.5, 6.2 * 60, 41.5 * 60, 2 * 60 * 60, 2 * 60 * 60)
uplift_tree = c(0.59, 1.47, 3.2, 7.5, 11.8, 15.4)

all_data = data.frame(causal_tree, DEEP, interaction_tree, uplift_tree)
mdata = melt(all_data)
mdata['group'] = rep(c(5000, 10000, 20000, 40000, 60000, 80000), 4)
colnames(mdata) = c('model', 'Time', 'Variables')

mdata %>%
  ggplot(aes(x=Variables, y=Time, group=model)) +
  geom_line() +
  geom_point(aes(shape=model)) + 
  xlab('The number of records') + 
  ylab('Running time (sec)')  + 
  theme_bw() +
  # theme(legend.position = c(0.8, 0.5)) +
  theme(legend.position = c(0.8, 0.7)) +
  scale_shape_manual(values=c(17, 8, 15, 19),
                     name = 'Model', 
                     labels = c('Causal tree', 'DEEP', 'Interaction tree', 'Uplift tree')) + 
  coord_cartesian(ylim=c(0, 21 * 60)) +
  scale_y_continuous(breaks = seq(0, 1500, len = 6))
  
  

