# Input: running time
# Output: line chart showing running time

remove(list = ls())
library(ggplot2)
library(dplyr)
library(reshape)

causal_tree = c(1.91 * 60, 2.13 * 60, 2.33 * 60, 2.30 * 60, 2.37 * 60)
DEEP = c(47.9, 47, 47, 47.2, 47.9)
interaction_tree = c(52.5, 56.5, 1.05 * 60, 1.05 * 60, 59.9)
uplift_tree = c(0.88, 1.47, 2.39, 3.50, 4.47)

all_data = data.frame(causal_tree, DEEP, interaction_tree, uplift_tree)
mdata = melt(all_data)
mdata['group'] = rep(c(20, 40, 60, 80, 100), 4)
colnames(mdata) = c('model', 'Time', 'Variables')

mdata %>%
  ggplot(aes(x=Variables, y=Time, group=model)) +
  geom_line() +
  geom_point(aes(shape=model)) + 
  xlab('Number of variables') + 
  ylab('Running time (sec)')  + 
  theme_bw() +
  theme(legend.position = c(0.8, 0.7)) +
  scale_shape_manual(values=c(17, 8, 15, 19),
                     name = 'Model', 
                     labels = c('Causal tree', 'DEEP', 'Interaction tree', 'Uplift tree'))   

