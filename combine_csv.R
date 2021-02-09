### Input: two csv files
### Output: a conbined csv file

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


file_1 = 'data_20201213/batch_1/binary_10000.csv'
file_2 = 'data_20201213/batch_1/binary_cf_10000.csv'

data_1 = read.csv(file_1)[,-1]
data_2 = read.csv(file_2)[,-1]

data_1 = data.frame(data_1, 0)

for (i in 1:nrow(data_1)) {
  
  if (data_1[i,'w'] == 1) {
    data_1[i,'X0'] = data_1[i,'y'] - data_2[i,'y_']
  } else {
    data_1[i,'X0'] = data_2[i,'y_'] - data_1[i,'y']
  }
}

write.csv(data_1, 'tmp.csv')





