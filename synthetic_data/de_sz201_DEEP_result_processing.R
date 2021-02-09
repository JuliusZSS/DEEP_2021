### Input: DEEP output csv files
### OUtput: DEEP CATE

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(stringr)

data_name = "data_20_4_4"

for (batch_number in 1:10) {
  ### import data
  data_input_path = paste(data_name,"/deep_self_training/deep_self_training_batch_",batch_number,".csv",sep="")
  data_to_match_path = paste(data_name,"/deep_self_training/output/deep_self_training_batch_",batch_number,"_DEEPOutput.csv",sep="")
  data_output_path = paste(data_name,"/deep_self_training/deep_self_training_batch_",batch_number,"_CATE.csv",sep="")
  
  data_input = read.csv(data_input_path)
  data_to_match = read.csv(data_to_match_path)
  data_to_match_pattern = data_to_match[,1:(which(colnames(data_to_match) == 'n11')-1)]
  merge_names = colnames(data_to_match_pattern)
  
  for (i in 1:length(merge_names)) {
    # remove potential other characters
    if (grepl(".", merge_names[i], fixed=TRUE)) {
      merge_names[i] = str_extract_all(merge_names[i], "(?<=\\.)[^.]+(?=\\.)")[[1]]
      colnames(data_to_match_pattern) = merge_names
    }
  }
  

  data_to_match_pattern['CATE'] = data_to_match$causalEffect
  
  ### output data
  data_big = merge(data_input, data_to_match_pattern, by.x=merge_names, by.y=merge_names)
  write.csv(data_big, data_output_path)
}



















