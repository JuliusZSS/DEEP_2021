### Input: a dataframe with features, treatment, outcome
### Output: n11, n12, n21, n22

get_contingency_table <- function(input_dataframe,treatment,outcome) {
  
  n11 = n12 = n21 = n22 = 0
  
  # for (i in 1:nrow(input_dataframe)) {
  #   this_row = input_dataframe[i,]
  #   if (this_row[treatment]==1 & this_row[outcome]==1) {
  #     n11 = n11 + 1
  #   }
  #   if (this_row[treatment]==1 & this_row[outcome]==0) {
  #     n12 = n12 + 1
  #   }
  #   if (this_row[treatment]==0 & this_row[outcome]==1) {
  #     n21 = n21 + 1
  #   }
  #   if (this_row[treatment]==0 & this_row[outcome]==0) {
  #     n22 = n22 + 1
  #   }
  #   
  # }
  
  n11 = nrow(input_dataframe[which(input_dataframe[treatment] == 1 & input_dataframe[outcome] == 1),])
  n12 = nrow(input_dataframe[which(input_dataframe[treatment] == 1 & input_dataframe[outcome] == 0),])
  n21 = nrow(input_dataframe[which(input_dataframe[treatment] == 0 & input_dataframe[outcome] == 1),])
  n22 = nrow(input_dataframe[which(input_dataframe[treatment] == 0 & input_dataframe[outcome] == 0),])

  # print(list(n11, n12, n21, n22))
  
  return(list(n11, n12, n21, n22))
  
}

