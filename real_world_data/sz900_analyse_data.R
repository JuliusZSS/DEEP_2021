# Input: data
# Output: unique records number

remove(list = ls())

data_1 = read.csv('data_marketing_campaign/model_vs_contingency_table_marketing_campaign/cross_validation_time_1_fold_1_for_model.csv')

data_1 = data_1[, -1]

data_1['records'] = apply(data_1, 1, function(x) {
  paste(x[2:68], collapse = "")
})

unique_records = unique(data_1['records'])

