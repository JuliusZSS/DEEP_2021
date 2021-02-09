# Input: pattern_1, pattern_2
# Output: merged pattern

source('get_z_significant.R')

merge_patterns <- function(first_hash_table, pattern_1, pattern_2, feature_length) {
  # # get patterns only for testing
  # pattern_1 = first_pattern
  # pattern_2 = second_pattern
  
  # compare patterns
  pattern_overlap = xor(pattern_1[,1:feature_length], pattern_2[,1:feature_length])
  # create new pattern
  new_pattern = pattern_1
  rownames(new_pattern) = c('new_pattern')
  new_pattern[1, which(pattern_overlap == TRUE)] = NA
  new_pattern[1, which(is.na(pattern_2))] = NA

  # get records from index
  pattern_1_index = c(unlist(strsplit(pattern_1$index, ' ')))
  pattern_2_index = c(unlist(strsplit(pattern_2$index, ' ')))
  get_patterns = c(pattern_1_index, pattern_2_index)
  get_patterns = unique(get_patterns)
  # print(get_patterns)
  
  # calculate n11 n12 n21 n22
  new_pattern$n11 = sum(first_hash_table[get_patterns,'n11'])
  new_pattern$n12 = sum(first_hash_table[get_patterns,'n12'])
  new_pattern$n21 = sum(first_hash_table[get_patterns,'n21'])
  new_pattern$n22 = sum(first_hash_table[get_patterns,'n22'])
  # calculate pi_1 pi_2 phi
  new_pattern$pi_1 = new_pattern$n11 / (new_pattern$n11 + new_pattern$n12)
  new_pattern$pi_2 = new_pattern$n21 / (new_pattern$n21 + new_pattern$n22)
  new_pattern$phi = new_pattern$pi_1 - new_pattern$pi_2
  # calculate significance
  new_pattern$z_significant = get_z_significant(new_pattern)
  
  # save new records
  new_pattern$index = paste(get_patterns, collapse = " ")
  
  return(new_pattern)
  
}


