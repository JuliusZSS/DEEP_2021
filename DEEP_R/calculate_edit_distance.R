# Input: two patterns
# Output: edit distance

calculate_edit_distance <- function(p1, p2) {
  
  # count NA
  n_NA = 0
  if (anyNA(p1) && anyNA(p2)) {
    # both have NA
    # print('yes both')
    pattern_1 = is.na(p1)
    n_NA = n_NA + sum(pattern_1)
    p1_no_NA = p1[-which(pattern_1)]
    p2_no_NA = p2[-which(pattern_1)]
    if (anyNA(p2_no_NA)) {
      # still NA in second one
      pattern_2 = is.na(p2_no_NA)
      n_NA = n_NA + sum(pattern_2)
      p1_no_NA = p1_no_NA[-which(pattern_2)]
      p2_no_NA = p2_no_NA[-which(pattern_2)]
      # print(df_i)
      # print(df_j)
    }
    
  } else if (anyNA(p1)) {
    # first has NA
    # print('yes first')
    pattern_1 = is.na(p1)
    n_NA = n_NA + sum(pattern_1)
    p1_no_NA = p1[-which(pattern_1)]
    p2_no_NA = p2[-which(pattern_1)]
    
  } else if (anyNA(p2)) {
    # second has NA
    # print('yes second')
    pattern_2 = is.na(p2)
    n_NA = n_NA + sum(pattern_2)
    p1_no_NA = p1[-which(pattern_2)]
    p2_no_NA = p2[-which(pattern_2)]
    
  } else {
    # no NA
    p1_no_NA = p1
    p2_no_NA = p2
  }
  
  pair_wise_dist = xor(p1_no_NA, p2_no_NA)
  pair_wise_dist = length(pair_wise_dist[pair_wise_dist == TRUE]) + n_NA

  return(pair_wise_dist)

}
