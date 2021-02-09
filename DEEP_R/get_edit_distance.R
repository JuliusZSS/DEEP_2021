### Input: a data frame containing features
### Output: edit distance matrix

get_edit_distance <- function(df, distance_matrix) {
  
  # df = data.frame(c(0,0,0), c(0,0,1), c(1,1,1))
  # df = data.frame(c(0,NA,0), c(0,NA,1), c(1,1,1), c(NA,NA,NA))
  # 
  # df = t(df)
  # distance_matrix = data.frame(matrix(nrow = nrow(df), ncol = nrow(df)))
  
  for (i in 1:(nrow(df)-1)) {
    for (j in (i+1):nrow(df)) {
      # count NA
      n_NA = 0
      if (anyNA(df[i,]) && anyNA(df[j,])) {
        # both have NA
        # print('yes both')
        pattern_1 = is.na(df[i,])
        n_NA = n_NA + sum(pattern_1)
        df_i = df[i,-which(pattern_1)]
        df_j = df[j,-which(pattern_1)]
        if (anyNA(df_j)) {
          # still NA in second one
          pattern_2 = is.na(df_j)
          n_NA = n_NA + sum(pattern_2)
          df_i = df_i[-which(pattern_2)]
          df_j = df_j[-which(pattern_2)]
          # print(df_i)
          # print(df_j)
        }
        
      } else if (anyNA(df[i,])) {
        # first has NA
        # print('yes first')
        pattern_1 = is.na(df[i,])
        n_NA = n_NA + sum(pattern_1)
        df_i = df[i,-which(pattern_1)]
        df_j = df[j,-which(pattern_1)]        

      } else if (anyNA(df[j,])) {
        # second has NA
        # print('yes second')
        pattern_2 = is.na(df[j,])
        n_NA = n_NA + sum(pattern_2)
        df_i = df[i,-which(pattern_2)]
        df_j = df[j,-which(pattern_2)]        

      } else {
        # no NA
        df_i = df[i,]
        df_j = df[j,]
      }
      
      pair_wise_dist = xor(df_i, df_j)
      pair_wise_dist = length(pair_wise_dist[pair_wise_dist == TRUE]) + n_NA
      distance_matrix[i,j] = pair_wise_dist
      
    }
  }

  return(distance_matrix)
  
}






