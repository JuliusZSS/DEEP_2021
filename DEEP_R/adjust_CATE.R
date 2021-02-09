# Input: unadjusted row, first hash table, column name of collider
# Output: adjusted CATE value

source('get_causal_effect_from_contingency_table.R')


adjust_CATE <- function(first_hash_table, row_1, collider_names) {
  
  # # remove after test
  # first_hash_table = first_hash_table
  # row_1 = need_adjustment[i,]
  # collider_names = col_adjustment
  
  # fetch all rows
  row_no_NA = row_1[1, 1:which(colnames(row_1) == 'n11')-1]
  row_no_NA = row_no_NA[1,-which(is.na(row_no_NA))]
  fetched_rows = merge(row_no_NA, first_hash_table, by.x = colnames(row_no_NA), by.y = colnames(row_no_NA))
  
  # group by collider_names
  fetched_rows_grouped = split(fetched_rows, fetched_rows[,collider_names])
  
  # to save CATE and total n
  CATE_n = data.frame(matrix(nrow = length(fetched_rows_grouped), ncol = 2))
  colnames(CATE_n) = c('CATE', 'n')
  
  # contingency table for each group
  for (i in 1:length(fetched_rows_grouped)) {
    n11 = sum(fetched_rows_grouped[[i]][,'n11'])
    n12 = sum(fetched_rows_grouped[[i]][,'n12'])
    n21 = sum(fetched_rows_grouped[[i]][,'n21'])
    n22 = sum(fetched_rows_grouped[[i]][,'n22'])
    
    n = sum(n11 + n12 + n21 + n22)
    phi = get_causal_effect_from_contingency_table(list(n11, n12, n21, n22))
    CATE_n[i, c('CATE', 'n')] = c(phi, n)
    
  }
  
  # remove NA rows
  if (anyNA(CATE_n)) {
    CATE_n = CATE_n[-which(is.na(CATE_n$CATE)),]
  }
  
  # calculate weighted average CATE
  adjusted_CATE_value = sum(CATE_n$CATE * CATE_n$n) / sum(CATE_n$n)
  
  return(adjusted_CATE_value)
  
}





