### Input: number of parents
### Output: hash table

get_pattern_table <- function(n_patterns) {
 
  output_table = data.frame(matrix(nrow = 2^n_patterns, ncol = n_patterns))
  
  for (i in 1:n_patterns) {
    
    a = rep(1, 2^n_patterns / 2^(i))
    b = rep(0, 2^n_patterns / 2^(i))
    c = c(a, b)
    
    column = rep(c, 2^(i-1))
    
    output_table[,i] = column
    
  }
  
  return(output_table)

}

