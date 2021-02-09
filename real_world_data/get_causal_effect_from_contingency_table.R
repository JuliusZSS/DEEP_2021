### Input: list(n11, n12, n21, n22)
### Output: pi_1, pi_2, phi

get_causal_effect_from_contingency_table <- function(input_list) {
  
  n11 = input_list[[1]]
  n12 = input_list[[2]]
  n21 = input_list[[3]]
  n22 = input_list[[4]]
  
  pi_1 = n11 / (n11 + n12)
  pi_2 = n21 / (n21 + n22)
  
  phi = pi_1 - pi_2
  
  return(phi)
}






