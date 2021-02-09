# Input: a data frame
# Output: the data frame with z_significant

get_z_significant <- function(df) {
  
  n1_ = df$n11 + df$n12
  n0_ = df$n21 + df$n22
  r_bar = (df$n11 + df$n21) / (df$n11 + df$n12 + df$n21 + df$n22)
  
  z = (abs(df$phi) - 0.5*(1/n1_ + 1/n0_)) / (sqrt(r_bar*(1-r_bar)*(1/n1_ + 1/n0_)))
  
  return(z)
}



