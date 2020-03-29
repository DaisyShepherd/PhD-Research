align_generate <- function(k, n, one_prop, two_prop, three_prop, four_prop){
  ## One nucleotide
  one_nuc <- matrix(NA, nrow=k, ncol=one_prop*n)
  for (i in 1:(one_prop*n)){
    one_nuc[,i] <- rep(sample(states, size=1), k)
  }
  
  ## Two nucleotides
  two_nuc <- matrix(NA, nrow=k, ncol=two_prop*n)
  for (i in 1:(two_prop*n)){
    chars <- sample(states, size=2, replace=FALSE)
    site <- c(chars, sample(chars, size=k-length(chars), replace=TRUE))
    two_nuc[,i] <- sample(site, size=k, replace=FALSE)
  }
  
  ## Three nucleotides
  three_nuc <- matrix(NA, nrow=k, ncol=three_prop*n)
  for (i in 1:(three_prop*n)){
    chars <- sample(states, size=3, replace=FALSE)
    site <- c(chars, sample(chars, size=k-length(chars), replace=TRUE))
    three_nuc[,i] <- sample(site, size=k, replace=FALSE)
  }
  
  ## Four nucleotides
  four_nuc <- matrix(NA, nrow=k, ncol=four_prop*n)
  for (i in 1:(four_prop*n)){
    site <- c(states, sample(states, size=k-length(states), replace=TRUE))
    four_nuc[,i] <- sample(site, size=k, replace=FALSE)
  }
  
  ## Combining into one data frame
  data <- cbind(one_nuc, two_nuc, three_nuc, four_nuc)
  alignment <- data[, sample(ncol(data))]
  return(alignment)
}
