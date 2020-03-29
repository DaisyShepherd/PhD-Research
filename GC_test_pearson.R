GC_test_pearson <- function(alignment, ml_object, N){
  # alignment = alignment in phyDat format
  # optimised output from pml for alignment
  # N = number of simulated data sets to use for GC distribution
  
  results <- rep(NA, N)
  
  for (i in 1:N){
    ## Step 1: Generate a seqgen alignment based on this -> Put into phydat format
    seq <- seqgen_aligns(alignment, ml_object)
    seq <- seq2phy(seq, alignment)
    
    ## Step 2: Optimise the model (need to work on how we know the optimal model here, currently GTR+I)
    optimised <- get_tree2(seq)
    
    ## Step 3: Calculate the deviance statistics for the simulated data set.
    results[i] <- pearson(seq, optimised)
  }
  return(results)
}