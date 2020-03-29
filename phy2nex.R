## Function to convert a phylip file to a nexus format

phy2nex <- function(phy_file, file_name){
  align_mat <- t(data.frame(phy_file))
  write.nexus.data(align_mat,
                   file=file_name)
}

