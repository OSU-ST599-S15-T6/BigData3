#  Read nifti (.nii) image files into R
#
#  oro.nifti allows us to read in the MRI data and view it.

#install.packages("oro.nifti")
require(package = "oro.nifti")


#  Function brain_loader() ---------------------------
#' 
#' In:  vector of brain names, directory containing brain folders
#' Out: list containing 3 dimensional brain arrays, one for each brain.
#' 
brain_loader <- function(brain_names = NULL, brains_directory = NULL, gray = F){
  require(package = "oro.nifti")
  brains <- list.dirs(path = brains_directory, recursive = F)
  
  if (is.null(brain_names)){
    brain_names <- list.dirs(path = brains_directory, recursive = F, full.names = F)
  }
  
  
  brain_list <- vector(mode = "list", length = length(brain_names))
  
  for(b in 1:length(brain_names))
  {
    # What brains are in this folder
    local_brains <- list.files(brains[b], recursive = F, pattern = ".nii", full.names = T)
    target_file  <- NULL
    # If we are reading in the gray matter,
    if(gray == T){
      message(paste("Loading Gray Image ", brain_names[b]))
      # Second image is gray
      target_file <- local_brains[2]
    }
    # if we are reading in the white matter list
    if(gray == F){
      message(paste("Loading White Image ", brain_names[b]))
      # First image is white
      target_file <- local_brains[1]
    }
    local_nifti <- readNIfTI(target_file)
    dat_out <- local_nifti@.Data
    #dim(dat_out)
    
    # Dimensions are incorrect.  
    # I think we get X, Y, Z when it should be Z, Y, X
    long_image <- matrix(rep(0,prod(dim(dat_out))), nrow = dim(dat_out)[1] )
    #dim(long_image)
    
    out_array <- array(rep(0,prod(dim(dat_out))), dim = dim(dat_out)[c(1,3,2)])
    
    # For each of the z images, store them in the long image, aligned along y (dimension 2), front to back.
    for(i in 1:dim(dat_out)[3])
      long_image[ , ((i-1)*dim(dat_out)[2]+1):(i*dim(dat_out)[2])] <- dat_out[,,i]
    
    image_length <- dim(dat_out)[3]
    
    for(i in 1:dim(out_array)[3]){
      out_array[,,i] <- long_image[ , ((i-1)*image_length+1):(i*image_length)]
    }
    
    #message(paste("storing brain ",b))
    
    brain_list[[b]] <- out_array
  }
  
  return(brain_list)
  
}

