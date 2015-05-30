#  Function sample_loader() ---------------------------
#' 
#' In:  directory containing brain folders, optional: list containing brain images
#' Out: list containing data about samples
#'      Null if no MA data exists for that brain
#' 
#' If you add the argument brain_list, (a list of 3-dimensional brain imagery created by brain_loader)
#'  we create a gray value for each sample using the brain imagery in the brain_list.
#'  
sample_loader <- function(brains_directory, brain_list = NULL){
  
  # Which brains are installed?
  brain_folders <- list.dirs(path = brains_directory, recursive = F)
  
  #  Create list for microarray data
  annotation_list <- vector(mode = "list", length = length(brain_folders))
  
  
  #  Get each brain's MA dta
  for( i in 1:length(brain_folders)){
    
    # If the annotation file exists, we write it to annotation list. Else, it remains null.
    if("SampleAnnot.csv" %in% list.files(brain_folders[i])){
      annotation_list[[i]]  <- read.csv(paste(brain_folders[i], "SampleAnnot.csv", sep = "/"))
      
      #  If we've included a brain list, then we can assign gray values.
      if(is.null(brain_list[[i]]) == F){
        #message("Assigning gray values.")
        annotation_list[[i]]  <- read_gray(local_brain = brain_list[[i]],
                                           local_sample = annotation_list[[i]])

      }
    }
  }
  
  #  Return the list of microarray data.
  return(annotation_list)
}

# function micro_loader ---------------------------
#'
#'  In:  directory containing brains,  which brain to read.
#'  Out: get the data from the 3d brain array and append it to the annotation.
#'  
micro_loader <- function(brains_directory, i = 1){
  
  brain_folders <- list.dirs(path = brains_directory, recursive = F)
  short_dir <- list.dirs(path = brains_directory, recursive = F, full.names = F)
  # If the annotation file exists, we write it to annotation list. Else, it remains null.
  if("MicroarrayExpression.csv" %in% list.files(brain_folders[i])){
    ma_data <- read.csv(paste(brain_folders[i], "MicroarrayExpression.csv", sep = "/"), header = F)
    message(paste("Reading", short_dir[i], "/ MicroarrayExpression.csv"))
  }
  # return the micro array.
  return(ma_data)
}

# function read_gray ---------------------------
#'
#'  In:  3d brain array, sample information
#'  Out: get the data from the 3d brain array and append it to the annotation.
#'  
#'  At each test location, we need to decide what they gray value is.
#'  
read_gray <- function(local_brain, local_sample){
  #  how many elements in the local sample?
  n_samples <- dim(local_sample)[1]
  
  gray_level <- rep(0,n_samples)
  coords <- local_sample[,8:10]
  
  im_dim <- dim(local_brain)
  trans_coords <- cbind(voxx = coords[1], 
                        voxy = im_dim[2] - coords[3],
                        voxz = im_dim[3] - coords[2])
  
  for(g in 1:dim(coords)[1]){
    gray_level[g] <- local_brain[trans_coords[g,1],trans_coords[g,2],trans_coords[g,3]]
  }
  local_sample <- cbind(local_sample, gray_level)
  
  #  Return the modified local_sample
  return(local_sample)
}

