#  Function micro_loader() ---------------------------
#' 
#' In:  directory containing brain folders
#' Out: list containing microarray data.  
#'      Null if no MA data exists for that brain
#' 
micro_loader <- function(brains_directory){
  
  #  Create list for microarray data
  annotation_list <- vector(mode = "list", length = length(brain_folders))
  
  
  #  Get each brain's MA dta
  for( i in 1:length(brain_folders)){
    
    # If the annotation file exists, we write it to annotation list. Else, it remains null.
    if("SampleAnnot.csv" %in% list.files(brain_folders[i])){
      annotation_list[[i]]  <- read.csv(paste(brain_folders[i], "SampleAnnot.csv", sep = "/"))
    }
  }
  
  #  $eturn the list of microarray data.
  return(annotation_list)
}


#  (Assign the appropriate local directory for your data and brain)
# Brain Choices: 
# directory <- "Z:/ST599/Project3/brain/brain_images/H0351.2001"
# brains_directory <- "Z:/ST599/Project3/brain/brain_images"
# brain_folders <- list.dirs(path = brains_directory, recursive = F)
# 
# annotation_list <- vector(mode = "list", length = length(brain_folders))
# 
# Select a brain (1-8)
# for( i in 1:length(brain_folders)){
#  if("SampleAnnot.csv" %in% list.files(brain_folders[i])){
#    annotation_list[[i]]  <- read.csv(paste(brain_folders[i], "SampleAnnot.csv", sep = "/"))
#  }
# }



# old style
#current_brain_folder <- brain_folders[5]
#annotation  <- read.csv(paste(current_brain_folder, "SampleAnnot.csv", sep = "/"))

# Read in the annotation
