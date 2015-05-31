#  (Assign the appropriate local directory for your data and brain)
#  In one directory, place the 8 folders containing brain images.
#  Tell brain_loader where to find those images.

brains_directory <- "Z:/ST599/Project3/brain/brain_images"


source("nifti_read.R")
source("microarray_read.R")

# Create two lists, one containing white voxel arrays and one containing gray voxel arrays
white_matter <- brain_loader(brains_directory = brains_directory, gray = F)
gray_matter  <- brain_loader(brains_directory = brains_directory, gray = T) 

samples <- load_sample_gw(brains_directory, white_matter = white_matter, gray_matter = gray_matter)


