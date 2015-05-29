#  (Assign the appropriate local directory for your data and brain)
# Brain Choices: 
#directory <- "Z:/ST599/Project3/brain/brain_images/H0351.2001"
brains_directory <- "Z:/ST599/Project3/brain/brain_images"
brain_folders <- list.dirs(path = brains_directory, recursive = F)

# Select a brain (1-8)
current_brain_folder <- brain_folders[5]

# Read in the annotation
annotation  <- read.csv(paste(current_brain_folder,"SampleAnnot.csv", sep = "/"))
