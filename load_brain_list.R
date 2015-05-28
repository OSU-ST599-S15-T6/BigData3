#  (Assign the appropriate local directory for your data and brain)
#  In one directory, place the 8 folders containing brain images.
#  Tell brain_loader where to find those images.

#directory <- "Z:/ST599/Project3/brain/brain_images/"
brains_directory <- "Z:/ST599/Project3/brain/brain_images"
source("nifti_read.R")

# Load the brains into a list.
brain_list <- brain_loader(brains_directory = brains_directory)

# When we look at a brain image in R, it is actually rotated 90 degrees to counterclockwise compared to the matrix.
# Our brains appear to point nose up, when viewed in z slices, but really the nose points right in the matrix.
image(brain_list[[8]][ , ,99])
image(brain_list[[8]][ , ,105])
image(brain_list[[1]][ , ,50])
image(brain_list[[1]][ , ,55])

#  It appears we loaded the data right, since we can view it from the side.
image(brain_list[[8]][75,,])
image(brain_list[[8]][100,,])
image(brain_list[[8]][,100,])


# Let's look at the value of an individual pixel within an image.
brain_list[[1]][50,120,25]

# dimesion 1 is x, 2 is y, and 3 is z
dim(brain_list[[8]])

# Lets look at the values of an individual brain
View(brain_list[[8]][ , , 99])
# We can see they range from 0 to 668.  Maybe some images go to 1000?  I do not know what the max is globally.
range(brain_list[[8]][ , ,99])

