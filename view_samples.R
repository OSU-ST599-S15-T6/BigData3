# first run load brain list.
source("load_brain_list.R")


# Looking at brains ---------------------------

# When we look at a brain image in R, it is actually rotated 90 degrees to counterclockwise compared to the matrix.
# Our brains appear to point nose up, when viewed in z slices, but really the nose points right in the matrix.
image(white_matter[[1]][ , ,99])
image(gray_matter[[1]][ , ,99])

#  We can view slices in z, and 
image(brain_list[[8]][75,,])
image(brain_list[[8]][100,,])
image(brain_list[[8]][,100,])

# A different brain:
image(white_matter[[3]][,,100])
image(white_matter[[2]][,,100])

# Let's look at the value of an individual pixel within an image.
white_matter[[1]][50,120,25]

# In the augmented annnotation (samples) We can see where the samples were taken and what structures they were taken from.
head(samples[[1]])  # All the info
head(samples[[1]][,c(6,8:10)])  # we can look at a subset if we want, just voxel and structure.

# Sample Locations ---------------------------

# Lets look at the location of samples in a particular brain.
local_brain <- white_matter[[1]]
#local_brain <- gray_matter[[1]]
local_sample <- samples[[1]]
brain_dim <- dim(local_brain)

# We have to transform the voxel coordinates because the brain was stored incorectly.
#  (This took a long time to figure out)
coords <- local_sample[,8:10]
trans_coords <- cbind(coords[1], brain_dim[2] - coords[3], brain_dim[3] - coords[2])
names(trans_coords) <- c("local_x", "local_y", "local_z")

# Convert the transformed coordinates to display on R's plot.
point_coords <- cbind((trans_coords[1]-1)/(brain_dim[1]-1), (trans_coords[2]-1)/(brain_dim[2]-1), (trans_coords[3]-1)/(brain_dim[3]-1))

# We want to look at the points that are unique within each slice of the brain.
#  Note: these are the transformed x,y,z unique values.
unique_x <- sort(unique(trans_coords$local_x))
unique_y <- sort(unique(trans_coords$local_y))
unique_z <- sort(unique(trans_coords$local_z))


# Imaging all the points in all slices. ---------------------------

# Slice the brain along X
for(x in unique_x){
  in_current_layer <- trans_coords$local_x == x
  image(local_brain[x,,], main = paste("x-layer:", x, "  n =", sum(in_current_layer)))
  # We step through x, so we view y, z.
  points(x = point_coords$local_y[in_current_layer], y = point_coords$local_z[in_current_layer], pch = 20)
}

# Slice the brain along Y
for(y in unique_y){
  in_current_layer <- trans_coords$local_y == y
  image(local_brain[,y,], main = paste("y-layer:", y, "  n =", sum(in_current_layer)))
  # We step through y, so we view x, z.
  points(x = point_coords$local_x[in_current_layer], y = point_coords$local_z[in_current_layer], pch = 20)
}

# Slice the brain along Z
for(z in unique_z){
  in_current_layer <- trans_coords$local_z == z
  image(local_brain[,,z], main = paste("z-layer:", z, "  n =", sum(in_current_layer)))
  # We step through z, so we view x, y.
  points(x = point_coords$local_x[in_current_layer], y = point_coords$local_y[in_current_layer], pch = 20)
}




