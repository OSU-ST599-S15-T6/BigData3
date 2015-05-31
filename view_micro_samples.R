# Objective: View samples by their microarray result.

source("load_brain_list.R")

# Choose the brain to use
b <- 1

# read in a single rain's MA data.
# If you want to read the whole microarray data, uncomment this.
# micro_1 <- micro_loader(brains_directory, i = 1)
pacall <- micro_loader(brains_directory, i = b, binary = T)
#  View(head(pacall))
dim(pacall) # There are 58,692 probes for the first brain.

# we want to use a certain probe number
probe_number = 1000
target_probe <- pacall[probe_number,]
sum(target_probe[-1])/length(target_probe[-1]) # Probe #1 has about a 42% activation rate across regions

# Sample Locations ---------------------------

# Lets look at the location of samples in a particular brain.
#local_brain <- white_matter[[b]]
local_brain <- gray_matter[[b]]
local_sample <- samples[[b]]
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


# Imaging all the points in all slices, with probe information ---------------------------
#  Red probes are positive, blue are negative.
#

# Slice the brain along X
for(x in unique_x){
  in_current_layer <- trans_coords$local_x == x
  in_and_probe_true <- in_current_layer & (target_probe[-1] == 1)
  in_and_probe_false <- in_current_layer & (target_probe[-1] == 0)
  
  image(local_brain[x,,], main = paste("x-layer:", x, "  probe =", target_probe[1], "   n =", sum(in_current_layer)), col = grey(seq(0, 1, length = 256)), axes = F)
  # We step through x, so we view y, z.
  points(x = point_coords$local_y[in_and_probe_true],  y = point_coords$local_z[in_and_probe_true],  pch = 20, col = "red")
  points(x = point_coords$local_y[in_and_probe_false], y = point_coords$local_z[in_and_probe_false], pch = 20, col = "blue")
}

# Slice the brain along Y
for(y in unique_y){
  in_current_layer <- trans_coords$local_y == y
  in_and_probe_true <- in_current_layer & (target_probe[-1] == 1)
  in_and_probe_false <- in_current_layer & (target_probe[-1] == 0)
  
  image(local_brain[,y,], main = paste("y-layer:", y, "  probe:", target_probe[1], "  n =", sum(in_current_layer)), col = grey(seq(0, 1, length = 256)), axes = F)
  # We step through y, so we view x, z.
  points(x = point_coords$local_x[in_and_probe_true],  y = point_coords$local_z[in_and_probe_true],  pch = 20, col = "red")
  points(x = point_coords$local_x[in_and_probe_false], y = point_coords$local_z[in_and_probe_false], pch = 20, col = "blue")
}

# Slice the brain along Z
for(z in unique_z){
  in_current_layer <- trans_coords$local_z == z
  in_and_probe_true <- in_current_layer & (target_probe[-1] == 1)
  in_and_probe_false <- in_current_layer & (target_probe[-1] == 0)
  
  image(local_brain[,,z], main = paste("z-layer:", z, "  probe:", target_probe[1], "  n =", sum(in_current_layer)), col = grey(seq(0, 1, length = 256)), axes = F)
  # We step through z, so we view x, y.
  points(x = point_coords$local_x[in_and_probe_true],  y = point_coords$local_y[in_and_probe_true],  pch = 20, col = "red")
  points(x = point_coords$local_x[in_and_probe_false], y = point_coords$local_y[in_and_probe_false], pch = 20, col = "blue")
}
