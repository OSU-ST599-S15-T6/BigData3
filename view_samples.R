# first run load brain list.

local_brain <- brain_list[[1]]
local_sample <- sample_list[[1]]
brain_dim <- dim(local_brain)


coords <- local_sample[,8:10]
head(coords)
trans_coords <- cbind(coords[1], brain_dim[2] - coords[3], brain_dim[3] - coords[2])
names(trans_coords) <- c("local_x", "local_y", "local_z")
head(trans_coords)

# Convert the transformed coordinates to display on R's plot.
point_coords <- cbind((trans_coords[1]-1)/(brain_dim[1]-1), (trans_coords[2]-1)/(brain_dim[2]-1), (trans_coords[3]-1)/(brain_dim[3]-1))
head(point_coords)

#  Note: these are the transformed x,y,z unique values.
unique_x <- sort(unique(trans_coords$local_x))
unique_y <- sort(unique(trans_coords$local_y))
unique_z <- sort(unique(trans_coords$local_z))


# Imaging all the points in all slices. ---------------------------

for(x in unique_x){
  in_current_layer <- trans_coords$local_x == x
  image(local_brain[x,,], main = paste("x-layer:", x, "  n =", sum(in_current_layer)))
  # We step through x, so we view y, z.
  points(x = point_coords$local_y[in_current_layer], y = point_coords$local_z[in_current_layer], pch = 20)
}


for(y in unique_y){
  in_current_layer <- trans_coords$local_y == y
  image(local_brain[,y,], main = paste("y-layer:", y, "  n =", sum(in_current_layer)))
  # We step through y, so we view x, z.
  points(x = point_coords$local_x[in_current_layer], y = point_coords$local_z[in_current_layer], pch = 20)
}

for(z in unique_z){
  in_current_layer <- trans_coords$local_z == z
  image(local_brain[,,z], main = paste("z-layer:", z, "  n =", sum(in_current_layer)))
  # We step through z, so we view x, y.
  points(x = point_coords$local_x[in_current_layer], y = point_coords$local_y[in_current_layer], pch = 20)
}




