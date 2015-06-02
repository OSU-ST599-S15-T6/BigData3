# Objective: View samples by their microarray result.

source("load_brain_list.R")

# Choose the brain to use
b <- 1

# read in a single rain's MA data.
# If you want to read the whole microarray data, uncomment this.
# micro_1 <- micro_loader(brains_directory, i = 1)
#pacall <- micro_loader(brains_directory, i = b, binary = T)
pacall <- probe_loader(brains_directory, i = b, binary = T, with_probes = T)
#  View(head(pacall))
dim(pacall) # There are 58,692 probes for the first brain.

# we want to use a certain probe number
probe_number = 1000
target_probe <- pacall[probe_number,]
sum(target_probe[-(1:8)])/length(target_probe[-(1:8)]) # Probe #1 has about a 42% activation rate across regions

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
  in_and_probe_true <- in_current_layer & (target_probe[-(1:8)] == 1)
  in_and_probe_false <- in_current_layer & (target_probe[-(1:8)] == 0)
  
  image(local_brain[x,,], main = paste("x-layer:", x, "  probe =", target_probe[1], "   n =", sum(in_current_layer)), col = grey(seq(0, 1, length = 256)), axes = F)
  # We step through x, so we view y, z.
  points(x = point_coords$local_y[in_and_probe_true],  y = point_coords$local_z[in_and_probe_true],  pch = 20, col = "red")
  points(x = point_coords$local_y[in_and_probe_false], y = point_coords$local_z[in_and_probe_false], pch = 20, col = "blue")
}

# Slice the brain along Y
for(y in unique_y){
  in_current_layer <- trans_coords$local_y == y
  in_and_probe_true <- in_current_layer & (target_probe[-(1:8)] == 1)
  in_and_probe_false <- in_current_layer & (target_probe[-(1:8)] == 0)
  
  image(local_brain[,y,], main = paste("y-layer:", y, "  probe:", target_probe[1], "  n =", sum(in_current_layer)), col = grey(seq(0, 1, length = 256)), axes = F)
  # We step through y, so we view x, z.
  points(x = point_coords$local_x[in_and_probe_true],  y = point_coords$local_z[in_and_probe_true],  pch = 20, col = "red")
  points(x = point_coords$local_x[in_and_probe_false], y = point_coords$local_z[in_and_probe_false], pch = 20, col = "blue")
}

# Slice the brain along Z
for(z in unique_z){
  in_current_layer <- trans_coords$local_z == z
  in_and_probe_true <- in_current_layer & (target_probe[-(1:8)] == 1)
  in_and_probe_false <- in_current_layer & (target_probe[-(1:8)] == 0)
  
  image(local_brain[,,z], main = paste("z-layer:", z, "  probe:", target_probe[1], "  n =", sum(in_current_layer)), col = grey(seq(0, 1, length = 256)), axes = F)
  # We step through z, so we view x, y.
  points(x = point_coords$local_x[in_and_probe_true],  y = point_coords$local_y[in_and_probe_true],  pch = 20, col = "red")
  points(x = point_coords$local_x[in_and_probe_false], y = point_coords$local_y[in_and_probe_false], pch = 20, col = "blue")
}

# Weight each test ---------------------------
# Plan:  get a weight for each test.  By counting the number of tests per gene, 
#        we will know what percentage of genes are 
View(head(pacall))
library(dplyr)
pac_df <- as.data.frame(pacall)

# Create a column with tests per gene.
pac_genes <- pac_df %>% group_by(gene_id) %>% mutate(V1 =  n())
pac_genes <- rename(pac_genes, tests = V1)

# Were we right?  Did we get the genes labeled with the correct number of tests per gene?
#View(head(arrange(pac_genes[,-(9:dim(pac_df)[2])], gene_id),200))

#  after grouping by gene ID, take the sum along each column of number of the sample locations.
sum_genes <- pac_genes %>% group_by(gene_id) %>% select(starts_with("V")) %>% summarise_each(funs(sum))

# Extract the header (about the gene being tested)
header <- pac_genes %>% group_by(gene_id) %>% select(-starts_with("V")) %>% distinct(gene_id)

# Join header and genes.  Joining matches the tables together by gene id #
pac_join <- inner_join(x = header, y = sum_genes, by = "gene_id")

# We can see the result of the join
# View(head(arrange(pac_join, gene_id),200))

# This allows us to see what percentage of the probes for a gene were positive at a location.
#View(head(round(pac_join[,(9:370)]/pac_join$tests, digits = 3)))

# Plot by gene, by slice ---------------------------
# This is amazing.
# still working with brain b
local_brain <- gray_matter[[b]]
local_sample <- samples[[b]]
brain_dim <- dim(local_brain)

# We have to transform the voxel coordinates because the brain was stored incorectly.
#  (This took a long time to figure out)
coords <- local_sample[,8:10]
trans_coords <- cbind(coords[1], brain_dim[2] - coords[3], brain_dim[3] - coords[2])
names(trans_coords) <- c("local_x", "local_y", "local_z")

# Convert the transformed coordinates to display on R's plot.
point_coords <- cbind((trans_coords[1] - 1) / (brain_dim[1] - 1), (trans_coords[2] - 1)/(brain_dim[2] - 1), (trans_coords[3] - 1) / (brain_dim[3] - 1))


# Pick a slice
x <- 100

# Pick n_genes random genes from 29180 total genes
n_genes <- 40
gene_sample <- sample(1:dim(pac_join)[1], size = n_genes )

# This function creates colors from steel blue to white.
colfunc <- colorRampPalette(c("white", "steelblue"))

# Step through some of the genes 
# (there's 21,000, but we took a random sample)
for(g in gene_sample){
  # Crete color ramp for this gene. 
  # There are up to "tests" number of tests per gene
  # We color the points based on number of successful tests.
  n_colors <- colfunc( (as.numeric(pac_join[g, 8]) + 1) )
  
  image(local_brain[x, , ], main = paste("x-layer:", x, "\n  gene:", pac_join$gene_name[g]), col = grey(seq(0, 1, length = 256)), axes = F)

  points(x = point_coords$local_y, y = point_coords$local_z, pch = 20, col = n_colors[( 1 + as.numeric(pac_join[g, -(1:8)]) )])

}
# White means no probes active.  
# Dark blue means all probes active

#  Let's try along Y and Z
y <- 85

# We already selected the genes we want to use

# Step through selected genes
for(g in gene_sample){
  # Crete color ramp for this gene. 
  # There are up to "tests" number of tests per gene
  # We color the points based on number of successful tests.
  n_colors <- colfunc( (as.numeric(pac_join[g, 8]) + 1) )
  
  image(local_brain[ ,y , ], main = paste("y-layer:", y, "\n  gene:", pac_join$gene_name[g]), col = grey(seq(0, 1, length = 256)), axes = F)
  
  points(x = point_coords$local_x, y = point_coords$local_z, pch = 20, col = n_colors[( 1 + as.numeric(pac_join[g, -(1:8)]) )])
  
}

# Let's check out Z:
z <- 85

# We already selected the genes we want to use

# Step through selected genes
for(g in gene_sample){
  # Crete color ramp for this gene. 
  # There are up to "tests" number of tests per gene
  # We color the points based on number of successful tests.
  n_colors <- colfunc( (as.numeric(pac_join[g, 8]) + 1) )
  
  image(local_brain[ , ,z ], main = paste("z-layer:", z, "\n  gene:", pac_join$gene_name[g]), col = grey(seq(0, 1, length = 256)), axes = F)
  
  points(x = point_coords$local_x, y = point_coords$local_y, pch = 20, col = n_colors[( 1 + as.numeric(pac_join[g, -(1:8)]) )])
  
}
