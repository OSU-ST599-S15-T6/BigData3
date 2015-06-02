#  (Assign the appropriate local directory for your data and brain)
#  In one directory, place the 8 folders containing brain images.
#  Tell brain_loader where to find those images.

brains_directory <- "Z:/ST599/Project3/brain/brain_images"


source("nifti_read.R")
source("microarray_read.R")

# Create two lists, one containing white voxel arrays and one containing gray voxel arrays
white_matter <- brain_loader(brains_directory = brains_directory, gray = F)
gray_matter  <- brain_loader(brains_directory = brains_directory, gray = T) 

# Samples contains information about where each sample was taken,
# and we added gray, white values.
#
# [1] "structure_id"      "slab_num"          "well_id"          
# [4] "slab_type"         "structure_acronym" "structure_name"   
# [7] "polygon_id"        "mri_voxel_x"       "mri_voxel_y"      
# [10] "mri_voxel_z"       "mni_x"             "mni_y"            
# [13] "mni_z"             "white"             "gray"
samples <- load_sample_gw(brains_directory, white_matter = white_matter, gray_matter = gray_matter)

# Load probes for brain i = 3, selecting binary = T uses the thresholded values from PACAll.csv, binary = F uses MicroarrayExpression.csv
# [1] "probe_id"    "probe_name"  "gene_id"     "gene_symbol" "gene_name"   "entrez_id"   "chromosome"  "V1"   "V2" ... V(p)
# V1 is a duplicate of probe_id, not a sample location.
brain_3_probes = probe_loader(brains_directory, i = 3, binary = T)

#  Summarize with gene_summary (collapses from 58,000 probes to 21,000 genes)
# V Entries are the sum of each probe.  If used with binary data it is number of positive probes for a given gene.
#  [1] "probe_id"    "probe_name"  "gene_id"     "gene_symbol" "gene_name"   "entrez_id"   "chromosome"  "tests"  "V2" ... V(p)
brain_3_genes <- gene_summary(brain_3_probes)

