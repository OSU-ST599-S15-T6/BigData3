# Function load_sample_gw() ---------------------------
#' 
#' in: brains_directory
#' out: sample annotation, with attached gray, white values.
#' 
load_sample_gw <- function(brains_directory, white_matter = NULL, gray_matter = NULL){
  source("nifti_read.R")
  
  # Load the brains into a list.
  if(is.null(white_matter) == T)
    white_matter <- brain_loader(brains_directory = brains_directory, gray = F)
  if(is.null(gray_matter)== T)
    gray_matter  <- brain_loader(brains_directory = brains_directory, gray = T)
  
  # Read in the sample data.
  white_sample <- sample_loader(brains_directory, white_matter, is_gray = F)
  gray_sample  <- sample_loader(brains_directory, gray_matter, is_gray = T)
  
  gw_sample <- vector(mode = "list", length = length(white_sample))
  
  for(i in 1:length(white_sample)){
    if(is.null(white_sample[[i]])==F){
      gw_sample[[i]] <- cbind(white_sample[[i]], gray = gray_sample[[i]]$gray)
    }
    
  }

  return(gw_sample)
}


#  Function sample_loader() ---------------------------
#' 
#' In:  directory containing brain folders, optional: list containing brain images
#' Out: list containing data about samples
#'      Null if no MA data exists for that brain
#' 
#' If you add the argument brain_list, (a list of 3-dimensional brain imagery created by brain_loader)
#'  we create a gray value for each sample using the brain imagery in the brain_list.
#'  
sample_loader <- function(brains_directory, brain_list = NULL, is_gray = F){
  
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
                                           local_sample = annotation_list[[i]], is_gray)

      }
    }
  }
  
  #  Return the list of microarray data.
  return(annotation_list)
}

# function micro_loader ---------------------------
#'
#'  In:  directory containing brains,  which brain to read, Use binary data or not
#'  Out: get the data from the 3d brain array and append it to the annotation.
#'  
#'  This is an older version of the microarray loader.
#'  The newer version (probe_loader) allows you to append information about
#'  each probe, (like its name and the gene it tests for),
#'  as well as options for loading the binary probe results 
#'  
micro_loader <- function(brains_directory, i = 1, binary = F){
  
  brain_folders <- list.dirs(path = brains_directory, recursive = F)
  short_dir <- list.dirs(path = brains_directory, recursive = F, full.names = F)
  # If the annotation file exists, we write it to annotation list. Else, it remains null.
  
  if(binary == F){
    if("MicroarrayExpression.csv" %in% list.files(brain_folders[i])){
      message(paste("Reading", short_dir[i], "/ MicroarrayExpression.csv"))
      ma_data <- read.csv(paste(brain_folders[i], "MicroarrayExpression.csv", sep = "/"), header = F)
    }
  }

  if(binary == T){
    if("PACall.csv" %in% list.files(brain_folders[i])){
      message(paste("Reading", short_dir[i], "/ PACall.csv"))
      ma_data <- read.csv(paste(brain_folders[i], "PACall.csv", sep = "/"), header = F)
    }
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
read_gray <- function(local_brain, local_sample, is_gray = F){
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
  
  if(is_gray == F)
    local_sample <- cbind(local_sample, white = gray_level)
    
  if(is_gray == T)
    local_sample <- cbind(local_sample, gray = gray_level)
  
  
  #  Return the modified local_sample
  return(local_sample)
}



# function probe_loader ---------------------------
#'
#'  In:  directory containing brains,  which brain to read, Use binary data or not
#'  Out: ma_data (probes data, with or without probe annotation.)
#'  
#'  We need to read the microarray expressions from one file,
#'  which contains the results of the probes.
#'  This can be binary (p>0.01?) or continuous.
#'  
#'  Additiona information probe details exist in a seperate file.
#'  Merge the probe details with probe results by using with_probes = T
#'  
probe_loader <- function(brains_directory, i = 1, binary = F, with_probes = T){
  
  brain_folders <- list.dirs(path = brains_directory, recursive = F)
  short_dir <- list.dirs(path = brains_directory, recursive = F, full.names = F)
  # If the annotation file exists, we write it to annotation list. Else, it remains null.
  

  
  if(binary == F){
    if("MicroarrayExpression.csv" %in% list.files(brain_folders[i])){
      message(paste("Reading", short_dir[i], "/ MicroarrayExpression.csv"))
      ma_data <- read.csv(paste(brain_folders[i], "MicroarrayExpression.csv", sep = "/"), header = F)
    }
  }
  
  
  if(binary == T){
    if("PACall.csv" %in% list.files(brain_folders[i])){
      message(paste("Reading", short_dir[i], "/ PACall.csv"))
      ma_data <- read.csv(paste(brain_folders[i], "PACall.csv", sep = "/"), header = F)
    }
  }
  
  if(with_probes == T){
    if("Probes.csv" %in% list.files(brain_folders[i])){
      message(paste("Reading", short_dir[i], "/ Probes.csv"))
      probe_data <- read.csv(paste(brain_folders[i], "Probes.csv", sep = "/"), header = T)
      #message("Appending probe details to microarray data")
      ma_data  <- cbind(probe_data, ma_data)
    }
  }
  
  
  # return the micro array.
  return(ma_data)
}

# function gene_loader ---------------------------
#'
#' In:   probes, in the format of probe_loader. (Must include 'with_probes' header information)
#' Out:  data frame containing microarray expression summarized by gene.
#' 
#' With the probes for a particular brain, we summarize (sum) results by gene.
# 
gene_summary <- function(probes){
  require("dplyr")
  
  # Weight each test ---------------------------
  # Plan:  get a weight for each test.  By counting the number of tests per gene, 
  #        we will know what percentage of genes are 
  pac_df <- as.data.frame(probes)
  
  # Create a column with tests per gene.
  pac_genes <- pac_df %>% group_by(gene_id) %>% mutate(V1 =  n())
  pac_genes <- rename(pac_genes, tests = V1)
  
  #  after grouping by gene ID, take the sum along each column of number of the sample locations.
  sum_genes <- pac_genes %>% group_by(gene_id) %>% select(starts_with("V")) %>% summarise_each(funs(sum))
  
  # Extract the header (about the gene being tested)
  header <- pac_genes %>% group_by(gene_id) %>% select(-starts_with("V")) %>% distinct(gene_id)
  
  # Join header and genes.  Joining matches the tables together by gene id #
  pac_join <- inner_join(x = header, y = sum_genes, by = "gene_id")
  
  return(pac_join)  
}
