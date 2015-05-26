#  Read nifti (.nii) image files into R

#  oro.nifti allows us to read in the MRI data and view it.
#install.packages("oro.nifti")
library(oro.nifti)

#  (Assign the appropriate local directory for your data and brain)
# Brain Choices: "H0351.1009" "H0351.1012" "H0351.1015" "H0351.1016" "H0351.2001" [6] "H0351.2002" "H0351.2003" "H372.0006"
directory <- "Z:/ST599/Project3/brain/brain_images/H0351.2001"

# T1 contains the white matter MRI
target_image <- paste(directory, "T1.nii", sep = "/")

n2 <- readNIfTI(target_image)

# Is it in type nifti-1?
n2
n3 <- n2


?magic
magic(n2)
magic(ffd)

magic(n3)
magic(n3) <- 0
vox_units(object = n3)
?vox_units
vox.offset(n3)
vox.offset(object = n3) <- 1296
vox.offset(n3)
image(n3)

vox.offset(ffd)
voxdim(n3)
n2
dim(n3)
qoffset.x(n3)
qoffset.y(n3)
dim(ffd)
qoffset.x(ffd)
qoffset.y(ffd)
qoffset.y(n3)<-0
image(n3)
image(n2)

slotNames(ffd)
slotNames(n3)
pixdim(n3)
slice_start(n3)
slice_start(ffd)
slice_start(n3) <- 10
image(n3)


# NIfTI sform ---------------------------
sform(object = n3, srow.y = c(0,0,0,0))
#sform(n3) <- sform(ffd)
sform(n3)
sform(ffd)
srow_x(n3) <- c(0,0,0,0)
srow_y(n3) <- c(0,0,0,0)
srow_z(n3) <- c(0,0,0,0)
image(n3)

# dropImageDimensions ---------------------------
nim <- nifti(array(rnorm(10^3), dim = rep(10, 3)))
nim2 <- nifti(array(rnorm(10^3), dim = c(10, 10, 1, 10)))
image(nim)
dropImageDimension(nim2)
nim1 <- dropImageDimension(nim2, onlylast = FALSE)
image(nim1)
nim3 <- nifti(array(rnorm(10^3), dim = c(10, 10, 10, 1)))
nim1 <- dropImageDimension(nim3)
nim1 <- dropImageDimension(nim3, onlylast = FALSE) # the same as above


nim4 <- nifti(array(rnorm(10^3), dim = c(10, 10, 10, 1, 10)))
nim4
dim(nim4[,,,1,])
dim(nim4[,,,1,,drop=TRUE])
dropImageDimension(nim4)

nim5 <- nifti(array(rnorm(10^4), dim = c(1, 10, 10, 10, 1, 10)))
dropImageDimension(nim5)
dropImageDimension(nim5, onlylast = FALSE)

nim6 <- nifti(array(rnorm(10^3), dim = c(1, 10, 10, 10, 1, 1)))
dropImageDimension(nim6)


# Example:
url <- "http://nifti.nimh.nih.gov/nifti-1/data/filtered_func_data.nii.gz"
urlfile <- file.path(system.file("nifti", package="oro.nifti"),
                     "filtered_func_data")
download.file(url, urlfile, quiet=TRUE)
urlfile <- file.path(system.file("nifti", package="oro.nifti"),
                     "filtered_func_data")
(ffd <- readNIfTI(urlfile))
image(ffd, oma=rep(2,4))
orthographic(ffd, oma=rep(2,4))

# Another example ---------------------------

out.img <- nifti(n2, datatype=16, dim=dim(n2))
image(out.img)
out.img
n2@.Data

# Ok those bastards use an @ sign to index variables ---------------------------
n2@sizeof_hdr
n2@data_type
n2@data_type
n2@extents
n2@session_error
n2@intent_p2
n2@regular
n2@dim_
n2

#  we get signal.
pixdim(out.img)[2:4] <- 4 
writeNIfTI(out.img, "fakeBrain");
in.img <- readNIfTI("fakeBrain.nii.gz", reorient=FALSE);
image(in.img)
dat_out <- n2@.Data
View(head(dat_out ))
dim(dat_out)
View(dat_out[,,50])

# Main screen turn on
image(dat_out[,,100], axes = F)
n4 <- nifti(dat_out)
image(n4)

dim(dat_out)
for(i in 50:100)
  image(dat_out[,,i], axes = F)

# Here is the first image in the slices.
View(dat_out[,,13])
#  All the data is located here. I know it looks rather streight on RHS , but 
#  I do not think it was cropped by a computer, since it is not near an edge.
View(dat_out[,25:45,13])
# Let's suppose the center of that thing is supposed to be the center of the image.
#  Line 95.  The dim is 185 in height.  
#  We might have a center of 92.5, or 90, with a shift of 5...

#  Compare to the next set.
image(dat_out[,,14])
image(dat_out[,,21])
View(dat_out[,25:45,14])

# last few images are also blank.

two_image <- cbind(dat_out[,,24], dat_out[,,25], dat_out[,,26], dat_out[,,28], dat_out[,,29])
two_image <- cbind(dat_out[,,22], dat_out[,,23], dat_out[,,24], dat_out[,,25])
image(two_image)
# This works.
two_image <- cbind(dat_out[,,50], dat_out[,,51], dat_out[,,52], dat_out[,,53])
image(two_image)

dim(dat_out)
long_image <- matrix(rep(0,prod(dim(dat_out))), ncol = dim(dat_out)[2] )
for(i in 1:dim(dat_out)[3])
  long_image[((i-1)*dim(dat_out)[1]+1):(i*dim(dat_out)[1]),] <- dat_out[,,i]
#access image number i
i <- 70
image(long_image[((i-1)*dim(dat_out)[1]+1):(i*dim(dat_out)[1]),])
# but it's really just one long vector.  we can slice it differently.
dim(dat_out)
#  dim(dat_out)[1]  # the previous value we'd been using. (185)
image_length  <- 185
image(long_image[((i-1)*image_length+1):(i*image_length),])
i <- 70

# Something is messed up below.**
#  We seem to be shifting x.  We need to store rotated images.
dim(long_image)
?dim

wide_image2 <- matrix(rep(0,prod(dim(dat_out))), nrow = dim(dat_out)[2] )
View(wide_image2)
for(i in 1:dim(dat_out)[3])
  wide_image2[,((i-1)*dim(dat_out)[1]+1):(i*dim(dat_out)[1])] <- dat_out[,,i]
#access image number i
i <- 70
image(wide_image2[,((i-1)*dim(dat_out)[1]+1):(i*dim(dat_out)[1])])
# but it's really just one long vector.  we can slice it differently.
dim(dat_out)
#  dim(dat_out)[1]  # the previous value we'd been using. (185)
image_length  <- 185
image(long_image[((i-1)*image_length+1):(i*image_length),])
i <- 70




# How does ffd differ from what I have?
ffd
n2


#  image shows the whole data.
image(n2)

#  Orthographic shows slices of the data.
orthographic(n2)
orthographic(n2, xyz = c(0,0,0))
orthographic.nifti(n2, text = "Fire the lasers!!", text.color = "red", col.crosshairs = "red")
n2
location <- c(45,45,90)
orthographic.nifti(n2, text = paste("H0351.2001 \n T1.nii \n ", paste(location, collapse = ", "), sep = ""), xyz = location)

# try reading in the gray matter T2?
directory <- "Z:/ST599/Project3/brain/brain_images/H0351.2001"
target_image <- paste(directory, "T2.nii", sep = "/")
gray <- readNIfTI(target_image)
image(gray)
location <- c(45,45,90)
orthographic.nifti(gray, text = paste("H0351.2001 \n T2.nii \n ", paste(location, collapse = ", "), sep = ""), xyz = location)
orthographic.nifti(gray, text = paste("H0351.2001 \n T2.nii \n ", paste(location, collapse = ", "), sep = ""), xyz = location, crosshairs = F)


# Lets try another patient.
directory <- "Z:/ST599/Project3/brain/brain_images/H0351.2002"
target_image <- paste(directory, "T1.nii", sep = "/")
white <- readNIfTI(target_image)
image(white)
location <- c(45,45,90)
orthographic.nifti(white, text = paste("H0351.2002 \n T1.nii \n ", paste(location, collapse = ", "), sep = ""), xyz = location)
orthographic.nifti(gray, text = paste("H0351.2001 \n T2.nii \n ", paste(location, collapse = ", "), sep = ""), xyz = location, crosshairs = F)




