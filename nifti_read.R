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
image(n2)

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




