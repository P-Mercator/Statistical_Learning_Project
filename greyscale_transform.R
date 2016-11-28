#In order to run the script, you need to set the working directory
#In a folder with a subfolder called Data, which has a subfolder
#with the uncompressed images

#This scipt transforms the rgb images to greyscale

setwd("C:\\Users\\ax30201\\Documents\\Master\\S_Learning")

require(data.table)
require(pixmap)
require(raster)

path_folders="./Data/Marcel-Train"
folders=list.files(path = path_folders)

files_A=list.files(paste0(path_folders,"/",folders[1]))

#I = 0.299R + 0.587G + 0.114B

image_1=read.pnm(paste0(path_folders,"/",folders[1],"/",files_A[1]))
grey_1=0.299*image_1@red + 0.587*image_1@green + 0.114*image_1@blue

grey_11=0.33*image_1@red + 0.33*image_1@green + 0.33*image_1@blue

plot(raster(1-grey_11))
