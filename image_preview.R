#In order to run the script, you need to set the working directory
#In a folder with a subfolder called Data, which has a subfolder
#with the uncompressed images
setwd("C:\\Users\\ax30201\\Documents\\Master\\S_Learning")

require(data.table)
require(pixmap)

path_folders="./Data/Marcel-Train"
folders=list.files(path = path_folders)

N=c()

for(folder in folders){
  N=c(N,length(list.files(paste0(path_folders,"/",folder))))
}

#Images by type
N

#total number of images
sum(N)

#read the first image for each type
file_1=list.files(paste0(path_folders,"/",folders[1]))[1]
image_1=read.pnm(paste0(path_folders,"/",folders[1],"/",file_1))
#the images are 76x66px images, RGB
plot(image_1)
#We can see that for "A" we have short of a closed hand


file_2=list.files(paste0(path_folders,"/",folders[2]))[1]
image_2=read.pnm(paste0(path_folders,"/",folders[2],"/",file_2))
plot(image_2)
#B looks like an open hand  - stop sign

file_3=list.files(paste0(path_folders,"/",folders[3]))[1]
image_3=read.pnm(paste0(path_folders,"/",folders[3],"/",file_3))
plot(image_3)
#C looks like a C with the hand - quite straightforward

file_4=list.files(paste0(path_folders,"/",folders[4]))[1]
image_4=read.pnm(paste0(path_folders,"/",folders[4],"/",file_4))
plot(image_4)
#Five is a high five sign

file_5=list.files(paste0(path_folders,"/",folders[5]))[1]
image_5=read.pnm(paste0(path_folders,"/",folders[5],"/",file_5))
plot(image_5)
#Point is the pointing sign

file_6=list.files(paste0(path_folders,"/",folders[6]))[1]
image_6=read.pnm(paste0(path_folders,"/",folders[6],"/",file_6))
plot(image_6)
#V has the peace sign