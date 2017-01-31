rm(list=ls)
require(data.table)
require(pixmap)
require(raster)
library(imager)
library(EBImage)

## path_folders="./Data/Marcel-Train"

path_folders="./Data/test"
folders=list.files(path = path_folders)

## folders=folders[folders!="Five"] #the five is quite broken

#there are 84x72, 82x70 and 76x66 images -> we will crop them
#update: there are also 64x56 images <- new cropped size


grey_df <- c()
gradient_df <- c()


size <- 50

for (folder in folders){

  files_infolder=list.files(paste0(path_folders,"/",folder))

  for (file in files_infolder){

    fpath = (paste0(path_folders,"/",folder,"/",file))
    image = read.pnm(fpath)

    grey_image=0.299*image@red + 0.587*image@green + 0.114*image@blue

    grey_image <- resize(grey_image,w=size,h=size)
    grey_vector=as.vector(t(grey_image))
    grey_df <- rbind(grey_df,c(folder,grey_vector))

    img_grad=imgradient(as.cimg(grey_image),"xy")
    im_gradients=as.matrix(sqrt(img_grad$x^2+img_grad$y^2))
    grad_vector=as.vector(t(im_gradients))
    gradient_df <- rbind(gradient_df,c(folder,grad_vector))

    print(paste("Done",fpath,sep = " "))
  }
}


row.names(grey_df) <- 1:length(grey_df[,1])
grey_df <- as.data.frame(grey_df)
names(grey_df) <- c("label", paste("pixel", c(1:(size*size))))

row.names(gradient_df) <- 1:length(gradient_df[,1])
gradient_df <- as.data.frame(gradient_df)
names(gradient_df) <- c("label", paste("pixel", c(1:(size*size))))

## save(grey_df,gradient_df,file="./img_train_50.Rdata")


###### TEST
plot(as.cimg(t(grey_image)))
plot(as.cimg(t(im_gradients)))

head(grey_df[,1:5])
head(gradient_df[,1:5])
