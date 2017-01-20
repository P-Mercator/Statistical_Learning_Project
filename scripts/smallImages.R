rm(list=ls)
require(data.table)
require(pixmap)
require(raster)
library(imager)
library(EBImage)

path_folders="./Data/Marcel-Train"

## path_folders="./Data/test"
folders=list.files(path = path_folders)

## folders=folders[folders!="Five"] #the five is quite broken

labels=c()

#there are 84x72, 82x70 and 76x66 images -> we will crop them
#update: there are also 64x56 images <- new cropped size

# Dataframe of resized images
folder <- folders[1]
file <- files_infolder[2]


rs_df <- c()

for (folder in folders){

  files_infolder=list.files(paste0(path_folders,"/",folder)) 

  for (file in files_infolder){

    fpath = (paste0(path_folders,"/",folder,"/",file))
    image = read.pnm(fpath)

    grey_image=0.299*image@red + 0.587*image@green + 0.114*image@blue
    
    grey_image <- resize(grey_image,w=28,h=28)
    img_vector=as.vector(t(grey_image))
    vec <- c(folder,img_vector)
    
    rs_df <- rbind(rs_df,vec)
    print(paste("Done",fpath,sep = " "))
  }
}

row.names(rs_df) <- 1:length(rs_df[,1])
rs_df <- as.data.frame(rs_df)
names(rs_df) <- c("label", paste("pixel", c(1:(28*28))))

write.csv(rs_df,"./Data/train_28.csv", row.names = FALSE)

plot(as.cimg(t(grey_image)))
