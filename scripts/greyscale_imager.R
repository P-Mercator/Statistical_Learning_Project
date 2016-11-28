##imager dummies demo on our data
##Im missing the lessons bc of this, not too interesting tbh

#In order to run the script, you need to set the working directory
#In a folder with a subfolder called Data, which has a subfolder
#with the uncompressed images

#This scipt transforms the rgb images to greyscale

setwd("C:\\Users\\ax30201\\Documents\\Repos\\Statistical_Learning_Project")

require(data.table)
require(pixmap)
require(raster)
library(imager)

path_folders="./Data/Marcel-Train"
folders=list.files(path = path_folders)

labels=c()
images=c()
grey_images=list()
vectors=list()
idx=1

#there are 84x72, 82x70 and 76x66 images -> we will crop them

for (folder in folders)
{
  files_infolder=list.files(paste0(path_folders,"/",folder)) 
  for (file in files_infolder){
    fpath = (paste0(path_folders,"/",folder,"/",file))
    image = read.pnm(fpath)
    images=c(images,image)
    
    grey_image=0.299*image@red + 0.587*image@green + 0.114*image@blue
    
    #crop to 76x66
    if(dim(grey_image)[1]>76){
      gap_x=dim(grey_image)[1]-76
      gap_y=dim(grey_image)[2]-66
      
      x_range=gap_x:dim(grey_image)[1]-gap_x
      y_range=gap_y:dim(grey_image)[2]-gap_y
      
      grey_image=grey_image[x_range,y_range]
    }
    
    grey_images[[idx]]=grey_image
    vectors[[idx]]=as.vector(grey_image)
    idx=idx+1
    labels=c(labels,folder)
  }
}

matrix_version=as.matrix(vectors)

foo=as.cimg(t(grey_image))
plot(foo)
#it actually works

#equalised
f <- ecdf(foo)
plot(f,main="Empirical CDF of luminance values")
f(foo) %>% as.cimg(dim=dim(foo)) %>% plot(main="Equalised")

#draw the gradient
gr <- imgradient(foo,"xy")
plot(sqrt(gr$x^2+gr$y^2),layout="row")

#draw the gradient
gr <- imgradient(f(foo) %>% as.cimg(dim=dim(foo)),"xy")
plot(sqrt(gr$x^2+gr$y^2),layout="row")
plot(sqrt(gr$x^2+gr$y^2),layout="row")

#template of first 200 images
template=grey_images[[1]]
for(i in 2:200){
  template=template+grey_images[[i]]
}
template=template/200

as.cimg(t(template)) %>% plot()
