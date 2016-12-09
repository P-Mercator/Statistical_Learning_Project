#trying pca on the images data

load("img_vec_lab.Rdata")

im_pca=prcomp(dt)
summary(im_pca)
