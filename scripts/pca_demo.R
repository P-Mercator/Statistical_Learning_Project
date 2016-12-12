#trying pca on the images data
library(data.table)
library(ggplot2)
setwd("C:\\Users\\ax30201\\Documents\\Repos\\Statistical_Learning_Project")

load("img_vec_lab.Rdata")

#run pca
im_pca=prcomp(dt,retx=T,center=T,scale=T,rank.=50)
summary(im_pca)

#pick the 15 first components
foo=as.data.table(cbind(
  im_pca$x[,1],
  im_pca$x[,2],
  im_pca$x[,3],
  im_pca$x[,4],
  im_pca$x[,5],
  im_pca$x[,6],
  im_pca$x[,7],
  im_pca$x[,8],
  im_pca$x[,9],
  im_pca$x[,10],
  im_pca$x[,11],
  im_pca$x[,12],
  im_pca$x[,13],
  im_pca$x[,14],
  im_pca$x[,15]))

foo[,label:=labels]

#representation of the 2 first components
ggplot(data=foo[sample(1:nrow(foo),1000)],aes(x=V1,y=V2))+geom_point(aes(colour=as.factor(label)))

foo[,labels:=as.factor(label)]

#train-test indeces
train_idx=sample(1:nrow(foo),3374)
test_idx=(1:nrow(foo))[!(1:nrow(foo))%in%train_idx]

#train a random Forest
library(randomForest)
im_rf=randomForest(as.factor(label) ~ V1 + V2 + V3 + V4 + V5 + V6 +
                     V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15,data=foo[train_idx],importance=T)
#show the importances
importance(im_rf)

#predict on the test set
foo[test_idx,prediction:=predict(im_rf, foo[test_idx])]

#foo[test_idx,.N]
#foo[test_idx][,list(label,prediction)]
foo[test_idx][,sum(label!=prediction)/.N]
#error prediction, arrond 15%ish

#this will come later :)
#tsne_out=tsne(dt)
