#trying pca on the images data
  library(data.table)
  library(ggplot2)
  setwd("C:\\Users\\ax30201\\Documents\\Repos\\Statistical_Learning_Project")
  
  load("img_vec_lab.Rdata")
  
  #run pca
  im_pca=prcomp(dt,retx=T,center=T,scale=T,rank.=50)
  #summary(im_pca)
  grad_pca=prcomp(dt_grad,retx=T,center=T,scale=T,rank.=50)
  
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
    im_pca$x[,15],
    im_pca$x[,16],
    im_pca$x[,17],
    im_pca$x[,18],
    im_pca$x[,19],
    im_pca$x[,20],
    im_pca$x[,21],
    im_pca$x[,22],
    im_pca$x[,23],
    im_pca$x[,24],
    im_pca$x[,25],
    grad_pca$x[,1],
    grad_pca$x[,2],
    grad_pca$x[,3],
    grad_pca$x[,4],
    grad_pca$x[,5],
    grad_pca$x[,6],
    grad_pca$x[,7],
    grad_pca$x[,8],
    grad_pca$x[,9],
    grad_pca$x[,10],
    grad_pca$x[,11],
    grad_pca$x[,12],
    grad_pca$x[,13],
    grad_pca$x[,14],
    grad_pca$x[,15],
    grad_pca$x[,16],
    grad_pca$x[,17],
    grad_pca$x[,18],
    grad_pca$x[,19],
    grad_pca$x[,20],
    grad_pca$x[,21],
    grad_pca$x[,22],
    grad_pca$x[,23],
    grad_pca$x[,24],
    grad_pca$x[,25]))
  
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

#same test with SVM
library(e1071)

im_svm=svm(as.factor(label) ~ V1 + V2 + V3 + V4 + V5 + V6 +
      V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15,data=foo[train_idx], scale = TRUE, type = NULL, kernel ="radial")

svm_pred=predict(im_svm, foo[test_idx], decision.values = TRUE)

foo[test_idx,prediction:=svm_pred]
foo[test_idx][,sum(label!=prediction)/.N]

#SVM performs slightly worse with default hyper parameters

#this will come later :)
#try max_iter=500 at tsne 
#tsne_out=tsne(dt)

load("./tsne_out.Rdata")

foo[,tsne_1:=tsne_out[,1]]
foo[,tsne_2:=tsne_out[,2]]
ggplot(data=foo[sample(1:nrow(foo),2000)],aes(x=tsne_1,y=tsne_2))+geom_point(aes(colour=as.factor(label)))

im_rf=randomForest(as.factor(label) ~ V1 + V2 + V3 + V4 + V5 + V6 +
                     V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + tsne_1 + tsne_2,data=foo[train_idx],importance=T)


#buid a random forest, a svm, and a gbm, and compare the ROC curves of these for Ncomp=5,10,20,40
#randomForest
#svm
#gbm
library(randomForest)
library(e1071)
library(gbm)

##hyperparameter tunning

#hyper parameter search RF -> optimal nodesize tree (always 300 trees) -> var nodesize, ntree=300
#hyper param serach for SVM -> gamma and cost -> var gamma, var cost
#gbm -> depth and learning rate (always 300 trees)

#5fold CV
#average AUC for every test
#cross validation in R: https://www.r-bloggers.com/cross-validation-for-predictive-analytics-using-r/

#RF hyper-param search
#nodesize as a % of train set size
#0.1% 0.5% 1% 2.5% 5% 7.5% 10%


n_folds <- 5
folds_i <- sample(rep(1:n_folds, length.out = nrow(foo)))
foo[,fold:=folds_i]

#for every index in n_folds
for (i in 1:n_folds){
  #train set as all cv set but the ith
  cv_train=foo[fold!=i]
  #test set as the ith set
  cv_test=foo[fold==i]
  
  #train the model on the cv_train 
  aux_mod=randomForest(as.factor(label) ~  V1 + V2 + V3 + V4 + V5 + V6 +
                         V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 ,data=cv_train)
  
  #predict on cv_test
  cv_test[,aux_pred:=predict(aux_mod,cv_test)]
  
  #evaluate performance on the cv_test, use the accuracy
  aux_acc=cv_test[,sum(label==aux_pred)/.N]
  
  print(aux_acc)
}


RF_nodesize=round(nrow(foo)*c(0.001,0.0025,0.005,0.01,0.025,0.05,0.075,0.1))

#use the 5cv previous sctrcture to look for optimal hyper param

param_acc_15=c()
for (param_idx in 1:length(RF_nodesize)){
  acc=c()
  for (i in 1:n_folds){
    #train set as all cv set but the ith
    cv_train=foo[fold!=i]
    #test set as the ith set
    cv_test=foo[fold==i]
    
    #train the model on the cv_train 
    aux_mod=randomForest(as.factor(label) ~  V1 + V2 + V3 + V4 + V5 + V6 +
                           V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 ,data=cv_train,
                         nodesize=RF_nodesize[param_idx])
    
    #predict on cv_test
    cv_test[,aux_pred:=predict(aux_mod,cv_test)]
    
    #evaluate performance on the cv_test, use the accuracy
    aux_acc=cv_test[,sum(label==aux_pred)/.N]
    
    #print(aux_acc)
    
    #add las acc value
    acc=c(acc,aux_acc)
  }
  print(mean(acc))
  param_acc_15=c(param_acc_15,mean(acc))
}

#same model, 5 components   
param_acc_5=c()
for (param_idx in 1:length(RF_nodesize)){
  acc=c()
  for (i in 1:n_folds){
    #train set as all cv set but the ith
    cv_train=foo[fold!=i]
    #test set as the ith set
    cv_test=foo[fold==i]
    
    #train the model on the cv_train 
    aux_mod=randomForest(as.factor(label) ~  V1 + V2 + V3 + V4 + V5 ,data=cv_train,
                         nodesize=RF_nodesize[param_idx])
    
    #predict on cv_test
    cv_test[,aux_pred:=predict(aux_mod,cv_test)]
    
    #evaluate performance on the cv_test, use the accuracy
    aux_acc=cv_test[,sum(label==aux_pred)/.N]
    
    #print(aux_acc)
    
    #add las acc value
    acc=c(acc,aux_acc)
  }
  print(mean(acc))
  param_acc_5=c(param_acc_5,mean(acc))
}

#same model 25 componentes
param_acc_25=c()
for (param_idx in 1:length(RF_nodesize)){
  acc=c()
  for (i in 1:n_folds){
    #train set as all cv set but the ith
    cv_train=foo[fold!=i]
    #test set as the ith set
    cv_test=foo[fold==i]
    
    #train the model on the cv_train 
    aux_mod=randomForest(as.factor(label) ~  V1 + V2 + V3 + V4 + V5 + V6 +
                           V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + 
                           V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25,data=cv_train,
                         nodesize=RF_nodesize[param_idx])
    
    #predict on cv_test
    cv_test[,aux_pred:=predict(aux_mod,cv_test)]
    
    #evaluate performance on the cv_test, use the accuracy
    aux_acc=cv_test[,sum(label==aux_pred)/.N]
    
    #print(aux_acc)
    
    #add las acc value
    acc=c(acc,aux_acc)
  }
  print(mean(acc))
  param_acc_25=c(param_acc_25,mean(acc))
}

out=data.table(c(param_acc_5,param_acc_15,param_acc_25),c(rep("5",length(RF_nodesize)),rep("15",length(RF_nodesize)),rep("25",length(RF_nodesize))))
out[,nodesize:=rep(RF_nodesize,3)]

setnames(out,c("V1","V2"),c("ACC","N_PC"))

head(out)

ggplot(out, aes(y=ACC,x=nodesize, group = N_PC, colour = N_PC)) +
  geom_path(alpha = 0.5)


