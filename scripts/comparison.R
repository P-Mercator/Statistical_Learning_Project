rm(list=ls())
library(data.table)
library(ggplot2)
                                        # ############ #
                                        # Prepare data #
                                        # ############ #

load("./img_train_50.Rdata")

pixels.grad <- matrix(as.numeric(unlist(gradient_df[,-1])),nrow=nrow(gradient_df[,-1]))

pixels <- matrix(as.numeric(unlist(grey_df[,-1])),nrow=nrow(grey_df[,-1]))

im_pca=prcomp(pixels,retx=T,center=T,scale=T,rank.=50)
grad_pca=prcomp(pixels.grad,retx=T,center=T,scale=T,rank.=50)

  #pick the 25 first components
grey.PCA <- as.data.table(cbind(im_pca$x[,1:25]))
gradient.PCA <- as.data.table(cbind(grad_pca$x[,1:25]))

# Remove outliers in PC1 of gradient
outlierKD(gradient.PCA,PC1)
gradient.PCA<- subset(gradient.PCA, gradient.PCA$PC1 > -4)

# Label data
labels <- grey_df[,1]
gradient.PCA[,label:=labels]
grey.PCA[,label:=labels]

# Plot 2 first PC
ggplot(data=grey.PCA,aes(x=PC1,y=PC2))+
  geom_point(aes(colour=as.factor(label)), alpha = 2/10)

ggplot(data=gradient.PCA,aes(x=PC1,y=PC2))+
  geom_point(aes(colour=as.factor(label)), alpha = 2/10)


                                        # ####### #
                                        # Analyis #
                                        # ####### #


                                        # Support Vector Machine #
                                        # ###################### #

#train-test indeces
train_idx=sample(1:nrow(gradient.PCA),3374)
test_idx=(1:nrow(gradient.PCA))[!(1:nrow(gradient.PCA))%in%train_idx]

#train a random Forest
library(randomForest)
im_rf=randomForest(as.factor(label) ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 +
                     PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15,data=gradient.PCA[train_idx],importance=T)
#show the importances
importance(im_rf)

#predict on the test set
gradient.PCA[test_idx,prediction:=predict(im_rf, gradient.PCA[test_idx])]

#gradient.PCA[test_idx,.N]
#gradient.PCA[test_idx][,list(label,prediction)]
gradient.PCA[test_idx][,sum(label!=prediction)/.N] #error prediction, arrond 15%ish #same test with SPCM
library(e1071)

im_svm=svm(as.factor(label) ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 +
      PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15,data=gradient.PCA[train_idx], scale = TRUE, type = NULL, kernel ="radial")

svm_pred=predict(im_svm, gradient.PCA[test_idx], decision.values = TRUE)

gradient.PCA[test_idx,prediction:=svm_pred]
gradient.PCA[test_idx][,sum(label!=prediction)/.N]

#SPCM performs slightly worse with default hyper parameters


                                        # Random Forest #
                                        # ############# #

#buid a random forest, a svm, and a gbm, and compare the ROC curves of these for Ncomp=5,10,20,40
#randomForest
#svm
#gbm
library(randomForest)
library(e1071)
library(gbm)

##hyperparameter tunning

#hyper parameter search RF -> optimal nodesize tree (always 300 trees) -> var nodesize, ntree=300
#hyper param serach for SPCM -> gamma and cost -> var gamma, var cost
#gbm -> depth and learning rate (always 300 trees)

#5fold CPC
#average AUC for every test
#cross validation in R: https://www.r-bloggers.com/cross-validation-for-predictive-analytics-using-r/

#RF hyper-param search
#nodesize as a % of train set size
#0.1% 0.5% 1% 2.5% 5% 7.5% 10%


n_folds <- 5
folds_i <- sample(rep(1:n_folds, length.out = nrow(gradient.PCA)))
gradient.PCA[,fold:=folds_i]

#for every index in n_folds
for (i in 1:n_folds){
  #train set as all cv set but the ith
  cv_train=gradient.PCA[fold!=i]
  #test set as the ith set
  cv_test=gradient.PCA[fold==i]
  
  #train the model on the cv_train 
  aux_mod=randomForest(as.factor(label) ~  PC1 + PC2 + PC3 + PC4 + PC5 + PC6 +
                         PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15 ,data=cv_train)
  
  #predict on cv_test
  cv_test[,aux_pred:=predict(aux_mod,cv_test)]
  
  #evaluate performance on the cv_test, use the accuracy
  aux_acc=cv_test[,sum(label==aux_pred)/.N]
  
  print(aux_acc)
}


RF_nodesize=round(nrow(gradient.PCA)*c(0.001,0.0025,0.005,0.01,0.025,0.05,0.075,0.1))

#use the 5cv previous sctrcture to look for optimal hyper param

param_acc_15=c()
for (param_idx in 1:length(RF_nodesize)){
  acc=c()
  for (i in 1:n_folds){
    #train set as all cv set but the ith
    cv_train=gradient.PCA[fold!=i]
    #test set as the ith set
    cv_test=gradient.PCA[fold==i]
    
    #train the model on the cv_train 
    aux_mod=randomForest(as.factor(label) ~  PC1 + PC2 + PC3 + PC4 + PC5 + PC6 +
                           PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15 ,data=cv_train,
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
    cv_train=gradient.PCA[fold!=i]
    #test set as the ith set
    cv_test=gradient.PCA[fold==i]
    
    #train the model on the cv_train 
    aux_mod=randomForest(as.factor(label) ~  PC1 + PC2 + PC3 + PC4 + PC5 ,data=cv_train,
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
    cv_train=gradient.PCA[fold!=i]
    #test set as the ith set
    cv_test=gradient.PCA[fold==i]
    
    #train the model on the cv_train 
    aux_mod=randomForest(as.factor(label) ~  PC1 + PC2 + PC3 + PC4 + PC5 + PC6 +
                           PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15 + 
                           PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25,data=cv_train,
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


                                        # K Nearest Neighbours #
                                        # #################### #

## Normalizing numeric data:
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

knn_n <- as.data.frame(lapply(gradient.PCA[,1:25], normalize))
summary(knn_n)

###

library(data.table)
knn_n=as.data.table(knn_n)
dim(knn_n)
cols=names(knn_n)
knn_n=knn_n[,(cols):=lapply(.SD, as.double),.SDcols=cols]
dt_knn_n <- (cbind(labels,knn_n))
dim(dt_knn_n)

## Creating training and test data set:

sub <- sample(nrow(dt_knn_n), floor(nrow(dt_knn_n) * 0.9))
train_knn <- dt_knn_n[sub, ]
test_knn <- dt_knn_n[-sub, ]
dim(train_knn)
dim(test_knn)
knn_train_labels <- train_knn[, labels]
knn_test_labels <- test_knn[, labels]
length(knn_train_labels)



## Training a model on data and building the prediction model.

library(class)
features=cols[grepl("PC",cols)]
accuracy <- rep(0, 20)
k <- 1:20
for(x in k){
  prediction <- knn(train = train_knn[,features,with=F], test = test_knn[,features,with=F], cl = knn_train_labels, k=x)
  accuracy[x] <- mean(prediction == knn_test_labels)
}
plot(k, accuracy, pch = 16, col = 'royalblue2', cex = 1.5, main= 'Accuarcy Vs. K', type = 'b'); box(lwd = 2);



## We can see it’s accuracy using table.
library(gmodels)
knn_test_pred <- knn(train = train_knn[,features,with=F], test = test_knn[,features,with=F], cl = knn_train_labels, k=5)
CrossTable(x= knn_test_labels, y= knn_test_pred, prop.chisq=FALSE)




# We can measure it’s accuracy as follows:
test_knn[,prediction:=knn_test_pred]
100*test_knn[labels==knn_test_pred,.N] / test_knn[,.N]



                                        # Logistic regression #
                                        # ################### #




                                        # #################### #
                                        # Additional resources #
                                        # #################### #
outlierKD <- function(dt, var) {
     var_name <- eval(substitute(var),eval(dt))
     na1 <- sum(is.na(var_name))
     m1 <- mean(var_name, na.rm = T)
     par(mfrow=c(2, 2), oma=c(0,0,3,0))
     boxplot(var_name, main="With outliers")
     hist(var_name, main="With outliers", xlab=NA, ylab=NA)
     outlier <- boxplot.stats(var_name)$out
     mo <- mean(outlier)
     var_name <- ifelse(var_name %in% outlier, NA, var_name)
     boxplot(var_name, main="Without outliers")
     hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
     title("Outlier Check", outer=TRUE)
     na2 <- sum(is.na(var_name))
     cat("Outliers identified:", na2 - na1, "n")
     cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
     cat("Mean of the outliers:", round(mo, 2), "n")
     m2 <- mean(var_name, na.rm = T)
     cat("Mean without removing outliers:", round(m1, 2), "n")
     cat("Mean if we remove outliers:", round(m2, 2), "n")
     response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
     if(response == "y" | response == "yes"){
          dt[as.character(substitute(var))] <- invisible(var_name)
          assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
          cat("Outliers successfully removed", "n")
          return(invisible(dt))
     } else{
          cat("Nothing changed", "n")
          return(invisible(var_name))
     }
}
