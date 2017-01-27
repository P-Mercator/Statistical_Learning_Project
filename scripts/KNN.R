load("img_vec_lab.Rdata")

## Normalizing numeric data:
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

knn_n <- as.data.frame(lapply(dt[,1:100], normalize))
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
features=cols[grepl("V",cols)]
accuracy <- rep(0, 20)
k <- 1:20
for(x in k){
  prediction <- knn(train = train_knn[,features,with=F], test = test_knn[,features,with=F], cl = knn_train_labels, k=x)
  accuracy[x] <- mean(prediction == knn_test_labels)
}
windows();
plot(k, accuracy, pch = 16, col = 'royalblue2', cex = 1.5, main= 'Accuarcy Vs. K', type = 'b'); box(lwd = 2);



## We can see it’s accuracy using table.
library(gmodels)
knn_test_pred <- knn(train = train_knn[,features,with=F], test = test_knn[,features,with=F], cl = knn_train_labels, k=5)
CrossTable(x= knn_test_labels, y= knn_test_pred, prop.chisq=FALSE)




# We can measure it’s accuracy as follows:
test_knn[,prediction:=knn_test_pred]
100*test_knn[labels==knn_test_pred,.N] / test_knn[,.N]
