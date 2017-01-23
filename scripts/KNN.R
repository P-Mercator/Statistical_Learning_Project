load("img_vec_lab.Rdata")


## Normalizing numeric data:
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

knn_n <- as.data.frame(lapply(dt[,1:100], normalize))
summary(knn_n)
knn_n=as.data.table(knn_n)
cols=names(knn_n)
knn_n=knn_n[,(cols):=lapply(.SD, as.double),.SDcols=cols]

dt_knn_n <- (cbind(labels,knn_n))



## Creating training and test data set:

sub <- sample(nrow(dt_knn_n), floor(nrow(dt_knn_n) * 0.9))
train_knn <- dt_knn_n[sub, ]
test_knn <- dt_knn_n[-sub, ]


dim(train_knn)
dim(test_knn)

knn_train_labels <- train_knn[, labels]
knn_test_labels <- test_knn[, labels]

length(knn_train_labels)

#str(train_knn)

## Training a model on data:

# install.packages("class")

library(class)

features=cols[grepl("V",cols)]

knn_test_pred <- knn(train = train_knn[,features,with=F], test = test_knn[,features,with=F], cl = knn_train_labels, k=5)

test_knn[,prediction:=knn_test_pred]

#accuracy
100*test_knn[labels==knn_test_pred,.N] / test_knn[,.N]

## sum(is.na.data.frame(dt_knn_n))



#install.packages("gmodels")
library(gmodels)

CrossTable(knn_train_labels, knn_test_pred, prop.chisq=FALSE)


### For different K.

accuracy <- rep(0, 10)
k <- 1:10
for(x in k){
  prediction <- knn(train = train_knn, test = test_knn, cl = knn_train_labels, k=x)
  accuracy[x] <- mean(prediction == knn_test_labels)
}
plot(k, accuracy, type = 'b')


