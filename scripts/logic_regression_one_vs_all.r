load("scripts/img_vec_lab.Rdata")

##################
## PCA (from Sergio's code)

#run pca
im_pca=prcomp(dt,retx=T,center=T,scale=T,rank.=50)
summary(im_pca)

#pick the 25 first components
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
  im_pca$x[,25]))

foo[,label:=labels]

#representation of the 2 first components
ggplot(data=foo[sample(1:nrow(foo),1000)],aes(x=V1,y=V2))+geom_point(aes(colour=as.factor(label)))

foo[,labels:=as.factor(label)]


################
# Prepare for 1 vs All

foo$A = 0
foo$A[foo$label == "A"] <- 1
foo$B = 0
foo$B[foo$label == "B"] <- 1
foo$C = 0
foo$C[foo$label == "C"] <- 1
foo$Point = 0
foo$Point[foo$label == "Point"] <- 1
foo$V = 0
foo$V[foo$label == "V"] <- 1

#train-test indeces
train_idx=sample(1:nrow(foo),3374)
test_idx=(1:nrow(foo))[!(1:nrow(foo))%in%train_idx]


###################################################


train_class <- function(y, Y) { # y is the name of the columns, Y is the label in test
  
  f <- substitute(glm(y~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10
                         + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20
                         + V21 + V22 + V23 + V24 + V25, 
                      data=foo[train_idx], 
                      family=binomial))

  model = eval(f) # in order to be able to pass values to glm inside a function
  
  fitted.results <- predict(model, 
                            newdata=subset(foo[test_idx]),
                            type='response')
  
  fitted.results <- ifelse(fitted.results > 0.45,1,0)
  
  misClasificError <- mean(fitted.results != Y)
  
  print(paste('Accuracy',1-misClasificError))
  
  return (fitted.results) # returns the prediction for y vs all
}


results.A = train_class(A, foo[test_idx]$A)
results.B = train_class(B, foo[test_idx]$B)
results.C = train_class(C, foo[test_idx]$C)
results.Point = train_class(Point, foo[test_idx]$Point)
results.V = train_class(V, foo[test_idx]$V)

print(table(foo$label))


results.label = 0
results.label[results.Point == 1] = "Point"
results.label[results.A == 1] = "A"
results.label[results.B == 1] = "B"
results.label[results.C == 1] = "C"
results.label[results.V == 1] = "V"


print(table(results.label, exclude = NULL))

# Right now there are a lot of NA (instances that have not been classified)
# I can substitute them by the most common label... but...
# Point is also the one with less accuracy when performing individual classification 1 vs All

results.label[is.na(results.label)] = "Point"

misClasificError <- mean(results.label != foo[test_idx]$label)

print(paste('Accuracy',1-misClasificError))


