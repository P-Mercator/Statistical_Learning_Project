rm(list=ls())

load("./Data/img_vec_lab.Rdata")

rs_df <- cbind(labels,dt)

rows <- length(rs_df[,1])
shuffled <- rs_df[sample(1:rows),]

train <- shuffled[1:3800,]
test <- shuffled[3801:rows,]

install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")
install.packages("mxnet")
require(devtools)
install_version("DiagrammeR", version = "0.8.1", repos = "http://cran.us.r-project.org")
install.packages("mxnet", type = "mac.binary.mavericks")
require(mxnet)

# Set up train and test datasets
train <- data.matrix(train)
train_x <- t(train[, -1])
train_y <- train[, 1]
train_array <- train_x
dim(train_array) <- c(28, 28, 1, ncol(train_x))

test_x <- t(test[, -1])
test_y <- test[, 1]
test_array <- test_x
dim(test_array) <- c(28, 28, 1, ncol(test_x))
