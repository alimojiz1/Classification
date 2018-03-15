# modification of https://gist.github.com/brendano/39760
# automatically obtains data from the web
# creates two data frames, test and train
# labels are stored in the y variables of each data frame
# can easily train many models using formula `y ~ .` syntax

# download data from http://yann.lecun.com/exdb/mnist/
download.file("http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz",
              "train-images-idx3-ubyte.gz")
download.file("http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz",
              "train-labels-idx1-ubyte.gz")
download.file("http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz",
              "t10k-images-idx3-ubyte.gz")
download.file("http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz",
              "t10k-labels-idx1-ubyte.gz")

# load image files

#Please note that I Took the following code to convert the image to CSV from Ryan posted on Piazza. 
load_image_file = function(filename) {
  ret = list()
  f = file(filename, 'rb')
  readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  n    = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  nrow = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  ncol = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  x = readBin(f, 'integer', n = n * nrow * ncol, size = 1, signed = FALSE)
  close(f)
  data.frame(matrix(x, ncol = nrow * ncol, byrow = TRUE))
}

# load label files
load_label_file = function(filename) {
  f = file(filename, 'rb')
  readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  n = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  y = readBin(f, 'integer', n = n, size = 1, signed = FALSE)
  close(f)
  y
}

# load images
train = load_image_file("train-images.idx3-ubyte")
test  = load_image_file("t10k-images.idx3-ubyte")

# load labels
train$y = as.factor(load_label_file("train-labels.idx1-ubyte"))
test$y  = as.factor(load_label_file("t10k-labels.idx1-ubyte"))

# create pixel header
pixel_header = function(x)
{
  out = array()
  for (ix in 1:x)
  {
    out[ix] = sprintf("pixel%i", ix-1)
  }
  out[ix+1] = "class"
  return (out)
}

ph = pixel_header(784) #adds "class" at [785]
names(train) = ph      #sets header of training set data frame
names(test) = ph       #sets header of test set data frame

write.csv(train, file="mnist_train.csv", row.names=FALSE)
write.csv(test, file="mnist_test.csv", row.names=FALSE)

train = read.csv("mnist_train.csv", header=TRUE)
test = read.csv("mnist_test.csv", header=TRUE)
train$class<-as.factor(train$class)
test$class<-as.factor(test$class)

#partA
#changing from 28x28 to 20x20
mt_train<-as.matrix(train)
mt_test<-as.matrix(test)
img_train_resize<-resize(autocrop(as.cimg(t(mt_train))), size_x = 20, size_y = 20) #taken from Michael Chan posted on piazza
img_test_resize<-resize(autocrop(as.cimg(t(mt_test))), size_x = 20, size_y = 20) #taken from Michael Chan posted on piazza
data_resized_train<-as.matrix(img_train_resize)
data_resized_test<-as.matrix(img_test_resize)

library(e1071)
#gaussian
model_nb_normal<-naiveBayes(class~.,data=train)
p<-predict(model_nb_normal, newdata=test[,-785],type="class")
accuracy<-mean(p==test$class)

#bernoulli
library(quanteda)

train_dfm<-as.dfm(train[,-785])
test_dfm<-as.dfm(test[,-785])
model_nb_b<-textmodel_nb(train_dfm,train$class,distribution = c("Bernoulli"))
pr<-predict(model_nb_b,newdata=test_dfm)
accuracy<-mean(pr$nb.predicted==test$class)


#partB
#untouched raw pixels
library(randomForest)
#depth=4 and #treees=10
model_rf_1<-randomForest(class~.,data=train,maxnodes=4, ntree=10)
pr<-predict(model_rf_1,newdata=test[,-785])
accuracy<-mean(pr==test$class)

#depth=4 and #treees=20
model_rf_2<-randomForest(class~.,data=train,maxnodes=4, ntree=20)
pr<-predict(model_rf_2,newdata=test[,-785])
accuracy<-mean(pr==test$class)

#depth=4 and #treees=30
model_rf_3<-randomForest(class~.,data=train,maxnodes=4, ntree=30)
pr<-predict(model_rf_3,newdata=test[,-785])
accuracy<-mean(pr==test$class)


#depth=8 and #treees=10
model_rf_4<-randomForest(class~.,data=train,maxnodes=8, ntree=10)
pr<-predict(model_rf_4,newdata=test[,-785])
accuracy<-mean(pr==test$class)


#depth=8 and #treees=20
model_rf_5<-randomForest(class~.,data=train,maxnodes=8, ntree=20)
pr<-predict(model_rf_5,newdata=test[,-785])
accuracy<-mean(pr==test$class)

#depth=8 and #treees=30
model_rf_6<-randomForest(class~.,data=train,maxnodes=8, ntree=30)
pr<-predict(model_rf_6,newdata=test[,-785])
accuracy<-mean(pr==test$class)

#depth=16 and #treees=10
model_rf_7<-randomForest(class~.,data=train,maxnodes=16, ntree=10)
pr<-predict(model_rf_7,newdata=test[,-785])
accuracy<-mean(pr==test$class)

#depth=16 and #treees=20
model_rf_8<-randomForest(class~.,data=train,maxnodes=16, ntree=20)
pr<-predict(model_rf_8,newdata=test[,-785])
accuracy<-mean(pr==test$class)

#depth=16 and #treees=30
model_rf_9<-randomForest(class~.,data=train,maxnodes=16, ntree=30)
pr<-predict(model_rf_9,newdata=test[,-785])
accuracy<-mean(pr==test$class)
