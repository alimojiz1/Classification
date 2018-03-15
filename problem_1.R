
library("purrr")
library("caret")
library("klaR")
library(MASS)
library(e1071)

diab_data<-read.csv("pima-indians-diabetes.data",header=F)
names(diab_data)<-c("Number.of.times.pregnant","Plasma.glucose.concentration", "Diastolic.blood.pressure",
"Triceps.skin.fold.thickness","2-Hour.serum.insulin","Body.mass.index","Diabetes.pedigree.function",
"Age","Class.variable")

head(diab_data)
diab_data$Class.variable<-as.factor(diab_data$Class.variable)



d<-createDataPartition(diab_data$Class.variable,p=0.8,list=FALSE)
train_data<-diab_data[d,]
test_data<-diab_data[-d,]

nrow(train_data)
nrow(test_data)

str(train_data)


#problem 1a
model_nb<-naiveBayes(Class.variable ~ . ,data=train_data)
p<-predict(model_nb, newdata=test_data[,c(1:8)],type = "class")
accuracy<-mean(p==test_data$Class.variable)


#problem 1b
diab_data_copy<-diab_data
diab_data_copy[diab_data_copy$Diastolic.blood.pressure==0,][,3]<-NA
diab_data_copy[diab_data_copy$Triceps.skin.fold.thickness==0,][,4]<-NA
diab_data_copy[diab_data_copy$Body.mass.index==0,][,6]<-NA
diab_data_copy[diab_data_copy$Age==0,][,8]<-NA
d<-createDataPartition(diab_data_copy$Class.variable,p=0.8,list=FALSE)
train_data<-diab_data_copy[d,]
test_data<-diab_data_copy[-d,]
model_nb<-naiveBayes(Class.variable ~ . ,data=train_data)
p<-predict(model_nb, newdata=test_data[,c(1:8)],type="class")
accuracy<-mean(p==test_data$Class.variable)

#problem 1c
diab_data_c<-diab_data
d<-createDataPartition(diab_data_c$Class.variable,p=0.8,list=FALSE)
train_data<-diab_data_c[d,]
test_data<-diab_data_c[-d,]
fit <- train(
  train_data[,c(1:8)], train_data$Class.variable, method = "nb", 
  trControl = trainControl(method = "cv", number = 10))
p<-predict(model_nb, newdata=test_data[,c(1:8)],type="class")
accuracy<-mean(p==test_data$Class.variable)

#problem 1d
svm<-svmlight(train_data[,-9], train_data$Class.variable, pathsvm='C:/Users/DELL/Documents/R/win-library/3.4/svm_light/')
labels<-predict(svm, test_data[,-9])
foo<-labels$class
accuracy<-mean(foo==test_data$Class.variable)
