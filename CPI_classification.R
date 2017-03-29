setwd("/home/chao/Documents/Project/Data/Compounds Data/")
set.seed(2016)
library(h2o)
library(e1071)
library(ROCR)
library(pROC)
h2o.init()
h2o.removeAll()
#read data
y.trian<- read.table("training.txt", header = T, row.names = 2)
y.test<- read.table("test.txt", header = T, row.names = 2)
train<-read.csv("training.csv", header = T, row.names = 1)
test<-read.csv("test.csv", header = T, row.names = 1)
#combine data
dataset.r<-rbind(test[1:23,],train)
dataset.r<- dataset.r[order(row.names(dataset.r)),]
y<-rbind(y.test[1:23,],y.trian)
y<- y[order(row.names(y)),]
#fix name 
rownames(dataset.r) <- sub("./", " ", rownames(dataset.r))
rownames(dataset.r) <- sub(".hin", " ", rownames(dataset.r))
#combine x and y 
dataset.r<-cbind(y$Exp,dataset.r)
dataset<-dataset.r
colnames(dataset)[1]<-"label"
#transform the target valure into binary if <0 let it be 0, if > than 0 let it be 1.
for(i in 1:nrow(dataset)){
  if(dataset[i,1]<0)
  {
    dataset[i,1]<-0
  }
}
for(i in 1:nrow(dataset)){
  if(dataset[i,1]>0)
  {
    dataset[i,1]<-1
  }
}
dataset<-cbind(dataset.r$`y$Exp`,dataset)
colnames(dataset)[1]<-"score"
# dataset[,2]<-as.factor(dataset[,2])

#split into train and test
train<-dataset[1:86,]
test<-dataset[87:109,]
  #validation<-dataset[100:109,1:1875]
#transform into h2o file
train.hex<-as.h2o(train)
test.hex<-as.h2o(test)
  #validation.hex<-as.h2o(validation)
#build model
model.dl <- h2o.deeplearning(x = 2:1876, y = 1,
                                  training_frame = train.hex,
                                  validation_frame=test.hex,
                                  hidden = (c(200,200,200)))
summary(drug.dl.train)

model.svm<-svm(subset(train,select = 3:1877),subset(train,select = 2),na.action = na.omit,scale = TRUE)

#plugin model
result.dl<-h2o.predict(model.dl,test.hex)
result.dl
result.svm<-predict(model.svm,test[,3:1877])
result.svm
#plot roc of dl
result.dl<-as.data.frame(result.dl) 
result.dl$predict<-as.numeric(result.dl$predict)
roc<-roc(test$label,result.dl$predict)#use true label as reponse and predicted real value as predictor
plot(roc)
#plot roc of svm
result.svm<-as.data.frame(result.svm)
test<-test[intersect(rownames(test),rownames(result.svm)),]#i don;t know why lose 5 data after plugin model but i remove those 5 data from test too so that can plot roc
result.svm$result.svm<-as.numeric(result.svm$result.svm)
roc<-roc(test$label,result.svm$result.svm)
plot(roc)
