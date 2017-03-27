setwd("/home/chao/Documents/Project/Data/Compounds Data/")
set.seed(2016)
library(h2o)
h2o.init()
h2o.removeAll()
#read data
y.trian<- read.table("training.txt", header = T, row.names = 2)
y.test<- read.table("test.txt", header = T, row.names = 2)
train<-read.csv("training.csv", header = T, row.names = 1)
test<-read.csv("test.csv", header = T, row.names = 1)
#combine data
dataset<-rbind(test[1:23,],train)
dataset<- dataset[order(row.names(dataset)),]
y<-rbind(y.test[1:23,],y.trian)
y<- y[order(row.names(y)),]
#fix name 
rownames(dataset) <- sub("./", " ", rownames(dataset))
rownames(dataset) <- sub(".hin", " ", rownames(dataset))
#combine x and y 
dataset<-cbind(y$Exp,dataset)
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
#split into train and test
train<-dataset[1:86,]
test<-dataset[87:109,]
#transform into h2o file
train<-as.h2o(train)
test<-as.h2o(test)

#build model
drug.dl.train <- h2o.deeplearning(x = 2:1876, y = 1,
                                  training_frame = train,
                                  validation_frame=test,
                                  hidden = (c(200,200,200)))
summary(drug.dl.train)
