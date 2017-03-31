setwd('/home/chao/Documents/Project/Data/Compounds Data/')
library('mxnet')
#import data and fix name
train.x<-read.csv('training.csv',row.names = 1,header = T)
train.y<-read.table('training.txt',row.names = 2,header = T)
train.x<-data.matrix(train.x)
train.y<-data.matrix(train.y)
test.x<-read.csv('test.csv',row.names = 1,header = T)
test.y<-read.table('test.txt',row.names = 2,header = T)
test.x<-data.matrix(test.x)
test.y<-data.matrix(test.y)
#combine data
train.x<-train.x[order(rownames(train.x)),]#re-order train.x so that let it corresbonding to train,y 
train<-cbind(train.x,train.y[,2])
test<-cbind(test.x[1:23,],test.y[,2])
data<-rbind(train,test)
#normalization
maxs<-apply(data,2,max)
mins<-apply(data,2,min)
data<-as.data.frame(scale(data,center = mins,scale = maxs-mins))
# Define the input data
data <- mx.symbol.Variable("data")
# A fully connected hidden layer
# data: input source
# num_hidden: number of neurons in this hidden layer
fc1 <- mx.symbol.FullyConnected(data, num_hidden=1)
# Use linear regression for the output layer
lro <- mx.symbol.LinearRegressionOutput(fc1)
mx.set.seed(0)
model <- mx.model.FeedForward.create(lro, X=train.x, y=train.y[,2],
                                     ctx=mx.cpu(),     num.round=50, array.batch.size=20,
                                     learning.rate=2e-6, momentum=0.9, array.layout = "colmajor", eval.metric=mx.metric.rmse)
