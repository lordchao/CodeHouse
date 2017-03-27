setwd("/home/chao/Documents/Project/Data/Compounds Data/")
set.seed(2016)
library(h2o)
h2o.init()
h2o.removeAll()
#read data
y.trian<- read.table("training.txt", header = T, row.names = 2)
y.test<- read.table("test.txt", header = T, row.names = 2)
training<-read.csv("training.csv", header = T, row.names = 1)
test<-testset<-read.csv("test.csv", header = T, row.names = 1)
#combine data
dataset<-rbind(testset[1:23,],trainingset)
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
#plot function
plotC <- function(name, model, data=train, g=grid) {
  data <- as.data.frame(data) #get data from into R
  pred <- as.data.frame(h2o.predict(model, g))
  n=0.5*(sqrt(nrow(g))-1); d <- 1.5; h <- d*(-n:n)/n
  plot(data[,-3],pch=19,col=data[,3],cex=0.5,
       xlim=c(-d,d),ylim=c(-d,d),main=name)
  contour(h,h,z=array(ifelse(pred[,1]=="Red",0,1),
                      dim=c(2*n+1,2*n+1)),col="blue",lwd=2,add=T)
}
par(mfrow=c(2,2))
plotC("GLM", h2o.glm(2:1876,1,training_frame = train,family="binomial"))
plotC( "DL", h2o.deeplearning(2:1876,1,train,epochs=1e3))
#build model
deep <- h2o.deeplearning(
  model_id="dl_model_first", 
  training_frame=train, 
  validation_frame=test,   ## validation dataset: used for scoring and early stopping
  x=2:1876,
  y=1,
  #activation="Rectifier",  ## default
  #hidden=c(200,200),       ## default: 2 hidden layers with 200 neurons each
  epochs=1,
  variable_importances=T    ## not enabled by default
) 
summary(m1)
