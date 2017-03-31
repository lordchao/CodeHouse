setwd('/home/chao/Documents/Project/Data/')
require(mxnet)
#read tabel
train <- read.csv('train.csv', header=TRUE)
test<-read.csv('test.csv',header = T)
#transfer datafram into matrix
train <- data.matrix(train)
test <- data.matrix(test)
#split matrix
train.x <- train[,-1]
train.y <- train[,1]
#transform the grayscale into [0,1]
train.x <- t(train.x/255)
test <- t(test/255)

table(train.y)
#configuring the network
data <- mx.symbol.Variable("data")
fc1 <- mx.symbol.FullyConnected(data, name="fc1", num_hidden=128)
act1 <- mx.symbol.Activation(fc1, name="relu1", act_type="relu")
fc2 <- mx.symbol.FullyConnected(act1, name="fc2", num_hidden=64)
act2 <- mx.symbol.Activation(fc2, name="relu2", act_type="relu")
fc3 <- mx.symbol.FullyConnected(act2, name="fc3", num_hidden=10)
softmax <- mx.symbol.SoftmaxOutput(fc3, name="sm")

devices <- mx.cpu()
#build model
mx.set.seed(0)
model <- mx.model.FeedForward.create(softmax, X=train.x, y=train.y,
                                     ctx=devices, num.round=10, array.batch.size=100,
                                     learning.rate=0.07, momentum=0.9,  eval.metric=mx.metric.accuracy,
                                     initializer=mx.init.uniform(0.07),
                                    epoch.end.callback=mx.callback.log.train.metric(100))
#predict
preds <- predict(model, test)
dim(preds)

pred.label <- max.col(t(preds)) - 1
table(pred.label)
