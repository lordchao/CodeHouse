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
dataset[,2]<-as.factor(dataset[,2]) #if you want to use h2o.deeplearning to do classification, then you need to transfer the label into factor
#split into train and test
train<-dataset[1:86,]
validation<-dataset[87:99,]
test<-dataset[100:109,1:1875]
remove(dataset.r)
response<-"label"
predictors<-setdiff(setdiff(names(train),y),response)
predictors

#transform into h2o file
train.hex<-as.h2o(train)
test.hex<-as.h2o(test)
validation.hex<-as.h2o(validation)

#build model
#deep learning with three hidden layers for classification
m1 <- h2o.deeplearning(x = predictors, y = response,
                             model_id="dl_three_layers_200_units",
                             training_frame = train.hex,
                             validation_frame=validation.hex,
                             distribution = "bernoulli",
                             hidden = (c(200,200,200)))
summary(m1)

m2 <- h2o.deeplearning(
  model_id="dl_model_faster", 
  training_frame=train.hex, 
  validation_frame=validation.hex,
  x=predictors,
  y=response,
  hidden=c(32,32,32,32),                  ## small network, runs faster
  epochs=1000000,                      ## hopefully converges earlier...
  score_validation_samples=10000,      ## sample the validation dataset (faster)
  stopping_rounds=2,
  stopping_metric="misclassification", ## could be "MSE","logloss","r2"
  stopping_tolerance=0.01
)

summary(m2)
plot(m2)

m3 <- h2o.deeplearning(
  model_id="dl_model_tuned", 
  training_frame=train.hex, 
  validation_frame=validation.hex, 
  x=predictors, 
  y=response, 
  overwrite_with_best_model=F,    ## Return the final model after 10 epochs, even if not the best
  hidden=c(128,128,128),          ## more hidden layers -> more complex interactions
  epochs=10,                      ## to keep it short enough
  score_validation_samples=10000, ## downsample validation set for faster scoring
  score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
  adaptive_rate=F,                ## manually tuned learning rate
  rate=0.01, 
  rate_annealing=2e-6,            
  momentum_start=0.2,             ## manually tuned momentum
  momentum_stable=0.4, 
  momentum_ramp=1e7, 
  l1=1e-5,                        ## add some L1/L2 regularization
  l2=1e-5,
  max_w2=10                       ## helps stability for Rectifier
) 
summary(m3)
model.svm<-svm(subset(train,select = 3:1877),subset(train,select = 1),
               na.action = na.omit,
               nfolds= 86,
               momentum_start=0.5,
               cost = 0.5,
               coef0 = 1,
               scale = TRUE,
               epsilon = 0.01)
#
#Let's compare the training error with the validation and test set errors
#
h2o.performance(m3, train=T)          ## sampled training data (from model building)
h2o.performance(m3, valid=T)          ## sampled validation data (from model building)
h2o.performance(m3, newdata=train.hex)    ## full training data
h2o.performance(m3, newdata=validation.hex)    ## full validation data
h2o.performance(m3, newdata=test.hex)     ## full test data
#
#### Hyper-parameter Tuning with Grid Search
#Since there are a lot of parameters that can impact model accuracy, hyper-parameter tuning is especially important for Deep Learning:
#
#The simplest hyperparameter search method is a brute-force scan of the full Cartesian product of all combinations specified by a grid search:
#
hyper_params <- list(
  hidden=list(c(32,32,32),c(64,64)),
  input_dropout_ratio=c(0,0.05),
  rate=c(0.01,0.02),
  rate_annealing=c(1e-8,1e-7,1e-6)
)
hyper_params
grid <- h2o.grid(
  algorithm="deeplearning",
  grid_id="dl_grid", 
  training_frame=train.hex,
  validation_frame=validation.hex, 
  x=predictors, 
  y=response,
  epochs=10,
  stopping_metric="misclassification",
  stopping_tolerance=1e-2,        ## stop when misclassification does not improve by >=1% for 2 scoring events
  stopping_rounds=2,
  score_validation_samples=10000, ## downsample validation set for faster scoring
  score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
  adaptive_rate=F,                ## manually tuned learning rate
  momentum_start=0.5,             ## manually tuned momentum
  momentum_stable=0.9, 
  momentum_ramp=1e7, 
  l1=1e-5,
  l2=1e-5,
  activation=c("Rectifier"),
  max_w2=10,                      ## can help improve stability for Rectifier
  hyper_params=hyper_params
)
grid
#                                
#Let's see which model had the lowest validation error:
#
grid <- h2o.getGrid("dl_grid",sort_by="err",decreasing=FALSE)
grid
## Find the best model and its full set of parameters
grid@summary_table[1,]
best_model <- h2o.getModel(grid@model_ids[[1]])
best_model

print(best_model@allparameters)
print(h2o.performance(best_model, valid=T))
print(h2o.logloss(best_model, valid=T))
#  
#    
####Checkpointing
#Let's continue training the manually tuned model from before, for 2 more epochs. Note that since many important parameters such as `epochs, l1, l2, max_w2, score_interval, train_samples_per_iteration, input_dropout_ratio, hidden_dropout_ratios, score_duty_cycle, classification_stop, regression_stop, variable_importances, force_load_balance` can be modified between checkpoint restarts, it is best to specify as many parameters as possible explicitly.
#
max_epochs <- 12 ## Add two more epochs
m_cont <- h2o.deeplearning(
  model_id="dl_model_tuned_continued", 
  checkpoint="dl_model_tuned", 
  training_frame=train.hex, 
  validation_frame=validation.hex, 
  x=predictors, 
  y=response, 
  hidden=c(128,128,128),          ## more hidden layers -> more complex interactions
  epochs=max_epochs,              ## hopefully long enough to converge (otherwise restart again)
  stopping_metric="logloss",      ## logloss is directly optimized by Deep Learning
  stopping_tolerance=1e-2,        ## stop when validation logloss does not improve by >=1% for 2 scoring events
  stopping_rounds=2,
  score_validation_samples=10000, ## downsample validation set for faster scoring
  score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
  adaptive_rate=F,                ## manually tuned learning rate
  rate=0.01, 
  rate_annealing=2e-6,            
  momentum_start=0.2,             ## manually tuned momentum
  momentum_stable=0.4, 
  momentum_ramp=1e7, 
  l1=1e-5,                        ## add some L1/L2 regularization
  l2=1e-5,
  max_w2=10                       ## helps stability for Rectifier
) 
summary(m_cont)
plot(m_cont)
####Cross-Validation
#For N-fold cross-validation, specify `nfolds>1` instead of (or in addition to) a validation frame, and `N+1` models will be built: 1 model on the full training data, and N models with each 1/N-th of the data held out (there are different holdout strategies). Those N models then score on the held out data, and their combined predictions on the full training data are scored to get the cross-validation metrics.
#    
dlmodel <- h2o.deeplearning(
  x=predictors,
  y=response, 
  training_frame=train.hex,
  hidden=c(10,10),
  epochs=1,
  nfolds=5,
  fold_assignment="Modulo" # can be "AUTO", "Modulo", "Random" or "Stratified"
)
dlmodel
#
#N-fold cross-validation is especially useful with early stopping, as the main model will pick the ideal number of epochs from the convergence behavior of the cross-validation models.
#

#build basic binomial glm model
m4 <- h2o.glm(x = predictors,
                    y = response,
                    training_frame = train.hex,
                    validation_frame = validation.hex,
                    model_id = "glm",
                    family = "binomial",
                    nfold=10,
                    lambda_search = TRUE) 
summary(m4)
h2o.performance(model = m4,
                newdata = test.hex)
h2o.auc(m4,train = TRUE)
# Random Forest
m5 <- h2o.randomForest(x = predictors,
                            y = response,
                            training_frame = train.hex,
                            model_id = "rf",
                            validation_frame = validation.hex,  #only used if stopping_rounds > 0
                            ntrees = 100,
                            seed = 1,
                            nfolds = 10)
summary(m5)
#naive bayes model
m6 <- h2o.naiveBayes(x = predictors,
                          y = response,
                          training_frame = train.hex,
                          model_id = "nb",
                          laplace = 6)
summary(m6)
