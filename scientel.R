setwd("D:/Scientel")
library(caret)
library(dplyr)
library(pROC)
library(ROSE)
library(randomForest)
library(mlbench)
library(caret)
library(caretEnsemble)


tr_raw = read.csv("diabetes_dataset_insample.csv")
str(tr_raw)

tr_raw$compl. = as.factor(tr_raw$compl.)


fun=function(x,con){
  if(con == "fac"){
    as.factor(x)}else{
      as.numeric(x)
    }
  }
tr_raw[,8:24] = data.frame(lapply(tr_raw[,8:24], fun,"fac"))

tr_raw[,30:38] = data.frame(lapply(tr_raw[,30:38], fun,"num"))
tr_raw$num_pre_dx_grps = as.numeric(tr_raw$num_pre_dx_grps)


summary(tr_raw)
tr_raw$ID = NULL
tr_raw$state = NULL



validation_index <- createDataPartition(tr_raw$compl., p=0.80, list=FALSE)
val <- tr_raw[-validation_index,]
tr <- tr_raw[validation_index,]

str(tr)

prop.table(table(tr$compl.))



tr_rose <- ROSE(compl. ~ ., data =tr, seed = 1)$data
prop.table(table(tr_rose$compl.))


up = upSample(x = tr, y=tr$compl., yname = "1")
str(up)
table(up$compl.)

rf = randomForest(compl. ~., tr_rose,classwt = c(0.5,2.5))
pred_val = predict(rf,val)
table(val$compl., pred_val)



  roc(as.numeric(val$compl.), as.numeric(pred_val)) %>%
  auc()



model_weights <- ifelse(tr$compl. == "1",
                        (1/table(tr$compl.)[1]) * 0.5,
                        (1/table(tr$compl.)[2]) * 0.5)

# Use the same seed to ensure same cross-validation splits

levels(tr_rose$compl.) = c("Z","O")
weighted_fit <- train(compl. ~ .,
                      data = tr_rose,
                      method = "gbm",
                      verbose = T,
                      metric = "ROC",
                      trControl = ctrl)
pred_val = predict(weighted_fit,val)

levels()
table(val$compl., pred_val)

# create submodels
control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
algorithmList <- c('lda', 'rpart', 'glm', 'knn', 'svmRadial')
set.seed(seed)
models <- caretList(compl. ~., data=tr_rose, trControl=control, methodList=algorithmList)
results <- resamples(models)
summary(results)
dotplot(results)



#http://dpmartin42.github.io/blogposts/r/imbalanced-classes-part-1


