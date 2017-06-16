setwd("D:/Scientel")
library(caret)
library(dplyr)
library(pROC)


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
library(ROSE)
library(randomForest)

rf = randomForest(compl. ~., tr_rose)


pred_val = predict(rf,val)
table(val$compl., pred_val)


test_roc <- function(model, data) {
  
  roc(data$Class,
      predict(model, data, type = "prob")[, "Class2"])
  
}

rf %>%
  roc(as.numeric(val$compl.), as.numeric(pred_val)) %>%
  auc()



model_weights <- ifelse(tr$compl. == "1",
                        (1/table(tr$compl.)[1]) * 0.5,
                        (1/table(tr$compl.)[2]) * 0.5)

# Use the same seed to ensure same cross-validation splits

ctrl$seeds <- orig_fit$control$seeds

# Build weighted model
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)


weighted_fit <- train(compl. ~ .,
                      data = tr,
                      method = "gbm",
                      verbose = FALSE,
                      weights = model_weights,
                      metric = "ROC",
                      trControl = ctrl)

# Build down-sampled model

ctrl$sampling <- "down"

down_fit <- train(Class ~ .,
                  data = imbal_train,
                  method = "gbm",
                  verbose = FALSE,
                  metric = "ROC",
                  trControl = ctrl)

# Build up-sampled model

ctrl$sampling <- "up"

up_fit <- train(Class ~ .,
                data = imbal_train,
                method = "gbm",
                verbose = FALSE,
                metric = "ROC",
                trControl = ctrl)

# Build smote model

ctrl$sampling <- "smote"

smote_fit <- train(Class ~ .,
                   data = imbal_train,
                   method = "gbm",
                   verbose = FALSE,
                   metric = "ROC",
                   trControl = ctrl)

#http://dpmartin42.github.io/blogposts/r/imbalanced-classes-part-1


