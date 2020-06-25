

# Correlation Matrix ----

correlationMatrix <- function(djia_core){
  
  gist2 <- djia_core[which(is.na(djia_core$Volume_spx) == FALSE)[1] : nrow(djia_core),]
  gist2 <- gist2 %>% filter(TPcand == 1) %>% select(-c("Date", "ObsNo", "L1P", "L1T", "L2P", 
                                                       "L2T", "L3P", "L3T", "IDObs", "TPcand",
                                                       "Weekday", "Month", "PosInaRow_TPcand",
                                                       "NegInaRow_TPcand", "GainInaRow_TPcand",
                                                       "LossInaRow_TPcand", "FDTR", "USGG10")) 
  
  gist2$CumVolROC_spx_TPcand[is.na(gist$CumVolROC_spx_TPcand)] <- 0
  
  dev.new()
  corrplot(cor(gist2), type = "lower", method = "circle", tl.srt = 45,
           tl.col = "black")

}



# Simple Decision Tree ----

simpleDecisionTree <- function(gist, proportion, seed_split){
  
  # Data Splitting
  set.seed(seed_split)
  trainIndex <- createDataPartition(gist$TPreal, p = proportion, list = FALSE)
  
  # Split with highly correlated predictors
  trainingSDT <- gist[trainIndex, ]
  testingSDT <- gist[-trainIndex, ]
  
  # Subsets validation
  print(prop.table(table(trainingSDT$TPreal)))
  print(prop.table(table(testingSDT$TPreal)))
  

  # Fitting the model
  rPartfit <- rpart(TPreal~., data = trainingSDT, method = 'class')
  
  
  # Extracting Predictions
  predict_unseen_Test <-predict(rPartfit, testingSDT, type = 'class')
  pred  <- prediction(as.numeric(testingSDT$TPreal), as.numeric(predict_unseen_Test))
  roc   <- performance(pred, measure="tpr", x.measure="fpr")
  
  
  # Extracting Prediction (cross-validation)
  predict_unseen_Train <-predict(rPartfit, trainingSDT, type = 'class')
  pred2  <- prediction(as.numeric(trainingSDT$TPreal), as.numeric(predict_unseen_Train))
  roc2   <- performance(pred2, measure="tpr", x.measure="fpr")

  
  # Plotting

  plot(roc, col="orange", lwd=2, print.auc = TRUE, main = "TEST set")
  lines(x=c(0, 1), y=c(0, 1), col="red", lwd=2)
  

  plot(roc2, col="orange", lwd=2, print.auc = TRUE, main = "TRAINING set")
  lines(x=c(0, 1), y=c(0, 1), col="red", lwd=2)
  

  rpart.plot(rPartfit, extra = 106)
  
  # Output
  
  cat(paste("\n"))
  print(confusionMatrix(data = predict_unseen_Test, testingSDT$TPreal))
  
  auc = performance(pred, 'auc')
  gauge_SDT <- slot(auc, 'y.values')
  print(paste("AUC TEST set:", gauge_SDT))
  
  auc2 = performance(pred2, 'auc')
  gauge_SDTcv <- slot(auc2, 'y.values')
  print(paste("AUC TRAIN set:", gauge_SDTcv))
}

# XGB Tree ----

xgbTree <- function(gist, proportion, seed_split, seed_train){
  
  # Data Splitting
  set.seed(seed_split)
  trainIndex  <- createDataPartition(gist$TPreal, p = proportion, list = FALSE)
  
  trainingXGB <- gist[trainIndex, ]
  testingXGB  <- gist[-trainIndex, ]
  
  # Subsets validation
  print(prop.table(table(trainingXGB$TPreal)))
  print(prop.table(table(testingXGB$TPreal)))
  
  # Tuning Grid
  xgbGrid <- expand.grid(nrounds = c(1, 10),
                         max_depth = c(1, 4),
                         eta = c(.1, .4),
                         gamma = 0,
                         colsample_bytree = .7,
                         min_child_weight = 1,
                         subsample = c(.8, 1))
  
  
  # Model Training and Tuning
  ctrlXGB_cv <- trainControl(method = "cv",
                             number = 10,
                             returnResamp = "all",
                             classProbs = TRUE,
                             summaryFunction = twoClassSummary)
  
  ctrlXGB_loocv <- trainControl(method = "LOOCV",
                                classProbs = TRUE, 
                                summaryFunction = twoClassSummary)
  
  ctrlXGB_none <- trainControl(method = "none",
                               classProbs = TRUE,
                               summaryFunction = twoClassSummary)
  
  
  set.seed(seed_train)
  xgbFit_cv <- train(TPreal~.,
                     data = trainingXGB,
                     method = "xgbTree",
                     trControl = ctrlXGB_cv,
                     metric = "ROC",
                     preProc = c("center", "scale"),
                     tuneGrid = xgbGrid)
  
  xgbFit_loocv <- train(TPreal~.,
                        data = trainingXGB,
                        method = "xgbTree",
                        trControl = ctrlXGB_loocv,
                        metric = "ROC",
                        preProc = c("center", "scale"),
                        tuneGrid = xgbGrid)
  
  xgbFit_none <- train(TPreal~.,
                       data = trainingXGB,
                       method = "xgbTree", 
                       trControl = ctrlXGB_none,
                       tuneGrid = xgbGrid[nrow(xgbGrid),],
                       metric = "ROC", 
                       preProc = c("center", "scale"))
  
  
  
  # dev.new()
  # ggplot(xgbFit_cv)
  # dev.new()
  # ggplot(xgbFit_loocv)
  
  
  # Extracting Predictions
  xgbTPreal_cv    <- predict.train(xgbFit_cv, newdata = testingXGB)
  xgbTPreal_loocv <- predict.train(xgbFit_loocv, newdata = testingXGB)
  xgbTPreal_none  <- predict.train(xgbFit_none, newdata = testingXGB)
  
  cat(paste("CV\n"))
  print(confusionMatrix(data = xgbTPreal_cv, testingXGB$TPreal))
  cat(paste("LOOCV\n"))
  print(confusionMatrix(data = xgbTPreal_loocv, testingXGB$TPreal))
  cat(paste("NONE\n"))
  print(confusionMatrix(data = xgbTPreal_none, testingXGB$TPreal))
  
  
  xgbTPreal_cv.probs    <- predict.train(xgbFit_cv, newdata = testingXGB, type = "prob")
  xgbTPreal_loocv.probs <- predict.train(xgbFit_loocv, newdata = testingXGB, type = "prob")
  xgbTPreal_none.probs  <- predict.train(xgbFit_none, newdata = testingXGB, type = "prob")
  
  
  xgbTPreal_cv.ROC    <- roc(xgbTPreal_cv.probs$False,
                             response = testingXGB$TPreal,
                             levels=rev(levels(testingXGB$TPreal)))
  
  xgbTPreal_loocv.ROC <- roc(xgbTPreal_loocv.probs$False,
                             response = testingXGB$TPreal,
                             levels=rev(levels(testingXGB$TPreal)))
  
  xgbTPreal_none.ROC  <- roc(xgbTPreal_none.probs$False,
                             response = testingXGB$TPreal,
                             levels=rev(levels(testingXGB$TPreal)))
  
  
  # Plotting
  (gauge_XGB_cv <- xgbTPreal_cv.ROC$auc)
  #dev.new()
  plot(xgbTPreal_cv.ROC, main = "xgboost ROC method: cv")
  
  (gauge_XGB_loocv <- xgbTPreal_loocv.ROC$auc)
  #dev.new()
  plot(xgbTPreal_loocv.ROC, main = "xgboost ROC method: LOOCV")
  
  (gauge_XGB_none <- xgbTPreal_none.ROC$auc)
  #dev.new()
  plot(xgbTPreal_none.ROC, main = "xgboost ROC method: none")
  
  # ROC scores cross-validation on training set
  xgbTPreal_loocvtrain.probs  <- predict.train(xgbFit_loocv, newdata = trainingXGB, type = "prob")
  xgbTPreal_cvtrain.probs     <- predict.train(xgbFit_cv, newdata = trainingXGB, type = "prob")
  xgbTPreal_nonetrain.probs   <- predict.train(xgbFit_none, newdata = trainingXGB, type = "prob")
  
  
  xgbTPreal_loocvtrain.ROC    <- roc(xgbTPreal_loocvtrain.probs$False,
                                     response = trainingXGB$TPreal,
                                     levels=rev(levels(trainingXGB$TPreal)))
  
  xgbTPreal_cvtrain.ROC       <- roc(xgbTPreal_cvtrain.probs$False,
                                     response = trainingXGB$TPreal,
                                     levels=rev(levels(trainingXGB$TPreal)))
  
  xgbTPreal_nonetrain.ROC     <- roc(xgbTPreal_nonetrain.probs$False,
                                     response = trainingXGB$TPreal,
                                     levels=rev(levels(trainingXGB$TPreal)))
  
  # AUC output
  cat(paste("AUC for loocv  on TEST     subset:", xgbTPreal_loocv.ROC$auc,'\n'))
  cat(paste("AUC for loocv  on TRAINING subset:", xgbTPreal_loocvtrain.ROC$auc,'\n'))
  cat(paste("AUC for cv     on TEST     subset:", xgbTPreal_cv.ROC$auc,'\n'))
  cat(paste("AUC for cv     on TRAINING subset:", xgbTPreal_cvtrain.ROC$auc,'\n'))
  cat(paste("AUC for none   on TEST     subset:", xgbTPreal_none.ROC$auc,'\n'))
  cat(paste("AUC for none   on TRAINING subset:", xgbTPreal_nonetrain.ROC$auc,'\n'))

}

# Partial least squares discriminant analysis (PLS) ----

pls <- function(gist, proportion, seed_split, seed_train){
  
  # Pre-processing:Zero-variance and Near Zero-Variance Predictors evaluation
  print(paste("Near zero variance predictors: ",str(nearZeroVar(gist))))
  
  # Data Splitting
  set.seed(seed_split)
  trainIndex <- createDataPartition(gist$TPreal, p = proportion, list = FALSE)
  
  trainingPLS <- gist[trainIndex, ]
  testingPLS <- gist[-trainIndex, ]
  
  # Subsets validation
  print(prop.table(table(trainingPLS$TPreal)))
  print(prop.table(table(testingPLS$TPreal)))
  
  # Model Training and Tuning
  ctrlPLS <- trainControl(method = "cv",
                          number = 10,
                          returnResamp = "all",
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary)
  
  
  set.seed(seed_train)
  plsFit <- train(TPreal~.,
                  data = trainingPLS, 
                  method = "pls", 
                  preProc = c("center", "scale"),
                  tuneLength = 15, 
                  trControl = ctrlPLS, 
                  metric = "ROC")
  
  
  ggplot(plsFit)
  
  # Extracting Predictions
  plsTPreal <- predict.train(plsFit, 
                             newdata = testingPLS)
  
  plsTPreal.probs <- predict.train(plsFit,
                                   newdata = testingPLS, 
                                   type = "prob")
  
  plsTPreal.ROC <- roc(plsTPreal.probs$False, 
                       response = testingPLS$TPreal,
                       levels = rev(levels(testingPLS$TPreal)))
  
  
  # Extracting Predictions (cross-validation)
  plsTPreal.probs.train <- predict.train(plsFit,
                                         newdata = trainingPLS, 
                                         type = "prob")
  
  plsTPreal.ROC.train <- roc(plsTPreal.probs.train$False, 
                             response = trainingPLS$TPreal,
                             levels = rev(levels(trainingPLS$TPreal)))
  
  
  # Plotting
  plot(plsTPreal.ROC, main = "PLS ROC resampling: cv")
  
  # Confusion Matrix
  print(confusionMatrix(data = plsTPreal, testingPLS$TPreal))
  
  # AUC output
  cat(paste("AUC for TEST     subset:", plsTPreal.ROC$auc,'\n'))
  cat(paste("AUC for TRAINING subset:", plsTPreal.ROC.train$auc,'\n'))
  
}
# Logistic Regression ----

glmModel <- function(gist, proportion, seed_split){
  
  # Data Splitting
  set.seed(seed_split)
  trainIndex <- createDataPartition(gist$TPreal, p = proportion, list = FALSE)
  
  trainingGLM <- gist[trainIndex, ]
  testingGLM <- gist[-trainIndex, ]
  
  # Subsets validation
  print(prop.table(table(trainingGLM$TPreal)))
  print(prop.table(table(testingGLM$TPreal)))
  
  
  # TrainControl Definition
  ctrlGLM1 <- trainControl(method = "cv",
                           number = 10,
                           returnResamp = "all",
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)
  
  ctrlGLM2 <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)
  
  # Training
  glmFit1 <- train(TPreal~.,
                   data = trainingGLM,
                   method = "glm",
                   trControl = ctrlGLM1,
                   metric = "ROC",
                   preProc = c("center", "scale"),
                   family = "binomial")
  
  glmFit2 <- train(TPreal~.,
                   data = trainingGLM,
                   method = "glm",
                   trControl = ctrlGLM2,
                   metric = "ROC",
                   preProc = c("center", "scale"),
                   family = "binomial")
  
  
  # Extracting Predictions
  glmFit1.predict <- predict.train(glmFit1, newdata = testingGLM)
  glmFit2.predict <- predict.train(glmFit2, newdata = testingGLM)
  
  glmFit1.probs <- predict.train(glmFit1, newdata = testingGLM, type = "prob")
  glmFit2.probs <- predict.train(glmFit2, newdata = testingGLM, type = "prob")
  
  glmFit1.roc <- roc(glmFit1.probs$False, response = testingGLM$TPreal,levels=rev(levels(testingGLM$TPreal)))
  glmFit2.roc <- roc(glmFit2.probs$False, response = testingGLM$TPreal,levels=rev(levels(testingGLM$TPreal)))
  
  #Extracting Predictions (cross-validation)
  glmFit1.probs.train <- predict.train(glmFit1, newdata = trainingGLM, type = "prob")
  glmFit2.probs.train <- predict.train(glmFit2, newdata = trainingGLM, type = "prob")
  
  glmFit1.roc.train <- roc(glmFit1.probs.train$False, response = trainingGLM$TPreal,levels=rev(levels(trainingGLM$TPreal)))
  glmFit2.roc.train <- roc(glmFit2.probs.train$False, response = trainingGLM$TPreal,levels=rev(levels(trainingGLM$TPreal)))
  
  
  # Plotting
  gauge_GLM1 <- glmFit1.roc$auc
  plot(glmFit1.roc, main = "GLM ROC resampling: cv")
  
  gauge_GLM2 <- glmFit2.roc$auc
  plot(glmFit2.roc, main = "GLM ROC resampling: repeatedcv")
  
  # Confusion Matrix
  cat(paste("CV\n"))
  print(confusionMatrix(data = glmFit1.predict, testingGLM$TPreal))
  cat(paste("REPEATEDCV\n"))
  print(confusionMatrix(data = glmFit2.predict, testingGLM$TPreal))
  
  # Output
  print(paste("AUC for CV         on TEST subset:", glmFit1.roc$auc))
  print(paste("AUC for CV         on TRAINING subset:", glmFit1.roc.train$auc))
  print(paste("AUC for REPEATEDCV on TEST subset:", glmFit2.roc$auc))
  print(paste("AUC for REPEATEDCV on TRAINING subset:", glmFit2.roc.train$auc))
  
}
# Penalized Regressions ----

PenalizedReg <- function(gist, proportion, seed_split, seed_train){
  
  # Data Splitting
  set.seed(seed_split)
  trainIndex <- createDataPartition(gist$TPreal, p = proportion, list = FALSE)
  
  trainingPEN <- gist[trainIndex, ]
  testingPEN <- gist[-trainIndex, ]
  
  # Subsets validation
  print(prop.table(table(trainingPEN$TPreal)))
  print(prop.table(table(testingPEN$TPreal)))
  
  # Lambda
  lambda <- 10^seq(-3, 3, length = 100)
  
  # TrainControl Definition
  ctrlPEN <- trainControl(method = "cv",
                          number = 10,
                          returnResamp = "all",
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary)
  
  # Training
  set.seed(seed_train)
  enrFit1 <- train(TPreal~.,
                   data = trainingPEN,
                   method = "glmnet",
                   trControl = ctrlPEN,
                   metric = "ROC",
                   preProc = c("center", "scale"),
                   tuneLength = 10)
  
  rdgFit1 <- train(TPreal~.,
                   data = trainingPEN,
                   method = "glmnet",
                   trControl = ctrlPEN,
                   metric = "ROC",
                   preProc = c("center", "scale"),
                   tuneGrid = expand.grid(alpha = 0, lambda = lambda))
  
  lsoFit1 <- train(TPreal~.,
                   data = trainingPEN,
                   method = "glmnet",
                   trControl = ctrlPEN,
                   metric = "ROC",
                   preProc = c("center", "scale"),
                   tuneGrid = expand.grid(alpha = 1, lambda = lambda))
  
  
  
  # ROC vs Regularization Parameters
  ggplot(enrFit1)
  ggplot(rdgFit1)
  ggplot(lsoFit1)
  
  
  # Extracting Predictions
  enrFit1.predict <- predict.train(enrFit1, newdata = testingPEN)
  rdgFit1.predict <- predict.train(rdgFit1, newdata = testingPEN)
  lsoFit1.predict <- predict.train(lsoFit1, newdata = testingPEN)
  
  enrFit1.probs <- predict.train(enrFit1, newdata = testingPEN, type = "prob")
  rdgFit1.probs <- predict.train(rdgFit1, newdata = testingPEN, type = "prob")
  lsoFit1.probs <- predict.train(lsoFit1, newdata = testingPEN, type = "prob")
  
  enrFit1.roc <- roc(enrFit1.probs$False, response = testingPEN$TPreal,levels=rev(levels(testingPEN$TPreal)))
  rdgFit1.roc <- roc(rdgFit1.probs$False, response = testingPEN$TPreal,levels=rev(levels(testingPEN$TPreal)))
  lsoFit1.roc <- roc(lsoFit1.probs$False, response = testingPEN$TPreal,levels=rev(levels(testingPEN$TPreal)))
  
  # Extracting Predictions (cross-validation)
  enrFit1.probs.train <- predict.train(enrFit1, newdata = trainingPEN, type = "prob")
  rdgFit1.probs.train <- predict.train(rdgFit1, newdata = trainingPEN, type = "prob")
  lsoFit1.probs.train <- predict.train(lsoFit1, newdata = trainingPEN, type = "prob")
  
  enrFit1.roc.train <- roc(enrFit1.probs.train$False,response = trainingPEN$TPreal,levels=rev(levels(trainingPEN$TPreal)))
  rdgFit1.roc.train <- roc(rdgFit1.probs.train$False,response = trainingPEN$TPreal,levels=rev(levels(trainingPEN$TPreal)))
  lsoFit1.roc.train <- roc(lsoFit1.probs.train$False,response = trainingPEN$TPreal,levels=rev(levels(trainingPEN$TPreal)))
  
  # Plotting
  gauge_ENR1 <- enrFit1.roc$auc
  plot(enrFit1.roc, main = "Elastic Net Regresion ROC resampling: cv")
  
  gauge_RDG1 <- rdgFit1.roc$auc
  plot(rdgFit1.roc, main = "Ridge Regression ROC resampling: cv")
  
  gauge_LSO1 <- lsoFit1.roc$auc
  plot(lsoFit1.roc, main = "Lasso Regression ROC resampling: cv")
  
  # Confusion Matrix
  cat(paste("Elastic Net\n"))
  print(confusionMatrix(data = enrFit1.predict, testingPEN$TPreal))
  cat(paste("Ridge\n"))
  print(confusionMatrix(data = rdgFit1.predict, testingPEN$TPreal))
  cat(paste("Lasso\n"))
  print(confusionMatrix(data = lsoFit1.predict, testingPEN$TPreal))
  
  #AUC output
  cat(paste("AUC for predictions on TEST subset(ENR):", enrFit1.roc$auc,'\n'))
  cat(paste("AUC for predictions on TRAINING subset(ENR):", enrFit1.roc.train$auc,'\n'))
  
  cat(paste("AUC for predictions on TEST subset(RiDGe):", rdgFit1.roc$auc,'\n'))
  cat(paste("AUC for predictions on TRAINING subset(RiDGe):", rdgFit1.roc.train$auc,'\n'))
  
  cat(paste("AUC for predictions on TEST subset(LaSsO):", lsoFit1.roc$auc,'\n'))
  cat(paste("AUC for predictions on TRAINING subset(LaSsO):", lsoFit1.roc.train$auc,'\n'))
  
}
# Support Vector Machines with Radial Basis Function Kernel----
svmModel <- function(gist, proportion, seed_split, seed_train){
  
  # Data Splitting
  set.seed(seed_split)
  trainIndex <- createDataPartition(gist$TPreal, p = proportion, list = FALSE)
  
  trainingSVM <- gist[trainIndex, ]
  testingSVM <- gist[-trainIndex, ]
  
  print(prop.table(table(trainingSVM$TPreal)))
  print(prop.table(table(testingSVM$TPreal)))
  
  # TrainControl definitions
  # ctrlSVM1 <- trainControl(method = "repeatedcv",
  #                         number = 10,
  #                         repeats = 10,
  #                         classProbs = TRUE,
  #                         summaryFunction = twoClassSummary)
  
  ctrlSVM2 <- trainControl(method = "cv",
                           number = 10,
                           returnResamp = "all",
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)
  
  # ctrlSVM3 <- trainControl(method = "boot",
  #                          number = 10,
  #                          returnResamp = "all",
  #                          classProbs = TRUE,
  #                          summaryFunction = twoClassSummary)
  
  # ctrlSVM4 <- trainControl(method = "LOOCV",
  #                          classProbs = TRUE, 
  #                          summaryFunction = twoClassSummary)
  
  # Training
  set.seed(seed_train)
  # svmFit1 <- train(TPreal~.,
  #                    data = trainingSVM,
  #                    method = "svmRadial",
  #                    trControl = ctrlSVM1,
  #                    metric = "ROC",
  #                    preProc = c("center", "scale"),
  #                    tuneLength = 8)
  
  svmFit2 <- train(TPreal~.,
                   data = trainingSVM,
                   method = "svmRadial",
                   trControl = ctrlSVM2,
                   metric = "ROC",
                   preProc = c("center", "scale"),
                   tuneLength = 8)
  
  # svmFit3 <- train(TPreal~.,
  #                  data = trainingSVM,
  #                  method = "svmRadial",
  #                  trControl = ctrlSVM3,
  #                  metric = "ROC",
  #                  preProc = c("center", "scale"),
  #                  tuneLength = 8)
  # 
  # svmFit4 <- train(TPreal~.,
  #                  data = trainingSVM,
  #                  method = "svmRadial",
  #                  trControl = ctrlSVM4,
  #                  metric = "ROC",
  #                  preProc = c("center", "scale"),
  #                  tuneLength = 8)
  
  
  # Summary
  # svmFit1
  # dev.new()
  # ggplot(svmFit1)
  
  ggplot(svmFit2)
  # svmFit3
  # dev.new()
  # ggplot(svmFit3)
  # svmFit4
  # dev.new()
  # ggplot(svmFit4)
  
  
  # Extracting Predictions
  
  # svmFit1.predict <- predict.train(svmFit1, newdata = testingSVM)
  svmFit2.predict <- predict.train(svmFit2, newdata = testingSVM)
  # svmFit3.predict <- predict.train(svmFit3, newdata = testingSVM)
  # svmFit4.predict <- predict.train(svmFit4, newdata = testingSVM)
  
  # svmFit1.probs <- predict.train(svmFit1, newdata = testingSVM, type = "prob")
  svmFit2.probs <- predict.train(svmFit2, newdata = testingSVM, type = "prob")
  # svmFit3.probs <- predict.train(svmFit3, newdata = testingSVM, type = "prob")
  # svmFit4.probs <- predict.train(svmFit4, newdata = testingSVM, type = "prob")
  
  # svmFit1.roc <- roc(svmFit1.probs$False, response = testingSVM$TPreal,levels=rev(levels(testingSVM$TPreal)))
  svmFit2.roc <- roc(svmFit2.probs$False, response = testingSVM$TPreal,levels=rev(levels(testingSVM$TPreal)))
  # svmFit3.roc <- roc(svmFit3.probs$False, response = testingSVM$TPreal,levels=rev(levels(testingSVM$TPreal)))
  # svmFit4.roc <- roc(svmFit4.probs$False, response = testingSVM$TPreal,levels=rev(levels(testingSVM$TPreal)))
  
  # Extracting Predictions (cross-validation)
  
  svmFit2.probs.train <- predict.train(svmFit2, newdata = trainingSVM, type = "prob")
  svmFit2.roc.train   <- roc(svmFit2.probs.train$False, response = trainingSVM$TPreal,levels=rev(levels(trainingSVM$TPreal)))
  
  
  # Plotting
  
  # plot(svmFit1.roc, main = "SVM ROC resampling: repeatedcv")
  plot(svmFit2.roc, main = "SVM ROC resampling: cv")
  # plot(svmFit3.roc, main = "SVM ROC resampling: boot")
  # plot(svmFit4.roc, main = "SVM ROC resampling: LOOCV")
  
  
  # Confusion Matrix
  # confusionMatrix(data = svmFit1.predict, testingSVM$TPreal)
  cat(paste("CV\n"))
  print(confusionMatrix(data = svmFit2.predict, testingSVM$TPreal))
  # confusionMatrix(data = svmFit3.predict, testingSVM$TPreal)
  # confusionMatrix(data = svmFit4.predict, testingSVM$TPreal)
  
  # AUC output
  print(paste("ROC for predicitons on TEST subset:", svmFit2.roc$auc))
  print(paste("ROC for predicitons on TRAINING subset:", svmFit2.roc.train$auc))
  
}