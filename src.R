# 1. Libraries --------------------------------------------------------------
#install.packages("pacman")

pacman::p_load(pacman,tidyverse,stringr, forcats,readxl,TTR,quantmod,corrplot,
               rpart,rpart.plot,lubridate,caret,e1071,xgboost,pROC,ROCR, grDevices)

source("mung.R")

# 2. Reading data ------------------------------------------

djia_core <- data.frame(read_excel("data/DJIA_PT.xlsx"))
spx500    <- data.frame(read_excel("data/SPX500_last_volume_daily.xlsx"))
cpi       <- data.frame(read_excel("data/CPI_yoy_mthly.xlsx"))
dvds      <- data.frame(read_excel("data/DIVIDENTS.xlsx"))
fdtr      <- data.frame(read_excel("data/FDTRMID.xlsx"))
ipmg      <- data.frame(read_excel("data/IPMGCHNG_mom_sa.xlsx"))
usgg10    <- data.frame(read_excel("data/USGG10yr.xlsx"))
usur      <- data.frame(read_excel("data/USURTOT_mthly.xlsx"))

colnames(spx500)  <- c("Date", "Price_spx", "Volume_spx")
colnames(cpi)     <- c("Date", "CPI")
colnames(dvds)    <- c("Date", "Net_dvds", "Gross_dvds" )
colnames(fdtr)    <- c("Date", "FDTR")
colnames(ipmg)    <- c("Date", "IPMG")
colnames(usgg10)  <- c("Date", "USGG10")
colnames(usur)    <- c("Date", "USUR")

spx500$Volume_spx[spx500$Volume_spx == 0] <- NA

# 3. Merging data -----------------------------------------

# Merging daily data ----

# SP500 ----
scanner(djia_core, spx500)
djia_core <- merge(djia_core, spx500, by = "Date", all.x = TRUE)
length(which(is.na(djia_core$Price_spx)))

#filling missing cells from "Price.spx" and "Volume.spx" columns with the average value of their closest neighbor cells
djia_core[which(is.na(djia_core$Price_spx)),c("Price_spx", "Volume_spx")] <- 
  (djia_core[which(is.na(djia_core$Price_spx))-1, c("Price_spx", "Volume_spx")] + 
     djia_core[which(is.na(djia_core$Price_spx))+1, c("Price_spx", "Volume_spx")])/2

length(which(is.na(djia_core$Price_spx)))

# FDTR ----
scanner(djia_core, fdtr)
djia_core <- merge(djia_core, fdtr, by = "Date", all.x = TRUE)

#tail(which(is.na(djia_core$FDTR)),200)

# DVDS ----
scanner(djia_core, dvds)
djia_core <- merge(djia_core, dvds, by = "Date", all.x = TRUE)
djia_core$Net_dvds <- NULL
length(which(is.na(djia_core$Gross_dvds)))

#filling missing "solitary" cells from "Gross_dvds" column with the average value of its closest neighbor cells
djia_core[which(is.na(djia_core$Gross_dvds))[-(1:19)],"Gross_dvds"] <- 
  (djia_core[which(is.na(djia_core$Gross_dvds))[-(1:19)]-1,"Gross_dvds"] + 
     djia_core[which(is.na(djia_core$Gross_dvds))[-(1:19)]+1, "Gross_dvds"])/2

length(which(is.na(djia_core$Gross_dvds)))


# USGG10 ----
scanner(djia_core, usgg10)
djia_core <- merge(djia_core, usgg10, by = "Date", all.x = TRUE)
length(which(is.na(djia_core$USGG10))[-(1:4108)])

#filling missing "solitary" cells from "USGG10" column with the average value of its closest neighbor cells
djia_core[which(is.na(djia_core$USGG10))[-(1:4108)],"USGG10"] <- 
  (djia_core[which(is.na(djia_core$USGG10))[-(1:4108)]-1,"USGG10"] + 
     djia_core[which(is.na(djia_core$USGG10))[-(1:4108)]+1, "USGG10"])/2

length(which(is.na(djia_core$USGG10))[-(1:4108)])

#filling the rest of missing cells with first previous available value
for(i in which(is.na(djia_core$USGG10))[-(1:4108)]){
  
  djia_core[i,"USGG10"] <- djia_core[i-1,"USGG10"]
  
}
rm(i)

length(which(is.na(djia_core$USGG10))[-(1:4108)])




# Merging monthly data ----

# CPI ----
djia_core     <- merger(djia_core, cpi)
djia_core$CPI <- filler(djia_core$CPI)
djia_core$CPI  <- monthlyTrimmer(djia_core, cpi)

# USUR ----
djia_core       <- merger(djia_core, usur)
djia_core$USUR  <- filler(djia_core$USUR)
djia_core$USUR  <- monthlyTrimmer(djia_core, usur)

# IPMG ----
djia_core     <- merger(djia_core, ipmg)
djia_core$IPMG <- filler(djia_core$IPMG)
djia_core$IPMG  <- monthlyTrimmer(djia_core, ipmg)



rownames(djia_core) <- 1:nrow(djia_core)

# 4. Low level features -----------------------------------------------------

# Logarithmic returns ----
djia_core$lReturn_djia  <- round(ROC(djia_core$Price), 6)
djia_core$lReturn_spx   <- round(ROC(djia_core$Price_spx), 6)


cor(djia_core$lReturn_djia[-1], djia_core$lReturn_spx[-1])

# Rate of chance of trading volume ----
djia_core$ROC_vol_spx   <- round(ROC(djia_core$Volume_spx), 6)

# Volatility ----
noSessions <- as.numeric(table(year(djia_core$Date)))
avgNoSessions <- round(mean(noSessions[2:length(noSessions)]))

djia_core$Volatility_W <- volatility(djia_core$Price, 5, calc = "close", N = avgNoSessions)
djia_core$Volatility_M <- volatility(djia_core$Price, 21, calc = "close", N = avgNoSessions)
djia_core$Volatility_Q <- volatility(djia_core$Price, 63, calc = "close", N = avgNoSessions)
djia_core$Volatility_Y <- volatility(djia_core$Price, n = avgNoSessions, calc = "close", N = avgNoSessions)

rm(noSessions,avgNoSessions)

# Weekday/ Month Identification ----
djia_core <- daysMonthsID(djia_core)

# RSI ----
djia_core$RSI_djia <- round(RSI(djia_core$Price, n = 21, maType="WMA"),2)

# EMA ----
djia_core$EMA_djia <- round(EMA(djia_core$Price, n = 21),2)

# SMA ----
djia_core$SMA_djia <- round(SMA(djia_core$Price, n = 21), 2)

# Returns structure between TPcand ----
djia_core <- returnStruct(djia_core)
# Returns chain distribution (mean) between TPcand ----
djia_core <- chainDistribution(djia_core)


# 5. High level features ------------------------------------------------

# Returns structure between TPcand and its identifier ----
djia_core <- returnStruct2(djia_core)
# Returns chain distribution (mean) between TPcand and its identifier ----
djia_core <- chainDistribution2(djia_core)



# 6. Cleaning data ------------------------------------------------------

gist <- djia_core[which(is.na(djia_core$Volume_spx) == FALSE)[1] : nrow(djia_core),]
gist <- gist %>% filter(TPcand == 1) %>% select(-c("Date", "ObsNo", "L1P", "L1T", "L2P", 
                                                         "L2T", "L3P", "L3T", "IDObs", "TPcand",
                                                         "Weekday", "Month", "PosInaRow_TPcand",
                                                         "NegInaRow_TPcand", "GainInaRow_TPcand",
                                                         "LossInaRow_TPcand", "FDTR", "USGG10")) %>%
  mutate(TPreal = factor(TPreal, levels = c(0,1), labels = c("False", "True")))

gist$CumVolROC_spx_TPcand[is.na(gist$CumVolROC_spx_TPcand)] <- 0


# 9. Simple Decision Tree---------------------------------------------------------------

# Data Splitting
set.seed(101)
trainIndex <- createDataPartition(gist$TPreal, p = 0.85, list = FALSE)

# Split with highly correlated predictors
trainingSDT <- gist[trainIndex, ]
testingSDT <- gist[-trainIndex, ]

# Model Training and Tuning

# Subsets validation
prop.table(table(trainingSDT$TPreal))
prop.table(table(testingSDT$TPreal))

# Fitting the model
set.seed(112)
rPartfit <- rpart(TPreal~., data = trainingSDT, method = 'class')
dev.new()
rpart.plot(rPartfit, extra = 106)

# Extracting Predictions
predict_unseen_Test <-predict(rPartfit, testingSDT, type = 'class')
confusionMatrix(data = predict_unseen_Test, testingSDT$TPreal)

predict_unseen_Train <-predict(rPartfit, trainingSDT, type = 'class')
confusionMatrix(data = predict_unseen_Train, trainingSDT$TPreal)

pred  <- prediction(as.numeric(testingSDT$TPreal), as.numeric(predict_unseen_Test))
roc   <- performance(pred, measure="tpr", x.measure="fpr")

auc = performance(pred, 'auc')
gauge_SDT <- slot(auc, 'y.values')
print(paste("AUC TEST set:", gauge_SDT))

pred2  <- prediction(as.numeric(trainingSDT$TPreal), as.numeric(predict_unseen_Train))
roc2   <- performance(pred2, measure="tpr", x.measure="fpr")

auc2 = performance(pred2, 'auc')
gauge_SDTcv <- slot(auc2, 'y.values')
print(paste("AUC TRAIN set:", gauge_SDTcv))

dev.new()
plot(roc, col="orange", lwd=2)
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=2)

#
#rm(predict_unseen_Test, pred,pred2, roc,roc2, auc, auc2)

# 10. Partial least squares discriminant analysis (PLS) ---------------------------------------

# Pre-processing:Zero-variance and Near Zero-Variance Predictors evaluation
nzv <- nearZeroVar(small_clean)
str(nzv)
if(length(nzv) != 0){small_clean <- small_clean_temp[, -nzv]}

# Data Splitting
# set.seed(101)
# trainIndex <- createDataPartition(small_clean$TPreal, p = 0.75, list = FALSE)

trainingPLS <- small_clean[trainIndex, ]
testingPLS <- small_clean[-trainIndex, ]

# Model Training and Tuning
ctrlPLS <- trainControl(method = "cv",
                        number = 10,
                        returnResamp = "all",
                        classProbs = TRUE,
                        summaryFunction = twoClassSummary)


set.seed(112)
plsFit <- train(TPreal~.,
                data = trainingPLS, 
                method = "pls", 
                preProc = c("center", "scale"),
                tuneLength = 15, 
                trControl = ctrlPLS, 
                metric = "ROC")

plsFit
dev.new()
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

dev.new()
plot(plsTPreal.ROC, main = "PLS ROC resampling: cv")

confusionMatrix(data = plsTPreal, testingPLS$TPreal)
gauge_PLS <- plsTPreal.ROC$auc
gauge_PLS

# ROC score cross-validation:
plsTPreal.probs.train <- predict.train(plsFit,
                                      newdata = trainingPLS, 
                                      type = "prob")

plsTPreal.ROC.train <- roc(plsTPreal.probs.train$False, 
                            response = trainingPLS$TPreal,
                            levels = rev(levels(trainingPLS$TPreal)))

gauge_PLS.train <- plsTPreal.ROC.train$auc
print(paste("ROC for predicitons on TEST subset:", gauge_PLS))
print(paste("ROC for predicitons on TRAINING subset:", gauge_PLS.train))


rm(plsTPreal)
# 11. XGB Tree -------------------------------------------------------------------

# Data Splitting

set.seed(101)
trainIndex <- createDataPartition(gist$TPreal, p = 0.85, list = FALSE)

trainingXGB <- gist[trainIndex, ]
testingXGB <- gist[-trainIndex, ]


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


set.seed(112)
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


xgbFit_cv
dev.new()
ggplot(xgbFit_cv)
xgbFit_loocv
dev.new()
ggplot(xgbFit_loocv)


# Extracting Predictions
xgbTPreal_cv <- predict.train(xgbFit_cv, newdata = testingXGB)
xgbTPreal_loocv <- predict.train(xgbFit_loocv, newdata = testingXGB)
xgbTPreal_none <- predict.train(xgbFit_none, newdata = testingXGB)

confusionMatrix(data = xgbTPreal_cv, testingXGB$TPreal)
confusionMatrix(data = xgbTPreal_loocv, testingXGB$TPreal)
confusionMatrix(data = xgbTPreal_none, testingXGB$TPreal)

xgbTPreal_cv.probs <- predict.train(xgbFit_cv, newdata = testingXGB, type = "prob")
xgbTPreal_loocv.probs <- predict.train(xgbFit_loocv, newdata = testingXGB, type = "prob")
xgbTPreal_none.probs <- predict.train(xgbFit_none, newdata = testingXGB, type = "prob")

xgbTPreal_cv.ROC <- roc(xgbTPreal_cv.probs$False,
                       response = testingXGB$TPreal,
                       levels=rev(levels(testingXGB$TPreal)))

xgbTPreal_loocv.ROC <- roc(xgbTPreal_loocv.probs$False,
                       response = testingXGB$TPreal,
                       levels=rev(levels(testingXGB$TPreal)))

xgbTPreal_none.ROC <- roc(xgbTPreal_none.probs$False,
                       response = testingXGB$TPreal,
                       levels=rev(levels(testingXGB$TPreal)))


gauge_XGB_cv <- xgbTPreal_cv.ROC$auc
gauge_XGB_cv
dev.new()
plot(xgbTPreal_cv.ROC, main = "xgboost ROC method: cv")

gauge_XGB_loocv <- xgbTPreal_loocv.ROC$auc
gauge_XGB_loocv
dev.new()
plot(xgbTPreal_loocv.ROC, main = "xgboost ROC method: LOOCV")

gauge_XGB_none <- xgbTPreal_none.ROC$auc
gauge_XGB_none
dev.new()
plot(xgbTPreal_none.ROC, main = "xgboost ROC method: none")

# ROC scores cross-validation
xgbTPreal_loocvtrain.probs  <- predict.train(xgbFit_loocv, newdata = trainingXGB, type = "prob")
xgbTPreal_cvtrain.probs     <- predict.train(xgbFit_cv, newdata = trainingXGB, type = "prob")

xgbTPreal_loocvtrain.ROC  <- roc(xgbTPreal_loocvtrain.probs$False,
                                response = trainingXGB$TPreal,
                                levels=rev(levels(trainingXGB$TPreal)))
xgbTPreal_cvtrain.ROC     <- roc(xgbTPreal_cvtrain.probs$False,
                                response = trainingXGB$TPreal,
                                levels=rev(levels(trainingXGB$TPreal)))

print(paste("AUC for loocv on TEST subset:", xgbTPreal_loocv.ROC$auc))
print(paste("AUC for cv on TEST subset:", xgbTPreal_cv.ROC$auc))
print(paste("AUC for loocv on TRAINING subset:", xgbTPreal_loocvtrain.ROC$auc))
print(paste("AUC for cv on TRAINING subset:", xgbTPreal_cvtrain.ROC$auc))


# 12. Support Vector Machines with Radial Basis Function Kernel-------------

# Data Splitting
# set.seed(101)
# trainIndex <- createDataPartition(small_clean$TPreal, p = 0.75, list = FALSE)

trainingSVM <- small_clean[trainIndex, ]
testingSVM <- small_clean[-trainIndex, ]

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
set.seed(112)
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
svmFit2
dev.new()
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

# Confusion Matrix
# confusionMatrix(data = svmFit1.predict, testingSVM$TPreal)
confusionMatrix(data = svmFit2.predict, testingSVM$TPreal)
# confusionMatrix(data = svmFit3.predict, testingSVM$TPreal)
# confusionMatrix(data = svmFit4.predict, testingSVM$TPreal)

# Plotting
# gauge_SVM1 <- svmFit1.roc$auc
gauge_SVM2 <- svmFit2.roc$auc
# gauge_SVM3 <- svmFit3.roc$auc
# gauge_SVM4 <- svmFit4.roc$auc

# dev.new()
# plot(svmFit1.roc, main = "SVM ROC resampling: repeatedcv")
dev.new()
plot(svmFit2.roc, main = "SVM ROC resampling: cv")
# dev.new()
# plot(svmFit3.roc, main = "SVM ROC resampling: boot")
# dev.new()
# plot(svmFit4.roc, main = "SVM ROC resampling: LOOCV")

# ROC score cross-validation:

svmFit2.probs.train <- predict.train(svmFit2, newdata = trainingSVM, type = "prob")
svmFit2.roc.train <- roc(svmFit2.probs.train$False, 
                         response = trainingSVM$TPreal,
                         levels=rev(levels(trainingSVM$TPreal)))

print(paste("ROC for predicitons on TEST subset:", svmFit2.roc$auc))
print(paste("ROC for predicitons on TRAINING subset:", svmFit2.roc.train$auc))

# 13. Logistic Regression-----

# Data Splitting
# set.seed(101)
# trainIndex <- createDataPartition(small_clean$TPreal, p = 0.75, list = FALSE)


trainingGLM <- small_clean[trainIndex, ]
testingGLM <- small_clean[-trainIndex, ]

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
set.seed(112)
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

# Summary
glmFit1
glmFit2

# Extracting Predictions
glmFit1.predict <- predict.train(glmFit1, newdata = testingGLM)
glmFit2.predict <- predict.train(glmFit2, newdata = testingGLM)

glmFit1.probs <- predict.train(glmFit1, newdata = testingGLM, type = "prob")
glmFit2.probs <- predict.train(glmFit2, newdata = testingGLM, type = "prob")

glmFit1.roc <- roc(glmFit1.probs$False, response = testingGLM$TPreal,levels=rev(levels(testingGLM$TPreal)))
glmFit2.roc <- roc(glmFit2.probs$False, response = testingGLM$TPreal,levels=rev(levels(testingGLM$TPreal)))

# Confusion Matrix
confusionMatrix(data = glmFit1.predict, testingGLM$TPreal)
confusionMatrix(data = glmFit2.predict, testingGLM$TPreal)

# Plotting
gauge_GLM1 <- glmFit1.roc$auc
dev.new()
plot(glmFit1.roc, main = "GLM ROC resampling: cv")

gauge_GLM2 <- glmFit2.roc$auc
dev.new()
plot(glmFit2.roc, main = "GLM ROC resampling: repeatedcv")

# ROC score cross-validation:

glmFit2.probs.train <- predict.train(glmFit2, newdata = trainingGLM, type = "prob")
glmFit2.roc.train <- roc(glmFit2.probs.train$False, 
                   response = trainingGLM$TPreal,
                   levels=rev(levels(trainingGLM$TPreal)))

print(paste("ROC for predicitons on TEST subset:", glmFit2.roc$auc))
print(paste("ROC for predicitons on TRAINING subset:", glmFit2.roc.train$auc))

# 14. Penalized Regressions----------------

# Data Splitting
# set.seed(101)
# trainIndex <- createDataPartition(small_clean$TPreal, p = 0.75, list = FALSE)

trainingPEN <- small_clean[trainIndex, ]
testingPEN <- small_clean[-trainIndex, ]

lambda <- 10^seq(-3, 3, length = 100)

# TrainControl Definition
ctrlPEN <- trainControl(method = "cv",
                         number = 10,
                         returnResamp = "all",
                         classProbs = TRUE,
                         summaryFunction = twoClassSummary)

# Training
set.seed(112)
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



# Summary
enrFit1
dev.new()
ggplot(enrFit1)

rdgFit1
dev.new()
ggplot(rdgFit1)

lsoFit1
dev.new()
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


# Confusion Matrix
confusionMatrix(data = enrFit1.predict, testingPEN$TPreal)
confusionMatrix(data = rdgFit1.predict, testingPEN$TPreal)
confusionMatrix(data = lsoFit1.predict, testingPEN$TPreal)


# Plotting
gauge_ENR1 <- enrFit1.roc$auc
dev.new()
plot(enrFit1.roc, main = "Elastic Net Regresion ROC resampling: cv")

gauge_RDG1 <- rdgFit1.roc$auc
dev.new()
plot(rdgFit1.roc, main = "Ridge Regression ROC resampling: cv")

gauge_LSO1 <- lsoFit1.roc$auc
dev.new()
plot(lsoFit1.roc, main = "Lasso Regression ROC resampling: cv")

# ROC score cross-validation:
enrFit1.probs.train <- predict.train(enrFit1, newdata = trainingPEN, type = "prob")
rdgFit1.probs.train <- predict.train(rdgFit1, newdata = trainingPEN, type = "prob")
lsoFit1.probs.train <- predict.train(lsoFit1, newdata = trainingPEN, type = "prob")

enrFit1.roc.train <- roc(enrFit1.probs.train$False,response = trainingPEN$TPreal,levels=rev(levels(trainingPEN$TPreal)))
rdgFit1.roc.train <- roc(rdgFit1.probs.train$False,response = trainingPEN$TPreal,levels=rev(levels(trainingPEN$TPreal)))
lsoFit1.roc.train <- roc(lsoFit1.probs.train$False,response = trainingPEN$TPreal,levels=rev(levels(trainingPEN$TPreal)))

print(paste("ROC for predicitons on TEST subset(ENR):", enrFit1.roc$auc))
print(paste("ROC for predicitons on TRAINING subset(ENR):", enrFit1.roc.train$auc))

print(paste("ROC for predicitons on TEST subset(RiDGe):", rdgFit1.roc$auc))
print(paste("ROC for predicitons on TRAINING subset(RiDGe):", rdgFit1.roc.train$auc))

print(paste("ROC for predicitons on TEST subset(LaSsO):", lsoFit1.roc$auc))
print(paste("ROC for predicitons on TRAINING subset(LaSsO):", lsoFit1.roc.train$auc))




# SUMMARY -------------------------------------------------------------------
print(paste(deparse(substitute(gauge_SDT)), gauge_SDT))
print(paste(deparse(substitute(gauge_SDT2)), gauge_SDT2))
print(paste(deparse(substitute(gauge_PLS)), gauge_PLS))
print(paste(deparse(substitute(gauge_XGB_cv)), gauge_XGB_cv))
print(paste(deparse(substitute(gauge_XGB_loocv)), gauge_XGB_loocv))
print(paste(deparse(substitute(gauge_XGB_none)), gauge_XGB_none))
# print(paste(deparse(substitute(gauge_SVM1)), gauge_SVM1))
print(paste(deparse(substitute(gauge_SVM2)), gauge_SVM2))
# print(paste(deparse(substitute(gauge_SVM3)), gauge_SVM3))
# print(paste(deparse(substitute(gauge_SVM4)), gauge_SVM4))
print(paste(deparse(substitute(gauge_GLM1)), gauge_GLM1))
print(paste(deparse(substitute(gauge_GLM2)), gauge_GLM2))
#print(paste(deparse(substitute(gauge_ENR1)), gauge_ENR1))
print(paste(deparse(substitute(gauge_RDG1)), gauge_RDG1))
#print(paste(deparse(substitute(gauge_LSO1)), gauge_LSO1))

# Plotting
dev.new()
plot(svmFit2.roc, main = "ROC curves for SVM, XGB & PLS")
plot(xgbTPreal_cv.ROC, add = TRUE, col = "blue")
plot(xgbTPreal_loocv.ROC, add = TRUE, col = "red")
plot(xgbTPreal_none.ROC, add = TRUE, col = "pink")
plot(plsTPreal.ROC, add = TRUE, col = "green")
plot(glmFit1.roc, add = TRUE, col = "yellow")
plot(glmFit2.roc, add = TRUE, col = "grey")
plot(enrFit1.roc, add = TRUE, col = "violet")
legend("right", legend = c("SVM", "XGBcv", "XGBloocv", "XGBnone", "PLS", "GLMcv", "GLMrcv", "ENR"),
       bty = "n", cex = 1, lty = 1,
       col = c("black", "blue", "red", "pink", "green", "yellow", "grey", "violet"))

# Comparing Resampling Distributions
resamps <- resamples(list(XGB = xgbFit_cv,
                          PLS = plsFit,
                          SVM = svmFit2,
                          GLM = glmFit1,
                          ENR = enrFit1))
resamps
summary(resamps)

# Differences
difValues <- diff(resamps)
difValues
summary(difValues)

# Density plot
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
bwplot(resamps, layout = c(3, 1))
