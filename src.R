# 1. Libraries --------------------------------------------------------------
#install.packages("pacman")

pacman::p_load(pacman,tidyverse,stringr, forcats,readxl,TTR,quantmod,corrplot,
               rpart,rpart.plot,lubridate,caret,e1071,xgboost,pROC,ROCR, grDevices)

source("mung.R")
source("eda.R")

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

#gist <- djia_core[which(is.na(djia_core$Volume_spx) == FALSE)[1] : nrow(djia_core),]


gist <- djia_core %>% filter(TPcand == 1) %>% select(-c("Date", "ObsNo", "L1P", "L1T", "L2P", 
                                                    "L2T", "L3P", "L3T", "IDObs", "TPcand",
                                                    "Weekday", "Month", "PosInaRow_TPcand",
                                                    "NegInaRow_TPcand", "GainInaRow_TPcand",
                                                    "LossInaRow_TPcand", "FDTR", "USGG10",
                                                    "Volume_spx", "ROC_vol_spx", "CumVolROC_spx_TPcand")) %>%
  mutate(TPreal = factor(TPreal, levels = c(0,1), labels = c("False", "True")))

#filling NAs

usur_sampel <- numeric(4)
for(i in 1:4){
  usur_sampel[i] <- mean(sample(gist$USUR[5:40], 5))
}
gist$USUR[1:4] <- usur_sampel

volatility_y_sample <- mean(gist$Volatility_Y[2:4])

gist$Volatility_Y[1] <- volatility_y_sample

rm(i, volatility_y_sample, usur_sampel)

#gist$CumVolROC_spx_TPcand[is.na(gist$CumVolROC_spx_TPcand)] <- 0

which(is.na(gist))


# 7. Fitting models ---------------------------------------------------------------

simpleDecisionTree(gist, proportion = 0.72, seed_split = 101)

glmModel(gist, proportion = 0.72, seed_split = 101)

pls(gist, proportion = 0.75, seed_split = 101, seed_train = 112)

xgbTree(gist, proportion = 0.72, seed_split = 101, seed_train = 112)

PenalizedReg(gist, proportion = 0.69, seed_split = 101, seed_train = 112)

svmModel(gist, proportion = 0.72, seed_split = 101, seed_train = 112)


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

# Plotting ----
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
