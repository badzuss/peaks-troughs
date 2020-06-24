# merger() ------

# merger() aligns single date points from addon_df (monthly dataset) with the nearest 
# previous date point from djia_core (daily dataset). Redundant observations are removed.

merger <- function(djia_core, addon_df){
  
  djia_core <- merge(djia_core, addon_df, by = "Date", all = TRUE)
  x_miss <- which(is.na(djia_core[,'ObsNo']))
  err <- 0
  
  for(i in x_miss){
    if(is.na(djia_core[i-1,'ObsNo']) == FALSE){
      djia_core[i-1, ncol(djia_core)] <- djia_core[i,ncol(djia_core)]
    }
    else{
      cat(paste("Merger error! Index: ", i)) 
      err <- 1}
  }
  
  if(err == 0){djia_core <- djia_core[!is.na(djia_core[,'ObsNo']),]}
  return(djia_core)

}


# filler() ------

# use scenario: merging two tables: x (daily data), y (monthly data)
#               merge(x,y, by = COMMON_COLUMN, all.x = TRUE)
#               As a result, table y (monthly data) contains tons of NAs
#               filler() detects first non-empty cell from COMMON_COLUMN and transcribe its value to empty cells prior to it,
#               filler() assumes y is of even monthly intervals.
#               Function assumes the same data range for both tables.
#               Consider using trimmer() if the above is not true.

filler <- function(vect){
  
  k <- 1
  for(i in 1:length(vect)){
    if(is.na(vect[i]) == FALSE){
      fill <- vect[i]
      for(j in k:i){
        if(is.na(vect[j]) == TRUE){
          vect[j] <- fill
        }
      }
      k <- i
    }
  }
  return(vect)
  
}


# monthlyTrimmer() --------

monthlyTrimmer <- function(djia_core, addon_df){
  
  djia_core[djia_core$Date < ceiling_date(addon_df$Date[1], "month") - months(1), ncol(djia_core)] <- NA
  djia_core[djia_core$Date > ceiling_date(addon_df$Date[nrow(addon_df)], "month"), ncol(djia_core)] <- NA
  return(djia_core[,ncol(djia_core)])
  
}
  


# scanner() ------

# scanner() temporarily merges original djia_core with addon_df (by ='Date', all = TRUE)
# and outputs discrepancies info

scanner <- function(djia_core, addon_df){
  
    temp <- merge(djia_core[,1:3], addon_df, by = "Date", all = TRUE)
    diag <- table(is.na(temp$ObsNo))
    cat(paste("Primary dataset observations:     ",diag[1],'\n'))
    cat(paste("Attached dataset observations:    ",nrow(addon_df),'\n'))
    cat(paste("Missing observations in Primary:  ",diag[2],'\n'))
    
}


# 


# returnsStruct() ----

# structure of daily returns between Turning Point candidates:
# Counting positive daily return & negative daily return between TPcand
# Trend shift counter (No. transitions from positive to negative daily returns) between TPcand
# Moving sum of consecutive positive returns between TPcand (chain length)
# Moving sum of consecutive negative returns between TPcand (chain length)
# Moving sum of consecutive gains between TPcand (gain magnitude)
# Moving sum of consecutive losses between TPcand (loss magnitude)

returnStruct <- function(djia_core){
  
  TPcand_index <- djia_core %>% filter(TPcand == 1) %>% select(ObsNo) %>% unlist() %>% as.numeric()
  TPcand_index <- c(TPcand_index, nrow(djia_core))
  
  djia_core$PosCount_TPcand   <- NA
  djia_core$NegCount_TPcand   <- NA
  djia_core$ShiftCount_TPcand <- NA
  djia_core$PosInaRow_TPcand  <- NA
  djia_core$NegInaRow_TPcand  <- NA
  djia_core$CumReturn_TPcand  <- NA
  djia_core$Sessions_TPcand   <- NA
  djia_core$Slope_TPcand      <- NA
  
  x <- 2
  
  for(i in TPcand_index){

    posCount      <- 0
    negCount      <- 0
    shiftCount    <- 0
    posInaRow     <- 0
    negInaRow     <- 0
    gainInaRow    <- 0
    lossInaRow    <- 0
    cumReturn     <- 0
    cumReturn_spx <- 0
    cumVolROC_spx <- 0
    sessionCount  <- 0
    
    
    for(j in x:i){
      
      cumReturn     <- cumReturn + djia_core$lReturn_djia[j]
      cumReturn_spx <- cumReturn_spx + djia_core$lReturn_spx[j]
      cumVolROC_spx <- cumVolROC_spx + djia_core$ROC_vol_spx[j]
      sessionCount  <- sessionCount + 1
      
      djia_core$CumReturn_TPcand[j]     <- cumReturn
      djia_core$CumReturn_spx_TPcand[j] <- cumReturn_spx
      djia_core$CumVolROC_spx_TPcand[j] <- cumVolROC_spx
      djia_core$Sessions_TPcand[j]      <- sessionCount
      djia_core$Slope_TPcand[j]         <- round(djia_core$CumReturn_TPcand[j] / djia_core$Sessions_TPcand[j],4)
      
      if(djia_core$lReturn_djia[j] >= 0){
        
        posCount    <- posCount + 1
        posInaRow   <- posInaRow + 1
        gainInaRow  <- gainInaRow + djia_core$lReturn_djia[j]
        negInaRow   <- 0
        lossInaRow  <- 0
        
        
        djia_core$PosCount_TPcand[j]    <- posCount
        djia_core$NegCount_TPcand[j]    <- negCount
        djia_core$PosInaRow_TPcand[j]   <- posInaRow
        djia_core$NegInaRow_TPcand[j]   <- negInaRow
        djia_core$GainInaRow_TPcand[j]  <- gainInaRow
        djia_core$LossInaRow_TPcand[j]  <- lossInaRow
        
        if((j >= x+1) & (djia_core$lReturn_djia[j-1] < 0)){
          
          shiftCount <- shiftCount + 1
          djia_core$ShiftCount_TPcand[j] <- shiftCount
        }
        
        else if((j >= x+1) & (djia_core$lReturn_djia[j-1] >= 0)){
          
          djia_core$ShiftCount_TPcand[j] <- shiftCount
        }
        
      }
      
      else if(djia_core$lReturn_djia[j] < 0){
        
        negCount    <- negCount + 1
        negInaRow   <- negInaRow + 1
        lossInaRow  <- lossInaRow + djia_core$lReturn_djia[j]
        posInaRow   <- 0
        gainInaRow  <- 0
        
        djia_core$NegCount_TPcand[j]    <- negCount
        djia_core$PosCount_TPcand[j]    <- posCount
        djia_core$PosInaRow_TPcand[j]   <- posInaRow
        djia_core$NegInaRow_TPcand[j]   <- negInaRow
        djia_core$GainInaRow_TPcand[j]  <- gainInaRow
        djia_core$LossInaRow_TPcand[j]  <- lossInaRow
        
        if((j >= x+1) & (djia_core$lReturn_djia[j-1] >= 0)){
          
          shiftCount <- shiftCount + 1
          djia_core$ShiftCount_TPcand[j] <- shiftCount
        }
        
        else if((j >= x+1) & (djia_core$lReturn_djia[j-1] < 0)){
          
          djia_core$ShiftCount_TPcand[j] <- shiftCount
        }
        
      }
    }
    
    x <- i + 1  
    
  }
  
  return(djia_core)
 
}

# returnStruct2() ----

returnStruct2 <- function(djia_core){
  
  TPcand_index    <- djia_core %>% filter(TPcand == 1) %>% select(ObsNo) %>% unlist() %>% as.numeric()
  TPcandID_index  <- djia_core %>% filter(TPcand == 1) %>% select(IDObs) %>% unlist() %>% as.numeric()
  
  
  djia_core$PosCount_TP2ID   <- NA
  djia_core$NegCount_TP2ID   <- NA
  djia_core$ShiftCount_TP2ID <- NA
  djia_core$CumReturn_TP2ID  <- NA
  djia_core$Sessions_TP2ID   <- NA
  djia_core$Slope_TP2ID      <- NA
  
  
  
  for(i in 1:length(TPcand_index)){
    
    TP <- TPcand_index[i]
    ID <- TPcandID_index[i]
    
    djia_core$PosCount_TP2ID[TP]     <- djia_core$PosCount_TPcand[ID]
    djia_core$NegCount_TP2ID[TP]     <- djia_core$NegCount_TPcand[ID]
    djia_core$ShiftCount_TP2ID[TP]   <- djia_core$ShiftCount_TPcand[ID]
    djia_core$CumReturn_TP2ID[TP]    <- djia_core$CumReturn_TPcand[ID]
    djia_core$Sessions_TP2ID[TP]     <- djia_core$Sessions_TPcand[ID]
    djia_core$Slope_TP2ID[TP]        <- djia_core$Slope_TPcand[ID]
    
  }
  
  return(djia_core)
}

# chainDistribution() ----

chainDistribution <- function(djia_core){
  
  TPcand_index    <- djia_core %>% filter(TPcand == 1) %>% select(ObsNo) %>% unlist() %>% as.numeric()
  TPcand_index    <- c(TPcand_index, nrow(djia_core))
  
  tempList <- list(djia_core$PosInaRow_TPcand,
                   djia_core$NegInaRow_TPcand,
                   djia_core$GainInaRow_TPcand,
                   djia_core$LossInaRow_TPcand)
  
  tempList <- lapply(tempList, function(x){
    
    for(i in 2: length(x)){
      if(x[i] == 0){next}
      else{x[i-1] <- 0}
    }
    x[1] <- 0
    x
  })
  
  djia_core$MeanLengthPos_TPcand  <- NA
  djia_core$MeanLengthNeg_TPcand  <- NA
  djia_core$MeanGainInaRow_TPcand <- NA
  djia_core$MeanLossInaRow_TPcand <- NA
  
  
  x <- 2
  
  for(i in TPcand_index){
    
    for(j in x:i){
      
      tmpPos      <- table(tempList[[1]][x:j], exclude = "0")
      tmpNeg      <- table(tempList[[2]][x:j], exclude = "0")
      tmpGain     <- table(tempList[[3]][x:j], exclude = "0")
      tmpLoss     <- table(tempList[[4]][x:j], exclude = "0")
      
      valuesPos   <- as.numeric(names(tmpPos))
      valuesNeg   <- as.numeric(names(tmpNeg))
      valuesGain  <- as.numeric(names(tmpGain))
      valuesLoss  <- as.numeric(names(tmpLoss))
      
      weightsPos  <- as.numeric(tmpPos)
      weightsNeg  <- as.numeric(tmpNeg)
      weightsGain <- as.numeric(tmpGain)
      weightsLoss <- as.numeric(tmpLoss)
      
      djia_core$MeanLengthPos_TPcand[j]  <- ifelse(is.na(weighted.mean(valuesPos,weightsPos)), 0, round(weighted.mean(valuesPos,weightsPos),4))
      djia_core$MeanLengthNeg_TPcand[j]  <- ifelse(is.na(weighted.mean(valuesNeg,weightsNeg)), 0, round(weighted.mean(valuesNeg,weightsNeg),4))
      djia_core$MeanGainInaRow_TPcand[j] <- ifelse(is.na(weighted.mean(valuesGain,weightsGain)), 0, round(weighted.mean(valuesGain,weightsGain),4))
      djia_core$MeanLossInaRow_TPcand[j] <- ifelse(is.na(weighted.mean(valuesLoss,weightsLoss)), 0, round(weighted.mean(valuesLoss,weightsLoss),4))
      
    }
    
    x <- i + 1  
  }
  
  return(djia_core)
}
# chainDistribution2() ----

chainDistribution2 <- function(djia_core){
  
  TPcand_index    <- djia_core %>% filter(TPcand == 1) %>% select(ObsNo) %>% unlist() %>% as.numeric()
  TPcandID_index  <- djia_core %>% filter(TPcand == 1) %>% select(IDObs) %>% unlist() %>% as.numeric()
  
  djia_core$MeanLengthPos_TP2ID  <- NA
  djia_core$MeanLengthNeg_TP2ID  <- NA
  djia_core$MeanGainInaRow_TP2ID <- NA
  djia_core$MeanLossInaRow_TP2ID <- NA
  
  
  tempList <- list(djia_core$PosInaRow_TPcand,
                   djia_core$NegInaRow_TPcand,
                   djia_core$GainInaRow_TPcand,
                   djia_core$LossInaRow_TPcand)
  
  tempList <- lapply(tempList, function(x, TPcand_index){
    
    results <- numeric(length(TPcand_index))
    
    for(i in 1:length(TPcand_index)){
      
      TP <- TPcand_index[i]+1
      ID <- TPcandID_index[i]
      
      temp <- x[TP:ID]
      
      for(u in 1:length(temp)){
        
        if(temp[u] == 0){next}
        else if(u == 1){next}
        else{temp[u-1] <- 0}
      }
      
      meh         <- table(temp, exclude = "0")
      values      <- as.numeric(names(meh))
      weights     <- as.numeric(meh)
      results[i]  <- weighted.mean(values,weights)
    }
    results
  }, TPcand_index)
  
  for(i in 1:length(TPcand_index)){
    
    TP <- TPcand_index[i]
    
    djia_core$MeanLengthPos_TP2ID[TP]  <- round(tempList[[1]][i],4)
    djia_core$MeanLengthNeg_TP2ID[TP]  <- round(tempList[[2]][i],4)
    djia_core$MeanGainInaRow_TP2ID[TP] <- round(tempList[[3]][i],4)
    djia_core$MeanLossInaRow_TP2ID[TP] <- round(tempList[[4]][i],4)
    
  }
  
  return(djia_core)
}
# daysMonthsID() ----

daysMonthsID <- function(djia_core){
  
  # Names extraction
  djia_core$Weekday <- weekdays(as.Date(djia_core$Date))
  djia_core$Month <- months(djia_core$Date)
  
  # Factorizing & levels re-ordering
  djia_core$Weekday <- factor(djia_core$Weekday,
                              levels(factor(djia_core$Weekday))[c(2,4,5,3,1)])
  djia_core$Month <- factor(djia_core$Month,
                            levels(factor(djia_core$Month))[c(5,4,8,1,9,7,6,2,12,11,10,3)])
  
  # Factor to numeric conversion
  djia_core$Weekdayid <- factor(as.numeric(djia_core$Weekday))
  djia_core$Monthid <- factor(as.numeric(djia_core$Month))
  
  djia_core$Weekdayid <- as.numeric(levels(djia_core$Weekdayid))[djia_core$Weekdayid]
  djia_core$Monthid <- as.numeric(levels(djia_core$Monthid))[djia_core$Monthid]
  
  return(djia_core)
}