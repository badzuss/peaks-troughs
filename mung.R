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

# structure of daily returns between Turning Point candidates

returnStruct <- function(){
  
  
  
}
