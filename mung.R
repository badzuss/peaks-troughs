# merger() ------

# merger() aligns single date points from addon_df (monthly dataset) with the nearest previous
# date point from djia_core (daily dataset). Redundant observations are removed. 

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
#               filler() replaces NAs with monthly value for a given month
#               Function assumes the same data range for both tables.
#               Consider trimming if above is not true.

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
  rm(fill,i,j,k)
  return(vect)
  
}



# scanner() ------

# scanner() temporarily merges original djia_core with addon_df (by ='Date', all = TRUE)
# and outputs discrepancies info

scanner <- function(djia_core, addon_df){
  
    temp <- merge(djia_core[,1:12], addon_df, by = "Date", all = TRUE)
    diag <- table(is.na(temp$ObsNo))
    cat(paste("Primary dataset observations:     ",diag[1],'\n'))
    cat(paste("Attached dataset observations:    ",nrow(addon_df),'\n'))
    cat(paste("Missing observations in Primary:  ",diag[2],'\n'))
    rm(temp, diag)
    
}


# -----
#djia_core$Close_spx[is.na(djia_core$Close_spx)]   <- lag(djia_core$Close_spx,1)
#djia_core$Volume_spx[is.na(djia_core$Volume_spx)] <- lag(djia_core$Volume_spx,1)
#djia_core$Close_spx[is.na(djia_core$Close_spx)]   <- lead(djia_core$Close_spx,1)
#djia_core$Volume_spx[is.na(djia_core$Volume_spx)] <- lead(djia_core$Volume_spx,1)
