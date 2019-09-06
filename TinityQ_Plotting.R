libary(lubridate)
MakeYMDinHYDF <- function(inHYDF, hydYear){
  lenDF <- dim(inHYDF)[1]
  # make vector of dates
  inYear <- hydYear - 1
  diffYear <- years(inYear - 2018) # back in time is negative
  #print(class(diffYear))
  dateV <- rep(NA, lenDF)
  for(i in 1:lenDF){
    dateV[i] <- as.Date(inHYDF$Date[i]) + diffYear
    
  }
  
  inHYDF$YMD <- as.Date(dateV, origin = lubridate::origin)
  

  return(inHYDF)
  
}



if(leap_year(hydYear)){
  outDF <- AddLeapYearDay(inHYDF)
}



GetProperQdf <- function(inDate){
  #  Returns the correct data frame for date
  out = 0
  if(inDate < histQ.StartDate){
    out <- -1 
  } else if(inDate < histQ.EndDate){
    out <- histQ  
  } else if(inDate < usgsQ.EndDate){
    out <- usgsQ  
  } else if (inDate < trrpQ0218.EndDate){
    out <- trrpQ0218
  } else { out <- -2}
  return(out)
}

GetQ <- function(inDate){
  df <- GetProperQdf(inDate)
  indx <- GetIndexFromDate(df,inDate)
  return(df$Q[indx])
  
}

GetQdf <- function(inHYDF,inDate){
  indx <- GetIndexFromDate(inHYDF,inDate)
  return(inHYDF$Q[indx])
  
}


AddLeapYearDay <- function(inHYDF){
  HY99 <- inHYDF[300,]
  HY99
  HY99$DoY <- 1000
  HY99$M <- 2
  HY99$D <- 29
  Q1 <- inHYDF$Q[151]
  Q2 <- inHYDF$Q[152]
  HY99$Q <- (Q1 + Q2)/2
  nuDate <- paste(HY99$Y,HY99$M,HY99$D)
  #print(nuDate)
  
  HY99$YMD <- ymd(nuDate)
  #  151 & 152
  outDF <- rbind(inHYDF,HY99)
  outDF <- outDF[order(outDF$YMD),]
  outDF$DoY <- seq(1,366)
  row.names(outDF) <- seq(1,366)
  return( outDF )
  
}

GetIndexFromDate <- function(inHY,inDate){
  startDate <- min(inHY$YMD)
  endDate <- max(inHY$YMD)
  out <- 1
  if(inDate < startDate){ out <- -1 }
  if(inDate > endDate){ out <- -2 }
  if(out > 0){
    out <- inDate - startDate
  }
  return(as.integer(out)+1)
}


