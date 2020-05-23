#----------------------Words per min (WPM) FORMULA---------------------------#

caculateWPM <- function(transcribeCol, timeCol){
  result <- (((nchar(transcribeCol)-1)/(timeCol/1000))*(60/5))
  return(result)
}

#----------------------Corrected Error Rate FORMULA---------------------------#
calculateCER <- function(IFcol, Ccol, INFcol){
  result<- ((IFcol)/(Ccol+INFcol+IFcol))
  return(result)
}
#----------------------Uncorrected Error Rate FORMULA---------------------------#
calculateUER <- function(IFcol, Ccol, INFcol){
  result<- ((INFcol)/(Ccol+INFcol+IFcol))
  return(result)
}

#----------------------Total Error Rate FORMULA---------------------------#
calculateTER <- function(IFcol, Ccol, INFcol){
  result<- ((IFcol+INFcol)/(Ccol+INFcol+IFcol))
  return(result)
}



#----------------------Inter Key Interval (IKI) FORMULA---------------------------#
calculateIKI <- function(timestampCol){
  result<-((timestampCol - lag(timestampCol, n = 1L, default = 0)) * (lag(timestampCol, n = 1L, default = 0) != 0))
  return(result)
}


#----------------------AVERAGE Inter Key Interval (IKI) FORMULA---------------------------#


#----------------------Keyboard Efficientcy FORMULA---------------------------#
calculateKE<- function(transcribeCol, actionLength){
  result <- ((nchar(transcribeCol))/(actionLength))
  return(result)
}






