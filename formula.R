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
# TODO use lag to ignore time to press first keystroke
calculateIKI <- function(Tcol, Ccol, INFcol, IFcol, Fcol){
  result <- ((Tcol)/(Ccol+INFcol+IFcol+Fcol))
  return(result)
}

#----------------------AVERAGE Inter Key Interval (IKI) FORMULA---------------------------#


#----------------------Keyboard Efficientcy FORMULA---------------------------#
calculateKE<- function(Ccol, INFcol, IFcol, Fcol){
  result <- ((Ccol+INFcol+IFcol+Fcol)/(Ccol+INFcol))
  return(result)
}






