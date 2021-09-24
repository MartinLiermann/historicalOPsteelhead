#' The % of the population that has past by day j
#'
#' @param est a vector with the three fitted beta parameters
#' @param j the day for which you would like to calculate the percentage
#' @param startStopDays
#'
#' @return a value or vector of values
#' @export
pJ <- function(est,j,startStopDays){
  dd <- startStopDays
  bDay <- (j-dd[1])/(dd[2]-dd[1])
  if(is.null(dim(est))){
    return(pbeta(bDay,est[1],est[2]))
  }else{
    return(pbeta(bDay,est[,1],est[,2]))
  }
}
