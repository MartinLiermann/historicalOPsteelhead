#' the Julian day at which p% of the population has past
#'
#' @param est a vector with the three fitted beta parameters
#' @param p the desired % of the run timing that has occured by the calculated day
#'
#' @return a value or vector of values
#' @export
qJ <- function(est,p,startStopDays){
  dd <- startStopDays
  if(is.null(dim(est))){
    bX <- qbeta(p,est[1],est[2])
  }else{
    bX <- qbeta(p,est[,1],est[,2])
  }
  bX*(dd[2]-dd[1]) + dd[1]
}
