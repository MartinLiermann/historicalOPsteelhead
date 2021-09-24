#' bootBetaTS. A function to bootstrap the beta fit using an auto regressive function for the residuals.
#'
#' @param fitObj an object returned by fitBeta
#' @param sims the number of bootstrap simulations
#'
#' @return a list with fitObj$est and estList; an array of sims estimates
#' @examples
#' xx <- fitBeta(cDat[cDat$population=="Quillayute",])
#' @export
bootBetaTS <- function(fitObj,sims=1000){
  estList <- array(NA,dim=c(sims,3))
  resid <- fitObj$data$resid
  n <- length(resid)
  tsMod <- ar(resid)
  pred <- fitObj$data$pred
  newDat <- fitObj$data[,c("julian","CPUE")]
  startStopDays <- fitObj$startStopDays
  initVals <- fitObj$est
  for(i in 1:sims){
    newDat$CPUE <- pred + as.numeric(arima.sim(n=n,list(coef(tsMod))))
    estList[i,] <- fitBeta(newDat,startStopDays=startStopDays,initVals=initVals)$est
  }
  list(est=fitObj$est,estList=estList)
}
