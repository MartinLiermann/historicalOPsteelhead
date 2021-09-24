#' fitBetas is a function to estimate run timing for multiple populations or periods by fitting a beta distribution to CPUE vs time data.
#'     julian needs to centered around day 1, So negative values from around july through december
#' @param dat data frame. Needs a CPUE, julian and population column
#' @param popNames names of the populations for which you would like to create estimates
#' @param startStopDays a vector with a start and stop dates for the run
#' @param initVals initial values for the alpha, beta and constant multiplier parameters.
#'
#' @return a list with est, predVal, dat, startStopDays,initVals, and nlmOut
#' @examples
#' xx <- fitBeta(cDat[cDat$population=="Quillayute",])
#' @export
fitBetas <- function(dat, popNames=NULL, startStopDays=NULL, initVals=NULL){
  if(is.null(popNames)) popNames=unique(dat$population)
  if(is.null(startStopDays)) startStopDays <- as.numeric(julian(mdy(c("10/15/1954","6/1/1955")),origin=mdy("1/1/1955")))
  if(is.null(initVals)) initVals=c(2,2,5)

  N <- length(popNames)
  fitList <- vector("list", N)
  estList <- vector("list", N)
  predVals <- vector("list", N)
  for(i in 1:N){
    fb1 <- fitBeta(dat[dat$population==popNames[i],],startStopDays=startStopDays,initVals=initVals)
    fitList[[i]] <- fb1
    estList[[i]] <- fb1$est
    fb1$predVal$population <- popNames[i]
    predVals[[i]] <- fb1$predVal
  }
  predVals <- Reduce(rbind,predVals)
  names(fitList) <- popNames
  list(fitList=fitList,estList=estList, predVals=predVals, data=dat, startTopDays=startStopDays, popNames=popNames, initVals=initVals)
}
