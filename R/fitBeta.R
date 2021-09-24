#' fitBeta is a function to estimat run timing by fitting a beta distribution to CPUE vs time data.
#'     julian needs to centered around day 1, So negative values from around july through december
#' @param dat data frame. Needs a CPUE and julian column
#' @param startStopDays a vector with a start and stop dates for the run
#' @param initVals initial values for the alpha, beta and constant multiplier parameters.
#'
#' @return a list with est, predVal, dat, startStopDays,initVals, and nlmOut
#' @examples
#' xx <- fitBeta(cDat[cDat$population=="Quillayute",])
#' @export
fitBeta <- function(dat, startStopDays=NULL, initVals=NULL){

  if(is.null(startStopDays)) startStopDays=as.numeric(julian(mdy(c("10/15/1954","6/1/1955")),origin=mdy("1/1/1955")))
  if(is.null(initVals)) initVals=c(2,2,5)

  # function to fit the timing curve (beta density function in this case)
  ssFit <- function(p,dat,dd){
    x <- (dat$julian-dd[1])/(dd[2]-dd[1])
    sum((dbeta(x,p[1],p[2])*p[3] - dat$CPUE)^2)
  }

  # start stop dates
  dd <- startStopDays

  # fit for all of the populations
  xx <- seq(dd[1],dd[2],by=0.25)
  xVals <- (xx-dd[1])/(dd[2]-dd[1])
  xLen <- length(xx)
  m1 <- nlm(ssFit,p=initVals,dat=dat,dd=dd)
  est <- m1$estimate
  adjDat <- (dat$julian-dd[1])/(dd[2]-dd[1])
  dat$pred <- dbeta(adjDat,est[1],est[2])*est[3]
  dat$resid <- dat$CPUE - dat$pred
  predVal <- data.frame(date=xx, pred=dbeta(xVals,est[1],est[2])*est[3], population=NA)

  list(est=est, predVal=predVal, data=dat, startStopDays=startStopDays,initVals=initVals, nlmOut=m1)
}
