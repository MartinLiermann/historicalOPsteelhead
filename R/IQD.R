#' the number of days between the point when 25% and 75% of the population has past (inter-quartile-range)
#'
#' @param est a vector with the three fitted beta parameters
#' @param startStopDays
#'
#' @return a value or vector of values
#' @export
IQD <- function(est,startStopDays){
  dd <- startStopDays
  q1 <- qJ(est,0.25,startStopDays)
  q2 <- qJ(est,0.75,startStopDays)
  q2-q1
}
