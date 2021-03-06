---
title: "Supplemental Material"
date: "9/13/2021"
output: 
  html_document:
    toc: TRUE
    toc_float: TRUE
    code_folding: hide
    toc_depth: 4
    keep_md: true
params:
  runBootstrap: FALSE
  workDir: "C:/Users/martin.liermann/Documents/GitHub/HistoricalOPsteelhead"
---

# Introduction

This document provides R code and data used for the analyses in the paper *Historical migration timing and abundance of Winter Steelhead (Oncorhynchus mykiss) in Olympic Peninsula rivers, Washington State, USA*. The GitHub repository **martinLiermann/historicalOPsteelhead** contains the .Rmd file used to generate this html file along with the necessary R functions and data. If you create a local version of the repository you should be able to knit the `analysis.Rmd` file (used to create this document). You will need to change the `workDir` parameter in the header to correspond to your working directory and make sure the following libraries are installed (dplyr, tidyr, lubridate, ggplot2, and scales). If you want to rerun the bootstrap analysis you will also have to change the parameter `runBootstrap` in the header to `TRUE`. We used R version 4.0.2.

Here we have included some limited descriptions of the methods in text and as comments in the embedded code. However, the manuscript should be consulted as the definitive description of the methods.

There may some small discrepancies between this document and the manuscript due to random elements to some of the algorithms (e.g. bootstrap).

# Run timing


```r
# set the random number generator seed
set.seed(123)

# load libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)

# define some constants
startStopDates <- mdy(c("10/15/1954","6/1/1955"))
startStopDays <- as.numeric(julian(startStopDates,origin=mdy("1/1/1955")))
dd <- startStopDays # short name for more compact code
monthNam <- c("aug","sep","oct","nov","dec","jan","feb","mar","apr","may","june","jul","aug")
monthPos <- as.numeric(julian(mdy(c("8/1/1954","9/1/1954","10/1/1954","11/1/1954","12/1/1954","1/1/1955","2/1/1955","3/1/1955","4/1/1955","5/1/1955","6/1/1955","7/1/1955","8/1/1955")),origin=mdy("1/1/1955")))

# Different numeric descriptions for the first day of each month
bMons <- mdy(c("10/1/1954","11/1/1954","12/1/1954","1/1/1955","2/1/1955","3/1/1955","4/1/1955","5/1/1955","6/1/1955"))
bMonNam <- format(bMons,"%b")
bMonPosJ <- as.numeric(julian(bMons,origin=mdy("1/1/1955")))
# This is the first of each month as a proportion of the interval from dd[1] and dd[2]
# This is used when calculating statistics using the fitted beta distribution
bMonPos <- (bMonPosJ-dd[1])/(dd[2]-dd[1])

popNames <- c("Quillayute","Hoh","Queets","Quinalt")
currPopNames <- c("Quillayute","Hoh","Queets")

col1 <- rgb(0.0,0.2,0.8,0.5) # blue
col2 <- rgb(0.0,0.8,0.2,0.5) # green
col3 <- rgb(0.8,0.2,0.0,0.5) # red

# work directory
workDir <- params$workDir

# source functions in the R directory
funcList <- c("bootBetaTS.R","fitBeta.R","fitBetas.R","IQD.R","pj.R","qJ.R")
for(fn in funcList) source(paste(workDir,"R",fn,sep="/"))
```

We fit the beta density function to CPUE data to estimate run timing for the current and historical periods. For the current period we estimated run timing for the hatchery and wild origin fish. We assume that fish can begin arriving on October 15 and the run has ended on June  1.

We have historical CPUE estimates by week for all four populations. The CPUE data is available as an average across several years between 1956 and 1963. The specific years vary by population (Table S1).


```r
cpueYrTab <- data.frame(Population=c("Quillayute","Hoh","Queets","Quinalt"), Years=c("1958-1963","1956-1959; 1961-1963","1957-1963","1956-1959; 1961-1963"))
knitr::kable(cpueYrTab, caption="Table S1. The years used in calculating average historical CPUE estimates.")
```



Table: Table S1. The years used in calculating average historical CPUE estimates.

|Population |Years                |
|:----------|:--------------------|
|Quillayute |1958-1963            |
|Hoh        |1956-1959; 1961-1963 |
|Queets     |1957-1963            |
|Quinalt    |1956-1959; 1961-1963 |
 
We have current CPUE data for the Quillayute, Hoh and Queets rivers. We restricted our analysis to 1997-2010, a period when data was available for all populations and during which effort appeared to be relatively consistent. Because some weeks were only fished in a subset of these years, we used a 2 way ANOVA on the log scale (main effects for week and year) to calculate an average year of weekly values comparable to what is available for the historical data.


```r
# load all of the the CPUE data (historical and current)
CPUEdat <- read.csv(file=paste(workDir,"data/cpueDatAvg.csv",sep="/"))

## Fit the beta density function to the HISTORICAL CPUE data
histDat <- CPUEdat[CPUEdat$period=="Historic",c("population","julian","CPUE","date")]
bHist <- fitBetas(histDat, popNames=popNames)
estListB <- bHist$estList
names(estListB) <- popNames
predValsB <- bHist$predVals

## Fit the beta density function to CURRENT CPUE data
currDat <- CPUEdat[CPUEdat$period=="Current" &
                   CPUEdat$HW=="Wild",
                   c("population","julian","CPUE","date")]
bCurr <- fitBetas(currDat, popNames=currPopNames)
bCurrW <- bCurr
estListBC <- bCurr$estList
names(estListBC) <- currPopNames
predValsBC <- bCurr$predVals
predValsBC$HW <- "Wild"

### now for the CURRENT hatchery data
currDat <- CPUEdat[CPUEdat$period=="Current" &
                   CPUEdat$HW=="Hatchery",
                   c("population","julian","CPUE","date")]
bCurr <- fitBetas(currDat, popNames=currPopNames)
bCurrH <- bCurr
estListBCH <- bCurr$estList
names(estListBCH) <- currPopNames
predValsBCH <- bCurr$predVals
predValsBCH$HW <- "Hatchery"

# Now combine all of the predicted values into a single data frame.
predValsBC <- rbind(predValsBC,predValsBCH)
predValsBC$period <- "Current"
predValsB$HW <- "Wild"
predValsB$period <- "Historic"
predVals <- rbind(predValsB,predValsBC)
```


## Run timing plots


```r
CPUEdat$category <- paste(CPUEdat$period,CPUEdat$HW,sep="-")
predVals$category <- paste(predVals$period,predVals$HW,sep="-")
ggplot(CPUEdat,aes(x=julian,y=CPUE, color=category)) +
  geom_point() +
  geom_line(data=predVals,aes(x=date,y=pred)) +
  scale_x_continuous(limits=dd,breaks=monthPos, labels=monthNam) +
  facet_grid(population~.,scales="free_y") +
  theme_bw()
```

![Figure S1. All of the CPUE data for the historical and current periods with fitted beta distributions. For the current period there is separate CPUE data for hatchery and wild fish. Quinalt data from the current period was not available.](analysis_files/figure-html/unnamed-chunk-1-1.png)

## Run timing comparisons

We compare timing for the current and historical wild runs and for the current hatchery and wild runs. We use four different metrics, 1) the Julian date at which 25% of the run has past (**q25**), 1) the Julian date at which 50% of the run has past (**q50**) ,3) the days elapsed between when 25% and 75% of the run passes (inter quartile range, **IQR**), and 4) the % of the run that has passed by January 1st (**pJan1**). The comparisons are made by subtracting the historical from current values or the wild from the hatchery. For example, $IQR_{diff} = IQR_{current} - IQR_{historical}$ or $IQR_{diff} = IQR_{hatchery} - IQR_{wild}$.

The estimates of the difference are uncertain due to a number of factors. These include, the fit of the beta function to the CPUE data and the assumption that the relationship between CPUE and run size stays the same across months (that is, runsize = q*CPUE where efficiency, q, does not change across months).

Here we account for uncertainty introduced through fitting the beta distribution to the CPUE data by constructing 95% bootstrap confidence intervals. We calculate the residuals for the relationship and then use the bootstrap procedure to construct a bootstrap distribution for the parameters of interest through repeated fitting to simulated data. To account for auto-correlation in the residuals, we used a parametric bootstrap where residuals were drawn from an auto-regressive time series model fit to the observed residuals. Bootstrap confidence intervals should be viewed as approximate because sample sizes are relatively small, capture efficiency may change during the fishing season, and start and stop dates for Steelhead migration were presumed. Because the bootstrap involves random draws from a distribution, there may be slight differences in the confidence bounds each time the procedure is repeated. The bootstrap is time consuming. Therefore, we store the results and only repeat the analysis when the `runBootstrap` parameter is `TRUE` in the header of the Rmd file used to generate this document.


```r
# only run if params$runBoostrap = TRUE
# bootstrap CIs for difference in quantities.
# first calculate a matrix of estimators for all populations and periods of interest.
diffPops <- currPopNames
diffTab <- data.frame(Population=diffPops)
diffTab$q25 <- NA
diffTab$q25.lci <- NA
diffTab$q25.uci <- NA
diffTab$q50 <- NA
diffTab$q50.lci <- NA
diffTab$q50.uci <- NA
diffTab$pJan1 <- NA
diffTab$pJan1.lci <- NA
diffTab$pJan1.uci <- NA
diffTab$IQR <- NA
diffTab$IQR.lci <- NA
diffTab$IQR.uci <- NA

bsTab <- rbind(diffTab,diffTab)
bsTab <- cbind(period=rep(c("Historic","Current"),each=3),bsTab)

bsSims <- 10000
ss <- startStopDays
bCI <- function(x) quantile(x,prob=c(0.025,0.975))
for(pop in diffPops){
  
  # generate bootstrap samples for historical and current periods
  bootHist <- bootBetaTS(bHist$fitList[[pop]],sims=bsSims)
  bootCurr <- bootBetaTS(bCurrW$fitList[[pop]],sims=bsSims)
  
  # calculate estimates and bootstrap CI's for each of the different metrics
  #    for each period and between period difference
  
  ## q25
  bsTab$q25[bsTab$Population==pop & bsTab$period=="Historic"] <- 
    qJ(bootHist$est,0.25,ss)
  bsTab$q25[bsTab$Population==pop & bsTab$period=="Current"] <- 
    qJ(bootCurr$est,0.25,ss)
  bsTab[bsTab$Population==pop & bsTab$period=="Historic",c("q25.lci","q25.uci")] <- 
    bCI(qJ(bootHist$estList,0.25,ss)) 
  bsTab[bsTab$Population==pop & bsTab$period=="Current",c("q25.lci","q25.uci")] <- 
    bCI(qJ(bootCurr$estList,0.25,ss))
  diffTab$q25[diffTab$Population==pop] <- 
    qJ(bootCurr$est,0.25,ss) - qJ(bootHist$est,0.25,ss)
  diffTab[diffTab$Population==pop,c("q25.lci","q25.uci")] <- 
    bCI(qJ(bootCurr$estList,0.25,ss) - qJ(bootHist$estList,0.25,ss))
  
  ## q50
  bsTab$q50[bsTab$Population==pop & bsTab$period=="Historic"] <- 
    qJ(bootHist$est,0.50,ss)
  bsTab$q50[bsTab$Population==pop & bsTab$period=="Current"] <- 
    qJ(bootCurr$est,0.50,ss)
  bsTab[bsTab$Population==pop & bsTab$period=="Historic",c("q50.lci","q50.uci")] <- 
    bCI(qJ(bootHist$estList,0.50,ss)) 
  bsTab[bsTab$Population==pop & bsTab$period=="Current",c("q50.lci","q50.uci")] <- 
    bCI(qJ(bootCurr$estList,0.50,ss))
  diffTab$q50[diffTab$Population==pop] <- 
    qJ(bootCurr$est,0.50,ss) - qJ(bootHist$est,0.50,ss)
  diffTab[diffTab$Population==pop,c("q50.lci","q50.uci")] <- 
    bCI(qJ(bootCurr$estList,0.50,ss) - qJ(bootHist$estList,0.50,ss))
  
  ## pJan1
  bsTab$pJan1[bsTab$Population==pop & bsTab$period=="Historic"] <- 
    pJ(bootHist$est,1,ss)
  bsTab$pJan1[bsTab$Population==pop & bsTab$period=="Current"] <- 
    pJ(bootCurr$est,1,ss)
  bsTab[bsTab$Population==pop & bsTab$period=="Historic",c("pJan1.lci","pJan1.uci")] <- 
    bCI(pJ(bootHist$estList,1,ss)) 
  bsTab[bsTab$Population==pop & bsTab$period=="Current",c("pJan1.lci","pJan1.uci")] <- 
    bCI(pJ(bootCurr$estList,1,ss))
  diffTab$pJan1[diffTab$Population==pop] <- 
    pJ(bootCurr$est,1,ss) - pJ(bootHist$est,1,ss)
  diffTab[diffTab$Population==pop,c("pJan1.lci","pJan1.uci")] <- 
    bCI(pJ(bootCurr$estList,1,ss) - pJ(bootHist$estList,1,ss))
  
  ## IQR
  bsTab$IQR[bsTab$Population==pop & bsTab$period=="Historic"] <- 
    IQD(bootHist$est,ss)
  bsTab$IQR[bsTab$Population==pop & bsTab$period=="Current"] <- 
    IQD(bootCurr$est,ss)
  bsTab[bsTab$Population==pop & bsTab$period=="Historic",c("IQR.lci","IQR.uci")] <- 
    bCI(IQD(bootHist$estList,ss)) 
  bsTab[bsTab$Population==pop & bsTab$period=="Current",c("IQR.lci","IQR.uci")] <- 
    bCI(IQD(bootCurr$estList,ss))
  diffTab$IQR[diffTab$Population==pop] <- 
    IQD(bootCurr$est,ss) - IQD(bootHist$est,ss)
  diffTab[diffTab$Population==pop,c("IQR.lci","IQR.uci")] <- 
    bCI(IQD(bootCurr$estList,ss) - IQD(bootHist$estList,ss))
}
bsTabCurrHist <- bsTab
diffTabCurrHist <- diffTab
allTabCurrHist <- rbind(cbind(period=rep("Difference",3),diffTab),bsTab)

# now calculate the values for the Current wild and hatchery values
#   Notice: We repurpose Hist and Curr to mean current natural and current hatchery

for(pop in diffPops){
  
  # generate bootstrap samples for current natural and hatchery values
  bootHist <- bootBetaTS(bCurrW$fitList[[pop]],sims=bsSims)
  bootCurr <- bootBetaTS(bCurrH$fitList[[pop]],sims=bsSims)
  
  # calculate estimates and bootstrap CI's for each of the different metrics
  # for natural and wild the difference
  
  ## q25
  bsTab$q25[bsTab$Population==pop & bsTab$period=="Historic"] <- 
    qJ(bootHist$est,0.25,ss)
  bsTab$q25[bsTab$Population==pop & bsTab$period=="Current"] <- 
    qJ(bootCurr$est,0.25,ss)
  bsTab[bsTab$Population==pop & bsTab$period=="Historic",c("q25.lci","q25.uci")] <- 
    bCI(qJ(bootHist$estList,0.25,ss)) 
  bsTab[bsTab$Population==pop & bsTab$period=="Current",c("q25.lci","q25.uci")] <- 
    bCI(qJ(bootCurr$estList,0.25,ss))
  diffTab$q25[diffTab$Population==pop] <- 
    qJ(bootCurr$est,0.25,ss) - qJ(bootHist$est,0.25,ss)
  diffTab[diffTab$Population==pop,c("q25.lci","q25.uci")] <- 
    bCI(qJ(bootCurr$estList,0.25,ss) - qJ(bootHist$estList,0.25,ss))
  
  ## q50
  bsTab$q50[bsTab$Population==pop & bsTab$period=="Historic"] <- 
    qJ(bootHist$est,0.50,ss)
  bsTab$q50[bsTab$Population==pop & bsTab$period=="Current"] <- 
    qJ(bootCurr$est,0.50,ss)
  bsTab[bsTab$Population==pop & bsTab$period=="Historic",c("q50.lci","q50.uci")] <- 
    bCI(qJ(bootHist$estList,0.50,ss)) 
  bsTab[bsTab$Population==pop & bsTab$period=="Current",c("q50.lci","q50.uci")] <- 
    bCI(qJ(bootCurr$estList,0.50,ss))
  diffTab$q50[diffTab$Population==pop] <- 
    qJ(bootCurr$est,0.50,ss) - qJ(bootHist$est,0.50,ss)
  diffTab[diffTab$Population==pop,c("q50.lci","q50.uci")] <- 
    bCI(qJ(bootCurr$estList,0.50,ss) - qJ(bootHist$estList,0.50,ss))
  
  ## pJan1
  bsTab$pJan1[bsTab$Population==pop & bsTab$period=="Historic"] <- 
    pJ(bootHist$est,1,ss)
  bsTab$pJan1[bsTab$Population==pop & bsTab$period=="Current"] <- 
    pJ(bootCurr$est,1,ss)
  bsTab[bsTab$Population==pop & bsTab$period=="Historic",c("pJan1.lci","pJan1.uci")] <- 
    bCI(pJ(bootHist$estList,1,ss)) 
  bsTab[bsTab$Population==pop & bsTab$period=="Current",c("pJan1.lci","pJan1.uci")] <- 
    bCI(pJ(bootCurr$estList,1,ss))
  diffTab$pJan1[diffTab$Population==pop] <- 
    pJ(bootCurr$est,1,ss) - pJ(bootHist$est,1,ss)
  diffTab[diffTab$Population==pop,c("pJan1.lci","pJan1.uci")] <- 
    bCI(pJ(bootCurr$estList,1,ss) - pJ(bootHist$estList,1,ss))
  
  ## IQR
  bsTab$IQR[bsTab$Population==pop & bsTab$period=="Historic"] <- 
    IQD(bootHist$est,ss)
  bsTab$IQR[bsTab$Population==pop & bsTab$period=="Current"] <- 
    IQD(bootCurr$est,ss)
  bsTab[bsTab$Population==pop & bsTab$period=="Historic",c("IQR.lci","IQR.uci")] <- 
    bCI(IQD(bootHist$estList,ss)) 
  bsTab[bsTab$Population==pop & bsTab$period=="Current",c("IQR.lci","IQR.uci")] <- 
    bCI(IQD(bootCurr$estList,ss))
  diffTab$IQR[diffTab$Population==pop] <- 
    IQD(bootCurr$est,ss) - IQD(bootHist$est,ss)
  diffTab[diffTab$Population==pop,c("IQR.lci","IQR.uci")] <- 
    bCI(IQD(bootCurr$estList,ss) - IQD(bootHist$estList,ss))
}
bsTabHatWild <- bsTab
bsTabHatWild <- cbind(Origin=rep(c("Natural","Hatchery"),each=3),bsTabHatWild[,-1])
diffTabHatWild <- diffTab
allTabHatWild <- rbind(cbind(Origin=rep("Difference",3),diffTab),bsTabHatWild)

# Save the tables as csv files
write.csv(allTabCurrHist,file=paste(workDir,"tables/BSresultsCurrHist.csv",sep="/"),row.names=FALSE)
write.csv(allTabHatWild,file=paste(workDir,"tables/BSresultsHatWild.csv",sep="/"),row.names=FALSE)
```


```r
# if the bootstrap simulations are not run, read saved results
allTabCurrHist <- read.csv(file=paste(workDir,"tables/BSresultsCurrHist.csv",sep="/"), stringsAsFactors=FALSE)
allTabHatWild <- read.csv(file=paste(workDir,"tables/BSresultsHatWild.csv",sep="/"), stringsAsFactors=FALSE)
```


```r
# create a pretty table
timingCurrHist <- allTabCurrHist %>% 
  mutate(q25 = paste(round(q25)," (",round(q25.lci),",",round(q25.uci),")",sep=""),
         q50 = paste(round(q50)," (",round(q50.lci),",",round(q50.uci),")",sep=""),
         pJan1 = paste(round(100*pJan1)," (",round(100*pJan1.lci),",",round(100*pJan1.uci),")",sep=""),
         IQR = paste(round(IQR)," (",round(IQR.lci),",",round(IQR.uci),")",sep="")) %>%
  select(Period=period,Population,q25,q50,pJan1,IQR)
knitr::kable(timingCurrHist,caption="Table S2. Metrics describing run timing. Values for the current period, historical period and the difference between the current and historical periods. The upper and lower limits for 95% confidence intervals are included in parentheses.")
```



Table: Table S2. Metrics describing run timing. Values for the current period, historical period and the difference between the current and historical periods. The upper and lower limits for 95% confidence intervals are included in parentheses.

|Period     |Population |q25           |q50         |pJan1         |IQR           |
|:----------|:----------|:-------------|:-----------|:-------------|:-------------|
|Difference |Quillayute |33 (24,40)    |25 (17,33)  |-18 (-23,-14) |-16 (-29,-6)  |
|Difference |Hoh        |71 (62,82)    |61 (47,71)  |-43 (-51,-35) |-26 (-52,-11) |
|Difference |Queets     |63 (24,86)    |54 (-46,70) |-31 (-42,-16) |-22 (-82,4)   |
|Historic   |Quillayute |1 (-5,7)      |35 (29,43)  |25 (21,29)    |69 (60,80)    |
|Historic   |Hoh        |-25 (-36,-16) |7 (-2,21)   |45 (37,53)    |69 (54,95)    |
|Historic   |Queets     |-10 (-33,28)  |24 (10,124) |33 (18,44)    |70 (46,130)   |
|Current    |Quillayute |33 (28,39)    |61 (56,65)  |7 (4,9)       |52 (47,58)    |
|Current    |Hoh        |46 (44,48)    |68 (67,70)  |2 (2,3)       |43 (41,45)    |
|Current    |Queets     |53 (46,60)    |79 (74,84)  |2 (1,4)       |48 (42,56)    |


```r
# create a pretty table
timingHatWild <- allTabHatWild %>% 
  mutate(q25 = paste(round(q25)," (",round(q25.lci),",",round(q25.uci),")",sep=""),
         q50 = paste(round(q50)," (",round(q50.lci),",",round(q50.uci),")",sep=""),
         pJan1 = paste(round(100*pJan1)," (",round(100*pJan1.lci),",",round(100*pJan1.uci),")",sep=""),
         IQR = paste(round(IQR)," (",round(IQR.lci),",",round(IQR.uci),")",sep="")) %>%
  select(Origin,Population,q25,q50,pJan1,IQR)
knitr::kable(timingHatWild,caption="Table S3. Metrics describing run timing. Values for the natural and hatchery origin run timing, and the difference between the natural and hatchery origin run timing metrics. The upper and lower limits for 95% confidence intervals are included in parentheses.")
```



Table: Table S3. Metrics describing run timing. Values for the natural and hatchery origin run timing, and the difference between the natural and hatchery origin run timing metrics. The upper and lower limits for 95% confidence intervals are included in parentheses.

|Origin     |Population |q25           |q50           |pJan1      |IQR           |
|:----------|:----------|:-------------|:-------------|:----------|:-------------|
|Difference |Quillayute |-67 (-74,-60) |-79 (-84,-74) |71 (66,76) |-21 (-28,-14) |
|Difference |Hoh        |-72 (-74,-69) |-77 (-79,-76) |62 (61,64) |-8 (-10,-6)   |
|Difference |Queets     |-71 (-79,-63) |-86 (-92,-80) |68 (59,77) |-27 (-36,-18) |
|Natural    |Quillayute |33 (28,39)    |61 (56,65)    |7 (4,9)    |52 (47,58)    |
|Natural    |Hoh        |46 (44,48)    |68 (67,70)    |2 (2,3)    |43 (41,45)    |
|Natural    |Queets     |53 (46,59)    |79 (74,84)    |2 (1,4)    |48 (42,56)    |
|Hatchery   |Quillayute |-33 (-38,-30) |-18 (-22,-15) |78 (74,82) |32 (27,37)    |
|Hatchery   |Hoh        |-25 (-27,-24) |-9 (-10,-8)   |64 (63,66) |35 (34,37)    |
|Hatchery   |Queets     |-18 (-22,-14) |-7 (-11,-4)   |70 (62,79) |21 (17,27)    |

# Historical runsize estimates

We used four different methods to estimate historical run size. Please see the manuscript for details.

## 1923 Queets cannery records 

The formula we used to expand the cannery records into total run size, $N$, is,

$$N= \frac {Cwt}{(1-Wa) \times STw \times R}$$

where, $Cwt$ represents the total weight of fish in all the cases combined, $Wa$ is the wastage rate, $STw$ is the average weight of Queets River Winter Steelhead, and $R$ is the harvest rate.

Each case contained 48 cans at 1 lb (~0.45 kg) per can. Canned fish weight is a fraction of the total fish weight at capture because non-essential fish parts (e.g., head, tail, and internal organs) were removed prior to canning. To account for this loss, we used a wastage rate ($Wa$) of 0.40.  The average historical weight of Queets Winter Steelhead, $STw$, was (9.8 lbs or ~4.4 kg; WDFW 1934). We used contemporary wild Winter Steelhead harvest rates to estimate the fraction of the total run harvested by the cannery ($R$). Because catch expansion estimates are sensitive to assumed harvest rates, we used a range of plausible values for R (0.28, 0.38, and 0.44) corresponding to the 25%, 50% (median), and 75% quantiles of contemporary annual harvest rates for Queets River wild Winter Steelhead.


```r
canTab <- data.frame(cases=rep(1500,3),Wst=rep(0.4,3), STw=rep(9.8,3),R=c(0.28,0.38, 0.44)) %>%
  mutate(Cwt=cases*48,N=Cwt/((1-Wst)*STw*R))

knitr::kable(canTab[,c("Cwt","Wst","STw","R","N")],digits=c(0,2,1,2,0), caption="Table S4. The values used to calculate the population size in the Queets River in 1923. Cwt is the total weight of canned fish, Wst is the wastage. STw is the average weight of a fish and R is the harvest rate. N is the estimated number of fish based on the different harvest rates.")
```



Table: Table S4. The values used to calculate the population size in the Queets River in 1923. Cwt is the total weight of canned fish, Wst is the wastage. STw is the average weight of a fish and R is the harvest rate. N is the estimated number of fish based on the different harvest rates.

|   Cwt| Wst| STw|    R|     N|
|-----:|---:|---:|----:|-----:|
| 72000| 0.4| 9.8| 0.28| 43732|
| 72000| 0.4| 9.8| 0.38| 32223|
| 72000| 0.4| 9.8| 0.44| 27829|


## 1948-1960 catch expansion

If we assume that total historical and current harvest rates are comparable, then we can use the current harvest rates to expand the historical catch. 

The formula we used for expanding historical catch to historical run size was:

$$expansion = {1 \over {RP}}$$

Where R is the assumed historical harvest rate (for tribal and sport fisheries combined) and P adjusts the estimate to account for differences in the proportion of the total run that was fished in the historical and current periods (see below for details).

#### **R**: the harvest rate

The current harvest rate used for expanding the catch is based on contemporary catch and run size data. Because, the contemporary harvest is inconsistent later in the season we truncated catch to months where fishing was consistent. This is accounted for in the **P**  parameter described below. Catch is the sum of commercial and sport catch. 



```r
# Define the julian date ranges with consistent effort for the current and historical periods
# Current Period
rngDatCur <- data.frame(
  population = c("Hoh","Queets","Quillayute","Quinalt"),
  start =c(-32,-32,-32,-32),
  stop = c(90,120,90,90)
)
# historical Period
rngDatHist <- data.frame(
  population = c("Hoh","Queets","Quillayute","Quinalt"),
  start =c(-32,-32,-32,-32),
  stop = c(59,59,59,59)
)

# read weekly tribal harvest data for current period
tcCatch <- read.csv(file=paste(workDir,"data/currentTribalCatch.csv",sep="/"),stringsAsFactors=FALSE)
tcCatch <- tcCatch %>% filter(period=="Current",HW=="Wild",year>1989) %>% droplevels()

# read annual Quinalt tribal catch for the current period 
# (don't have by week like the other pops)
quinCatch <- read.csv(file=paste(workDir,"data/quinaltCurrentTribalCatch.csv",sep="/"),stringsAsFactors=FALSE)

quinCatch <- quinCatch %>% 
  mutate(population="Quinalt",catchTr=NA) %>% 
  select(population,year,catch,catchTr)

# Now filter the tribal and sport catch to months with consistent fishing effort
# and sum to population and year
tcCatchTr <- tcCatch %>% 
  left_join(rngDatCur,by="population") %>%
  filter(julian >= start & julian < stop) %>%
  group_by(population,year) %>%
  summarize(catchTr=sum(catch))
tcCatchG <- tcCatch %>%
  group_by(population,year) %>%
  summarize(catch=sum(catch)) %>%
  left_join(tcCatchTr,by=c("population","year")) %>%
  bind_rows(quinCatch) %>%
  mutate(type="Tribal")

# load the sport harvest data for the current period
sport <- read.csv(file=paste(workDir,"data/currentSportCatch.csv",sep="/"),stringsAsFactors=FALSE)
scCatch <- sport %>% 
  filter(run=="W", MU=="U",year>1989) %>%
  select(population,year,month,catch)

# Now filter sport catch by month and sum to year and population
# Notice, to filter we used the 15th of each month since this is 
#   monthly data
scCatchTr <- scCatch %>% 
  mutate(cYear=ifelse(month<7,1955,1954),
         julian=julian(mdy(paste(month,"15",cYear,sep="/")),origin=mdy("1/1/1955"))) %>%
  left_join(rngDatCur,by="population") %>%
  filter(julian >= start & julian < stop) %>%
  group_by(population,year) %>%
  summarize(catchTr=sum(catch))
scCatchG <- scCatch %>%
  group_by(population,year) %>%
  summarize(catch=sum(catch)) %>%
  left_join(scCatchTr,by=c("population","year")) %>%
  mutate(type="Sport")

# combine the tribal and sport catch
catchG <- bind_rows(scCatchG,tcCatchG)

# calculate the average % of the catch caught during the truncation period for
# the Quillayute and the Hoh (similar fishing period to Quinalt) and then
# apply that to the Quinalt where we don't have catch broken down by month.
aTRp <- catchG %>% ungroup() %>%
  filter(population %in% c("Hoh","Quillayute"),type=="Tribal") %>%
  summarize(ratio=mean(catchTr/catch)) %>% as.numeric()
catchG$catchTr[catchG$population=="Quinalt"] <- catchG$catch[catchG$population=="Quinalt"] * aTRp

# read run size data
runDat <- read.csv(file=paste(workDir,"data/currentRunSize.csv",sep="/"),stringsAsFactors=FALSE)
runDat <- runDat %>% 
  filter(population %in% c("Hoh","Queets","Quinalt","Quillayute")) %>%
  select(population,year,runSize)

# combine run size data with catch data and calculate harvest rates
hrDat <- catchG %>% 
  pivot_wider(id_cols=c("population","year"),names_from="type",values_from=c("catch","catchTr")) %>%
  left_join(runDat,by=c("population","year")) %>%
  mutate(HR=(catchTr_Sport+catchTr_Tribal)/runSize,
         HRut=(catch_Sport+catch_Tribal)/runSize) 

# summarize the harvest rate data 
RRsum <- hrDat %>% group_by(population) %>% mutate(harvestRate=HR) %>%
  filter(year > 1995, year< 2016) %>%
  summarize(q25=quantile(harvestRate,prob=0.25,na.rm=TRUE),
            q50=quantile(harvestRate,prob=0.5,na.rm=TRUE),
            q75=quantile(harvestRate,prob=0.75,na.rm=TRUE),
            mean=mean(harvestRate,na.rm=TRUE),
            sd=sd(harvestRate,na.rm=TRUE))

# create a data frame with the population specific estimates and bounds
Rsum <- data.frame(Population=RRsum$population,Parameter=rep("R",4),
                   Estimate=RRsum$q50,LowerValue=RRsum$q25,UpperValue=RRsum$q75)
```

The median (q50) harvest rates for the current period range from about 0.23 to 0.39. The 25% and 75% quantiles range from 0.19 to 0.33 and 0.26 to 0.47, respectively.


```r
# Display the results with a table
knitr::kable(RRsum,digits=2,caption="Table S5. Summary of truncated harvest rates for the current period. q25,q50 and q75 represent the quartiles and median of the rates for the years 1996-2015.")  
```



Table: Table S5. Summary of truncated harvest rates for the current period. q25,q50 and q75 represent the quartiles and median of the rates for the years 1996-2015.

|population |  q25|  q50|  q75| mean|   sd|
|:----------|----:|----:|----:|----:|----:|
|Hoh        | 0.19| 0.24| 0.35| 0.26| 0.11|
|Queets     | 0.23| 0.31| 0.39| 0.31| 0.12|
|Quillayute | 0.20| 0.23| 0.26| 0.24| 0.06|
|Quinalt    | 0.33| 0.39| 0.47| 0.39| 0.10|



```r
### Combining historical sport and tribal catch and creating catches limited to Dec-Feb

# read the early tribal catch data by month and winter/summer
tDat <- read.csv(paste(workDir,"/data/historicTribalCatch.csv",sep=""),stringsAsFactors=FALSE) %>% filter(run=="winter", year %in% 1948:1960)

# limit monthly data to Dec,Jan,Feb. 
tribalDat <- tDat %>%  filter(month %in% c("Dec","Jan","Feb")) %>%
  # rename fish to catchLim (i.e. limited to Dec-Feb)
  rename(catchLim=fish) %>% 
  # join with the full data (so you have all months again)
  full_join(tDat,by=c("stream","year","month")) %>% 
  # now aggregate by population and year
  group_by(population=stream,year) %>%
  # create two summaries, one with the limited catch and the other with the unlimited catch
  summarize(tribalCatchLim=sum(catchLim,na.rm=TRUE),tribalCatch=sum(fish)) 

### Now combine the tribal data with the sport data and create total catch
# read in sport catch
sacDat <- read.csv(paste(workDir,"data/historicSportCatch.csv",sep="/"),stringsAsFactors=FALSE) 
# now join sport catch with tribal catch
#  create a total catch where the proportion of catch in Dec-Feb for
#  the tribal catch is applied to the sport catch (since we don't have monthly sport data)
catchDatW <- sacDat %>% 
  filter(marked=="no",year %in% 1948:1960) %>%
  select(!marked) %>%
  # put in long form
  pivot_longer(cols=c("Quillayute","Queets","Hoh","Quinalt"),names_to="population", values_to="sportCatch") %>%
  # combine with the tribal catch data
  left_join(tribalDat,by=c("year","population")) %>%
  # create an estimate sport catch limited to Dec-Feb (based on tribal proportion)
  mutate(sportCatchLim=sportCatch*tribalCatchLim/tribalCatch) %>%
  # create a total catch data that is tribalCatchLim + sportCatchLim
  mutate(catchLim=sportCatchLim + tribalCatchLim) %>%
  # create a total catch data that is tribalCatch + sportCatch
  mutate(catchTot=sportCatch + tribalCatch)
```

#### **P**: Proportion of potential catch


```r
# calculate the estimated percentage of the run in each month 
# use the fitted beta distributions
# for all four rivers during the historical period
nn <- length(bMonNam)
pTab <- data.frame(Month=c(bMonNam[-nn],"Dec-Feb"))
for(i in 1:length(popNames)){
  pop <- popNames[i]
  est <- estListB[[pop]]
  pTab[1:(nn-1),pop] <- pbeta(bMonPos[-1],est[1],est[2])*100
  pTab[nn,pop] <- pTab[pTab$Month=="Feb",pop] - pTab[pTab$Month=="Nov",pop]
}
pTabHist <- pTab
```





```r
# initialize the table
ppTab <- data.frame(population=popNames,pHist=rep(NA,4),pCurr=rep(NA,4),P=rep(NA,4))

# add the historical P values
for(pop in popNames){
  ppTab$pHist[ppTab$population==pop] <- pTabHist[pTabHist$Month=="Dec-Feb",pop]
}

# add current P values for the Quillayute, Hoh and Queets taking into account the spotty effort in different months

for(pop in currPopNames){
  tTab <- pTabAll[pTabAll$pop==pop,]
  if(pop=="Queets"){
    # Dec - Apr
    ppTab$pCurr[ppTab$population=="Queets"] <- 
      tTab[tTab$Month=="Apr","currentCummulative"] -
      tTab[tTab$Month=="Nov","currentCummulative"]
  }else{
    # Dec - Mar
    ppTab$pCurr[ppTab$population==pop] <- 
      tTab[tTab$Month=="Mar","currentCummulative"] -
      tTab[tTab$Month=="Nov","currentCummulative"]
  }
}

# Use the average pCurr from the Hoh and Quillayute (same truncation period)
# as an estimate for the Quinalt
ppTab$pCurr[ppTab$population=="Quinalt"] <- 
  mean(ppTab$pCurr[ppTab$population %in% c("Hoh","Quillayute")],na.rm=TRUE)

# calculate the ratio of historical to current P values 
# This is the P (confusingly) used for the harvest rate expansion analysis
ppTab$P <- ppTab$pHist/ppTab$pCurr

# in a form for the larger table
pp <- ppTab$P
Psum <- data.frame(Population=ppTab$population,Parameter=rep("P",4), Estimate=pp,LowerValue=pp*0.8,UpperValue=pp*1.2)
```

When estimating the relative abundance for current and historical time periods using harvest rates, we need to take into account the proportion of the total run timing that was fished in each period, since it appears that during the historical period, a smaller proportion of the total run timing was fished. The approach we take is to truncate the historical and current catches to months during which fishing was relatively consistent and then account for this in the historical catch expansion by using the estimated run time distributions to calculate the proportion of the total run that was fished during these truncated periods. In the historical period, catches in March and April are difficult to interpret due to sporadic effort during these months. Therefore, we truncate the catch series by only including catches from December through February. The proportion of run timing fished therefore only includes those months. Notice that this value,$P_{hist}$, is relatively consistent across populations, averaging 61% (see Table S6). 

For the current period we have weekly catches for the Quillayute, Hoh and Queets rivers. Fishing is relatively consistent from the beginning of December through the end of March for the Hoh and Quillayute and from the beginning of December though the end of April for the Queets. For the Quinalt river, where no weekly or monthly current CPUE data was available, We used the average value of $P_{curr}$ from Hoh and Quillayute since we also adjusted catch for the Quinalt based on these two rivers.

Dividing $P_{hist}$ by $P_{curr}$ to produce $P$ resulted in values less than less than 1 for all populations (Table S6).


```r
ppTab2 <- ppTab %>% mutate(pHist=pHist/100,pCurr=pCurr/100)
knitr::kable(ppTab2,digits=c(NA,3,3,3), caption="Table S6. Estimates of the proportion of the run that was fished consistently for the historical (pHist) and current (pCurr) periods. P is the pHist/pCurr.")
```



Table: Table S6. Estimates of the proportion of the run that was fished consistently for the historical (pHist) and current (pCurr) periods. P is the pHist/pCurr.

|population | pHist| pCurr|     P|
|:----------|-----:|-----:|-----:|
|Quillayute | 0.597| 0.780| 0.766|
|Hoh        | 0.625| 0.759| 0.824|
|Queets     | 0.622| 0.913| 0.681|
|Quinalt    | 0.578| 0.769| 0.751|

Here are The $R$ and $P$ values used to expand the truncated historical catch. The upper and lower values for $R$ represent the 25% and 75% quantiles and the upper and lower values for P are calculated by multipling the estimate by 0.8 and 1.2. In both cases these bounds represent rough guesses of the range of plausible values. 



```r
pC <- c("Population","Parameter","Estimate","LowerValue","UpperValue")
paramVals <- rbind(Rsum[pC],Psum[pC])

# combine with the parameter values (P and R) 
paramVals2 <- paramVals %>% filter(Parameter %in% c("R","P")) %>% 
  rename(population=Population) %>% pivot_wider("population",names_from=Parameter,values_from=c("Estimate","LowerValue","UpperValue")) 

# calculate historical run size estimates based on parameter values and truncated catch
totDat <- catchDatW %>%
  left_join(paramVals2, by="population") %>% 
  mutate(expansion = 1/(Estimate_P*Estimate_R),
         runSizeEst = catchLim/(Estimate_P*Estimate_R),
         runSizeUp = catchLim/(LowerValue_P*LowerValue_R),
         runSizeLow = catchLim/(UpperValue_P*UpperValue_R))

# write the historical catch data and runsize estimates
write.csv(totDat,file=paste(workDir,"tables/historicalCatchExpansion.csv",sep="/"),row.names=FALSE)
```



```r
# create a "pretty" table of R and P values 
rv <- 2
paramTab <- paramVals2 %>% 
  mutate(Population=population,
         R=paste(round(Estimate_R,rv)," (",round(LowerValue_R,rv),",",round(UpperValue_R,rv),")",sep=""),
         P=paste(round(Estimate_P,rv)," (",round(LowerValue_P,rv),",",round(UpperValue_P,rv),")",sep="")) %>%
  select(Population,R,P)
knitr::kable(paramTab[c(3,1,2,4),],caption="Table S7. Values of P and R used to expand the historical catch. Values in parentheses represent upper and lower bounds.")
```



Table: Table S7. Values of P and R used to expand the historical catch. Values in parentheses represent upper and lower bounds.

|Population |R                |P                |
|:----------|:----------------|:----------------|
|Quillayute |0.23 (0.2,0.26)  |0.77 (0.61,0.92) |
|Hoh        |0.24 (0.19,0.35) |0.82 (0.66,0.99) |
|Queets     |0.31 (0.23,0.39) |0.68 (0.54,0.82) |
|Quinalt    |0.39 (0.33,0.47) |0.75 (0.6,0.9)   |



```r
# Mean and standard deviation of the annual number of wild winter steelhead caught from 1948-1960 in commercial and recreational fisheries in the Quillayute, Hoh, Queets and Quinault rivers and mean annual winter steelhead population size estimated by expanding the catch to account for harvest rates and the proportion of the steelhead run that was fished. Catch numbers include only those fish caught from December 1 - February 28 due to inconsistent fishing effort outside of this period across years. Upper and lower bounds for population size estimates are based on lower and upper bounds for parameters R and P given in Table 3.
estTab <- totDat %>% filter(year %in% 1948:1960) %>%
  group_by(population) %>%
  summarize(CatchLim=mean(catchLim,na.rm=TRUE),
            CatchLimSD=sd(catchLim,na.rm=TRUE),
            Estimate=mean(runSizeEst,na.rm=TRUE),
            EstLower=mean(runSizeLow,na.rm=TRUE),
            EstUpper=mean(runSizeUp,na.rm=TRUE),
            min=min(runSizeEst,na.rm=TRUE),
            max=max(runSizeEst,na.rm=TRUE)) %>% ungroup()
popOrd <- c("Quillayute","Hoh","Queets","Quinalt")
knitr::kable(estTab[c(3,1,2,4),],digits=c(NA,0,0,0,0,0,0,0), caption="Table S8. Historical run sizes estimates based on the expansion of historical catches. Estimate represent the estimate average across years. EstLower and EstUpper represent the average estimate based on the lower and upper bounds for R and P. The min and max columns represent the smallest and largest estimates across the year range.",format.args = list(big.mark = ",",
  scientific = FALSE))
```



Table: Table S8. Historical run sizes estimates based on the expansion of historical catches. Estimate represent the estimate average across years. EstLower and EstUpper represent the average estimate based on the lower and upper bounds for R and P. The min and max columns represent the smallest and largest estimates across the year range.

|population | CatchLim| CatchLimSD| Estimate| EstLower| EstUpper|   min|    max|
|:----------|--------:|----------:|--------:|--------:|--------:|-----:|------:|
|Quillayute |    3,970|      1,337|   22,567|   16,733|   31,591| 6,702| 34,757|
|Hoh        |    3,138|        885|   15,923|    9,023|   24,901| 7,118| 24,684|
|Queets     |    4,187|      2,347|   19,875|   13,025|   32,878| 6,191| 52,200|
|Quinalt    |    3,977|      2,313|   13,743|    9,345|   20,258| 7,475| 30,332|

## 1955-1963 CPUE-based estimates


```r
yrRange <- list(Quillayute=1997:2010, Hoh=1997:2010, Queets=1997:2010, Quinalt=1997:2010)
qTab <- data.frame(pop=popNames,currentRunSize=rep(NA,4),
                   cpueTotCurr=rep(NA,4),cpueTotHist=rep(NA,4))
for(pop in popNames){
  # current runsize
  qTab$currentRunSize[qTab$pop==pop] <- 
    mean(runDat$runSize[runDat$population==pop & runDat$year %in% yrRange[[pop]]])
  # historical CPUE (area under the beta function)
  estH <- estListB[[pop]]
  qTab$cpueTotHist[qTab$pop==pop] <- estH[3]*(dd[2]-dd[1])
  # current CPUE (area under the beta function)
  if(pop!="Quinalt"){ # don't have current CPUE data for the Quinalt
    estC <- estListBC[[pop]]
    qTab$cpueTotCurr[qTab$pop==pop] <- estC[3]*(dd[2]-dd[1])
  }else{
    qTab$cpueTotCurr[qTab$pop==pop] <- NA
  }
}
# use the estimated current CPUE for Quillayute and Queets to 
# calculate an average current CPUE
# notice that we did not use the Hoh CPUE values
# because they were very high for the current period
# not sure if this is due to better fishing conditions
# or reporting methods
qTab$q <- qTab$cpueTotCurr/qTab$currentRunSize
qAvg <- mean(qTab$q[qTab$pop %in% c("Queets","Quillayute")])
qTab$qAvg <- qAvg
qTab$RSav.1 <- qTab$cpueTotHist/qAvg
qTab$RSav.2 <- 2*qTab$cpueTotHist/qAvg
qTab$RSav.3 <- 3*qTab$cpueTotHist/qAvg
```

Since catch is equal to efficiency times effort times run size ($C=EqR$), run size is catch per unit effort divided by efficiency ($R=C/(Eq)=CPUE/q$). Since we have catch per unit effort for the historical period, this means that if we can estimate efficiency, we can estimate the historical run size. While historical efficiency data is not available, we can estimate efficiency for the Quillayute, Hoh and Queets in the current period since we have catch, effort and run size data ($q=C/(ER)$). Using average values for the current period (see Table below). We would expect current efficiency to be higher than historical efficiency due to improvements in gear and access. We assume current efficiency is 1, 1.5 and 2 times the historical efficiency. We use the year range 1997-2010 for the current period.



```r
knitr::kable(qTab,caption="Table S9. CPUE based estimates of historical run size. RSav.1 is historical runsize assuming efficiencies are the same and RSav.2 and RSav.3 are historical run sizes assuming that efficiency is 2 and 3 times higher in the current period. The current efficiency used for all populations is the average of the Quillayute and Queets efficiencies.", digits=c(NA,0,0,0,3,3,0,0,0),format.args = list(big.mark = ",",
  scientific = FALSE))
```



Table: Table S9. CPUE based estimates of historical run size. RSav.1 is historical runsize assuming efficiencies are the same and RSav.2 and RSav.3 are historical run sizes assuming that efficiency is 2 and 3 times higher in the current period. The current efficiency used for all populations is the average of the Quillayute and Queets efficiencies.

|pop        | currentRunSize| cpueTotCurr| cpueTotHist|     q|  qAvg| RSav.1| RSav.2| RSav.3|
|:----------|--------------:|-----------:|-----------:|-----:|-----:|------:|------:|------:|
|Quillayute |         14,635|         754|         834| 0.052| 0.071| 11,695| 23,391| 35,086|
|Hoh        |          3,925|       1,882|         505| 0.479| 0.071|  7,080| 14,160| 21,240|
|Queets     |          6,445|         587|         483| 0.091| 0.071|  6,776| 13,553| 20,329|
|Quinalt    |          5,142|          NA|         793|    NA| 0.071| 11,113| 22,226| 33,339|

## Watershed size

Here we examine the relationship between accessible stream km and average adult winter steelhead numbers from the late 19th through the mid 20th century (Table 1). We then use this relationship to make predictions for the OP winter steelhead populations. See Appendix A in the manuscript for the data.

The historical steelhead estimates for Waddel Creek, SF Eel, Mad River and N. Umpqua are based on fish counts from weirs and other techniques used to directly estimate population size during the historical period. The rest of the populations (all Puget Sound) were estimated based on a Catch expansion in Gayeski et al. 2011. Notice, however, that while Gayeski, used the maximum observed catch during the historical period, we used the average catch. This produced estimates that were one third the size, but more consistent with our other estimates which were also based on average values. We also excluded the "Rest of Puget Sound" estimate from Gayeski since it represented multiple basins.

We First plotted the data on the original and log scale to determine what type of functional relationship might be appropriate. It appears that a log-log relationship would be reasonable.


```r
dat <- read.csv(paste(workDir,"data/basinSizeData.csv",sep="/"),stringsAsFactors=FALSE)
```


```r
par(mfrow=c(1,2))

plot(dat$SKMs,dat$meanSteelhead,
     xlab="", ylab="", main="Original scale",
     pch=16, bty="l",xaxs="i",yaxs="i",xpd=TRUE)


plot(dat$SKMs,dat$meanSteelhead,
     xlab="", ylab="", main="Log scale",
     pch=16, bty="l",log="xy")

mtext(side=1, text="Accessible River Km", outer=TRUE)
mtext(side=2, text="Mean historical winter steelhead", outer=TRUE)
```

![Figure S2. Winter Steelhead population size versus accessible kilometers on the original and log10 scales.](analysis_files/figure-html/unnamed-chunk-13-1.png)


```r
dat$SKMs[9] <- NA
dat$meanSteelhead[9] <- NA
m1 <- lm(log10(meanSteelhead)~log10(SKMs),data=dat[-9,])

eqText <- paste("$$log(Pop) = ",round(coef(m1)[1],2)," + ",round(coef(m1)[2],2),"log(km)$$",sep="")
eqText2 <- paste("$$Pop = ",round(10^coef(m1)[1],2),"  km^{",round(coef(m1)[2],2),"}$$",sep="")
eqText3 <- paste("$$",round(10^coef(m1)[1],2)," \\times \\frac {500^{",round(coef(m1)[2],2),"}} {500} = ",round(10^coef(m1)[1] * 500^(coef(m1)[2]-1),2),"$$",sep="")
```

When we fit the log-log model we get:

$$log(Pop) = 1.71 + 0.91log(km)$$

or 

$$Pop = 51.34  km^{0.91}$$

The fit on the log-log scale is convincing, $R^2=$ 0.96, although, without Waddel Creek on the far left, the relationship would be quite a bit less impressive (Figure S3). 

The largest predicted Steelhead/km values are for small basins. For basins with Kms near 0 we get values of, 51.34, while at 500km we would get:

$$51.34 \times \frac {500^{0.91}} {500} = 29.78$$

The predicted historical steelhead numbers for the OP populations are summarized in Table 2 and Figure 2.


```r
logAt <- 10^(1:20)
tickAt <- c(1:9,(1:9)*10,(1:9)*100,(1:9)*1000,(1:9)*10000,(1:9)*100000)
plot(dat$SKMs,dat$meanSteelhead,
     xlab="Accessible River Km", ylab="Mean historical winter steelhead abundance", main="",
     pch=1, bty="l",log="xy",xaxt="n",yaxt="n")#,
     #xlim=c(10,10000),ylim=c(100,100000))
abline(m1)
axis(side=1,at=logAt,labels=logAt, lwd.ticks = 2)
axis(side=2,at=logAt,labels=logAt, lwd.ticks = 2)
axis(side=1,at=tickAt,labels=NA, lwd.ticks = 0.5, tck=-0.02)
axis(side=2,at=tickAt,labels=NA, lwd.ticks = 0.5, tck=-0.02)


# prediction intervals
rkms <- seq(10,5000,by=10)
p1 <- predict(m1,newdata=data.frame(SKMs=rkms),interval="prediction")
lines(rkms,10^p1[,2],lty=3)
lines(rkms,10^p1[,3],lty=3)
dat1 <- data.frame(SKMs=rkms,ymin=10^p1[,2],ymax=10^p1[,3])

rkms2 <- dat$SKMs[10:13]
p2 <- predict(m1,newdata=data.frame(SKMs=rkms2),interval="prediction")
segments(x0=rkms2,x1=rkms2,y0=10^p2[,2],y1=10^p2[,3],lwd=2,col="gray")
points(rkms2,10^p2[,1],pch=16,cex=1.25,col="black")
```

![Figure S3. Accessible river kilometers plotted against estimated historical population sizes (open circles). The log-log linear fit is overlayed with 95% prediction intervals. The solid points represent predictions for the four Olympic Penninsula populations.](analysis_files/figure-html/unnamed-chunk-15-1.png)

```r
dat2 <- data.frame(SKMs=rkms2,est=10^p2[,1],ymin=10^p2[,2],ymax=10^p2[,3])
```



```r
datOP <- dat[10:13,1:2]
datOP$PreditedSteelhead <- round(10^p2[,1])
datOP$lowerPI <- round(10^p2[,2])
datOP$upperPI <- round(10^p2[,3])

knitr::kable(datOP[c(2,4,1,3),],digits=c(NA,0,0,0,0),align=c("l","c","c","c","c"),caption="Table S10. The predicted historical population size in the four basins along with 95% prediction intervals.",format.args = list(big.mark = ",",
  scientific = FALSE),row.names=FALSE)
```



Table: Table S10. The predicted historical population size in the four basins along with 95% prediction intervals.

|Location             | SKMs | PreditedSteelhead | lowerPI | upperPI |
|:--------------------|:----:|:-----------------:|:-------:|:-------:|
|Quillayute River, WA | 675  |      19,571       |  7,910  | 48,423  |
|Hoh River, WA        | 338  |      10,431       |  4,247  | 25,615  |
|Queets River, WA     | 400  |      12,144       |  4,945  | 29,821  |
|Quinalt River, WA    | 494  |      14,723       |  5,986  | 36,215  |

## All estimates together

It can be helpful to look at all of the estimates together (Figure S3 and Table S12).


```r
datOP$population <- c("Queets","Quillayute","Quinalt","Hoh")
qTab$population <- qTab$pop

curRunsize <- runDat %>% 
  filter(year %in% 1995:2017) %>% 
  group_by(population) %>% 
  summarize(run=mean(runSize)) %>%
  ungroup()

cTab <- estTab %>% left_join(qTab,by="population") %>% 
  left_join(datOP,by="population") %>% left_join(curRunsize,by="population") %>%
  select(Population=population,run,
         ceEst = Estimate,ceLwr=EstLower,ceUpr=EstUpper,
         cpueEst = RSav.2,cpueLwr=RSav.1,cpueUpr=RSav.3,
         rkmEst = PreditedSteelhead,rkmLwr=lowerPI,rkmUpr=upperPI) %>%
  mutate(canEst = ifelse(Population=="Queets",canTab$N[2],NA),
         canLwr = ifelse(Population=="Queets",canTab$N[3],NA),
         canUpr = ifelse(Population=="Queets",canTab$N[1],NA)) %>%
  group_by(Population) %>%
  mutate(
         estAvg = mean(c(ceEst,cpueEst,rkmEst,canEst),na.rm=TRUE)) %>%
  ungroup()

ff <- 1000
mCol <- c("#999999", "#E69F00", "#56B4E9")
ggplot(cTab) +
  geom_pointrange(aes(x=1,ymin=ceLwr/ff,ymax=ceUpr/ff,y=ceEst/ff), lwd=1, color=mCol[1]) +
  geom_pointrange(aes(x=2,ymin=cpueLwr/ff,ymax=cpueUpr/ff,y=cpueEst/ff), lwd=1, color=mCol[2]) +
  geom_pointrange(aes(x=3,ymin=rkmLwr/ff,ymax=rkmUpr/ff,y=rkmEst/ff), lwd=1, color=mCol[3]) +
  geom_pointrange(aes(x=4,ymin=canLwr/ff,ymax=canUpr/ff,y=canEst/ff), lwd=1, color="black") +
  geom_segment(aes(x=0.75,xend=4.25,y=estAvg/ff,yend=estAvg/ff),lwd=1) +
  facet_grid(.~Population) +
  coord_cartesian(ylim=c(0,50),expand=c(0,0)) +
  scale_x_continuous(breaks=1:4,labels=c("Expansion","CPUE","RiverKm","Cannery"),limits=c(0.5,4.5)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90,hjust=1,vjust=0.5)) +
  ylab("Estmated historic run size in 1000's") 
```

![Figure S3. A side by side comparison of the different historical estimates include the Queets cannery data, the catch expansion, the CPUE based method, the river km relationship. The horizontal lines are the averages of the historical estimates for each population.](analysis_files/figure-html/unnamed-chunk-17-1.png)


# Sensitivity to changes in efficiency

Here we examine the sensitivity of our run timing results to the assumption of constant efficiency. By using weekly CPUE values across the season to estimate run timing, we are assuming that efficiency remains constant through time. We know that this is not true, but assume any differences do not have a large effect on our conclusions about run timing differences. One set of factors that may affect efficiency are in river conditions due to weather (e.g. flow, turbidity, debris, etc...). For example, efficiency may be lower during periods of high flow. Fish behavior may also change across months. Fish, may be more or less grouped in different periods and may respond to river conditions like flow creating an interaction between effort and run timing, further complicating the interpretation of CPUE data. While these problems pose real issues with the interpretation of CPUE data, we believe that this approach is better than relying an raw catch data. 

To test the sensitivity of the results to plausible changes in efficiency during the season, we used discharge data from the Calawah River (a major tributary to the Quillayute River).


```r
# Average monthly discharge data for the Calawah River from 1989 through 2018
disDat <- read.csv(paste(workDir,"data/dischargeDat.csv",sep="/"))
ggplot(disDat,aes(x=month,y=discharge)) + 
  geom_col(fill="skyblue") + 
  ylab("Discharge (cubic feet per second)") +
  xlab("Month") + 
  scale_x_continuous(breaks=1:12,labels=1:12) +
  theme_bw()
```

![Figure S4. Average monthly discharge in the Calawah River from 1989 to 2015.](analysis_files/figure-html/unnamed-chunk-18-1.png)

Notice that the discharge is around 1,300 for Nov-Jan, around 1,100 for Feb-Mar, about 850 in April and 450 in May.

Here, I'll assume that efficiency is higher at lower flows (during the period when fishing occurred). Again, efficiency is the proportion of the available run captured per unit effort. I'll assume efficiency for Nov-Jan is $q_1$, $1.5q_1$ for Feb-Mar, $1.75q_1$ for April, and $2q_1$ for May. 


```r
# prepare data (i.e. create data set with CPUE adjusted to account for different q)

currDat <- CPUEdat %>% filter(population=="Quillayute",HW=="Wild",period=="Current")

currDatQ <- currDat %>% mutate(month=month(date),
                               eff=ifelse(month %in% c(11,12,1),1,
                                   ifelse(month %in% 2:3,1.5,
                                   ifelse(month == 4,1.75,2))),
                               CPUEold = CPUE,
                               CPUE = CPUE/eff,
                               CPUE = CPUE * sum(CPUEold)/sum(CPUE)
                               )

histDat <- CPUEdat %>% filter(population=="Quillayute",HW=="Wild",period=="Historic")

histDatQ <- histDat %>% mutate(month=month(date),
                               eff=ifelse(month %in% c(11,12,1),1,
                               ifelse(month %in% 2:3,1.5,
                               ifelse(month == 4,1.75,2))),
                               CPUEold = CPUE,
                               CPUE = CPUE/eff,
                               CPUE = CPUE * sum(CPUEold)/sum(CPUE)
                               )

# fit the beta distribution to all different permutations of data
# for the Quillayute population
fbCurr <- fitBeta(currDat,initVals=c(6,4,3))
fbCurrQ <- fitBeta(currDatQ,initVals=c(6,4,3))
fbHist <- fitBeta(histDat,initVals=c(2,2,5))
fbHistQ <- fitBeta(histDatQ,initVals=c(2,2,5))
q25 <- list(curr=qJ(fbCurr$est,0.25,dd),
         currQ=qJ(fbCurrQ$est,0.25,dd),
         hist=qJ(fbHist$est,0.25,dd),
         histQ=qJ(fbHistQ$est,0.25,dd))
q50 <- list(curr=qJ(fbCurr$est,0.5,dd),
         currQ=qJ(fbCurrQ$est,0.5,dd),
         hist=qJ(fbHist$est,0.5,dd),
         histQ=qJ(fbHistQ$est,0.5,dd))
pJan <- list(curr=pJ(fbCurr$est,1,dd),
         currQ=pJ(fbCurrQ$est,1,dd),
         hist=pJ(fbHist$est,1,dd),
         histQ=pJ(fbHistQ$est,1,dd))
```

Here's a plot of the historic(blue) vs current(green) timing without changes to efficiency.


```r
# Comparing current and historic without q adjustment
ggplot(histDat,aes(x=julian,y=CPUE)) +
  geom_point(col=col1) +
  geom_line(data=fbHist$predVal,aes(x=date,y=pred),col=col1,lwd=1.25) +
  geom_point(data=currDat,aes(x=julian,y=CPUE),col=col2,lwd=1.25) +
  geom_line(data=fbCurr$predVal,aes(x=date,y=pred),col=col2,lwd=1.25) +
  scale_x_continuous(limits=dd,breaks=monthPos, labels=monthNam) +
  labs(title="Comparing current and historic timing without q adjustment") +
  theme_light()
```

![](analysis_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

Here's a plot of the historic(blue) vs current(green) timing with the assumed changes to efficiency.


```r
# Comparing current and historic with q adjustment
ggplot(histDatQ,aes(x=julian,y=CPUE)) +
  geom_point(col=col1) +
  geom_line(data=fbHistQ$predVal,aes(x=date,y=pred),col=col1,lwd=1.25) +
  geom_point(data=currDatQ,aes(x=julian,y=CPUE),col=col2,lwd=1.25) +
  geom_line(data=fbCurrQ$predVal,aes(x=date,y=pred),col=col2,lwd=1.25) +
  scale_x_continuous(limits=dd,breaks=monthPos, labels=monthNam) +
  labs(title="Comparing current and historic timing with q adjustment") +
  theme_light()
```

![](analysis_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

For both the adjusted and unadjusted efficiencies the historic timing is shifted to the left. while there are some differences in the shapes of the curves, the magnitude of the shift is about the same but. For example, the difference in the proportion of the run that had past by January 1st was 0.18 assuming constant efficiency and 0.21 for the varying efficiency. So, while the timing distributions can be sensitive to the addition of time-varying efficiency, it does not change the qualitative comparison between the two time periods. If the time-varying efficiency is substantially different for the two periods, this could create problems. However, it seems more likely that the historic fisheries were more sensitive to flow (in terms of efficiency). If that were the case, then the difference might be even more pronounced since the historic timing would be shifted to the left more than the current timing.

Fishing towards the end of the season, April on, in the current period is often targeting Chinook salmon. This means that the mesh size may be larger and the efficiency is therefore lower (for catching steelhead). Correcting for this would inflate the CPUE values towards the end of the season which would result in even larger estimated differences in run timing between the current and historic periods and the current wild and hatchery runs. We chose to not make an adjustment since we were not able to get estimates of changes in efficiency and fishing during this period was sporadic.

