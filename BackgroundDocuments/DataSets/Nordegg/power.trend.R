# Estimate the power to detect a 10, 20, 50, 100, 200% increase in the abundance over 5 years
# This data has information on the within-year variation, but no information on process error.

# As usually, convert the estimates of variablity to the log-scale to answer the above questions.

library(ggplot2)
library(plyr)
library(reshape2)

# read in the data
cpue <- read.csv("Nordegg2016SiteCUESummary_streamorder.csv", header=TRUE, as.is=TRUE, strip.white=TRUE, skip=1)

# rename some variables
cpue <- plyr::rename(cpue, replace=c("BKTR_100m"="BKTR_catch",
                       "BLTR_100m"="BLTR_catch",
                       "BURB_100m"="BURB_catch",
                       "MNWH_100m"="MNWH_catch"))
head(cpue)


# Convert actual catch per length to catch per 100 m
cpue$BKTR_100m = cpue$BKTR_catch / cpue$DistanceSa * 100
cpue$BLTR_100m = cpue$BLTR_catch / cpue$DistanceSa * 100
cpue$BURB_100m = cpue$BURB_catch / cpue$DistanceSa * 100
cpue$MNWH_100m = cpue$MNWH_catch / cpue$DistanceSa * 100

# convert actual catch to per 100s
cpue$BKTR_100s = cpue$BKTR_catch / cpue$Effort_s * 100
cpue$BLTR_100s = cpue$BLTR_catch / cpue$Effort_s * 100
cpue$BURB_100s = cpue$BURB_catch / cpue$Effort_s * 100
cpue$MNWH_100s = cpue$MNWH_catch / cpue$Effort_s * 100

head(cpue)

# summary of the data
xtabs(~HUC10, data=cpue, exclude=NULL, na.action=na.pass)
xtabs(~STR_ORDER, data=cpue, exclude=NULL, na.action=na.pass)

# melt the data
cpue.melt <- reshape2::melt(cpue,
                            id.vars=c("Site","HUC10","STR_ORDER"),
                            measure.vars=c("BKTR_100m","BLTR_100m","BURB_100m","MNWH_100m",
                                           "BKTR_100s","BLTR_100s","BURB_100s","MNWH_100s"),
                            variable.name="Measure",
                            value.name="value")
head(cpue.melt)

# at the moment we will ignore any stratification when estimating the mean CPUE

# Estimate the initial mean and standard deviation 
cpue.var <- plyr::ddply(cpue.melt, c("Measure"), plyr::summarize,
                        n          =length(value),
                        mean       = mean(value),
                        SDsampling = sd(value))
cpue.var$rSDsampling <- cpue.var$SDsampling / cpue.var$mean
cpue.var

temp <- cpue.var
temp[, c(3:5)] <- round(temp[, c(3:5)],2)
temp

# now for power charts to detect 10%, 30%, 50%, 100%, 200%, 300% change over 5 years 
# by varying the number of sites at different levels of process error
source("../regression.power.R")

x <- cpue.var[1,]
SDprocess=0.50
alpha=0.05

d_ply(cpue.var, "Measure", function (x, SDprocess, alpha=0.05){
  # set up the scenarios to estimate the power 
  scenarios <- expand.grid(PerChange=c(10, 30, 50, 100, 200),
                           Years=c(5,10),
                           Sampling.SD=x$SDsampling/x$mean,  # on the log-regular scale
                           Process.SD=SDprocess,
                           sites.per.year=seq(20,100,10),
                           alpha=alpha)
  scenarios$Scenario <- 1:nrow(scenarios)
  # estimate the power to detect a trend for each scenario
  power <- plyr::ddply(scenarios, "Scenario", function(x){
      # estimate compounded trend line on the log scale
      Trend <-  (x$PerChange/100+1)^(1/(x$Years-1))-1
      # sampling every year
      Xvalues <- rep(1:x$Year, each=x$sites.per.year)
      res <- slr.power.stroup(Trend=Trend, 
                              Xvalues    =Xvalues   , 
                              Process.SD =x$Process.SD, 
                              Sampling.SD=x$Sampling.SD, 
                              alpha      =x$alpha)
      res <- cbind(res, x)
      res
   })
   # make a plot
   plotdata <- power
   plotdata$Years2 <- paste("Over ", plotdata$Year," Years",sep="")
   plotdata$PerChangeF <- factor(plotdata$PerChange)
   power.plot <- ggplot2::ggplot(data=plotdata, aes(x=sites.per.year, y=power.1sided.a, color=PerChangeF))+
      ggtitle(paste("Power to detect changes over time for ", x$Measure,"\n alpha=",power$alpha[1],
                    "; rProcess SD= ", format(round(power$Process.SD[1] ,2),nsmall=2), 
                    "; rSampling SD= ",format(round(power$Sampling.SD[1],2),nsmall=2), sep=""))+
      geom_line(aes(group=PerChangeF, linetype=PerChangeF))+
      ylab("Power")+ylim(0,1)+geom_hline(yintercept=0.80)+
      facet_wrap(~Years2, ncol=1, scales='fixed')+
      scale_color_discrete(name="Percent\nChange")+
      scale_linetype_discrete(name="Percent\nChange")
   plot(power.plot)
   ggsave(plot=power.plot, file=paste('power-',x$Measure,"-rProcessSD-",format(power$Process.SD[1],nsmall=2),".png",sep=""),
           h=6, w=6, dpi=300)
  }, SDprocess=SDprocess, alpha=alpha)





