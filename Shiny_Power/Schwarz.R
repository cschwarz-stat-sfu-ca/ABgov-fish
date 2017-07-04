
library(ggplot2); library(plyr); library(lme4)

#---------------------------------------------------------------------------
# Simple Linear regression Power function that incorporates process and sampling error
# While this is commonly used for trend analysis where the X variable is time, it can
# also be used for any regression problem.
# Autocorrelation is not accounted for in this power analysis.

# 2014-11-21 CJS Removed Ivalue from the call as not needed
# 2014-06-24 CJS First Edition for web

# This function computes the power for a simple linear regression design that allows
# for process and sampling error
#
# The information we need is:
#     Alpha level (usually .05 or .10)
#     Variance components (these were obtained from the an analysis of previous data)
#        Process.SD  - standard deviation of the process variation over the X value
#        Sampling.SD - standard deviation of the sampling variation at each X value
#     Trend   - slope for which the power to detect is to be estimated
#     X       - vector of X values. These can be in any order. Multiple X values
#               indicated multiple measurements at each X value.


# The computations are based on the 
#    Stroup, W. W. (1999)
#    Mixed model procedures to assess power, precision, and sample size in the design of experiments.
# paper where "dummy" data is generated and "analyzed" and the resulting F-statistics etc
# provide information needed to compute the power




#-----------------------------------------------

slr.power.stroup <- function(Trend, Xvalues, Process.SD, Sampling.SD, alpha=0.05){
# This computes the power of a simple linear regression that potentially includes 
# process and sampling error. Arguments are defined above

# There are 3 cases to consider
# (a) Process.SD >0, Sampling.SD >0, replicates at some X values
#      This is the classical regression model with process error and sampling error.
#      Because there are multiple measurements at some X values, it is possible to
#      fit a model of the form
#        lmer( Y ~ X + (1|XF), data=blah)
#      where XF are the X values treated as a factor.
#      In this case, the df for testing a hypothesis about the slope is
#      approximately equal to the
#          number of unique X values - 2 (essentially, you analze the averages)
#      The power refers to the ability to detect a non-zero slope of the lmer model.
#
# (b) Process.SD >0, Sampling.SD >0, NO replicates at any X value
#     This is quite common where an estimate of a population parameter is obtained
#     each year with a measure of precision (SE of each estimate). The sampling SD
#     is essentially the average of the SE. Process error is obatined by subtracting
#     the average SE from the residual sd after fitting a mean (or simple slope)
#     In this case, all that need be done is fit a 
#           lm( Y ~ X, data=blah)
#     as then the resiudal sd is the (proper) mixture of process and sampling SD
#     The df for hypothesis tests about the slope is again approximately equal to 
#           number of unique X values - 2
#     If you want to investigate the impact of increasing effort in each year, treat
#     the current average se as obtained from a "unit of effort". So if you take two 
#     measurement at an X value, this is (approximately) equivalent to doubling the effort.
#
# (c) Process.SD = 0, Sampling.SD >0, any set of X values (with or without replicates
#     at an paticular X values)
#     This is a classical simple linear regression with data points scattered about
#     the regression line and all points are independent of all other points,i.e.
#          lm(Y ~ X)
#     This is a VERY strong assumption and typically not valid when testing for 
#     trends over time where it is VERY common to have a process error that corresponds to
#     year specific effects over which you typically do not have any control.
#     The df for testing hypothese about the slope is number of data points - 2.
      
#  browser()
  # Total sample size
  n <- length(Xvalues)
  # Compute the mean response (before adding process o sampling error)
  mu <- 0 + Trend*(Xvalues-min(Xvalues))
  
  # Create the various design matrices
  # Fixed effects
  X  <- cbind(1, Xvalues)
  XF <- model.matrix(~ -1 +as.factor(Xvalues))

  # We solve for the regression estimates using weighted least squares
  # based on the variance-covariance matrix for the data
  V <- diag(Sampling.SD^2,n,n) + XF %*% t(XF)*Process.SD^2 

  # Get fixed effects and fixed effects varcovar matrix
  beta <- solve(t(X)%*%solve(V)%*%X) %*% t(X)%*%solve(V)%*%mu

# the vector to extract the slope coefficient
  K <- c(0,1)

#  calculate the non-centrality parameter, and then the power
  ncp <- as.numeric(t(t(K)%*%beta)%*%solve(t(K)%*%solve(t(X)%*%solve(V)%*%X)%*%K)%*%(t(K)%*%beta))

# What is the denominator df for the hypothesis test. See notes above
  dfdenom <- length(unique(Xvalues))-2 # approximation to df for slope is number of unique X values -2 
  if(Process.SD ==0){ dfdenom = length(Xvalues)-2}

  Fcrit <- qf(1-alpha, 1, dfdenom)
  power.2sided <- 1 - pf(Fcrit, 1, dfdenom,ncp)

#  Compute the one-sided power, i.e. to detect the change in one direction only
#  Because I don't know which direction of interest, the one-sided power is 
#  computed in both directions.
#
  Tcrit <- qt(1-alpha,dfdenom)
  power.1sided.a <- 1-pt(Tcrit,dfdenom,sqrt(ncp))
  power.1sided.b <- pt(-Tcrit,dfdenom,sqrt(ncp))
  #browser()
  return(data.frame(alpha=alpha, 
     Trend=Trend, 
     Process.SD=Process.SD, Sampling.SD=Sampling.SD,
     Beta=beta[2],
     dfdenom=dfdenom, ncp=ncp, Fcrit=Fcrit, power.2sided=power.2sided,
     Tcrit=Tcrit, power.1sided.a=power.1sided.a, power.1sided.b=power.1sided.b))
}



#-----------------------------------------------------------------
estimate.power <- function(vc, y=5, alpha=0.05){
  # given a set of vc with SD.sampling and SD.process, create the power graphs
  # set up the scenarios to estimate the power 
  #browser()
  scenarios <- expand.grid(PerChange=c(30, 50, 100, 200, 300),
                           Years=c(y),
                           Sampling.SD=vc$SD.sampling,
                           Process.SD=vc$SD.process,
                           sites.per.year=seq(5,50,5),
                           Watershed=vc$Watershed,
                           Measure=vc$Measure,
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
   #browser()
   plotdata <- power
   plotdata$Years2 <- paste("Over ", plotdata$Year," Years",sep="")
   plotdata$PerChangeF <- factor(plotdata$PerChange)
   power.plot <<- ggplot2::ggplot(data=plotdata, aes(x=sites.per.year, 
                                                    y=power.1sided.a, 
                                                    color=PerChangeF))+
      ggtitle(paste("Power to detect changes over time for \n", 
                    "alpha=",power$alpha[1],
                    "; rProcess SD= ", format(round(power$Process.SD[1] ,2),nsmall=2), 
                    "; rSampling SD= ",format(round(power$Sampling.SD[1],2),nsmall=2),
                    sep=""))+
      geom_line(aes(group=PerChangeF, linetype=PerChangeF),size=1.5)+
      ylab("Power")+ylim(0,1)+geom_hline(yintercept=0.80)+
      facet_wrap(~Years2, ncol=1, scales='fixed')+
      scale_color_discrete(name="Percent\nChange")+
      scale_linetype_discrete(name="Percent\nChange")
   plot(power.plot)
   ggsave(plot=power.plot, file="output.png",
           h=6, w=6, dpi=300)
   power
  }
