
#----------------------------------------------------------------------------------------
# Fit a bayesian model to estimate p(being in each risk category) and credible intervals
# but with a linear trend over time to "smooth" the risk.
# It would be possible to fit a spline in a more complex model but this is not done here


library("R2jags")  # used for call to JAGS
library(coda)


# The model file.
# The cat() command is used to save the model to the working directory.
# Notice that you CANNOT have any " (double quotes) in the bugs code
# between the start and end of the cat("...",) command.

cat(file="model.txt", "
    ############################################################
    data {
       Nspecies <- max(Species.num)
       Nyears   <- max(Year.num)
       Nsites   <- max(Site.num)
    }    

    model {
    
    # log normal distribution of single pass electrofishing values
    for(i in 1:Nyears){
       for(j in 1:Nspecies){
          mu.trend  [i,j] <- beta0[j]+beta1[j]*i
          mu.process[i,j] <- mu.trend[i,j] + year.eff[i,j]
       }
    }
    for(i in 1:Ndata){
       mu.data[i] <- mu.trend[Year.num[i],Species.num[i]] + 
                     site.eff[Site.num[i],Species.num[i]] +  
                     year.eff[Year.num[i],Species.num[i]]
       Density[i] ~ dlnorm( mu.data[i], tau[Species.num[i]])
    }

    # tau is 1/sd
    for(i in 1:Nspecies){
       tau[i] <- 1/(sd[i]*sd[i])
       sd[i] ~ dunif(.05, 3)   # on the log-scale sd is proportion of the mean
    }

    # priors for the intercept and slope
    for(i in 1:Nspecies){
       beta0[i] ~ dnorm(0, .001)
       beta1[i] ~ dnorm(0, .001)
    }

    # random effect of Year for each species
    for(i in 1:Nyears){
       for(j in 1:Nspecies){
          year.eff[i,j] ~ dnorm(0, tau.year.eff[j])
       }
    }
    for(i in 1:Nspecies){
       tau.year.eff[i] <- 1/(sd.year.eff[i]*sd.year.eff[i])
       sd.year.eff[i]  ~ dunif(.01,2)
    }

    # random effect of Sites
    for(i in 1:Nsites){
       for(j in 1:Nspecies){
          site.eff[i,j] ~ dnorm(0, tau.site.eff[j])
       }
    }
    for(i in 1:Nspecies){
       tau.site.eff[i] <- 1/(sd.site.eff[i]*sd.site.eff[i])
       sd.site.eff[i]  ~ dunif(.01, 2)
    }


    # what is the probability that the trend is negative
    for(i in 1:Nspecies){
       p.beta1.lt.0[i] <- ifelse(beta1[i]<0,1,0)
    }


   # derived variables.
   # med.den is antilog of  lognormal 
   for(i in 1:Ndata){
      med.den[i] <- exp(mu.data[i])
   }

   for(i in 1:Nyears){
       for(j in 1:Nspecies){
          med.den.trend [i, j] <- exp(mu.trend  [i,j])
          med.den.process[i,j] <- exp(mu.process[i,j])
       }
    }

   # probability of being in a threshold category for individual observations
   for(i in 1:Nyears){
      for(j in 1:Nspecies){
         for(k in 1:NFSI){
             prob.FSI.cat[i,j,k] <- ifelse((med.den.process[i,j] >= FSI.lower[k]) && (med.den.process[i,j] < FSI.upper[k]),1,0)
         }
      }
   }

   # probability of being in a threshold category for trend line
   for(i in 1:Nyears){
      for(j in 1:Nspecies){
         for(k in 1:NFSI){
             prob.FSI.cat.trend[i,j,k] <- ifelse((med.den.trend[i,j] >= FSI.lower[k]) && (med.den.trend[i,j] < FSI.upper[k]),1,0)
         }
      }
   }
}
") # End of the model



# Next create the data.txt file.
# Initialize the data values using standard R code by either reading
# in from an external file, or plain assignment.

# The datalist will be passed to JAGS with the names of the data
# values.

dim(pass.melt)
pass.melt.red <- pass.melt[ !is.na(pass.melt$Density),]
dim(pass.melt.red)

# Convert the species code to a species number because JAGS cannot use character data
species.code <- data.frame(Species    =unique(pass.melt.red$Species),
                               Species.num=1:length(unique(pass.melt.red$Species)), stringsAsFactors=FALSE)
species.code
pass.melt.red <- merge(pass.melt.red, species.code)
xtabs(~Species+Species.num, data=pass.melt.red, exclude=NULL, na.action=na.pass)

# Convert year number to a unique year number
year.code <- data.frame(Year    =sort(unique(pass.melt.red$Year)), 
                        Year.num=1:length(unique(pass.melt.red$Year)))
year.code
pass.melt.red <- merge(pass.melt.red, year.code)
head(pass.melt.red)

# Convert Site.no to a unique numeric values
site.code <- data.frame(Site.No  =unique(pass.melt.red$Site.No),
                        Site.num =1:length(unique(pass.melt.red$Site.No)), stringsAsFactors=FALSE)
site.code
pass.melt.red <- merge(pass.melt.red, site.code)
head(pass.melt.red)

pass.melt.red <- pass.melt.red[ order(pass.melt.red$Species.num, pass.melt.red$Year.num, pass.melt.red$Site.num),]


data.list <- list(Ndata      =nrow(pass.melt.red),
                  Year.num   =pass.melt.red$Year.num,
                  Year       =pass.melt.red$Year,
                  YearUnique =sort(unique(pass.melt.red$Year)),
                  Site.num   =pass.melt.red$Site.num,
                  Species.num=pass.melt.red$Species.num,
                  Density    =pass.melt.red$Density+.1*min(pass.melt.red$Density[pass.melt.red$Density>0]),
                  NFSI       =nrow(FSI.threshold),
                  FSI.lower  =FSI.threshold$lower,
                  FSI.upper  =FSI.threshold$upper)
data.list






# Next create the initial values.
# If you are using more than one chain, you need to create a function
# that returns initial values for each chain.

init.list <- list(
   list(), list(), list()
)







# Next create the list of parameters to monitor.
# The deviance is automatically monitored.
# 
monitor.list <- c("mu.data","mu.trend","site.eff","year.eff",
                  "sd","sd.year.eff","sd.site.eff",
                  "med.den", "med.den.trend","med.den.process",
                  "prob.FSI.cat","prob.FSI.cat.trend",
                  "beta0","beta1","p.beta1.lt.0")




# Finally, the actual call to JAGS
set.seed(4534534)  # intitalize seed for MCMC 

results <- jags( 
  data      =data.list,   # list of data variables
  inits     =init.list,   # list/function for initial values
  parameters=monitor.list,# list of parameters to monitor
  model.file="model.txt",  # file with bugs model
  n.chains=3,
  n.iter  =5000,          # total iterations INCLUDING burn in
  n.burnin=2000,          # number of burning iterations
  n.thin=2,               # how much to thin
  DIC=TRUE,               # is DIC to be computed?
  working.dir=getwd()    # store results in current working directory
)


# now results is a BIG list of stuff
names(results)
names(results$BUGSoutput)

#
#######################################
# extract some of the usual stuff and use R code directly
# use the standard print method
results

# get the summary table
results$BUGSoutput$summary
results$BUGSoutput$summary[,c("mean", "sd", "2.5%","97.5%","Rhat", "n.eff")]

results$BUGSoutput$summary[grepl("sd",row.names(results$BUGSoutput$summary)),c("mean", "sd", "2.5%","97.5%","Rhat", "n.eff")]
results$BUGSoutput$summary[grepl("beta",row.names(results$BUGSoutput$summary)),c("mean", "sd", "2.5%","97.5%","Rhat", "n.eff")]

# make a table of the slope etc
beta.table <- data.frame(Species.num=row.names(results$BUGSoutput$summary)[(grepl("beta1[",row.names(results$BUGSoutput$summary),fixed=TRUE))],
                         slope      =results$BUGSoutput$mean$beta1,
                         sd         =results$BUGSoutput$sd$beta1,
                         p.slope.lt.0=results$BUGSoutput$mean$p.beta1.lt.0, stringsAsFactors=FALSE)
beta.table$Species.num <- as.numeric(substr(beta.table$Species.num,1+regexpr('[',beta.table$Species.num, fixed=TRUE),
                                                       -1+regexpr(']',beta.table$Species.num, fixed=TRUE)))
beta.table <- merge(species.code, beta.table)
beta.table
beta.table[, 3:5] <- round(beta.table[,3:5],2)
beta.table



# get just the means
results$BUGSoutput$mean
results$BUGSoutput$mean$parm


# Extract the means and posterior density plots for each species
select <- grepl('^med.den.process[', row.names(results$BUGSoutput$summary), fixed=TRUE)
meandata <- data.frame(med.density=matrix(results$BUGSoutput$mean$med.den.process, ncol=1))
meandata$Year.num <- 1:nrow(results$BUGSoutput$mean$med.den.process)
meandata$Species.num <- rep(1:ncol(results$BUGSoutput$mean$med.den.process), each=nrow(results$BUGSoutput$mean$med.den.process))
meandata <- merge(meandata, species.code)
meandata <- merge(meandata, year.code)
head(meandata)
all.year.species <- expand.grid(Year=min(pass.melt$Year,na.rm=TRUE):max(pass.melt$Year, na.rm=TRUE),
                                Species=unique(pass.melt$Species),stringsAsFactors=FALSE)
meandata <- merge(meandata, all.year.species, all=TRUE)

# Extract the underlying trend
select <- grepl('^mu.trend', row.names(results$BUGSoutput$summary))
trenddata <- data.frame(med.density=results$BUGSoutput$summary[select, "mean"])
trenddata$Year.Species <- row.names(results$BUGSoutput$summary)[select]
trenddata$Year.Species <- gsub("mu.trend[","", trenddata$Year.Species, fixed=TRUE)
trenddata$Year.Species <- gsub("]",  "", trenddata$Year.Species, fixed=TRUE)
head(trenddata)
# convert the year,species code to actual years and species
trenddata$Year.num   <- as.numeric(substr(trenddata$Year.Species,1,-1+regexpr(',',trenddata$Year.Species)))
trenddata$Species.num<- as.numeric(substring(trenddata$Year.Species,1+regexpr(',',trenddata$Year.Species)))
head(trenddata)
trenddata <- merge(trenddata, species.code)
trenddata <- merge(trenddata, year.code)
head(trenddata)
all.year.species <- expand.grid(Year=min(pass.melt$Year,na.rm=TRUE):max(pass.melt$Year, na.rm=TRUE),
                                Species=unique(pass.melt$Species),stringsAsFactors=FALSE)
trenddata <- merge(trenddata, all.year.species, all=TRUE)
trenddata$med.density = exp(trenddata$med.density) # convert from log to median

# get the posterior density values
select <- grepl('^mu.trend', colnames(results$BUGSoutput$sims.matrix))
plotdata <- reshape2::melt(as.data.frame( results$BUGSoutput$sims.matrix[, select]),
                           variable.name='Year.Species',
                           value.name='med.density')
plotdata$Year.Species <- gsub("mu.trend[","", plotdata$Year.Species, fixed=TRUE)
plotdata$Year.Species <- gsub("]",  "", plotdata$Year.Species, fixed=TRUE)
head(plotdata)
# convert the year,species code to actual years and species
plotdata$Year.num   <- as.numeric(substr(plotdata$Year.Species,1,-1+regexpr(',',plotdata$Year.Species)))
plotdata$Species.num<- as.numeric(substring(plotdata$Year.Species,1+regexpr(',',plotdata$Year.Species)))
head(plotdata)
plotdata <- merge(plotdata, species.code)
plotdata <- merge(plotdata, year.code)
plotdata$med.density <- exp(plotdata$med.density)
head(plotdata)
# make a plot of the posterior median for each species x year 
# with the superimposed trend

postplot <- ggplot2::ggplot( data=meandata, aes(x=Year, y=med.density))+
  ggtitle("Estimated MEDIAN density with trend line")+
  ylab("Median from fitted model and posterior beliefs")+
  geom_point(data=plotdata, aes(group=Year), alpha=0.01, position=position_jitter(w=0.2))+
  geom_point()+
  geom_line(color="black", size=1)+
  geom_line(data=trenddata, aes(x=Year, y=med.density), color="blue", size=2)+
  geom_hline(data=FSI.threshold[1:3,], aes(yintercept=lower), alpha=1, color="red")+
  facet_wrap(~Species, ncol=1, scales="free_y")+
  ylim(0,150)
postplot
ggsave(plot=postplot, file='plot-post-trend.png',  h=4, w=6, units="in", dpi=300)






# plot of the probability of being in each category over time for the underlying trend
prob.FSI.cat <- results$BUGSoutput$mean$prob.FSI.cat.trend
prob.FSI.cat[1,1,]
plotdata <- data.frame(prob=matrix(prob.FSI.cat,ncol=1))
plotdata$Year.num    <- rep(1:nrow(year.code))
plotdata$Species.num <- rep(1:nrow(species.code), each=nrow(year.code))
plotdata$FSI.num.cat <- rep(1:nrow(FSI.threshold), each=nrow(species.code)*nrow(year.code))
head(plotdata)
# convert the year, species, and FSI category numeric code to actual year, species, FSI
plotdata <- merge(plotdata, year.code,        all=TRUE)
plotdata <- merge(plotdata, species.code, all=TRUE)
plotdata <- merge(plotdata, FSI.threshold[,c("FSI.num.cat","FSI.cat")])
head(plotdata)


# for stacked bar charts for each species
# we need to sort the data frame in reverse order of FSI category
plotdata <- plotdata[ order(plotdata$FSI.cat),] #, decreasing=TRUE),]
plotdata$FSI.cat2 <- factor(plotdata$FSI.cat, levels=rev(levels(plotdata$FSI.cat)), order=TRUE)
fsi.plot <- ggplot(data=plotdata, aes(x=Year, y=prob, fill=FSI.cat2))+
  ggtitle("Probability of being in FSI category based on trend line")+
  ylab("Cumulative probability of being in FSI category")+
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="RdYlGn", direction =-1)+
  facet_wrap(~Species, ncol=1)
fsi.plot
ggsave(plot=fsi.plot, file='plot-prob-cat-trend.png', h=4, w=6, units="in", dpi=300)



#--------------------------------------------------------------
# Sample size determination

results$BUGSoutput$summary[grepl("sd",row.names(results$BUGSoutput$summary)),c("mean", "sd", "2.5%","97.5%","Rhat", "n.eff")]
results$BUGSoutput$summary[grepl("beta",row.names(results$BUGSoutput$summary)),c("mean", "sd", "2.5%","97.5%","Rhat", "n.eff")]

# extract the site, year and process error for the three species
varcomp <- data.frame(Species.num=1:3,
                     SDresid = results$BUGSoutput$mean$sd,
                     SDsite  = results$BUGSoutput$mean$sd.site.eff,
                     SDyear  = results$BUGSoutput$mean$sd.year.eff, stringsAsFactors=FALSE)
varcomp <- merge(varcomp, species.code)
varcomp


# regression power 
source("../regression.power.R")

power.res <- ddply(varcomp, "Species", function (vc){
   scenarios <- expand.grid(Trend=.05, nyears=seq(5,50,5), nsites=c(5,10,50))
   scenarios$scenario <- 1:nrow(scenarios)
   power <-ddply(scenarios, "scenario", function(x,vc){
          Xvalues <- rep(1:x$nyears, each=x$nsites)
          power <- slr.power.stroup(x$Trend, Xvalues=Xvalues, Process.SD=vc$SDyear, Sampling.SD=vc$SDresid)
          #browser()
          data.frame(x,power)
   }, vc=vc) 
   data.frame(vc,power)
})
power.res

power.plot <- ggplot(data=power.res, aes(x=nyears, y=os.power1, color=factor(nsites)))+
    ggtitle(paste("Estimated power to detect trend of ", power.res$Trend[1]*100,"% per year",sep=""))+
    ylab("Power (at alpha=0.05)")+
    xlab("Number of years of monitoring")+
    scale_color_discrete(name="Number\nsites\nper year")+
    ylim(c(0,1))+
    geom_line()+
    geom_hline(yintercept=0.80)+
    facet_wrap(~Species, ncol=1, scales="fixed")
power.plot
ggsave(plot=power.plot, file='plot-power-trend.png', h=6, w=6, units="in", dpi=300)
