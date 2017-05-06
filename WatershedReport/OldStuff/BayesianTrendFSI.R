BayesianTrendFSI <- function( catch.rate, FSI.threshold, postthin=.1){
   # do a bayesian trend analysis on the cpue data and and compute the 
   # posterior probability of belonging to each FSI category
   # and create some plots
  
   # Input data
   #     catch.rate - data frame with 
   #         WatershedName  - name of watershed
   #         Species.Code   - species codes (could be multiple)
   #         LocationTTM    - names of locations of sampling
   #         Year           - year of data
   #         CPUE.300m      - fish per 300 m
   #
   #     FSI.threshold - data frame with FSI thresholds by species
   #         Species.Code   - species
   #         FSI.num.cat    - numerical FSI category
   #         FSI.cat        - FSI category (alphabetic)
   #         lower          - lower value (CPUE.300m/300 m)
   #         upper          - upper value (CPUE.300m/300 m)
   #
   #     postthin           - what fraction of posterior should be plotted (to make plots smaller)
   # Output is a list with several plots and summary statistics (see end of function)
  

#----------------------------------------------------------------------------------------
# Fit a bayesian model to estimate p(being in each risk category) and credible intervals
# but with a linear trend over time to "smooth" the risk.
# It would be possible to fit a spline in a more complex model but this is not done here



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
       minYear  <- min(Year)
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
             prob.FSI.cat[i,j,k] <- ifelse((med.den.process[i,j] >= FSI.lower[j,k]) && (med.den.process[i,j] < FSI.upper[j,k]),1,0)
         }
      }
   }

   # probability of being in a threshold category for trend line
   for(i in 1:Nyears){
      for(j in 1:Nspecies){
         for(k in 1:NFSI){
             prob.FSI.cat.trend[i,j,k] <- ifelse((med.den.trend[i,j] >= FSI.lower[j,k]) && (med.den.trend[i,j] < FSI.upper[j,k]),1,0)
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

dim(catch.rate)
catch.rate.red <- catch.rate[ !is.na(catch.rate$CPUE.300m),]
dim(catch.rate.red)

# Convert the species code to a species number because JAGS cannot use character data
species.code <- data.frame(Species.Code =unique(catch.rate.red$Species),
                           Species.num  =1:length(unique(catch.rate.red$Species)), stringsAsFactors=FALSE)
species.code
catch.rate.red <- merge(catch.rate.red, species.code)
xtabs(~Species.Code+Species.num, data=catch.rate.red, exclude=NULL, na.action=na.pass)

FSI.threshold.select <- FSI.threshold[ FSI.threshold$Species.Code %in% species.code$Species.Code,]
FSI.threshold.select <- merge(FSI.threshold.select, species.code)
FSI.threshold.select <- FSI.threshold.select[ order(FSI.threshold.select$Species.num,FSI.threshold.select$FSI.cat ),]

# Convert year number to a unique year number
year.code <- data.frame(Year    =sort(unique(catch.rate.red$Year)), 
                        Year.num=1:length(unique(catch.rate.red$Year)))
year.code
catch.rate.red <- merge(catch.rate.red, year.code)
head(catch.rate.red)

# Convert LocationTTM to a unique numeric values
site.code <- data.frame(LocationTTM  =unique(catch.rate.red$LocationTTM),
                        Site.num =1:length(unique(catch.rate.red$LocationTTM)), stringsAsFactors=FALSE)
site.code
catch.rate.red <- merge(catch.rate.red, site.code)
head(catch.rate.red)

catch.rate.red <- catch.rate.red[ order(catch.rate.red$Species.num, catch.rate.red$Year.num, catch.rate.red$Site.num),]




data.list <- list(Ndata      =nrow(catch.rate.red),
                  Year.num   =catch.rate.red$Year.num,
                  Year       =catch.rate.red$Year,
                  YearUnique =sort(unique(catch.rate.red$Year)),
                  Site.num   =catch.rate.red$Site.num,
                  Species.num=catch.rate.red$Species.num,
                  Density    =catch.rate.red$CPUE.300m+.1*min(catch.rate.red$CPUE.300m[catch.rate.red$CPUE.300m>0]),
                  NFSI       =5,
                  FSI.lower  =matrix(FSI.threshold.select$lower, nrow=length(unique(catch.rate.red$Species.Code)), ncol=5, byrow=TRUE),
                  FSI.upper  =matrix(FSI.threshold.select$upper, nrow=length(unique(catch.rate.red$Species.Code)), ncol=5, byrow=TRUE))
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
  n.iter  =10000,          # total iterations INCLUDING burn in
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
# Need to customize the code to deal with the case of a single species where [] are not given
if(nrow(species.code) > 1){
   beta.table <- data.frame(Species.num=row.names(results$BUGSoutput$summary)[(grepl("beta1[",row.names(results$BUGSoutput$summary),fixed=TRUE))],
                            slope      =results$BUGSoutput$mean$beta1,
                            sd         =results$BUGSoutput$sd$beta1,
                            p.slope.lt.0=results$BUGSoutput$mean$p.beta1.lt.0, stringsAsFactors=FALSE)
   beta.table$Species.num <- as.numeric(substr(beta.table$Species.num,1+regexpr('[',beta.table$Species.num, fixed=TRUE),
                                                       -1+regexpr(']',beta.table$Species.num, fixed=TRUE)))}
if(nrow(species.code) == 1){
   beta.table <- data.frame(Species.num=1,
                            slope      =results$BUGSoutput$mean$beta1,
                            sd         =results$BUGSoutput$sd$beta1,
                            p.slope.lt.0=results$BUGSoutput$mean$p.beta1.lt.0, stringsAsFactors=FALSE)}
beta.table <- merge(species.code, beta.table)
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
all.year.species <- expand.grid(Year        =min(catch.rate$Year,na.rm=TRUE):max(catch.rate$Year, na.rm=TRUE),
                                Species.Code=unique(catch.rate$Species.Code),stringsAsFactors=FALSE)
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
all.year.species <- expand.grid(Year         =min(catch.rate$Year,na.rm=TRUE):max(catch.rate$Year, na.rm=TRUE),
                                Species.Code =unique(catch.rate$Species.Code),stringsAsFactors=FALSE)
trenddata <- merge(trenddata, all.year.species, all=TRUE)
trenddata$med.density = exp(trenddata$med.density) # convert from log to median

# get the posterior density values
select <- grepl('^mu.trend', colnames(results$BUGSoutput$sims.matrix))
thin <- runif(nrow(results$BUGSoutput$sims.matrix)) <= postthin  # select portion of posterior prob rows
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
  geom_point(color="red")+
  geom_line(data=meandata[!is.na(meandata$Year.num),],color="red", size=1)+
  geom_line(data=trenddata[!is.na(trenddata$Year.num),], aes(x=Year, y=med.density), color="blue", size=2)+
  geom_hline(data=FSI.threshold.select, aes(yintercept=lower), alpha=1, color="green")+
  facet_wrap(~Species.Code, ncol=1, scales="free_y")
postplot




#browser()

# plot of the probability of being in each category over time for the underlying trend
# dimensions are prob.FSI.cat[year, species.num, category]
prob.FSI.cat <- results$BUGSoutput$mean$prob.FSI.cat.trend
prob.FSI.cat[1,1,]
plotdata <- data.frame(prob=matrix(prob.FSI.cat,ncol=1))
plotdata$Year.num    <- rep(1:nrow(year.code))
plotdata$Species.num <- rep(1:nrow(species.code), each=nrow(year.code))
plotdata$FSI.num.cat <- rep(1:length(unique(FSI.threshold$FSI.num.cat)), each=nrow(species.code)*nrow(year.code))
# convert the year, species, numeric code to actual year, species, and FSI category
plotdata <- merge(plotdata, year.code,        all=TRUE)
plotdata <- merge(plotdata, species.code, all=TRUE)
plotdata <- merge(plotdata, FSI.threshold[,c("Species.Code","FSI.num.cat","FSI.cat")])
plotdata


# for stacked bar charts for each species
# we need to sort the data frame in reverse order of FSI category
plotdata <- plotdata[ order(plotdata$FSI.cat),] #, decreasing=TRUE),]
plotdata$FSI.cat2 <- factor(plotdata$FSI.cat, levels=rev(levels(plotdata$FSI.cat)), order=TRUE)
fsi.plot <- ggplot(data=plotdata, aes(x=Year, y=prob, fill=FSI.cat2))+
  ggtitle("Probability of being in FSI category based on trend line")+
  ylab("Cumulative probability of being in FSI category")+
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="RdYlGn", direction =-1, name="FSI\nCategory")+
  facet_wrap(~Species.Code, ncol=1)
fsi.plot

# Return the results and plots
  list(species.code=species.code,
       year.code=year.code,
       site.code=site.code,
       beta.table=beta.table,
       meandata =meandata,
       trenddata=trenddata,
       postplot = postplot,
       fsi.plot  = fsi.plot,
       results  = results  # all of the BUGs output
       )
}