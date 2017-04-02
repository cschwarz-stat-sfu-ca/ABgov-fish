
#----------------------------------------------------------------------------------------
# Fit a bayesian model to estimate p(being in each risk category) and credible intervals
# without any trend in the study. 


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
    for(i in 1:Ndata){
       mu.data[i] <- mu.grand[Species.num[i]] + 
                     site.eff[Site.num[i],Species.num[i]] +  
                     year.eff[Year.num[i],Species.num[i]]
       Density[i] ~ dlnorm( mu.data[i], tau[Species.num[i]])
    }

    # tau is 1/sd
    for(i in 1:Nspecies){
       tau[i] <- 1/(sd[i]*sd[i])
       sd[i] ~ dunif(.01, 2)   # on the log-scale sd is proportion of the mean
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

    # grand mean for each species across all year and sites
    for(i in 1:Nspecies){
       mu.grand[i] ~ dnorm(0, .001)
    }


    # estimate the mean (over all sites) in year i for species j
    for(i in 1:Nyears){
       for(j in 1:Nspecies){
         mu.year[i, j] <- mu.grand[j] + year.eff[i,j]
       }
    }

   # derived variables.
   # med.yearlog is antilog of mu.year[i,j] and is the median (over all sites) 
   for(i in 1:Nyears){
       for(j in 1:Nspecies){
          med.year[i, j] <- exp(mu.year[i,j])
       }
   }

   # probability that the median density in year i for species j being in a threshold category
   for(i in 1:Nyears){
      for(j in 1:Nspecies){
         for(k in 1:NFSI){
             prob.FSI.cat[i,j,k] <- ifelse((med.year[i,j] >= FSI.lower[k]) && (med.year[i,j] < FSI.upper[k]),1,0)
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


# remove missing values
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
                  Species.num=pass.melt.red$Species.num,
                  Site.num   =pass.melt.red$Site.num,
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
monitor.list <- c("mu.grand","site.eff","year.eff",
                  "sd","sd.year.eff","sd.site.eff", 
                   "mu.data","mu.year","med.year", "prob.FSI.cat")




# Finally, the actual call to JAGS
set.seed(342334)  # intitalize seed for MCMC 

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
results$BUGSoutput$summary[grepl("mu.year",row.names(results$BUGSoutput$summary)),c("mean", "sd", "2.5%","97.5%","Rhat", "n.eff")]
results$BUGSoutput$summary[grepl("med.year",row.names(results$BUGSoutput$summary)),c("mean", "sd", "2.5%","97.5%","Rhat", "n.eff")]


# get just the means
results$BUGSoutput$mean
results$BUGSoutput$mean$parm


# Extract the means and posterior density plots for each species
select <- grepl('^med.year', row.names(results$BUGSoutput$summary))
meandata <- data.frame(med.density=results$BUGSoutput$summary[select, "mean"])
meandata$Year.Species <- row.names(results$BUGSoutput$summary)[select]
meandata$Year.Species <- gsub("med.year[","", meandata$Year.Species, fixed=TRUE)
meandata$Year.Species <- gsub("]",  "", meandata$Year.Species, fixed=TRUE)
head(meandata)
# convert the year,species code to actual years and species
meandata$Year.num   <- as.numeric(substr(meandata$Year.Species,1,-1+regexpr(',',meandata$Year.Species)))
meandata$Species.num<- as.numeric(substring(meandata$Year.Species,1+regexpr(',',meandata$Year.Species)))
head(meandata)
meandata <- merge(meandata, species.code)
meandata <- merge(meandata, year.code)
head(meandata)
all.year.species <- expand.grid(Year=min(pass.melt$Year,na.rm=TRUE):max(pass.melt$Year, na.rm=TRUE),
                                Species=unique(pass.melt$Species),stringsAsFactors=FALSE)
meandata <- merge(meandata, all.year.species, all=TRUE)
dim(meandata)

# add the violin plots
select <- grepl('^med.year', colnames(results$BUGSoutput$sims.matrix))
plotdata <- reshape2::melt(as.data.frame( results$BUGSoutput$sims.matrix[, select]),
                           variable.name='Year.Species',
                           value.name='med.density')
plotdata$Year.Species <- gsub("med.year[","", plotdata$Year.Species, fixed=TRUE)
plotdata$Year.Species <- gsub("]",  "", plotdata$Year.Species, fixed=TRUE)
head(plotdata)
# convert the year,species code to actual years and species
plotdata$Year.num   <- as.numeric(substr(plotdata$Year.Species,1,-1+regexpr(',',plotdata$Year.Species)))
plotdata$Species.num<- as.numeric(substring(plotdata$Year.Species,1+regexpr(',',plotdata$Year.Species)))
head(plotdata)
plotdata <- merge(plotdata, species.code)
plotdata <- merge(plotdata, year.code)
head(plotdata)
sum(is.na(plotdata$med.density))

# make a plot of the posterior mean

postplot <- ggplot2::ggplot( data=meandata, aes(x=Year, y=med.density))+
  ggtitle("Estimated MEDIAN density")+
  ylab("Median from fitted model and posterior beliefs")+
  geom_point(data=plotdata, aes(group=Year), alpha=0.01, position=position_jitter(w=0.2))+
  geom_point(data=meandata)+
  geom_line(color="blue", size=2)+
  geom_hline(data=FSI.threshold[1:3,], aes(yintercept=lower), alpha=1, color="red")+
  facet_wrap(~Species, ncol=1, scales="free_y")+
  ylim(0,150)
postplot
ggsave(plot=postplot, file='plot-posterior-simple.png', h=4, w=6, units="in", dpi=300)



# plot of the probability of being in each category over time
prob.FSI.cat <- results$BUGSoutput$mean$prob.FSI.cat
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
prob.cat <- ggplot(data=plotdata, aes(x=Year, y=prob, fill=FSI.cat2))+
  ggtitle("Probability of being in FSI category")+
  ylab("Cumulative probability of being in FSI category")+
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="RdYlGn", direction =-1)+
  facet_wrap(~Species, ncol=1)
prob.cat
ggsave(plot=prob.cat, file='plot-prob-cat-simple.png', h=4, w=6, units="in",dpi=300)



#--------------------------------------------------------------
# Sample size determination

results$BUGSoutput$summary[grepl("sd",row.names(results$BUGSoutput$summary)),c("mean", "sd", "2.5%","97.5%","Rhat", "n.eff")]
results$BUGSoutput$summary[grepl("mu.grand",row.names(results$BUGSoutput$summary)),c("mean", "sd", "2.5%","97.5%","Rhat", "n.eff")]

# extract the site, year and process error for the three species
varcomp <- data.frame(Species.num=1:3,
                      med.log =results$BUGSoutput$mean$mu.grand,
                      med.regular=exp(results$BUGSoutput$mean$mu.grand),
                      SDresid = results$BUGSoutput$mean$sd,
                      SDsite  = results$BUGSoutput$mean$sd.site.eff,
                      SDyear  = results$BUGSoutput$mean$sd.year.eff, stringsAsFactors=FALSE)
varcomp <- merge(species.code, varcomp)
varcomp

temp<-varcomp
temp[, 3:7] <- round(temp[, 3:7],2)
temp[, 2:7]

# Generate sample size and see probability of belonging to correct category
prob.cat <- plyr::ddply(varcomp, "Species", function (x){
    res <- ldply(seq(5,50,5), function(n.sites, varcomp){
       # generate the median estimate for the required sample size
       year.eff <- rnorm(10000, mean=0, sd=varcomp$SDyear)
       site.eff <- rnorm(10000, mean=0, sd=(varcomp$SDsite^2+varcomp$SDresid^2)/sqrt(n.sites))
       med <- x$med.log + year.eff + site.eff
       p.med.lt.30 <- mean( med < log(30))
       data.frame(n.sites=n.sites, mean.med=mean(med), p.med.lt.30=p.med.lt.30)
   }, varcomp=x)
   res}) 
prob.cat

prob.cat$p.correct <- prob.cat$p.med.lt.30
prob.cat$p.correct[prob.cat$Species=='BKTR'] <- 1-prob.cat$p.correct[prob.cat$Species=='BKTR'] 

plot.correct <- ggplot(data=prob.cat, aes(x=n.sites, y=p.correct))+
   ggtitle("Probability of correct classification")+
   geom_line()+
   facet_wrap(~Species, ncol=1)+
   ylim(c(0,1))
plot.correct 
ggsave(plot=plot.correct, file="plot-correct-class.png", h=6, w=6, units="in", dpi=300)
