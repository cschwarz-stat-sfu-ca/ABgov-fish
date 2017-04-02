# read in Quirk trout densities and compare to FSI thresholds
# ONly one measurement on Quirk/site/year so no estimate of natural variation available.
# Difficult to know the uncertainty in the estimates.


library(ggplot2)
library(plyr)
library(readxl)

# FSI Thresholds
FSI.threshold.csv<- textConnection(
"FSI.num.cat, FSI.cat, lower, upper
1, VHR,   0,   35
2,  HR,  35,  90
3,  MR,  90, 120
4,  LR, 120, 170
5, VLR, 170, 3000")

FSI.threshold <- read.csv(FSI.threshold.csv, header=TRUE, as.is=TRUE, strip.white=TRUE)
FSI.threshold$FSI.cat <- factor( FSI.threshold$FSI.cat, levels= FSI.threshold$FSI.cat[ FSI.threshold$FSI.num.cat], order=TRUE)



pass <- read_excel("Ghost_Quirk_compiledCPUE_AllSpp.xlsx", sheet="Quirk", skip=1)
names(pass) <- make.names(names(pass))
head(pass)


#Not all variables are given in all rows of the df. Copy from the first entry
pass$Watershed <- pass$Watershed[1]
pass$HUC       <- pass$HUC[1]
pass$Waterbody.Name <-pass$Waterbody.Name[1]

dim(pass)
pass <- pass[ !is.na(pass$Year),]  # get rid of blank rows in data table
dim(pass)
head(pass)


pass.melt <- reshape2::melt(pass,
                            id.vars=c("Watershed","Year","Site.No"),
                            measure.vars=c("BKTR","CTTR","BLTR"),
                            variable.name="Species",
                            value.name="Density")
pass.melt$Species <- as.character(pass.melt$Species)
str(pass.melt)
# note that the data frame has density as fish/100m, but the standards are in 
# terms of fish/300m so we need to multiply by 3
pass.melt$Density <- pass.melt$Density*3


# add in all combinations of year and species so that plot "breaks" when data stops
all.year.species <- expand.grid(Year=min(pass.melt$Year,na.rm=TRUE):max(pass.melt$Year, na.rm=TRUE),
                                Species=unique(pass.melt$Species), 
                                Site.No=unique(pass.melt$Site.No), 
                                Watershed=unique(pass.melt$Watershed),stringsAsFactors=FALSE)
dim(pass.melt)
pass.melt <- merge(pass.melt, all.year.species, all=TRUE)
dim(pass.melt)
xtabs(~Species+Year, data=pass.melt, exclude=NULL, na.action=na.pass)


raw.trend <- ggplot2::ggplot(data=pass.melt, aes(x=Year, y=Density, color=Species, linetype=Site.No))+
   ggtitle("Raw data for Quirk Creek with FSI categories")+
   geom_point()+
   geom_line()+
   geom_hline(data=FSI.threshold, aes(yintercept=lower), alpha=0.2)+
   ylab("Density (fish/300 m2)")
raw.trend
ggsave(plot=raw.trend, file='plot-raw-trend.png', h=4, w=6, units="in", dpi=300)


raw.trend.log <- ggplot2::ggplot(data=pass.melt, aes(x=Year, y=log(Density+.1), color=Species, linetype=Site.No))+
   ggtitle("Raw data for Quirk Creek with FSI categories")+
   geom_point()+
   geom_line()+
   geom_hline(data=FSI.threshold, aes(yintercept=log(lower+.1)), alpha=0.2)+
   ylab("log of Density (fish/300 m2)")
raw.trend.log
ggsave(plot=raw.trend.log, file='plot-raw-trend-log.png', h=4, w=6, units="in", dpi=300)


# lets look at the Quirk watershed level, i.e. average over both sites.
# the design is balanced so I don't have do anything special to impute for missing values
quirk.stat <- plyr::ddply(pass.melt, c("Watershed", "Year", "Species"), plyr::summarize,
                          n.sites     =sum(!is.na(Density)),
                          mean.density=mean(Density, na.rm=TRUE),
                          sd.density   =sd  (Density, na.rm=TRUE))
head(quirk.stat)
ggplot2::ggplot(data=quirk.stat, aes(x=mean.density, y=sd.density, color=Species))+
   ggtitle("Relationship between mean and sd")+
   geom_point()
# plot indicates a log-normal distribution may be appropriate

range(log(quirk.stat$mean.density+.3), na.rm=TRUE)

