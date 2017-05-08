# Estimate the power to detect a 10, 20, 50, 100, 200% increase in the abundance over 5 years
# This data has information on the within-year variation, but no information on process error.

# As usually, convert the estimates of variablity to the log-scale to answer the above questions.

library(ggplot2)
library(lme4)
library(lmerTest)
library(xlsx)
library(plyr)
library(reshape2)

source("functions.R")

WatershedName <- "Nordegg"

#target.species <- 'ALL'
target.species <- c("BKTR","BLTR","MNWH")

# equipment type
select.equipment = "Backpack"

FWIS.files.csv<- textConnection("
workbookName, sheetName
FWMIS Loadform BLTR 2015 Nordegg and Tribs-fake.xls,     Electrofishing
FWMIS Loadform BLTR 2016 Nordegg and Tribs.xls,          Electrofishing")

FWIS.files <- read.csv(FWIS.files.csv, header=TRUE, as.is=TRUE, strip.white=TRUE )
cat("List of FWIS files \n")
FWIS.files


# Check that all FWIS files exist
plyr::d_ply(FWIS.files, "workbookName", function (x){
  if(!file.exists(x$workbookName))stop(paste("File ",x,' not found '))
})




#-------------------------------------------------------
# Read in the raw data and estimate the variance components.
#

# read the individual workbooks (each year) and put into a list
cpue.list <- dlply(FWIS.files, "workbookName", function(x, WatershedName, target.species){
   cpue <- read.FWIS.workbook(
        workbookName=x$workbookName,
        WatershedName=WatershedName,
        sheetName=x$sheetName,
        target.species=target.species,
        select.equipment=select.equipment)
   list(cpue)
}, WatershedName=WatershedName, target.species=target.species)


# extract the elements of the list and stack together so that multiple
# year of data are all together
cpue <- ldply(cpue.list, function (x){ x[[1]]})
head(cpue)

# After reading in the data, you should check  basic statistics such as the years
# measurements are taken, the Location names are spelled consistently, etc

xtabs(~Location..+Year,      data=cpue , exclude=NULL, na.action=na.pass)
xtabs(~Species.Code + Year,  data=cpue , exclude=NULL, na.action=na.pass)
xtabs(~data.type + Year,     data=cpue , exclude=NULL, na.action=na.pass)
xtabs(~equip.type + Year,    data=cpue , exclude=NULL, na.action=na.pass)

xtabs(~interaction(Watershed,Type,Measure, drop=TRUE)+Year, data=cpue, exclude=NULL, na.action=na.pass)


# Preliminary plot of the data to check for outliers etc 

prelim <- ggplot(data=cpue, aes(x=Start.Date, y=value))+
   ggtitle("Baseline data")+
   geom_point( position=position_jitter(w=.2))+
   geom_smooth(method="lm", se=FALSE)+
   facet_wrap(~interaction(Watershed,Type,Measure,sep="   "), scales="free_y", ncol=1)
prelim
ggsave(plot=prelim, file='preliminary-plot.png', h=6, w=6, units="in", dpi=300)

prelim.log <- ggplot(data=cpue, aes(x=Start.Date, y=log(value+.2)))+
  ggtitle("Baseline data - log scale")+
  geom_point( position=position_jitter(w=.2))+
  facet_wrap(~Watershed, scales="free_y", ncol=1)+
  geom_smooth(method="lm", se=FALSE)+
  facet_wrap(~interaction(Watershed,Type,Measure,sep="   "), scales="free_y", ncol=1)
prelim.log
ggsave(plot=prelim.log, file='preliminary-plot-log.png', h=6, w=6, units="in", dpi=300)


outliers <- cpue$Type=="FloatBoat" & cpue$Measure=="BLTR_km"  & (cpue$value > 2)  |
            cpue$Type=="FloatBoat" & cpue$Measure=="BLTR_km"  & (cpue$Year > 2005)
cpue[outliers,]

cpue <- cpue[ !outliers,]

# fit the model to the log to get the sampling and process error relative to the mean
vc <-  plyr::ddply(cpue, c("Watershed","Measure","Type"), estimate.var.comp)
cat("Estimated variance components\n")
vc

# you need to manually change SD.process if there is only 1 year of sampling
# or if they cannot be estimated.
# At the moment, I just delete then
vc <- vc[ !(is.na(vc$SD.sampling) | is.na(vc$SD.process)),]
vc


# now for power charts to detect 10%, 30%, 50%, 100%, 200%, 300% change over 5 years 
# by varying the number of sites at different levels of process error

power.res <- ddply(vc, c("Watershed","Type","Measure"), estimate.power)

# The actual power values are available if you want to do furter analyses
# or plots
head(power.res)