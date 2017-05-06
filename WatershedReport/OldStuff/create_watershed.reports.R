# Create the watershed report
# This is the brewing document that reads the data and gets everything set up.


library(brew)
library(car)
library(ggplot2)
library(gridExtra)
library(Hmisc) # to create LaTeX code
library(knitr)
library(lmerTest)
library(maptools)
library(plyr)
library(reshape2)
library(scales)
library(tm)
library(xlsx)
library(xtable)

source("functions.R")


########################################################################
# Get the raw data 

target.species <- 'ALL'
#target.species <- c("BKTR","BLTR","MNWH")

FWIS.files.csv<- textConnection("
workbookName, sheetName
Data/Quirk Creek.xls,            F&W")

FWIS.files <- read.csv(FWIS.files.csv, header=TRUE, as.is=TRUE, strip.white=TRUE )
cat("List of FWIS files \n")
FWIS.files


# Check that all FWIS files exist
plyr::d_ply(FWIS.files, "workbookName", function (x){
  if(!file.exists(x$workbookName))stop(paste("File ",x,' not found '))
})


# read the individual workbooks (each year) and put into a list
cpue.list <- dlply(FWIS.files, "workbookName", function(x, target.species){
   cpue <- read.FWIS.workbook(
        workbookName  =x$workbookName,
        sheetName     =x$sheetName,
        target.species=target.species)
   list(cpue)
}, target.species=target.species)


# extract the elements of the list and stack together so that multiple
# year of data are all together
cpue <- ldply(cpue.list, function (x){ x[[1]]})
head(cpue)



# After reading in the data, you should check  basic statistics such as the years
# measurements are taken, the Location names are spelled consistently, etc
xtabs(~Activity.Date+Inventory.Survey.ID, data=cpue, exclude=NULL, na.action=na.pass)
unique(cpue[,c("Activity.Date","Inventory.Survey.ID")])


xtabs(~LocationTTM+Year,      data=cpue , exclude=NULL, na.action=na.pass)
# are there any dates that are invalid
cpue[ is.na(cpue$Year),]

xtabs(~Species.Code + Year,  data=cpue , exclude=NULL, na.action=na.pass)
cpue[ is.na(cpue$Species.Code) | cpue$Species.Code=="",]
# remove records with blank species codes
dim(cpue)
cpue <- cpue[ !(is.na(cpue$Species.Code) | cpue$Species.Code==""),]
dim(cpue)
xtabs(~Species.Code + Year,  data=cpue , exclude=NULL, na.action=na.pass)


xtabs(~Survey.Type + Year,     data=cpue , exclude=NULL, na.action=na.pass)
# Remove all non-electrofishing survey types
dim(cpue)
cpue <- cpue[ cpue$Survey.Type %in% c("Electrofishing"),]
dim(cpue)
xtabs(~Survey.Type + Year,     data=cpue , exclude=NULL, na.action=na.pass)


# Check the equipment type field
xtabs(~Equipment.Type + Year,    data=cpue , exclude=NULL, na.action=na.pass)
xtabs(~Equipment.Type + Survey.Type, data=cpue, exclude=NULL, na.action=na.pass)


# Check the fork length field
plyr::ddply(cpue, c("Species.Code"), plyr::summarize, 
            nfish=length(Fork.Length..mm.),
            fl.min=min(Fork.Length..mm.),
            fl.max=max(Fork.Length..mm.))
# which records are missing Fork Length
cpue[ is.na(cpue$Fork.Length..mm.),]


# check the Distance and time fields
xtabs(~Distance..m., data=cpue, exclude=NULL, na.action=na.pass)
xtabs(~LocationTTM+Distance..m., data=cpue, exclude=NULL, na.action=na.pass)

# impute a distance field
cpue$Distance..m.[ is.na(cpue$Distance..m.)] <- 100
xtabs(~Distance..m., data=cpue, exclude=NULL, na.action=na.pass)
xtabs(~LocationTTM+Distance..m., data=cpue, exclude=NULL, na.action=na.pass)

xtabs(~Inventory.Survey.ID+Distance..m., data=cpue, exclude=NULL, na.action=na.pass)

xtabs(~Time..s.,     data=cpue, exclude=NULL, na.action=na.pass)# remove data where no distance or time


# check the counts by species
xtabs(~Species.Code+Total.Count.of.Species.by.SurveyID, data=cpue, exclude=NULL, na.action=na.pass)


##### Function to create report for a single watershed ####
WatershedReport <- function(watershed.selected){
   # Create Sweave file names for individual report. 
   Knitfile <- file.path("Reports",paste(cpue$Watershed[1],".Rnw", sep=""))
   Knitfile <- gsub(" ","-", Knitfile)
   cat("Creating report for ", Knitfile, "\n")
    
   # Brew and knit (i.e. create the separate Sweave files and then generate report)
   brew::brew("WatershedReportTemplate-2017-04-26.Rnw", Knitfile)
   reportdir <- file.path("Reports")
   opts_knit$set(base.dir = reportdir)
   reportfile <- gsub(".Rnw",".tex", Knitfile)
   knitr::knit2pdf(Knitfile, reportfile)
}

# Generate report for each watershed


d_ply(cpue, "WatershedName" , WatershedReport)  # individual park reports



# Keep only pdf report
# unlink(file.path("Reports",c("*.aux", "*.log", "*.Rnw", "*.tex", "*.toc","figure")),recursive=TRUE)


