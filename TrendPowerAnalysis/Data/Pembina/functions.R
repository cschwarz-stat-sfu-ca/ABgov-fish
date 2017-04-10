
read.FWIS.workbook<- function(
        workbookName,
        WatershedName='Unknown',
        sheetName="Electrofishing",
        target.species='ALL', # what target species to select
        select.equipment='Backpack',            # what equipment to select
        select.data     ='measure'             # what data to include ("count" and "measure" are two possibilities)
        ){

load <- require(xlsx)
if(!load)stop("unable to load xlsx package")
load <- require(plyr)
if(!load)stop("unable to load plyr package")

# Read the reaw data from the FWMIS workbooks.
# Input parameters are
#      workbookName   - name of workbook with FWIS information
#      wateshedName   - name of the Watershed represented by the FWIS workbook
#      sheetName      - name of the sheed with the CPUE data
#      target.species - vector of species codes for which the CPUE is to be extracted
#      select.equipment-vector of equipment codes (lowercase) for which the CPUE is to be extracted
#      select.data     -which CPUE data sould be included. "measure" (measured fish only), 
#                                                          "count",  (counted fish only) or 
#                                                       c("measure","count") (both types of data)
#
# Ouput 
#   List with elements
#      cpue - the CPUE data
#      excluded.data - data the was excluded from the cpue. This should be checked for problem such as
#                      misspelling of the equipment name etc
#

# What are the valid species codes?
valid.species.codes.csv <- textConnection(
"Species.Code  ,  Species.Common.Name  ,  Scientific.Name  ,  Species.Code2
AFJW  ,  AFRICAN JEWELFISH  ,  HEMICHROMIS BIMACULATUS  ,  AFJW
ARCH  ,  ARCTIC CHAR  ,  SALVELINUS ALPINUS  ,  ARCH
ARGR  ,  ARCTIC GRAYLING  ,  THYMALLUS ARCTICUS  ,  ARGR
AGMN  ,  ARCTIC GRAYLING (BELLY POPLN)  ,  THYMALLUS ARCTICUS MONTANUS  ,  AGMN
ARLM  ,  ARCTIC LAMPREY  ,  LAMPETRA JAPONICA  ,  ARLM
BSSN  ,  BANFF SPRINGS SNAIL  ,  PHYSELLA JOHNSONI  ,  BSSN
BRMN  ,  BRASSY MINNOW  ,  HYBOGNATHUS HANKINSONI  ,  BRMN
BRST  ,  BROOK STICKLEBACK  ,  CULAEA INCONSTANS  ,  BRST
BKTR  ,  BROOK TROUT  ,  SALVELINUS FONTINALIS  ,  BKTR
BNTR  ,  BROWN TROUT  ,  SALMO TRUTTA  ,  BNTR
BLTR  ,  BULL TROUT  ,  SALVELINUS CONFLUENTUS  ,  BLTR
BLBK  ,  BULL TROUT X BROOK TROUT HYBRID  ,  SALVELINUS CONFLUENTUS X SALVELINUS FONTINALIS  ,  BLBK
BURB  ,  BURBOT  ,  LOTA LOTA  ,  BURB
CCHL  ,  CICHLID  ,  PSEUDOTROPHEUS  ,  CCHL
CHSL  ,  COHO SALMON  ,  ONCORHYNCHUS KISUTCH  ,  CHSL
CTTR  ,  CUTTHROAT TROUT  ,  ONCORHYNCHUS CLARKI  ,  CTTR
CRTR  ,  CUTTHROAT TROUT X RAINBOW TROUT  ,  ONCORHYNCHUS CLARKI X ONCORHYNCHUS MYKISS  ,  CRTR
DPSC  ,  DEEPWATER SCULPIN  ,  MYOXOCEPHALUS THOMPSONI  ,  DPSC
DLVR  ,  DOLLY VARDEN  ,  SALVELINUS MALMA  ,  DLVR
EMSH  ,  EMERALD SHINER  ,  NOTROPIS ATHERINOIDES  ,  EMSH
FTMN  ,  FATHEAD MINNOW  ,  PIMEPHALES PROMELAS  ,  FTMN
FNDC  ,  FINESCALE DACE  ,  PHOXINUS NEOGAEUS  ,  FNDC
FLCH  ,  FLATHEAD CHUB  ,  PLATYGOBIO GRACILIS  ,  FLCH
GLTR  ,  GOLDEN TROUT  ,  ONCORHYNCHUS AQUABONITA  ,  GLTR
GOLD  ,  GOLDEYE  ,  HIODON ALOSOIDES  ,  GOLD
GOFS  ,  GOLDFISH  ,  CARASSIUS AURATUS  ,  GOFS
IWDR  ,  IOWA DARTER  ,  ETHEOSTOMA EXILE  ,  IWDR
KOKA  ,  KOKANEE  ,  ONCORHYNCHUS NERKA  ,  KOKA
LKCH  ,  LAKE CHUB  ,  COUESIUS PLUMBEUS  ,  LKCH
LKST  ,  LAKE STURGEON  ,  ACIPENSER FULVESCENS  ,  LKST
LKTR  ,  LAKE TROUT  ,  SALVELINUS NAMAYCUSH  ,  LKTR
LKWH  ,  LAKE WHITEFISH  ,  COREGONUS CLUPEAFORMIS  ,  LKWH
LRSC  ,  LARGESCALE SUCKER  ,  CATOSTOMUS MACROCHEILUS  ,  LRSC
LGPR  ,  LOGPERCH  ,  PERCINA CAPRODES  ,  LGPR
LNDC  ,  LONGNOSE DACE  ,  RHINICHTHYS CATARACTAE  ,  LNDC
LNSC  ,  LONGNOSE SUCKER  ,  CATOSTOMUS CATOSTOMUS  ,  LNSC
LOTS  ,  LONGTAIL TADPOLE SHRIMP  ,  TRIOPS LONGICAUDATUS  ,  LOTS
MOON  ,  MOONEYE  ,  HIODON TERGISUS  ,  MOON
MNSC  ,  MOUNTAIN SUCKER  ,  CATOSTOMUS PLATYRHYNCHUS  ,  MNSC
MNWH  ,  MOUNTAIN WHITEFISH  ,  PROSOPIUM WILLIAMSONI  ,  MNWH
NNST  ,  NINESPINE STICKLEBACK  ,  PUNGITIUS PUNGITIUS  ,  NNST
NOCY  ,  NORTHERN CRAYFISH  ,  ORCONECTES VIRILIS  ,  NOCY
NRPK  ,  NORTHERN PIKE  ,  ESOX LUCIUS  ,  NRPK
NRSQ  ,  NORTHERN PIKEMINNOW  ,  PTYCHOCHEILUS OREGONENSIS  ,  NRSQ
NRDC  ,  NORTHERN REDBELLY DACE  ,  PHOXINUS EOS  ,  NRDC
PMCH  ,  PEAMOUTH CHUB  ,  MYLCHEILUS CAURINUS  ,  PMCH
PRDC  ,  PEARL DACE  ,  MARGARISCUS MARGARITA  ,  PRDC
PRSC  ,  PRICKLY SCULPIN  ,  COTTUS ASPER  ,  PRSC
PGWH  ,  PYGMY WHITEFISH  ,  PROSOPIUM COULTERI  ,  PGWH
QUIL  ,  QUILLBACK  ,  CARPIODES CYPRINUS  ,  QUIL
RNTR  ,  RAINBOW TROUT  ,  ONCORHYNCHUS MYKISS  ,  RNTR
RDSH  ,  REDSIDE SHINER  ,  RICHARDSONIUS BALTEATUS  ,  RDSH
RVSH  ,  RIVER SHINER  ,  NOTROPIS BLENNIUS  ,  RVSH
RNWH  ,  ROUND WHITEFISH  ,  PROSOPIUM CYLINDRACEUM  ,  RNWH
SLML  ,  SAILFIN MOLLY  ,  POECILIA LATIPINNA  ,  SLML
SAUG  ,  SAUGER  ,  STIZOSTEDION CANADENSE  ,  SAUG
SHRD  ,  SHORTHEAD REDHORSE  ,  MOXOSTOMA MACROLEPIDOTUM  ,  SHRD
SHCS  ,  SHORTJAW CISCO  ,  COREGONUS ZENITHICUS  ,  SHCS
SLRD  ,  SILVER REDHORSE  ,  MOXOSTOMA ANISURUM  ,  SLRD
SLSC  ,  SLIMY SCULPIN  ,  COTTUS COGNATUS  ,  SLSC
SMBS  ,  SMALLMOUTH BASS  ,  MICROPTERUS DOLOMIEU  ,  SMBS
SPLA  ,  SPLAKE  ,  SALVELINUS NAMAYCUSH X SALVELINUS FONTINALIS  ,  SPLA
SPSC  ,  SPOONHEAD SCULPIN  ,  COTTUS RICEI  ,  SPSC
SPSH  ,  SPOTTAIL SHINER  ,  NOTROPIS HUDSONIUS  ,  SPSH
SMSC  ,  ST. MARY SCULPIN  ,  COTTUS BAIRDII PUNCTULATUS  ,  SMSC
STON  ,  STONECAT  ,  NOTURUS FLAVUS  ,  STON
THST  ,  THREESPINE STICKLEBACK  ,  GASTEROSTEUS ACULEATUS  ,  THST
TRPR  ,  TROUT-PERCH  ,  PERCOPSIS OMISCOMAYCUS  ,  TRPR
CISC  ,  TULLIBEE (CISCO)  ,  COREGONUS ARTEDI  ,  CISC
TLWH  ,  TULLIBEE (CISCO) X LAKE WHITEFISH  ,  COREGONUS ARTEDI X COREGONUS CLUPEAFORMIS  ,  TLWH
WALL  ,  WALLEYE  ,  SANDER VITREUS  ,  WALL
WEMO  ,  WESTERN MOSQUITOFISH  ,  GAMBUSIA AFFINIS  ,  WEMO
WSMN  ,  WESTERN SILVERY MINNOW  ,  HYBOGNATHUS ARGYRITIS  ,  WSMN
WHSC  ,  WHITE SUCKER  ,  CATOSTOMUS COMMERSONI  ,  WHSC
YLPR  ,  YELLOW PERCH  ,  PERCA FLAVESCENS  ,  YLPR")

valid.species.codes <- read.csv(valid.species.codes.csv, header=TRUE, as.is=TRUE, strip.white=TRUE)

# Argument checking
    if(!file.exists(workbookName))stop("workbookName not found", call.=TRUE)
    wb <- xlsx::loadWorkbook(workbookName)
    sheets <- xlsx::getSheets(wb)
    if(!sheetName %in% names(sheets))stop(paste("sheetName ",sheetName," not found in workbook"), call.=TRUE)
    
    # check that target species are ok
    if(!all(toupper(target.species) %in% toupper(c(valid.species.codes$Species.Code, 'ALL'))))
        stop(paste('at least one species code is not valid:', target.species, collapse=""))
    if('ALL' %in% toupper(target.species))target.species<- toupper(valid.species.codes$Species.Code)
     
    if(!all(tolower(select.equipment) %in% tolower(c("Backpack","Float"))))stop("invalid value for select.equipment")
    if(!all(tolower(select.data) %in% c("measure","count")))stop("invalid value for select.data")
    select.data <- tolower(select.data)
    
    
    
  
#  get the raw data
   cat("\n\n\n*** Starting to read data from ", workbookName,  "***** \n")
   cat(      "    Worksheet ", sheetName, "\n")
   cat(      "    In the ", WatershedName, ' Watershed \n')
   
   fish <- xlsx::read.xlsx(
                 workbookName, 
                 sheetName="Electrofishing",
                 header=TRUE,
                 startRow=3, endRow=2000,
                 colIndex=1:58, # columns A ... BF
                 colClasses='character',  # read all as character 
                 stringsAsFactors=FALSE)
   cat("total rows read ", dim(fish), "\n")

   # The first row of the fish dataframe (corresponding to row 4 of the worksheet) needs to be removed
   fish <- fish[ -1,]
   cat("Removing worksheet row 4. Rows remaining ", dim(fish), "\n")
   

   # The workbook only enters information on the first row of a set corresponding to a survey of a site.
   # Propogate the information in columns 1:36 down until the next row entry
   # There is no easy was to do this except with a loop
   cat(   'First few rows read in \n')
   print(head(fish))
   cat(   'Propogating entries to blank rows\n')
   for(i in 1:nrow(fish)){
     if(!is.na(fish$Survey.Type.Code[i])){ copyRow <- i}
     if( is.na(fish$Survey.Type.Code[i])){ fish[i, 1:36] <- fish[copyRow, 1:36]}
   }
   cat(   'Finished propogating entries to blank rows \n')
   print(head(fish))

   fish.old <- fish
   # Convert to R dates. The date columns has been transformed as the number of day since the Excel orgin (1990-01-01)
   cat(    'Converting start and end dates to R format \n')
   select <- grepl('Start.Date', names(fish))
   names(fish)[select] <- 'Start.Date'
   fish$Start.Date <- as.Date(as.numeric(fish$Start.Date), origin=as.Date('1900-01-01')-2)

   select <- grepl('End.Date', names(fish))
   names(fish)[select] <- 'End.Date'
   fish$End.Date <- as.Date(as.numeric(fish$End.Date), origin=as.Date('1900-01-01')-2)

   # check the diatance and time value
   xtabs(~Distance..m., data=fish, exclude=NULL, na.action=na.pass)
   fish$Distance..m. <- as.numeric(fish$Distance..m.)
   
   xtabs(~Time..seconds., data=fish, exclude=NULL, na.action=na.pass)
   fish$Time..seconds. <- as.numeric(fish$Time..seconds.)
   
   # check to see which equipment code is being used.
   # browser()
   fish$Equipment.Type.Code <- tolower(fish$Equipment.Type.Code)
   fish <- fish[fish$Equipment.Type.Code %in% tolower(select.equipment),]  # select which equipment is used
   fish$Type <- select.equipment
   
   # Extract the unique survey name, site, date, length (m and sec)
   site.date <- unique( fish[, c("Location..","Start.Date","Time..seconds.","Distance..m.")])
   dim(site.date)
   cat("\n\n\nWhat are the unique sites/dates in this worksheet????\n")
   print(site.date)

   # Deal with the counting section of the worksheet.
   # Here fish are counted (but not measured)

   # Pull out the species codes for counted (but not measured fish)
   cat("\n\n\nCheck the species codes for counted but not measured fish \n")
   print(xtabs(~Species.Code, data=fish, exclude=NULL, na.action=na.pass))
   fish$Species.Code <- toupper(substring(fish$Species.Code, nchar(fish$Species.Code)-3))
   print(xtabs(~Species.Code, data=fish, exclude=NULL, na.action=na.pass))

   # convert the oberved count to numeric and remove all NA
   cat("\n\n\nCheck the observed counts\n")
   print(xtabs(~Observed.Count, data=fish, exclude=NULL, na.action=na.pass))

   # remove any > sign
   cat("\n\n Removing any > signs\n")
   fish$Observed.Count <- gsub(">","", fish$Observed.Count, fixed=TRUE)
   fish$Observed.Count <- as.numeric(fish$Observed.Count)
   fish.count <- fish[ !is.na(fish$Observed.Count),]
   cat("\n\nFinal tabuation of species counts")
   print(xtabs(~Observed.Count, data=fish.count, exclude=NULL, na.action=na.pass))

   # Summarized to the Site, Date level of the counted quantities
   fish.count <- plyr::ddply(fish.count, 
                    c("Location..","Type","Start.Date","Time..seconds.","Distance..m.","Species.Code"),function(x){
                     Count=sum(x$Observed.Count, na.rm=TRUE)
                     equip.type=paste(substr(unique(x$Equipment.Type.Code),1,1),sep="", collapse="")
                     data.type="count"
                     data.frame(Count=Count, equip.type=equip.type, data.type='count')
                     })

   # Deal with the measured fish section of the worksheet
   # Summarize to the Site, Date level of the measured fish
   cat("\n\n\nCheck the species codes for measured fish \n")
   print(xtabs(~Species.Code.1, data=fish, exclude=NULL, na.action=na.pass))
   fish$Species.Code.1 <- substring(fish$Species.Code.1, nchar(fish$Species.Code.1)-3)
   fish$Species.Code.1 <- toupper(fish$Species.Code.1)
   print(xtabs(~Species.Code.1, data=fish, exclude=NULL, na.action=na.pass))

   # Summarized to the Site, Date level of the counted quantities
   fish.measure <- plyr::ddply(fish, 
                       c("Location..","Type","Start.Date","Time..seconds.","Distance..m.","Species.Code.1"),function(x){
                        Count=length(x$Species.Code.1)
                        data.type="measure"
                        equip.type=paste(substr(unique(x$Equipment.Type.Code),1,1),sep="", collapse="")
                        data.frame(Count=Count, equip.type=equip.type, data.type=data.type)
                    })
   names(fish.measure)[ names(fish.measure)=='Species.Code.1'] <- 'Species.Code'
   cat("\n\nSummary of fish measured \n")
   print(head(fish.measure))
   

   # add 0's for no fish of a species caught on a site/date combinations
   # We only want the set of unique codes seen somewhere in this worksheets
   watershed.species <- toupper(unique(c(fish.count$Species.Code, fish.measure$Species.Code)))
   cat("\n\nImputing zeros \n")
   zero.count <- plyr::ldply(watershed.species, function(Species.Code, site.date, Type){
       site.date$Species.Code   <- Species.Code
       site.date$Count <- 0
       site.date$Type  <- Type
       site.date
   }, site.date=site.date, Type=select.equipment)
   zero.count$data.type = ''
   zero.count$equip.type=""
   #browser()
   # select which sources of information to be combined together
   data.list <- list(zero.count=zero.count)
   if('measure' %in% select.data)data.list <- c(data.list, list(fish.measure=fish.measure))
   if('count'   %in% select.data)data.list <- c(data.list, list(fish.count  =fish.count))
   fish.sum <- do.call("rbind", data.list)  # combine the dataset togethter
   fish.sum <- plyr::ddply(fish.sum, 
        c("Location..","Type","Start.Date","Species.Code"), function(x){
          Time..seconds. <- sum(x$Time..seconds., na.rm=TRUE)
          Distance..m.   <- sum(x$Distance..m.  , na.rm=TRUE)
          Count <-sum(x$Count, na.rm=TRUE)
          data.type =paste(substr(unique(x$data.type),1,1),sep="",collapse="")
          equip.type=paste(substr(x$equip.type,1,1),sep="", collapse="")
          data.frame(Count=Count, data.type=data.type, equip.type=equip.type, 
                     Time..seconds.=Time..seconds.,
                     Distance..m. = Distance..m.)})
      
   # Remove records from non-target species
   cat("\n\nRetaining records only for target species", target.species,  "\n")
   fish.sum <- fish.sum[ toupper(fish.sum$Species.Code) %in% toupper(target.species),]
   dim(fish.sum)

   
   fish.sum$Watershed <- WatershedName
   fish.sum$Year      <- as.numeric(format(fish.sum$Start.Date, "%Y"))
   #browser()

   # convert to count/100m or count/100s and stack these two measurements
   # together
   if(tolower(select.equipment) %in% tolower('Backpack')){
      fish.sum$Count_100m <- fish.sum$Count/fish.sum$Distance..m. *100
      fish.sum$Count_100s <- fish.sum$Count/fish.sum$Time..seconds. * 100
      fish.measure.vars <- c("Count_100m","Count_100s")
   }
   if(tolower(select.equipment) %in% tolower('Float')){
      fish.sum$Count_km   <- fish.sum$Count/fish.sum$Distance..m. *1000
      fish.sum$Count_100s <- fish.sum$Count/fish.sum$Time..seconds. * 100
      fish.measure.vars <- c("Count_km","Count_100s")
   }

   # convert from wide to long format
   fish.long <- reshape2::melt(fish.sum, 
                  id.vars=c("Watershed",'Location..',"Type","Year",'Start.Date','Species.Code',"data.type","equip.type"),
                  measure.vars=fish.measure.vars,
                  variable.name="Measure",
                  value.name='value')
   fish.long$Measure <- as.character(fish.long$Measure)  # convert from factor to character
   fish.long$Measure <- paste(fish.long$Species.Code,substring(fish.long$Measure,6),sep="")
   cat("Standardize to per 100m and per 100s\n")
   print(head(fish.long))

   fish.long
}



#---------------------------------------------------------------------------
# Estimate the variance components.
# There are 3 cases.
#    One year of data 
#       - estimate only the sampling SD. Unable to estimate process SD
#    Two years of data
#       - assume no trend (i.e. constant mean) to estimate sampling and process SD
#    Three (or more) years of data
#       - estimate trend, plus process and sampling sd
#
#  This is all done on the log-scale to estimate variance components
#  for detecting % trends over xx years.
#  To avoid taking log(0), add 1/2 of smallest positive count to data in the analysis
#

estimate.var.comp <- function(cpue){
   # How many years of data
   cat("Estimating variance components for ", cpue$Watershed[1],cpue$Type[1], cpue$Measure[1], "\n")
   nYears <- length(unique(cpue$Year))
   offset <- 0.5 *min( cpue$value[ cpue$value >0]) # add to avoid 0 counts
   
   message=""

   # create factor as needed
   cpue$YearF     <- factor(cpue$Year)
   cpue$LocationF <- factor(cpue$Location..)
   # the three cases
   # If one year of data, only estimate sampling variation 
   if(nYears == 1){
       SD.sampling <- sd(log(cpue$value+offset))
       SD.process  <- NA
       message <- 'nYears==1'
   }
   # If two years of data, assume no trend between two years to 
   # estimate sampling and process sd
   #browser()
   if(nYears == 2){
      #browser()
      if(length(unique(cpue$Location..)) < nrow(cpue)){  # some locations measured more than once}
         fit <- try(lme4::lmer(log(value+offset)~ (1|YearF)+(1|LocationF), data=cpue ), silent=TRUE)
      } 
      if(length(unique(cpue$Location..)) == nrow(cpue)){  # each location measured only once}
         fit <- try(lme4::lmer(log(value+offset)~ (1|YearF), data=cpue ), silent=TRUE)
      }  
      if(inherits(fit,'try-error') ){  # unable to fit. 
             message='unable to fit lmer'
             SD.sampling <- NA
             SD.process  <- NA
      } else {
         vc <- as.data.frame(VarCorr(fit))
         SD.sampling=vc[vc$grp=="Residual","sdcor"]
         SD.process =vc[vc$grp=="YearF"   ,"sdcor"]
         message='nYears==2'
      }   
   }
   # If three or more years of data, fit a trend and extract the variance components
   if(nYears > 2){
      if(length(unique(cpue$Location..)) < nrow(cpue)){  # some locations measured more than once}
         fit <- try(lme4::lmer(log(value+offset)~ Start.Date + (1|YearF)+(1|LocationF),data=cpue), silent=TRUE)
      } 
      if(length(unique(cpue$Location..)) == nrow(cpue)){  # each location measured only once}
         fit <- try(lme4::lmer(log(value+offset)~ Start.Date + (1|YearF),data=cpue), silent=TRUE)
      }  
      if(inherits(fit,'try-error') ){  # unable to fit. 
          message='unable to fit lmer'
          SD.sampling <- NA
          SD.process  <- NA
      } else {
         vc <- as.data.frame(VarCorr(fit))
         SD.sampling=vc[vc$grp=="Residual","sdcor"]
         SD.process =vc[vc$grp=="YearF"   ,"sdcor"]
         massge='nyears>2'
      }
   }
   data.frame(SD.sampling=SD.sampling,
              SD.process =SD.process,
              message=message)
}



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

library(lme4)


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
estimate.power <- function(vc, alpha=0.05){
  # given a set of vc with SD.sampling and SD.process, create the power graphs
  # set up the scenarios to estimate the power 
  #browser()
  scenarios <- expand.grid(PerChange=c(10, 30, 50, 100, 200),
                           Years=c(5,10),
                           Sampling.SD=vc$SD.sampling,
                           Process.SD=vc$SD.process,
                           sites.per.year=seq(20,100,10),
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
   power.plot <- ggplot2::ggplot(data=plotdata, aes(x=sites.per.year, y=power.1sided.a, color=PerChangeF))+
      ggtitle(paste("Power to detect changes over time for \n", vc$Watershed, " ",vc$Type,"  ", vc$Measure,"\n alpha=",power$alpha[1],
                    "; rProcess SD= ", format(round(power$Process.SD[1] ,2),nsmall=2), 
                    "; rSampling SD= ",format(round(power$Sampling.SD[1],2),nsmall=2), sep=""))+
      geom_line(aes(group=PerChangeF, linetype=PerChangeF))+
      ylab("Power")+ylim(0,1)+geom_hline(yintercept=0.80)+
      facet_wrap(~Years2, ncol=1, scales='fixed')+
      scale_color_discrete(name="Percent\nChange")+
      scale_linetype_discrete(name="Percent\nChange")
   plot(power.plot)
   ggsave(plot=power.plot, file=paste('power-',vc$Watershed,"-",vc$Type,"-",vc$Measure,"-rProcessSD-",
                                      format(round(power$Process.SD[1],2),nsmall=2),".png",sep=""),
           h=6, w=6, dpi=300)
   power
  }
