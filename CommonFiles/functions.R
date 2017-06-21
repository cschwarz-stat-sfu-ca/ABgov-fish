
read.FWIS.workbook<- function(
        workbookName,
        sheetName="F&W",
        target.species='ALL', # what target species to select
        select.equipment='Backpack',            # what equipment to select
        select.data     ='measure'             # what data to include ("count" and "measure" are two possibilities)
        ){

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

    sheets <- readxl::excel_sheets(workbookName)
    if(!sheetName %in% sheets)stop(paste("sheetName ",sheetName," not found in workbook"), call.=TRUE)
    
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

   fish <- readxl::read_excel(workbookName, sheet=sheetName)
   colnames(fish) <- make.names(colnames(fish))
   cat("total rows read ", dim(fish), "\n")


   # do some data checking
   if(length(unique(fish$Waterbody.Offical.Name))>1)stop("More than one waterbody name present")

   
   # convert ActivityDate to R date format- Date returned in Excel date values 
#  fish$Activity.Date <- as.Date(as.numeric(fish$Activity.Date), origin=as.Date('1900-01-01')-2)
   fish$Activity.Date <- as.Date(fish$Activity.Date)
   fish$Year          <- as.numeric(format(fish$Activity.Date, "%Y"))
  
   # convert TTM.Easting and TTM.Northing to numeric values
   fish$TTM.Easting  <- as.numeric(fish$TTM.Easting)
   fish$TTM.Northing <- as.numeric(fish$TTM.Northing)
   fish$Longitude    <- as.numeric(fish$Longitude)
   fish$Latitude     <- as.numeric(fish$Latitude)
 
   # Group together points that are within +/- 30 m of each other
   # Taken from https://gis.stackexchange.com/questions/17638/how-to-cluster-spatial-data-in-r
   
   # create LocationTTM based on Easting/Northing values
   fish$LocationTTMold <- paste( "E.",fish$TTM.Easting,"-","N.",fish$TTM.Northing, sep="")
   
   # cluster the points to avoid rounding errors
   cluster <- cluster_point(fish)
   
   # merge the cluster id back with the fish data on the Long/Lat co-ordinates
   fish <- merge(fish, cluster, all=TRUE)
   
   # create the LocationTTM variable based on Long/Lat of the clustered data
   fish$LocationTTM <- paste("LO.",fish$Longitude.center,"-","LA.",fish$Latitude.center, sep="")
   
   
   # create Watershed Name using proper case
   if(!'Waterbody.Official.Name' %in% names(fish))stop("Missing Waterbody Official Name")
   # See email dated 2017-06-06 from Andrew Paul
   #fish$WatershedName <- stringr::str_to_title(fish$Waterbody.Official.Name)
   fish$WatershedName <- stringr::str_to_title(fish$HUC)
   
   # convert forklength and weight to numeric
   fish$"Fork.Length..mm." <- as.numeric(fish$"Fork.Length..mm.")
   fish$"Weight..g."       <- as.numeric(fish$"Weight..g.")
   
   # convert Distance and time to numeric
   fish$Distance..m.       <- as.numeric(fish$Distance..m.)
   fish$Time..s.           <- as.numeric(fish$Time..s.)
   
   # convert count to numeric
   fish$Total.Count.of.Species.by.SurveyID <- as.numeric(fish$Total.Count.of.Species.by.SurveyID)
   
   fish}

# In case you can't get JAVA to work. save the sheet as a CSV file and use the
# following function.
read.FWIS.workbook.csv<- function(
        csvfile,
        target.species='ALL', # what target species to select
        select.equipment='Backpack',            # what equipment to select
        select.data     ='measure'             # what data to include ("count" and "measure" are two possibilities)
        ){

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
    if(!file.exists(csvfile))stop("csv file not found", call.=TRUE)

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

   fish <- read.csv(csvfile, header=TRUE, as.is=TRUE, strip.white=TRUE)
   cat("total rows read ", dim(fish), "\n")


   # do some data checking
   if(length(unique(fish$Waterbody.Offical.Name))>1)stop("More than one waterbody name present")

   
   # convert ActivityDate to R date format- Date returned in Excel date values 
   fish$Activity.Date <- as.Date(fish$Activity.Date, "%d-%b-%y")
   fish$Year          <- as.numeric(format(fish$Activity.Date, "%Y"))
  
   # convert TTM.Easting and TTM.Northing to numeric values
   fish$TTM.Easting  <- as.numeric(fish$TTM.Easting)
   fish$TTM.Northing <- as.numeric(fish$TTM.Northing)
   fish$Longitude    <- as.numeric(fish$Longitude)
   fish$Latitude     <- as.numeric(fish$Latitude)
 
   # create LocationTTM based on Lat/Long with some buffer around the points
   fish$LocationTTMold <- paste( "E.",fish$TTM.Easting,"-","N.",fish$TTM.Northing, sep="")
   
   # cluster the points to avoid rounding errors
   cluster <- cluster_point(fish)
   
   # merge the cluster id back with the fish data on the Long/Lat co-ordinates
   fish <- merge(fish, cluster, all=TRUE)
   
   # create the LocationTTM variable based on Long/Lat of the clustered data
   fish$LocationTTM <- paste("LO.",fish$Longitude.center,"-","LA.",fish$Latitude.center, sep="")

      
   # create Watershed Name using proper case
   if(!'Waterbody.Official.Name' %in% names(fish))stop("Missing Waterbody Official Name")
   # See email dated 2017-06-06 from Andrew Paul
   #fish$WatershedName <- stringr::str_to_title(fish$Waterbody.Official.Name)
   fish$WatershedName <- stringr::str_to_title(fish$HUC)
   
   # convert forklength and weight to numeric
   fish$"Fork.Length..mm." <- as.numeric(fish$"Fork.Length..mm.")
   fish$"Weight..g."       <- as.numeric(fish$"Weight..g.")
   
   # convert Distance and time to numeric
   fish$Distance..m.       <- as.numeric(fish$Distance..m.)
   fish$Time..s.           <- as.numeric(fish$Time..s)
   
   # convert count to numeric
   fish$Total.Count.of.Species.by.SurveyID <- as.numeric(fish$Total.Count.of.Species.by.SurveyID)
   
   fish}

read.zero.catch <- function(survey.type.wanted,
                      workbookName=file.path("..","CommonFiles","No fish caught 04021001.xls"),
                      sheetName="NO FISH CAUGHT"){
   # survey.type.wanted. - what type of survey types are wanted here?
  
   # read in the zero catch data base                            
   zero.catch <- readxl::read_excel(workbookName, sheet=sheetName)
   names(zero.catch) <- make.names(names(zero.catch))

   zero.catch$Activity.Date <- as.Date(zero.catch$Activity.Date)
   zero.catch$Year          <- as.numeric(format(zero.catch$Activity.Date, "%Y"))

   # convert TTM.Easting and TTM.Northing to numeric values
   zero.catch$TTM.Easting  <- as.numeric(zero.catch$TTM.Easting)
   zero.catch$TTM.Northing <- as.numeric(zero.catch$TTM.Northing)
   zero.catch$Longitude    <- as.numeric(zero.catch$Longitude)
   zero.catch$Latitude     <- as.numeric(zero.catch$Latitude)
 
   # create LocationTTM based on Easting/Northing values
   # at the moment, no checking for locations that are "close"
   
   zero.catch$LocationTTM <- paste( "E.",zero.catch$TTM.Easting,"-","N.",zero.catch$TTM.Northing, sep="")
   
   # create Watershed Name using proper case
   if(!'Waterbody.Official.Name' %in% names(zero.catch))stop("Missing Waterbody Official Name")
   zero.catch$WatershedName <- stringr::str_to_title(zero.catch$Waterbody.Official.Name)

   # select which survey type is wanted
   zero.catch <- zero.catch[ zero.catch$Survey.Type == survey.type.wanted, ]
   zero.catch
}

#--------------------------------------------------------------------
cluster_point <- function( fish, radius=60){
  # cluster the location points within a 60 m radius of each other
  # Taken from https://gis.stackexchange.com/questions/17638/how-to-cluster-spatial-data-in-r
  
  # assume that the fish data frame has two variables "Longitude" and "Latitude"
  # returns a TTM based on controid of clusters
  require(sp)
  require(geosphere)
  require(stats)
  
  unique.Lat.Long <- unique(fish[,c("Longitude","Latitude")])
  xy <- sp::SpatialPointsDataFrame(
    as.matrix(unique.Lat.Long), data.frame(ID=seq(1:nrow(unique.Lat.Long))),
    proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  
  # use the distm function to generate a geodesic distance matrix in meters
  mdist <- geosphere::distm(xy)
  
  # cluster all points using a hierarchical clustering approach
  hc <- stats::hclust(as.dist(mdist), method="complete")
  
  # define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
  xy$clust <- stats::cutree(hc, h=radius)
  
  # extract the matrix of co-ordinates and cluster number
  coord <- as.data.frame(coordinates(xy))
  coord$clust <- xy@data$clust
  
  # get the centroid for each cluster
  cent <- matrix(ncol=2, nrow=max(xy$clust))
  for (i in 1:max(xy$clust))
    # gCentroid from the rgeos package
    cent[i,] <- rgeos::gCentroid(subset(xy, clust == i))@coords
  
  # add the centroid to the coord data frame
  coord$Longitude.center <- cent[ coord$clust, 1]
  coord$Latitude.center  <- cent[ coord$clust, 2]
  
  # return the data frame of centroids
  coord
  
}

#--------------------------------------------------------------------

extract.inventory <- function(fish, zero.catches){
   # split the fish FWIS data into 3 data files
   #  we need to create 3 files (corresponding to 3rd normal forms for relational tables)
   # for further processing
   #  (a) Inventory records - when and where was an inventory taken
   #          watershedName Year Location Inventory.Survey.ID
   #      Here is where inventories with zero catches are added
   # 
   #  (b) Catch Summary - how many fish by species were captured. 
   #      We expand (a) to include catch including 0 catch for all species 
   #      seen during any inventory
   #          WatershedName Year Location Inventory.Survey.Id  Distance Time Species.Code Catch
   # 
   #  Cc) Fish length records - records for fish actually captured and measured this is basically the FWIS data readin
   #          WatershedName Year Location Inventory.Survey.ID  Species.Code Fork.Length..m. 

   # Input is the fish data.frame extracted from the FWIS system
   #         and  zero.catch data.frame extracted from FWIS system.
   #         The zero.catch data.frame includes ALL watersheds so we
   #         need to extract the relevant record
   #
   # Output is a list with the three data.frames
   
  inventory <- plyr::ddply(fish, c("Inventory.Survey.ID"), function(x){
    # just pull out the first record and drop all of the fish information
    inventory <- x[1,, drop=FALSE]
    drop.columns <- c("Angler.Count","Pass.Number","Captured.Count","Total.Count.of.Species.by.SurveyID",
                      "Species.Code","Species.Common.Name","Weight..g.","Fork.Length..mm.",
                      "Total.Length..mm.","Gender","Age")
    inventory <- inventory[, !names(inventory) %in% drop.columns]
    as.data.frame(inventory)
   })
  
   # extract the zero catch that matches the inventory id
   zero.catch<- zero.catch[ zero.catch$WatershedName == inventory$WatershedName[1],drop=FALSE]
  
   # select the columns that match the inventory data frame
   zero.catch <- zero.catch[, names(zero.catch)[names(zero.catch) %in% names(inventory)]]
   
   inventory <- plyr::rbind.fill(
                 inventory,
                 zero.catch[ !zero.catch$Inventory.Survey.ID %in% inventory$Inventory.Survey.ID,drop=FALSE])
   inventory
}

extract.catch.summary <- function (fish,zero.catch, inventory){
   #---------------------------------------------------------------------------------------
   #  Create the catch summary
   # We first extract for each Inventory.Survey.ID, one record per species
   catch.summary <- plyr::ddply(fish, c("Species.Code","Inventory.Survey.ID"), plyr::summarize,
                            Count        = mean(Total.Count.of.Species.by.SurveyID))
   # Any inventory with 0 counts for all species will have Species.Code = NA
   if( sum(is.na(catch.summary$Species.Code))>0){
      print("Inventory Survey Id Records with no species code which are deleted. Check that imputed properly \n")
      cat(catch.summary$Inventory.Survey.ID[ is.na(catch.summary$Species.Code)])
   }
   dim(catch.summary)
   catch.summary <- catch.summary[ !is.na(catch.summary$Species.Code),]
   dim(catch.summary)

   # Now we need to expand for each Inventory.Survey.ID the full set of species 
   # and impute 0 values for species not seen for this inventory (i.e 0 catch of this species)
   which.species <- unique(catch.summary$Species.Code)
   all.catch <- plyr::ddply(inventory, "Inventory.Survey.ID", function(x, species.list){
     all.species <- expand.grid(Inventory.Survey.ID=x$Inventory.Survey.ID[1],
                                Species.Code       =species.list,
                                stringsAsFactors=FALSE)
     all.species
   }, species.list=which.species)
   catch.summary <- merge(catch.summary, all.catch, all=TRUE)
   catch.summary$Count[ is.na(catch.summary$Count)] <- 0
   catch.summary <- merge(catch.summary, inventory, all.x=TRUE)   # merge it back with inventory records get inventory infor

   catch.summary <- plyr::ddply(catch.summary, c("WatershedName","Species.Code","Year","LocationTTM"), plyr::summarize,
                             Distance..m. = sum(Distance..m., na.rm=TRUE),
                             Count        = sum(Count,        na.rm=TRUE),
                             CPUE.300m    = Count / Distance..m. * 300)
   catch.summary
}

extract.fish <- function(fish){
    #---------------------------------------------------------------------------------------
    #  Look at the fish records - only those fish that are measured are capture
   drop.columns <- c("Captured.Count", "Total.Count.of.Species.by.SurveyID")
   fish <- fish[, !names(fish) %in% drop.columns]
   
   #   Drop fish data with no species code
   fish <- fish[ !is.na(fish$Species.Code),, drop=FALSE]

   fish
}
  


#--------------------------------------------------------------------


# Convert to Proper Case (see help from toupper)
toProperCase <- function(x) {
    x <- tolower(x)
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}

# Keep track of figure and table numbers. Do not modify the global variables .FIGNUM and .TABNUM
getFIGNUM <- function(){
    .FIGNUM <<- .FIGNUM +1
    .FIGNUM
}

getTABNUM <- function(){
  .TABNUM <<- .TABNUM +1
  .TABNUM
}


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
prob.fsi.cat.trend <- plotdata


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
       prob.fsi.cat.trend = prob.fsi.cat.trend,  # probability of being in each fsi category
       results  = results  # all of the BUGs output
       )
}

BayesianNoTrendFSI <- function( catch.rate, FSI.threshold, postthin=.1){
   # Typically USED WITH 2 YEARS of DATA when you cannot fit a slope 
   # do a bayesian analysis on the cpue data and and compute the 
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
# but with NO linear trend over time to "smooth" the risk.
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
          mu.trend  [i,j] <- beta0[j]    # No trend here
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

    # priors for the intercept 
    for(i in 1:Nspecies){
       beta0[i] ~ dnorm(0, .001)
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
                  "beta0")




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


NSpecies <- nrow(species.code)
NYears   <- nrow(year.code)
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

# make a table of the intercept
# Need to customize the code to deal with the case of a single species where [] are not given
if(nrow(species.code) > 1){
   beta.table <- data.frame(Species.num=row.names(results$BUGSoutput$summary)[(grepl("beta0[",row.names(results$BUGSoutput$summary),fixed=TRUE))],
                            intercept  =results$BUGSoutput$mean$beta0,
                            sd         =results$BUGSoutput$sd$beta0,
                            stringsAsFactors=FALSE)
   beta.table$Species.num <- as.numeric(substr(beta.table$Species.num,1+regexpr('[',beta.table$Species.num, fixed=TRUE),
                                                       -1+regexpr(']',beta.table$Species.num, fixed=TRUE)))}
if(nrow(species.code) == 1){
   beta.table <- data.frame(Species.num=1,
                            intercept  =results$BUGSoutput$mean$beta0,
                            sd         =results$BUGSoutput$sd$beta0,
                            stringsAsFactors=FALSE)}
beta.table <- merge(species.code, beta.table)
beta.table

# get just the means
results$BUGSoutput$mean
results$BUGSoutput$mean$parm


# Extract the means and posterior density plots for each species
select <- grepl('^med.den.process[', row.names(results$BUGSoutput$summary), fixed=TRUE)
meandata <- data.frame(med.density=matrix(results$BUGSoutput$mean$med.den.process, ncol=1))
meandata$Year.num <- 1:NYears
meandata$Species.num <- rep(1:NSpecies, each=NYears)
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
#browser()
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
prob.fsi.cat.trend <- plotdata


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
       prob.fsi.cat.trend = prob.fsi.cat.trend,  # probability of being in each fsi category
       results  = results  # all of the BUGs output
       )
}


BayesianSingleYearFSI <- function( catch.rate, FSI.threshold, postthin=.1){
   # If you have only a single year of data, there isn't much that can be
   # done since process error and sampling error (site effects) are completely
   # confounded.
  
   # do a bayesian analysis on the cpue data and and compute the 
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
# based on a single year of data

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
          mu.trend  [i,j] <- beta0[j]    # No trend here
       }
    }
    for(i in 1:Ndata){
       mu.data[i] <- mu.trend[Year.num[i],Species.num[i]]
       Density[i] ~ dlnorm( mu.data[i], tau[Species.num[i]])
    }

    # tau is 1/sd
    for(i in 1:Nspecies){
       tau[i] <- 1/(sd[i]*sd[i])
       sd[i] ~ dunif(.05, 3)   # on the log-scale sd is proportion of the mean
    }

    # priors for the intercept 
    for(i in 1:Nspecies){
       beta0[i] ~ dnorm(0, .001)
    }

   # derived variables.
   # med.den is antilog of  lognormal 
   for(i in 1:Ndata){
      med.den[i] <- exp(mu.data[i])
   }

   for(i in 1:Nyears){
       for(j in 1:Nspecies){
          med.den.trend [i, j] <- exp(mu.trend  [i,j])
          med.den.process[i,j] <- med.den.trend [i,j]
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
                  "beta0")




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



NSpecies <- nrow(species.code)
NYear    <- nrow(year.code)


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

# make a table of the intercept
# Need to customize the code to deal with the case of a single species where [] are not given
if(nrow(species.code) > 1){
   beta.table <- data.frame(Species.num=row.names(results$BUGSoutput$summary)[(grepl("beta0[",row.names(results$BUGSoutput$summary),fixed=TRUE))],
                            intercept  =results$BUGSoutput$mean$beta0,
                            sd         =results$BUGSoutput$sd$beta0,
                            stringsAsFactors=FALSE)
   beta.table$Species.num <- as.numeric(substr(beta.table$Species.num,1+regexpr('[',beta.table$Species.num, fixed=TRUE),
                                                       -1+regexpr(']',beta.table$Species.num, fixed=TRUE)))}
if(nrow(species.code) == 1){
   beta.table <- data.frame(Species.num=1,
                            intercept  =results$BUGSoutput$mean$beta0,
                            sd         =results$BUGSoutput$sd$beta0,
                            stringsAsFactors=FALSE)}
beta.table <- merge(species.code, beta.table)
beta.table

# get just the means
results$BUGSoutput$mean
results$BUGSoutput$mean$parm


# Extract the means and posterior density plots for each species
select <- grepl('^med.den.process[', row.names(results$BUGSoutput$summary), fixed=TRUE)
meandata <- data.frame(med.density=matrix(results$BUGSoutput$mean$med.den.process, ncol=1))
meandata$Year.num <- 1:NYear
meandata$Species.num <- rep(1:NSpecies, each=NYear)
meandata <- merge(meandata, species.code)
meandata <- merge(meandata, year.code)
head(meandata)
all.year.species <- expand.grid(Year        =min(catch.rate$Year,na.rm=TRUE):max(catch.rate$Year, na.rm=TRUE),
                                Species.Code=unique(catch.rate$Species.Code),stringsAsFactors=FALSE)
meandata <- merge(meandata, all.year.species, all=TRUE)

# Extract the underlying trend
select <- grepl('^mu.trend', row.names(results$BUGSoutput$summary))
trenddata <- data.frame(med.density=results$BUGSoutput$summary[select, "mean"])
trenddata$Year.Species <- paste(1,",",1:NSpecies, sep="")
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
plotdata$Year.Species <- "1,1"
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
prob.fsi.cat.trend <- plotdata


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
       prob.fsi.cat.trend = prob.fsi.cat.trend,  # probability of being in each fsi category
       results  = results  # all of the BUGs output
       )
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
   #cat("Estimating variance components for ", cpue$Watershed[1],cpue$Type[1], cpue$Measure[1], "\n")
   nYears <- length(unique(cpue$Year))
   offset <- 0.5 *min( cpue$value[ cpue$value >0]) # add to avoid 0 counts
   
   message=""
   #browser()
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
         fit <- try(lme4::lmer(log(value+offset)~ Year + (1|YearF)+(1|LocationF),data=cpue), silent=TRUE)
      } 
      if(length(unique(cpue$Location..)) == nrow(cpue)){  # each location measured only once}
         fit <- try(lme4::lmer(log(value+offset)~ Year + (1|YearF),data=cpue), silent=TRUE)
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
      ggtitle(paste("Power to detect changes over time for \n", 
                    vc$Watershed, " ",vc$Type,"  ", vc$Measure,"\n alpha=",power$alpha[1],
                    "; rProcess SD= ", format(round(power$Process.SD[1] ,2),nsmall=2), 
                    "; rSampling SD= ",format(round(power$Sampling.SD[1],2),nsmall=2), sep=""))+
      geom_line(aes(group=PerChangeF, linetype=PerChangeF))+
      ylab("Power")+ylim(0,1)+geom_hline(yintercept=0.80)+
      facet_wrap(~Years2, ncol=1, scales='fixed')+
      scale_color_discrete(name="Percent\nChange")+
      scale_linetype_discrete(name="Percent\nChange")
   #plot(power.plot)
   ggsave(plot=power.plot, file=paste('power-',vc$Watershed,"-",vc$Type,"-",vc$Measure,"-rProcessSD-",
                                      format(round(power$Process.SD[1],2),nsmall=2),".png",sep=""),
           h=6, w=6, dpi=300)
   power
  }
