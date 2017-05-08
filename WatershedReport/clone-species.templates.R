# Clone the BKTR species files for other species
library(plyr)

species.list <- c("ARGR","BLBK",'BLTR',"BURB",'CTTR',"LKCH","RNTR")

l_ply(species.list, function (x){
   # read the BKTR SpeciesSubsection, change all BKTR to the species list, and write out new file
   filename <- 'QC-SpeciesSubsection-BKTR.Rmd'
   template <- readLines(filename)
   template <- gsub("BKTR", x, template, fixed=TRUE)
   filename <- gsub("BKTR", x, filename, fixed=TRUE) 
   writeLines(template, filename, sep='\r\n' )
})