#install packages for Carl Schwarz's
#Alberta Fish and Wildlife
#power and watershed assessment tools

#packages for power analysis
install.packages("ggplot2")
install.packages("lme4")
install.packages("lmerTest")
install.packages("pander")
install.packages("plyr")
install.packages("reshape2")

#packages for watershed Assessment
#some packages overlap with power analysis
#but no big deal just wastes a bit of time
#to install twice
install.packages("car")
install.packages("coda")
install.packages("ggmap")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("knitr")
install.packages("lmerTest")
install.packages("pander")
install.packages("plyr")
install.packages("readxl")
install.packages("reshape2")
install.packages("R2jags")  # used for call to JAGS
install.packages("scales")
install.packages("stringr")

#R markdown
install.packages("formatR")
install.packages("caTools")
install.packages("rprojroot")
install.packages("rmarkdown")

#packages for ggmap
install.packages("devtools")
install.packages("sp")
install.packages("maps")
install.packages("DBI")
install.packages("assertthat")
#latest version of ggmap is not on CRAN
devtools::install_github("dkahle/ggmap")
