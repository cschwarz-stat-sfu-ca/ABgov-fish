#part of the Shiny app
#for global variables
#and initial set up

#clear all existing objects
rm(list=ls())

first<-TRUE

source("Schwarz.R")

#create reactiveValue for updating tables and figures
rv<-reactiveValues(update=0)
vc<<-data.frame(Watershed="",Measure="",SD.sampling=0.5,
               SD.process=0.1)
i.years<<-5
alpha.set<<-0.05


