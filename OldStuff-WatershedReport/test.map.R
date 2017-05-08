library(ggplot2)
library(ggmap)
library(maptools)
library(plyr)


stations.csv <- textConnection(
"lon, lat, Station, Type
 -124.9974, 48.91608, B, Air 
 -124.9974, 48.91608, B, Water 
 -124.9956, 48.91664, Sec3,Water
 -125.0013, 48.91199, A, Air
 -124.9947, 48.91795, 750, Air
 -124.9947, 48.91795, 750, Water 
 -124.9847, 48.91983, 1600 , Air
 -124.9847, 48.91983, 1600, Water 
 -124.9778, 48.92227,Sec8, Air
 -124.9778, 48.92227,Sec8, Water 
 -124.9842, 48.91860,Sec5, Water  
 -124.9762, 48.91998,C, Air 
 -124.9762, 48.91998,C, Water 
 -124.9762, 48.91998,CTrib, Water 
 -124.9475, 48.92985,E, Air 
 -124.9475, 48.92985,E, Water 
 -124.9762, 48.92410, 2600, Air 
 -124.9762, 48.92410, 2600, Water 
 -124.9305, 48.93458, ETribUp, Air 
 -124.9305, 48.93458, ETribUp, Water  
 -124.9511, 48.93134, H, Air 
 -124.9511, 48.93134, H, Water 
 -124.9479, 48.93111, RPTrib, Water 
 -124.9454, 48.92344,N, Air  ")
stations <- read.csv(stations.csv, header=TRUE, as.is=TRUE, strip.white=TRUE)

library(ggmap)
# Get the Carnation Creek map

cc.boundaries<-c( -125.01,   48.9, -124.9,     48.94)
 
cc <- get_map(cc.boundaries, maptype="terrain",  source="google")#zoom=13)
cc.map <- ggmap(cc)
plot1 <- cc.map + geom_path(data=boundary.df) +
         geom_point(data=stations, position=position_jitter(h=.002,w=.002), aes(x=lon, y=lat, shape=Type, color=Type),size=2 )+
         geom_text(data=unique(stations[,1:3]), aes(label=Station),hjust="left", nudge_x=.002,size=2)+
         xlab("Latitude")+ylab("Longitude")
plot1