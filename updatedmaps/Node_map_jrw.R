###load in data from Ben's folders####
#libraries
library(igraph)
library(leaflet)
library(png)
library(plyr)
library(sp)

setwd("/Users/jannawilloughby/Google Drive/My Drive/Willoughby lab/projects - active/maritime invasion/manuscript_ben/Chapter 2 - Maritime Translocation/Data/")
#t1<-read.csv("file:///C:/Users/mcken/Documents/Chapter 2/Data/gulf_edges2.csv")
t1<-read.csv("gulf_edges2.csv")
t2<-count(t1)
t3<-as.character(t1[,1])
t4<-as.character(t1[,2])
t5<-sort(unique(append(t3,t4)))
t6<-read.csv("nodes.csv",header=T)
write.table(t2, "travel routes.csv", col.names=T, row.names=F, sep=",") #moved to new directory
write.table(t6, "port_locations.csv", col.names=T, row.names=F, sep=",")#moved to new directory

####begin making new map####
setwd("/Users/jannawilloughby/Google Drive/My Drive/Willoughby lab/projects - active/maritime invasion/maritime_transport/updatedmaps/")
library(maps)
library(dplyr)
library(geosphere)
library(scales)
locations = read.table("port_locations.csv", header=T, sep=",")
colnames(locations) = c("port", "lat", "long")
routes = read.table("travel_routes.csv", header=T, sep=",")
aedes  = read.table("port_aedes.csv", header=T, sep=",")
all_pairs = merge(x=routes, y=locations, by.x="From", by.y="port")
colnames(all_pairs) = c("From", "To", "freq", "lat_from", "long_from")
all_pairs = merge(x=all_pairs, y=locations, by.x="To", by.y="port")
colnames(all_pairs) = c("To", "From", "freq", "lat_from", "long_from", "lat_to", "long_to")
all_pairs$longchange = rep(0, nrow(all_pairs))
all_pairs$longchange[(all_pairs$long_from <= 0 & all_pairs$long_to >= 0) | (all_pairs$long_from >= 0 & all_pairs$long_to <= 0) ] = 1
all_pairs = all_pairs[order(all_pairs$longchange, decreasing = FALSE),]
rownames(all_pairs) = 1:nrow(all_pairs)
all_pairs$freqlog = log(all_pairs$freq)+0.5

####world####
#map looks
par(mar=c(0,0,0,0))
linewd = 0.5
shadewd = 2
shade  = 0.5
map('world', col="grey80", fill=TRUE, bg="white", lwd=0.05, mar=rep(0,4),border=0, ylim=c(-80,80))

#connections that are easy to plot
for(r in 1:nrow(all_pairs[all_pairs$longchange==0,])){ #nrow(rroutes) 776
  city1 = c(all_pairs$long_from[r], all_pairs$lat_from[r])
  city2 = c(all_pairs$long_to[r], all_pairs$lat_to[r])
  croute = gcIntermediate(city1,  city2, n=50, addStartEnd=TRUE, breakAtDateLine=F)
  lines(croute, col=alpha("slateblue", 0), lwd=0.5)
  lines(croute, col=alpha("slateblue", 0.5), lwd=all_pairs$freqlog[r])
}

#the rest of the routes that made me wanna cry but in the end it was ok
problemroutes = all_pairs[(nrow(all_pairs[all_pairs$longchange==0,])+1):nrow(all_pairs),]
for(r in 1:nrow(problemroutes)){ #nrow(problemroutes)
  city1 = c(problemroutes$long_from[r], problemroutes$lat_from[r])
  city2 = c(problemroutes$long_to[r], problemroutes$lat_to[r])
  croute = gcIntermediate(city1,  city2, n=50, addStartEnd=TRUE, breakAtDateLine=F)
  crouteP = croute[croute[,1] > 0,,drop=F]
  crouteN = croute[croute[,1] < 0,,drop=F]
  if(nrow(crouteP)>1){
    lines(crouteP, col=alpha("slateblue", 0), lwd=0.5)
    lines(crouteP, col=alpha("slateblue", 0.5), lwd=problemroutes$freqlog[r])
  }
  if(nrow(crouteN)>1){
    lines(crouteN, col=alpha("slateblue", 0), lwd=0.5)
    lines(crouteN, col=alpha("slateblue", 0.5), lwd=problemroutes$freqlog[r])
  }
}
#ports
points(x=locations$long, y=locations$lat, col=alpha("palevioletred", 0.8), cex=0.5, pch=20)
#text(locations$port, x=locations$long, y=locations$lat,  col="slateblue", cex=0.5, pos=4)

####gom####
par(mar=c(0,0,0,0))
map('world', col="grey80", fill=TRUE, bg="white", lwd=0.05, mar=rep(0,4),border=0, ylim=c(25,33), xlim=c(-100,-80))
gom = c("FreeportTexas", "Houston", "Galveston", "Gulfport", "New_Orleans", "Mobile", "Tampa")
all_pairs$gom = rep(0,nrow(all_pairs))
all_pairs$gom[all_pairs$From %in% gom & all_pairs$To %in% gom] = 1
gom_pairs = all_pairs[all_pairs$gom==1,]
other_pairs = all_pairs[all_pairs$gom==0,]
other_pairs$gom[other_pairs$From %in% gom | other_pairs$To %in% gom] = 1
other_pairs = other_pairs[other_pairs$gom==1,]

#connections that are easy to plot
for(r in 1:nrow(other_pairs[other_pairs$longchange==0,])){ #nrow(rroutes) 776
  city1 = c(other_pairs$long_from[r], other_pairs$lat_from[r])
  city2 = c(other_pairs$long_to[r], other_pairs$lat_to[r])
  croute = gcIntermediate(city1,  city2, n=50, addStartEnd=TRUE, breakAtDateLine=F)
  lines(croute, col=alpha("slateblue", 0), lwd=0.5)
  lines(croute, col=alpha("slateblue", 0.5), lwd=all_pairs$freqlog[r])
}

#the rest of the routes that made me wanna cry but in the end it was ok
problemroutes = other_pairs[(nrow(other_pairs[other_pairs$longchange==0,])+1):nrow(other_pairs),]
for(r in 1:nrow(problemroutes)){ #nrow(problemroutes)
  city1 = c(problemroutes$long_from[r], problemroutes$lat_from[r])
  city2 = c(problemroutes$long_to[r], problemroutes$lat_to[r])
  croute = gcIntermediate(city1,  city2, n=50, addStartEnd=TRUE, breakAtDateLine=F)
  crouteP = croute[croute[,1] > 0,,drop=F]
  crouteN = croute[croute[,1] < 0,,drop=F]
  if(nrow(crouteP)>1){
    lines(crouteP, col=alpha("slateblue", 0), lwd=0.5)
    lines(crouteP, col=alpha("slateblue", 0.5), lwd=problemroutes$freqlog[r])
  }
  if(nrow(crouteN)>1){
    lines(crouteN, col=alpha("slateblue", 0), lwd=0.5)
    lines(crouteN, col=alpha("slateblue", 0.5), lwd=problemroutes$freqlog[r])
  }
}

#connections that are easy to plot - within gom
for(r in 1:nrow(gom_pairs)){ #nrow(rroutes)
  city1 = c(gom_pairs$long_from[r], gom_pairs$lat_from[r])
  city2 = c(gom_pairs$long_to[r], gom_pairs$lat_to[r])
  croute = gcIntermediate(city1,  city2, n=50, addStartEnd=TRUE, breakAtDateLine=F)
  lines(croute, col=alpha("palevioletred", 0.5), lwd=3)
  lines(croute, col=alpha("slateblue", 0.5), lwd=gom_pairs$freqlog[r])
}
#ports
gom_locations = locations[locations$port %in% gom, ]
points(x=gom_locations$long, y=gom_locations$lat, col=alpha("palevioletred", 1), cex=1.5, pch=20)
#text(locations$port, x=locations$long, y=locations$lat,  col="slateblue", cex=0.5, pos=4)



####average stops and distances####
distance = NULL
all_pairs_di = NULL
for(r in 1:nrow(all_pairs)){
  temp = all_pairs[r,]
  di   = distm(c(temp$long_from, temp$lat_from), c(temp$long_to, temp$lat_to), fun = distHaversine)
  all_pairs_di = rbind(all_pairs_di, c(temp,di))
  for(n in 1:all_pairs$freq[r]){
    distance = rbind(distance, c(temp,di))
  }
}
all_pairs_mv = as.data.frame(distance)
alldistances = as.numeric(all_pairs_mv$V10)
range(alldistances)
hist(alldistances, xlim=c(0,19400000), breaks=seq(0,17000000,100000), ylim=c(0,2000), main="", col=alpha("slateblue", 0.8), xlab="Route Distance (km)")
abline(v=median((alldistances)), col="palevioletred", lwd=2)
median((alldistances))
all_pairs_di = as.data.frame(all_pairs_di)
colnames(all_pairs_di)[10] = "distance"
write.table(alldistances, "../maritime_transport/Output/routedistances.csv", col.names=T, row.names=F, sep=",")

####aedes map####
#map looks
par(mar=c(0,0,0,0))
linewd = 0.5
shadewd = 2
shade  = 0.5
map('world', col="grey80", fill=TRUE, bg="white", lwd=0.05, mar=rep(0,4),border=0, ylim=c(-80,80))
points(x=aedes$longitude[aedes$AegyptiAlb==2], y=aedes$latitude[aedes$AegyptiAlb==2], col=alpha("darkorchid3", 1), cex=0.3, pch=20)
points(x=aedes$longitude[aedes$Alb==1 & aedes$AegyptiAlb==1], y=aedes$latitude[aedes$Alb==1 & aedes$AegyptiAlb==1], col=alpha("dodgerblue3", 1), cex=0.3, pch=20)
points(x=aedes$longitude[aedes$Aegypti==1 & aedes$AegyptiAlb==1], y=aedes$latitude[aedes$Aegypti==1 & aedes$AegyptiAlb==1], col=alpha("firebrick", 1), cex=0.3, pch=20)
points(x=aedes$longitude[aedes$AegyptiAlb==0], y=aedes$latitude[aedes$AegyptiAlb==0], col=alpha("chartreuse4", 1), cex=0.3, pch=20)

