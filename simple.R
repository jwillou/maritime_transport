setwd("/Users/jannawilloughby/GDrive/Willoughby lab/maritime invasion/maritime_transport/")
directory = getwd()
outdir    = paste(directory,"/Output/", sep="")

####model setup####
#parameters and values
prob.shore.P  = 0.5#seq(0,1,0.1)       #probability of of container getting to shore
prob.ship.P   = 0.5#seq(0,1,0.1)       #probability of of container getting transfered to another ship
prob.est.P    = 0.5#seq(0.5,1,0.1)     #probabiility that mozzies establish in new location (once container is on shore)--Kramer study?
nstops.P      = seq(1,10,1)        #number of stops on route between picking up mozzie and reaching destination port
prob.surv.P   = 0.9#seq(0.8,1,0.1)     #probability of enough mozzies surviving; this is applied between each port
prob.det.P    = seq(0,1,0.2)       #probability of detecting and removing mosquitos from container on land
prob.sdet.P   = seq(0,1,0.2)       #probability of detecting and removing mosquitos from container on ship

#model run management
reps       = 1    #number of replicate runs
contains   = 100  #number of containers per ship
parameters = expand.grid(prob.shore.P, prob.ship.P, prob.est.P, nstops.P, prob.surv.P, prob.det.P, prob.sdet.P)
colnames(parameters) = c("prob.shore", "prob.ship", "prob.est", "nstops", "prob.surv", "prob.det", "prob.sdet")

####run model####
#iterate over unique combinations of parameter values, replicating reps number of times
output = NULL
for(p in 1:nrow(parameters)){
  #get parameters for this set of runs
  prob.shore = parameters$prob.shore[p]
  prob.ship  = parameters$prob.ship[p]
  prob.est   = parameters$prob.est[p]
  nstops     = parameters$nstops[p]
  prob.surv  = parameters$prob.surv[p]
  prob.det   = parameters$prob.det[p]
  prob.sdet  = parameters$prob.sdet[p]
  
  for(r in 1:reps){
    OUT = NULL
    ship = data.frame(containers = rep(1,contains), shore=rep(0,contains), shorestop=rep(0,contains), rmshore=rep(0,contains), 
                      transfer=rep(0,contains), rmtrans=rep(0,contains), dieroute=rep(0,contains), establish=rep(0,contains)) 
    removed = NULL
    for(n in 1:nstops){
      #remove mozzies that died in transit
      ship$dieroute = sample(c(1,0), nrow(ship), prob=c(1-prob.surv, prob.surv), replace=T)
      dead    = ship[ship$dieroute==1,,drop=F]
      if(nrow(dead)>0){dead$dieroute=n}
      ship    = ship[ship$dieroute==0,,drop=F]
      removed = rbind(removed, dead)
      if(nrow(ship)<1){break}
      
      #move containers to shore, update dataframe; find and move to shore noting the stop
      ship$shore = sample(c(1,0), nrow(ship), prob=c(prob.shore, 1-prob.shore), replace=T)
      toshore    = ship[ship$shore==1,,drop=F]
      ship       = ship[ship$shore==0,,drop=F]
      if(nrow(toshore)>0){toshore$shorestop = n}
      
      #check shore container mozzies, check to see if established
      toshore$rmshore = sample(c(1,0), nrow(toshore), prob=c(prob.det, 1-prob.det), replace=T)
      toshore$establish[toshore$rmshore==0] = sample(c(1,0), length(toshore$establish[toshore$rmshore==0]), prob=c(prob.est, 1-prob.est), replace=T)
      removed = rbind(removed, toshore)
      if(nrow(ship)<1){break}
      
      #move containers to new ship, update dataframe; find and move noting the stop
      ship$transfer = sample(c(1,0), nrow(ship), prob=c(prob.ship, 1-prob.ship), replace=T)
      toship        = ship[ship$transfer==1,,drop=F]
      ship          = ship[ship$transfer==0,,drop=F]
      if(nrow(toship)>0){toship$shorestop = n}
      
      #check new ship container mozzies, see if establsihed
      toship$rmtrans = sample(c(1,0), nrow(toship), prob=c(prob.sdet, 1-prob.sdet), replace=T)
      toship$establish[toship$rmtrans==0] = sample(c(1,0), length(toship$establish[toship$rmtrans==0]), prob=c(prob.est, 1-prob.est), replace=T)
      removed = rbind(removed, toship)
      if(nrow(ship)<1){break}
    }
    OUT = rbind(ship, removed)
    towrite = c(prob.shore, prob.ship, prob.est, nstops, prob.surv, prob.det, prob.sdet, apply(OUT, 2, sum, na.rm=T))
    output = rbind(output, towrite)
  }
}
####copy and save data####
colnames(output) = c("prob.shore","prob.ship","prob.est","nstops","prob.surv","prob.det","prob.sdet","containers","shore","shorestop","rmshore","transfer","rmtrans","dieroute","establish")
write.table(output, paste(outdir,"outputsummary.csv", sep=""), append=F, sep=",", row.names=F, col.names=T)
folder = gsub(":", "_", gsub(" ", "", paste("output_", Sys.time(), ""), fixed = TRUE), fixed = TRUE)
dir.create(paste(outdir,folder,sep=""))

#copy output into folder to use later
data = read.table("Output/outputsummary.csv", header=T, sep=",")
write.table(data, paste("Output/", folder, "/outputsummary.csv", sep=""), row.names=F, col.names=T, sep=",")

####analyze model outpu####
colors6 = c("#d3f2a3","#97e196","#6cc08b","#4c9b82","#217a79","#074050") ##105965 <-extra color, second to last

#1. how much does change in land detection influence establishment?
pdf(paste("Output/", folder, "/landdet_.pdf", sep=""), width = 5, height = 5)
plot(-100,-100, xlim=c(0,1), ylim=c(0,1), xlab="P(removing from unloaded containers)", ylab="established populations (%)")
for(v in 1:length(prob.sdet.P)){
  t = data[data$prob.shore==0.5 & data$prob.ship==0.5 & data$prob.est==0.5 & data$nstops==5 & data$prob.surv==0.9 & data$prob.sdet==prob.sdet.P[v], ]
  lines(x=t$prob.det, y=t$establish/100, lwd=2, col=colors6[v])
  points(x=t$prob.det, y=t$establish/100, pch=19, col=colors6[v])
}
dev.off()

#2. how much does change in ship-board detection influence establishment?
pdf(paste("Output/", folder, "/shipdet_.pdf", sep=""), width = 5, height = 5)
plot(-100,-100, xlim=c(0,1), ylim=c(0,1), xlab="P(removing from onboard containers)", ylab="established populations (%)")
for(v in 1:length(prob.det.P)){
  t = data[data$prob.shore==0.5 & data$prob.ship==0.5 & data$prob.est==0.5 & data$nstops==5 & data$prob.surv==0.9 & data$prob.det==prob.det.P[v], ]
  lines(x=t$prob.sdet, y=t$establish/100, lwd=2, col=colors6[v])
  points(x=t$prob.sdet, y=t$establish/100, pch=19, col=colors6[v])
}
dev.off()

#3. how much does number of stops influence establishment?  ####STOPPED HERE< NEEX to UPDATE
pdf(paste("Output/", folder, "/stops_det.pdf", sep=""), width = 5, height = 5)
plot(-100,-100, xlim=c(0,1), ylim=c(0,1), xlab="number of stops", ylab="established populations (%)")
for(v in 1:length(prob.det.P)){
  t = data[data$prob.shore==0.5 & data$prob.ship==0.5 & data$prob.est==0.5 & data$prob.surv==0.9 & data$prob.det==prob.det.P[v], ]
  lines(x=t$prob.sdet, y=t$establish/100, lwd=2, col=colors6[v])
  points(x=t$prob.sdet, y=t$establish/100, pch=19, col=colors6[v])
}
dev.off()


