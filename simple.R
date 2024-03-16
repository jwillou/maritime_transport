setwd("/Users/jannawilloughby/Google Drive/My Drive/Willoughby lab/projects - active/maritime invasion/maritime_transport/")
directory = getwd()
outdir    = paste(directory,"/Output/", sep="")

####model setup####
#parameters and values
prob.shore.P  = seq(0,1,0.1)       #probability of of container getting to shore
prob.ship.P   = 0 #seq(0,1,0.1)       #probability of of container getting transferred to another ship
nstops.P      = seq(1,10,1)             #number of stops on route between picking up mozzie and reaching destination port
onshore.P     = seq(0,1,0.2)            #probability of detecting and removing mosquitos from container on land
onship.P      = 0                       #probability of detecting and removing mosquitos from container on ship
prob.surv.P   = seq(0.5,0.9,0.1)        #probability of enough mozzies surviving; this is applied between each port
prob.est.P    = seq(0.5,0.9,0.1)        #probabiility that mozzies establish in new location (once container is on shore)
distances     = read.table("Output/routedistances.csv", sep=",", header=T)

#model run management
reps       = 1     #number of replicate runs
contains   = 100   #number of containers per ship
parameters = expand.grid(prob.shore.P, prob.ship.P, nstops.P, onshore.P, onship.P,prob.surv.P,prob.est.P)
colnames(parameters) = c("prob.shore", "prob.ship", "nstops", "onshore", "onship", "prob.surv", "prob.est")
distances  = distances[distances$x>0,]

####track progress####
print(nrow(parameters))

####run model####
#iterate over unique combinations of parameter values, replicating reps number of times
output = NULL
for(p in 1:nrow(parameters)){
  print(p)
  
  #get parameters for this set of runs
  prob.shore = parameters$prob.shore[p]
  prob.ship  = parameters$prob.ship[p]
  nstops     = parameters$nstops[p]
  onshore    = parameters$onshore[p]
  onship     = parameters$onship[p]
  prob.surv  = c(parameters$prob.surv[p],0.035)
  prob.est   = c(parameters$prob.est[p],0.035)
  
  for(r in 1:reps){
    OUT = NULL
    ship = data.frame(containers = rep(1,contains), shore=rep(0,contains), shorestop=rep(0,contains), rmshore=rep(0,contains), 
                      transfer=rep(0,contains), rmtrans=rep(0,contains), dieroute=rep(0,contains), establish=rep(0,contains)) 
    removed = NULL
    for(n in 1:nstops){
      #remove mozzies that died in transit
      #hist(distances/1.660472e+08, xlab="Route distance survival probability pentalty", main="")
      #abline(v=mean(distances/1.660472e+08), col="firebrick3", lwd=2)
      bionomprobs = rnorm(nrow(ship),prob.surv[1],prob.surv[2]) - sample(x=distances, size=nrow(ship), replace=T)/1.660472e+08
      bionomprobs[bionomprobs<0] = 0
      bionomprobs[bionomprobs>1] = 1
      dies = rbinom(n=nrow(ship), size=1, prob=bionomprobs)
      dies[is.na(dies)] = 1
      ship$dieroute = dies
      dead    = ship[ship$dieroute==0,,drop=F]
      if(nrow(dead)>0){dead$dieroute=n}
      ship    = ship[ship$dieroute==1,,drop=F]
      removed = rbind(removed, dead)
      if(nrow(ship)<1){break}
      
      #move containers to shore, update dataframe; find and move to shore noting the stop
      ship$shore = sample(c(1,0), nrow(ship), prob=c(prob.shore, 1-prob.shore), replace=T)
      toshore    = ship[ship$shore==1,,drop=F]
      ship       = ship[ship$shore==0,,drop=F]
      if(nrow(toshore)>0){toshore$shorestop = n}
      
      #implement on-shore survival check
      bionomprobs = rnorm(nrow(toshore),0.85,0.035)
      bionomprobs[bionomprobs<0] = 0
      bionomprobs[bionomprobs>1] = 1
      dies = rbinom(n=nrow(toshore), size=1, prob=bionomprobs)
      dies[is.na(dies)] = 1
      toshore$dieroute = dies
      dead    = toshore[toshore$dieroute==0,,drop=F]
      if(nrow(dead)>0){dead$dieroute=n}
      toshore    = toshore[toshore$dieroute==1,,drop=F]
      removed = rbind(removed, dead)
      if(nrow(toshore)<1){break}
      
      #check shore container mozzies, check to see if established
      toshore$rmshore = sample(c(1,0), nrow(toshore), prob=c(onshore, 1-onshore), replace=T)
      estabs = suppressWarnings((rbinom(n=length(toshore$establish[toshore$rmshore==0]), size=1, prob=rnorm(length(toshore$establish[toshore$rmshore==0]), prob.est[1], prob.est[2]))), classes = "warning")
      estabs[is.na(estabs)] = 1
      toshore$establish[toshore$rmshore==0] = estabs
      removed = rbind(removed, toshore)
      if(nrow(ship)<1){break}
      
      # #move containers to new ship, update dataframe; find and move noting the stop
      # ship$transfer = sample(c(1,0), nrow(ship), prob=c(prob.ship, 1-prob.ship), replace=T)
      # toship        = ship[ship$transfer==1,,drop=F]
      # ship          = ship[ship$transfer==0,,drop=F]
      # if(nrow(toship)>0){toship$shorestop = n}
      # 
      # #check new ship container mozzies
      # toship$rmtrans = sample(c(1,0), nrow(toship), prob=c(onship, 1-onship), replace=T)
      # removed = rbind(removed, toship)
      # if(nrow(ship)<1){break}
    }
    OUT = rbind(ship, removed)
    
    towrite = c(prob.shore, prob.ship, prob.est[1], nstops, prob.surv[1], onshore, onship, sum(OUT[,1]), sum(OUT[,2]), mean(OUT[,3]), sd(OUT[,3]), sum(OUT[,4]), mean(OUT[,4]), sd(OUT[,4]), sum(OUT[,5]), mean(OUT[,5]), sd(OUT[,5]), sum(OUT[,6]), mean(OUT[,6]), sd(OUT[,6]), sum(OUT[,7]), mean(OUT[,7]), sd(OUT[,7]), sum(OUT[,8]), mean(OUT[,8]), sd(OUT[,8]))
    output = rbind(output, towrite)
  }
}
####copy and save data####
colnames(output) = c("prob.shore","prob.ship","prob.est","nstops","prob.surv","onshore","onship","containers","shore","shorestopM","shorestopSD","rmshoreS","rmshoreM","rmshoreSD","transferS","transferM","transferSD","rmtransS","rmtransM","rmtransSD","dierouteS","dierouteM","dierouteSD","establishS","establishM","establishSD")
write.table(output, paste(outdir,"outputsummary.csv", sep=""), append=F, sep=",", row.names=F, col.names=T)
folder = gsub(":", "_", gsub(" ", "", paste("output_", Sys.time(), ""), fixed = TRUE), fixed = TRUE)
dir.create(paste(outdir,folder,sep=""))

#copy output into folder to use later
data = read.table("Output/outputsummary.csv", header=T, sep=",")
write.table(data, paste("Output/", folder, "/outputsummary.csv", sep=""), row.names=F, col.names=T, sep=",")
#data = read.table("Output/output_2024-03-1609_43_14.560823/outputsummary.csv", header=T, sep=",") #main text figures

####analyze model outpu####
colors6   = c("#d3f2a3","#97e196","#6cc08b","#4c9b82","#217a79","#074050") ##105965 <-extra color, second to last
colors6.2 = c("#fb8bad","#e65c9d","#cf278a","#ae1281","#6a0674","#350068") 

#1. how much does change in land detection influence establishment? -- main figure
pdf(paste("Output/", folder, "/landdet_", prob.surv, "_", prob.est, ".pdf", sep=""), width = 5, height = 5)
#pdf(paste("Output/output_2024-03-1609_43_14.560823/landdet_", prob.surv, "_", prob.est, ".pdf", sep=""), width = 5, height = 5)
plot(-100,-100, xlim=c(0,1), ylim=c(0,1), xlab="probability of detection/removing mosquitos", ylab="prop. established populations")
segments(x0=0,y0=1,x1=1,y1=0,col="grey50", lty=2, lwd=1.5)
for(v in 1:length(onshore.P)){
  t = data[data$prob.shore==as.character(onshore.P[v]) & data$prob.ship==0 & data$prob.est==0.9 & data$nstops==10, ]
  lines(x=t$onshore, y=t$establishM, col=colors6[v], lwd=2.5)
  points(x=t$onshore, y=t$establishM, pch=19, col=colors6[v], cex=1.5)
}
dev.off()

#2. how much does number of stops influence establishment? -- main figure
pdf(paste("Output/", folder, "/stops_det_", prob.surv, "_", prob.est, ".pdf", sep=""), width = 5, height = 5)
#pdf(paste("Output/output_2024-03-1609_43_14.560823/stops_det_", prob.surv, "_", prob.est, ".pdf", sep=""), width = 5, height = 5)
plot(-100,-100, xlim=c(1,10), ylim=c(0,1), xlab="number of stops", ylab="prop. established populations")
for(v in 1:length(onshore.P)){
  t = data[data$prob.shore==0.5 & data$prob.ship==0 & data$prob.est==0.9 & data$prob.surv==0.9 & data$onshore==as.character(onshore.P[v]), ]
  lines(x=t$nstops, y=t$establishM, col=colors6.2[v], lwd=2.5)
  points(x=t$nstops, y=t$establishM, pch=19, col=colors6.2[v], cex=1.5)
}
dev.off()

###sensitivity survival figures####
#1. how much does change in land detection influence establishment? -- supp figure
for(s in 1:length(prob.surv.P)){
  prob.surv=prob.surv.P[s]
  pdf(paste("Output/", folder, "/landdet_", prob.surv, "_", prob.est, ".pdf", sep=""), width = 5, height = 5)
  plot(-100,-100, xlim=c(0,1), ylim=c(0,1), xlab="probability of detection/removing mosquitos", ylab="prop. established populations", main=paste("prob.surv=",prob.surv))
  segments(x0=0,y0=1,x1=1,y1=0,col="grey50", lty=2, lwd=1.5)
  for(v in 1:length(onshore.P)){
    t = data[data$prob.shore==as.character(onshore.P[v]) & data$prob.ship==0 & data$prob.est==0.9 & data$nstops==10 & data$prob.surv==prob.surv, ]
    lines(x=t$onshore, y=t$establishM, col=colors6[v], lwd=2.5)
    points(x=t$onshore, y=t$establishM, pch=19, col=colors6[v], cex=1.5)
  }
  dev.off()
}

for(s in 1:length(prob.est.P)){
  prob.est=prob.est.P[s]
  pdf(paste("Output/", folder, "/landdet_", prob.surv, "_", prob.est, ".pdf", sep=""), width = 5, height = 5)
  plot(-100,-100, xlim=c(0,1), ylim=c(0,1), xlab="probability of detection/removing mosquitos", ylab="prop. established populations", main=paste("prob.est=",prob.est))
  segments(x0=0,y0=1,x1=1,y1=0,col="grey50", lty=2, lwd=1.5)
  for(v in 1:length(onshore.P)){
    t = data[data$prob.shore==as.character(onshore.P[v]) & data$prob.ship==0 & data$prob.est==prob.est & data$nstops==10 & data$prob.surv==0.9, ]
    lines(x=t$onshore, y=t$establishM, col=colors6[v], lwd=2.5)
    points(x=t$onshore, y=t$establishM, pch=19, col=colors6[v], cex=1.5)
  }
  dev.off()
}

#2. how much does number of stops influence establishment? -- main figure
for(s in 1:length(prob.surv.P)){
  prob.surv=prob.surv.P[s]
  pdf(paste("Output/", folder, "/stops_det_", prob.surv, "_", prob.est, ".pdf", sep=""), width = 5, height = 5)
  plot(-100,-100, xlim=c(1,10), ylim=c(0,1), xlab="number of stops", ylab="established populations (%)", main=paste("prob.surv=",prob.surv))
  for(v in 1:length(onshore.P)){
    t = data[data$prob.shore==0.5 & data$prob.ship==0 & data$prob.est==0.9 & data$prob.surv==prob.surv & data$onshore==as.character(onshore.P[v]), ]
    lines(x=t$nstops, y=t$establishM, col=colors6.2[v], lwd=2.5)
    points(x=t$nstops, y=t$establishM, pch=19, col=colors6.2[v], cex=1.5)
  }
  dev.off()
}

for(s in 1:length(prob.est.P)){
  prob.est=prob.est.P[s]
  pdf(paste("Output/", folder, "/stops_det_", prob.surv, "_", prob.est, ".pdf", sep=""), width = 5, height = 5)
  plot(-100,-100, xlim=c(1,10), ylim=c(0,1), xlab="number of stops", ylab="established populations (%)", main=paste("prob.est=",prob.est))
  for(v in 1:length(onshore.P)){
    t = data[data$prob.shore==0.5 & data$prob.ship==0 & data$prob.est==prob.est & data$prob.surv==0.9 & data$onshore==as.character(onshore.P[v]), ]
    lines(x=t$nstops, y=t$establishM, col=colors6.2[v], lwd=2.5)
    points(x=t$nstops, y=t$establishM, pch=19, col=colors6.2[v], cex=1.5)
  }
  dev.off()
}



