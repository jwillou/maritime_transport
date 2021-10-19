setwd("~/Desktop/Maritime prob. of invasion/")

#movement parameters
prob.srce.P  = 0.9 #seq(0.1,0.9,0.2)    #probability of enough mozzies getting onto the ship that establishment is possible (whatever number this is)
prob.term.P  = 0.5 #seq(0.1,0.9,0.2)    #probabiility that enough mozzies made it to shore 
nstops.P     = seq(10,100,10)      #number of stops on route between picking up mozzie and reaching destination port
prob.surv.P  = 0.5 #seq(0.1,0.9,0.2)    #probability of enough mozzies surviving; this is applied between each port

#terminal port parameters
prob.det.t.P = 0.5 #seq(0.5,0.9,0.2)   #probability of detecting infestation at terminal port
rm.eff.t.P   = 0.5 #seq(0.5,0.9,0.2)   #effectiveness of removal actions at terminal port

#source port parameters
prob.det.s.P = 0.5 #seq(0.5,0.9,0.2)   #probability of detecting infestation at source port
rm.eff.s.P   = 0.5 #seq(0.5,0.9,0.2)   #effectiveness of removal actions at source port

#model run management
reps       = 1000  #number of replicate runs
parameters = expand.grid(prob.srce.P, prob.term.P, nstops.P, prob.surv.P, prob.det.t.P, rm.eff.t.P, prob.det.s.P, rm.eff.s.P)
colnames(parameters) = c("prob.srce.P", "prob.term", "nstops", "prob.surv", "prob.det.t", "rm.eff.t", "prob.det.s", "rm.eff.s")

#iterate over unique combinations of parameter values, replicating reps number of times
summary.output = NULL
for(p in 1:nrow(parameters)){
  #get parameters for this set of runs
  prob.srce  = parameters$prob.srce[p]
  prob.term = parameters$prob.term[p]
  nstops     = parameters$nstops[p]
  prob.surv  = parameters$prob.surv[p]
  prob.det.t = parameters$prob.det.t[p]
  rm.eff.t   = parameters$rm.eff.t[p]
  prob.det.s = parameters$prob.det.s[p]
  rm.eff.s   = parameters$rm.eff.s[p]
  
  #set temp output objects
  OUT = detect.source = detect.terminal = NULL
  for(r in 1:reps){
    #determine if mozzies make it onto ship at source port
    ship.mozzies = sample(c(0,1), 1, prob=c((1-prob.srce), prob.srce))

    if(ship.mozzies==1){ #if mozzies on ship
      #determine if  mozzies detected/removed prior to leaving port
      detect.mozzies = sample(c(0,1), 1, prob=c((1-prob.det.s), prob.det.s))
      detect.source  = c(detect.source, detect.mozzies)
      
      if(detect.mozzies==1){
        #try to remove mozzies
        remove.mozzies = sample(c(0,1), 1, prob=c((1-rm.eff.s), rm.eff.s))
        if(remove.mozzies==1){
          OUT = rbind(OUT, c(r, 1, "removed.at.source"))
          next
        }
      }
      
      #determine if mozzies survive at each step in journey
      survive = sample(c(0,1), nstops, prob=c((1-prob.surv), prob.surv), replace=T)
      
      if(sum(survive)==length(survive)){ #if mozzies on survive ship
        #determine if mozzies make it to land at terminal port
        shore.mozzies = sample(c(0,1), 1, prob=c((1-prob.term), prob.term))
        
        if(shore.mozzies==1){ #mozzies make it off of ship
          #determine if  mozzies detected/removed prior to leaving terminal ship
          detect.mozzies  = sample(c(0,1), 1, prob=c((1-prob.det.t), prob.det.t))
          detect.terminal = c(detect.terminal, detect.mozzies)
          
          if(detect.mozzies==1){
            #try to remove mozzies
            remove.mozzies = sample(c(0,1), 1, prob=c((1-rm.eff.t), rm.eff.t))
            if(remove.mozzies==1){
              OUT = rbind(OUT, c(r, 1, "removed.at.terminal"))
              next
            }
          }
          OUT = rbind(OUT, c(r, 1, "released"))
          next
        }else{ #mozzies remain on ship
          OUT = rbind(OUT, c(r, 0, "stayed.terminal"))
          next
        }
      }else{ #mozzies died in transit
        OUT = rbind(OUT, c(r, 0, "died.in.transit"))
        next
      }
    }else{ #mozzies did not make it onto ship at source
      OUT = rbind(OUT, c(r, 0, "missed.source"))
      next
    }
  }
  removed.at.source   = nrow(OUT[OUT[,3]=="removed.at.source",,drop=F])
  removed.at.terminal = nrow(OUT[OUT[,3]=="removed.at.terminal",,drop=F])
  released            = nrow(OUT[OUT[,3]=="released",,drop=F])
  stayed.terminal     = nrow(OUT[OUT[,3]=="stayed.terminal",,drop=F])
  died.in.transit     = nrow(OUT[OUT[,3]=="died.in.transit",,drop=F])
  missed.source       = nrow(OUT[OUT[,3]=="missed.source",,drop=F])
  
  summary.output = rbind(summary.output, c(unlist(parameters[p,]), removed.at.source, removed.at.terminal, released, stayed.terminal, died.in.transit, missed.source, (prob.srce*prob.term*(prob.surv^nstops)), sum(detect.source)/length(detect.source), sum(detect.terminal)/length(detect.terminal)))
}
rownames(summary.output) = seq(1,nrow(summary.output), 1)
colnames(summary.output) = c(colnames(parameters), "removed.at.source", "removed.at.terminal", "released", "stayed.terminal", "died.in.transit", "missed.source", "quant.prob", "detect.source", "detect.terminal")
