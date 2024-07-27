rm(list=ls())  

library(mvtnorm)

path = "P:/Xiaomin Lu/CSE/Journal Club/NASH/ATLAS - Ph3"
setwd(path)

find.m1 <- function(m0, n0, n1, st, alpha){
  low = st; up = n1; mid = round((low+up)/2)
  p = fisher.test(cbind(c(m0, low), c(n0-m0, n1-low)), alternative = "less")$p.value
  if (round(p, 10) <= alpha){
    return(low)
  } else{
    p = fisher.test(cbind(c(m0, up), c(n0-m0, n1-up)), alternative = "less")$p.value
    if(round(p, 10) > alpha){
      return(NA)
    } else{
      while(round(up-low) > 1){
        p = fisher.test(cbind(c(m0, mid), c(n0-m0, n1-mid)), alternative = "less")$p.value
        if (round(p, 10) <= alpha){
          up = mid; mid = round((low+up)/2)
        } else{
          low = mid; mid = round((low+up)/2)
        }
      }
      return(up)
    }
  }
}


pwr.fisher <- function(p0, p1, n0, n1, alpha){
  prob = 0; st = 0
  for(i1 in 0:n0){
    i2 = n1
    st = find.m1(i1, n0, n1, st, alpha)
    if(is.na(st)){
      break
    } else {
      prob = prob + sum(dbinom(i1, n0, p0)*dbinom(c(st:n1), n1, p1))
    }
  }
  
  prob
}

###########################################################################
##  Overall power of Hochberg (overall population and subpopulation F4)  ##
##                                                                       ##
##  p.S:    proportion of subpopulation                                  ##  
##  p0.S:   subpopulation response rate in control                       ##
##  del.S:  subpopulation improvement in treatment                       ##
##  p0:     overall response rate in control                             ##
##  del:    Overall improvement in treatment                             ##
##  N0:     overall sample size in control                               ##
##  N1:     overall sample size in treatment                             ##
##                                                                       ##
###########################################################################

Sim.Pwr.Hochberg <- function(p.S, p0.S, del.S, p0, del, N0, N1, alpha){
  set.seed(1234)
  
  N0.S = N0 * p.S; N1.S = N1 * p.S
  N0.NS = N0 - N0.S; N1.NS = N1 - N1.S
  p0.NS = (N0*p0 - N0.S*p0.S)/N0.NS
  p1.S = p0.S + del.S
  p1 = p0 + del
  p1.NS = (N1*p1 - N1.S*p1.S)/N1.NS
  
  Pwr.Fisher = pwr.fisher(p0, p1, N0, N1, alpha)
  
  Rej1 = NULL; Rej2 = NULL; Rej3 = NULL
  Rej1.F = NULL; Rej2.F = NULL; Rej3.F = NULL
  Rej.F = NULL
  Z = NULL; Z.S = NULL
  for(i in 1:nrep){
    Y0.S = rbinom(1, N0.S, p0.S)
    Y0.NS = rbinom(1, N0.NS, p0.NS)
    Y1.S = rbinom(1, N1.S, p1.S)
    Y1.NS = rbinom(1, N1.NS, p1.NS)
    
    pval.S = fisher.test(cbind(c(Y0.S, N0.S-Y0.S), c(Y1.S, N1.S-Y1.S)), alternative="less")$p.value
    pval = fisher.test(cbind(c(Y0.S+Y0.NS, N0-Y0.S-Y0.NS), c(Y1.S+Y1.NS, N1-Y1.S-Y1.NS)), alternative="less")$p.value
    
    Rej = 0 
    if (pval <= alpha & pval.S <= alpha) Rej = 1
    Rej1.F = c(Rej1.F, Rej)
    
    Rej = 0
    if (pval <= alpha/2 & pval.S > alpha) Rej = 1
    Rej2.F = c(Rej2.F, Rej)
    
    Rej = 0
    if (pval > alpha & pval.S <= alpha/2) Rej = 1
    Rej3.F = c(Rej3.F, Rej)
    
    Rej = 0 
    if (pval <= alpha) Rej = 1
    Rej.F = c(Rej.F, Rej)
    
  }
  
  ## prob of rejection
  Sim.Prob1.Fisher = mean(Rej1.F)
  Sim.Prob2.Fisher = mean(Rej2.F)
  Sim.Prob3.Fisher = mean(Rej3.F)
  
  Sim.Pwr.O.Fisher = Sim.Prob1.Fisher + Sim.Prob2.Fisher
  Sim.Pwr.S.Fisher = Sim.Prob3.Fisher
  Sim.Pwr.Fisher = Sim.Pwr.O.Fisher + Sim.Pwr.S.Fisher
  
  Sim.Pwr.Fisher1 = mean(Rej.F)
  
  data.frame(Sim.Prob1.Fisher, Sim.Prob2.Fisher, Sim.Prob3.Fisher, Sim.Pwr.O.Fisher, Sim.Pwr.S.Fisher, Sim.Pwr.Fisher, Sim.Pwr.Fisher1, Pwr.Fisher)
}


Rslt <- function(p.S, p0.S, del.S, p0, del, N0, N1, alpha){
  Sim = Sim.Pwr.Hochberg (p.S, p0.S, del.S, p0, del, N0, N1, alpha)
  dat = cbind(alpha, p.S, p0.S, del.S, p0, del, N0, N1, Sim)
  dat
}


nrep = 10000

cmb <- function(N0, N1){
  outdat = NULL
  for(del.S in c(8:12)/100)
    outdat = rbind(outdat, Rslt(p.S = 0.5, p0.S = 0.12, del.S = del.S, p0 = 0.12, del = del.S, N0, N1, alpha = 0.01/2))
  
  for(del.S in c(5:8)/50){
    outdat = rbind(outdat, Rslt(p.S = 0.5, p0.S = 0.12, del.S = del.S, p0 = 0.12, del = 0.08, N0, N1, alpha = 0.01/2))
  }
  
  outdat = rbind(outdat, Rslt(p.S = 0.5, p0.S = 0.12, del.S = 0.12, p0 = 0.12, del = 0.06, N0, N1, alpha = 0.01/2))
}

outdat = NULL
outdat = rbind(outdat, cmb(N0 = 600, N1 = 900))

write.csv(outdat, "Overall Power Hochberg_Fisher (0.005) 03-26-2020.csv", row.names = FALSE)

