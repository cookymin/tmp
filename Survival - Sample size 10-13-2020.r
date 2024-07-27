###############################################################
##    Event Projection - spec for S1944 01-23-2018 v1        ##
##    Option 1 based on protocoal assumption                 ##
###############################################################


rm(list=ls())  

library(segmented)
library(ggplot2)
library(flexsurv)
library(survival)
# library(extrafont)

systime = Sys.time()



path = "P:/Xiaomin Lu/Liver/Journal Club/NASH/ATLAS - Ph3"
setwd(path)

t.AccrComp = 1
t.interim = 3

###########################################################
#                Time-to-event: Exponential               #
###########################################################
## dist. of time-to-event per option 1 of the spec
Exp_curv <- function(lmd){
  Surv <- function(t) exp(-lmd*t)
  pdf <- function(t) lmd*exp(-lmd*t)
  
  return(list(lmd = lmd, Surv = Surv, pdf = pdf))
}


accr <- function(u, Accr.rate, dur){
  rate = (0 < u & u <= dur) * Accr.rate
  return(rate)
} 


## -----------  Calculate  AD(t0,t) ----------- ##

AD.t0_t <- function(t0, t, fn.C, fn.E, Accr.rate, dur){
  intgrd1 <- function(t) fn.C$Surv(t)*fn.E$pdf(t)
  intgrd2 <- function(u){
    tmp = try(integrate(intgrd1, lower = 0, upper = t-t0-u, abs.tol = 1e-20)$value, silent = TRUE)
    if("try-error" %in% class(tmp)) tmp = integrate(Vectorize(intgrd1), lower = 0, upper = t-t0-u, abs.tol = 1e-20)$value
    
    return(accr(u+t0, Accr.rate, dur)*tmp)
  } 
  
  AD = try(integrate(intgrd2,  lower = 0, upper = t-t0, abs.tol = 1e-20)$value, silent = TRUE)
  if("try-error" %in% class(AD)) AD = integrate(Vectorize(intgrd2), lower = 0, upper = t-t0, abs.tol = 1e-20)$value
  
  return(AD)
}


fun.evt <- function(Event.rate, HR, rate.drop, Accr.rate, ratio, Accr.dur, t.interim){
  EFS = 1 - Event.rate
  lmd.c = round(-log(1-rate.drop)/5,6)
  fn.C = Exp_curv(lmd = lmd.c)
  
  lmd.E1 = round(-log(EFS)/3,6)
  lmd.E2 = HR*lmd.E1
  fn.E1 = Exp_curv(lmd = lmd.E1)
  fn.E2 = Exp_curv(lmd = lmd.E2)
  
  TD = AD.t0_t(t0 = 0, t = t.interim, fn.C, fn.E1, Accr.rate, Accr.dur)/(ratio+1) + AD.t0_t(t0 = 0, t = t.interim, fn.C, fn.E2, Accr.rate, Accr.dur)*ratio/(ratio+1)
  
  data.frame(Event.rate, HR, rate.drop, Accr.rate, ratio, Accr.dur, t.interim, TD)
}


t.interim = 3
outdat = NULL
for(ratio in c(1,2))
  for(rate.drop in c(0.2, 0.3))
    for(Event.rate in c(0.2, 0.25, 0.3, 0.35))
      for(HR in c(0.7, 0.75, 0.8, 0.85, 0.9))
        for(Accr.rate in c(400, 600, 800))
          for (Accr.dur in c(1, 1.2, 1.4, 1.6, 1.8, 2))
            for(t.interim in c(3, 4, 5))
            outdat = rbind(outdat, fun.evt(Event.rate, HR, rate.drop, Accr.rate, ratio, Accr.dur, t.interim))

write.csv(outdat, "Event Estimate 01-14-2020.csv", row.names = FALSE)




# ######################################################
# ##                   Simulation                     ##
# ######################################################
# set.seed(1234)
# nrep = 10000; md = NULL
# ratio = 1; rate.drop = 0.2; EFS = 0.65; HR = 0.8; Accr.rate = 400; t.AccrComp = 1.2; t.interim = 4
# N = t.AccrComp * Accr.rate
# 
# lmd.c = round(-log(1-rate.drop)/5,6)
# fn.C = Exp_curv(lmd = lmd.c)
# 
# lmd.E1 = round(-log(EFS)/3,6)
# lmd.E2 = HR*lmd.E1
# fn.E1 = Exp_curv(lmd = lmd.E1)
# fn.E2 = Exp_curv(lmd = lmd.E2)
# 
# for(k in 1:nrep){
#   T1 = rexp(N/(ratio+1), rate=lmd.E1)
#   T2 = rexp(N*ratio/(ratio+1), rate=lmd.E2)
#   C1 = rexp(N/(ratio+1), rate=lmd.c)
#   C2 = rexp(N*ratio/(ratio+1), rate=lmd.c)
#   U1 = runif(N/(ratio+1), 0, N/Accr.rate)
#   U2 = runif(N*ratio/(ratio+1), 0, N/Accr.rate)
#   d1 = sum((T1+U1)<=t.interim & T1<=C1)
#   d2 = sum((T2+U2)<=t.interim & T2<=C2)
#   md = c(md, d1+d2)
# }
# 
# # round(c(mean(md), quantile(md, probs = c(0, 0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975, 1))))
# 
# mean(md)
