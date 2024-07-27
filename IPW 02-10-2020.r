rm(list=ls())   

library(sas7bdat)
library(dplyr)

path = "P:/Xiaomin Lu/Liver/Journal Club/NASH/Propensity/IPW"
setwd(path)

datps = read.sas7bdat("datps.sas7bdat")
ps_wt = read.sas7bdat("ps_wt.sas7bdat")


# dat = c(Y, trt, s1, s2, wt)
CMH <- function(dat){
  dat_c = dat[rowSums(is.na(dat)) == 0,] 
  
  s = do.call(sprintf, c(dat_c[c("s1","s2")], '%s-%s'))
  if (sum(table(s)==0) > 0) s = dat_c$s1
  
  dat_c = cbind(dat_c, s)

  s.val = unique(s)
  
  Y = dat_c$Y; trt = dat_c$trt; wt = dat_c$wt
  p_A = sum(wt*trt*Y)/sum(wt*trt)
  p_B = sum(wt*(1-trt)*Y)/sum(wt*(1-trt))
  
  d_h = NULL; w_h = NULL; V_h = NULL
  for(i in 1:length(s.val)){
    dat_s = dat_c[dat_c$s == s.val[i],]
    Y = dat_s$Y; trt = dat_s$trt; wt = dat_s$wt
    n_Ah = sum(wt*trt); n_Bh = sum(wt*(1-trt))
    p_Ah = sum(wt*trt*Y)/n_Ah
    p_Bh = sum(wt*(1-trt)*Y)/n_Bh
    
    d_h = c(d_h, p_Ah - p_Bh)
    w_h = c(w_h, n_Ah*n_Bh/(n_Ah+n_Bh))
    p_Ah.s = (sum(wt*trt*Y)+0.5)/(n_Ah+1)
    p_Bh.s = (sum(wt*(1-trt)*Y)+0.5)/(n_Bh+1)
    V_h = c(V_h, p_Ah.s*(1-p_Ah.s)/(n_Ah-1)+p_Bh.s*(1-p_Bh.s)/(n_Bh-1))
  }
  
  diff = sum(d_h*w_h)/sum(w_h)
  SE = sqrt(sum(w_h^2*V_h)/(sum(w_h))^2)
  LLC = diff - qnorm(1-alpha/2)*SE
  ULC = diff + qnorm(1-alpha/2)*SE
  pval = 2-2*pnorm(abs(diff/SE))
  data.frame(p_A, p_B, diff, SE, LLC, ULC, pval)
}


# datwt <- function(dat, var_clas, var_num){
#   logis <- glm(formula(paste0("trt ~ ", var_clas, " + ", var_num)), data=dat, family=binomial)
#   
#   p = predict(logis, type = "response")
#   p.num = as.numeric(names(p))
#   PS = rep(NA, nrow(dat))
#   PS[p.num] = p
#   dat.wt = cbind(dat, PS)
#   dat.wt <- mutate(dat.wt, wt = trt/PS + (1-trt)/(1-PS), wt = ifelse(wt>10, 10, wt)) %>%
#     filter(Yc != '')  %>%
#     select(Y, trt, s1, s2, wt)
#   
#   dat.wt
# }

datwt <- function(dat, var_clas, var_num){
  logis <- glm(formula(paste0("trt ~ ", var_clas, " + ", var_num)), data=dat, family=binomial)
  
  p = predict(logis, type = "response")
  p.num = as.numeric(names(p))
  PS = rep(NA, nrow(dat))
  PS[p.num] = p
  dat.wt = cbind(dat, PS)
  dat.wt <- mutate(dat.wt, wt = trt/PS + (1-trt)/(1-PS)) %>%
    filter(Yc != '', wt <= 10)  %>%
    select(Y, trt, s1, s2, wt)
  
  dat.wt
}


rslt <- function(datps, resp, var_clas, var_num, modelno){
  dat <- datps %>%
    mutate(Yc = eval(as.name(resp)), Y = as.numeric(Yc == "Y"), trt = as.numeric(ARM == "CILO/FIR"), s1 = STRAT2V, s2 = STRAT1V)
    # filter(is.na(eval(as.name(paste0("WT_", resp, "_", modelno)))) == FALSE)
    
  
  est = CMH(datwt(dat, var_clas, var_num))
  
  ## bootstrap
  set.seed(1234)
  bt = NULL
  for(k in 1:nrep){
    x = sample(c(1:nrow(dat)), size=nrow(dat), replace=TRUE)
    # x = sample(c(1:nrow(dat))) 
    dat.bt = dat[x,]
    row.names(dat.bt) = NULL
    bt = rbind(bt, CMH(datwt(dat.bt, var_clas, var_num)))
  }
  
  SE.p_A.bt = sqrt(var(bt$p_A, na.rm = TRUE))
  LLC.p_A.bt = est$p_A - qnorm(1-alpha/2)*SE.p_A.bt
  ULC.p_A.bt = est$p_A + qnorm(1-alpha/2)*SE.p_A.bt
  
  SE.p_B.bt = sqrt(var(bt$p_B, na.rm = TRUE))
  LLC.p_B.bt = est$p_B - qnorm(1-alpha/2)*SE.p_B.bt
  ULC.p_B.bt = est$p_B + qnorm(1-alpha/2)*SE.p_B.bt
  
  SE.diff.bt = sqrt(var(bt$diff, na.rm = TRUE))
  LLC.diff.bt = est$diff - qnorm(1-alpha/2)*SE.diff.bt
  ULC.diff.bt = est$diff + qnorm(1-alpha/2)*SE.diff.bt
  pval.diff.bt = 2-2*pnorm(abs(est$diff/SE.diff.bt))
  diff.bt = mean(bt$diff, na.rm = TRUE)
  
  data.frame(resp, model = modelno, var_num, est, diff.bt, SE.p_A.bt, LLC.p_A.bt, ULC.p_A.bt, SE.p_B.bt, LLC.p_B.bt, ULC.p_B.bt, SE.diff.bt, LLC.diff.bt, ULC.diff.bt, pval.diff.bt)
}

alpha = 0.05
nrep = 5000
# resp = "PE48OC"
var_clas = "as.factor(STRAT1V) + as.factor(STRAT2V)"
# var_num = "AGE + ELFSCORE_BL + FIBSCAN_BL + AST_BL + GGT_BL + PLAT_BL + CY18M30_BL + NASLI_BL + NASHB_BL + COLLAGEN_BL + ALPHASMA_BL + BILI_BL"

outdat = NULL
outdat = rbind(outdat, rslt(datps = ps_wt, resp = "PE48OC", var_clas, 
                            var_num = "AGE + ELFSCORE_BL + FIBSCAN_BL + AST_BL + GGT_BL + PLAT_BL + CY18M30_BL + NASLI_BL + NASHB_BL + COLLAGEN_BL + ALPHASMA_BL + BILI_BL",
                            modelno = 1))

outdat = rbind(outdat, rslt(datps = ps_wt, resp = "PE48OC", var_clas,
                            var_num = "AGE + ELFSCORE_BL + AST_BL + GGT_BL + PLAT_BL + CY18M30_BL + NASLI_BL + NASHB_BL + COLLAGEN_BL + ALPHASMA_BL + BILI_BL",
                            modelno = 2))

View(outdat)

write.csv(outdat, "IPW bootstrap results (trim) 01-11-2020.csv", row.names = FALSE)


# resp = "PE48OC"
# dat <- ps_wt %>%
#   mutate(resp = eval(as.name(resp)), Y = (resp == "Y"), trt = as.numeric(ARM == "CILO/FIR"), s1 = STRAT2V, s2 = STRAT1V, wt = 1) %>%
#   filter(STUDYID == "GS-US-454-4378", resp != '') %>%
#   select(Y, trt, s1, s2, wt)
# 
# est = CMH(dat)
# 
# dattmp <- ps_wt %>%
#   mutate(resp = eval(as.name(resp)), Y = (resp == "Y"), trt = as.numeric(ARM == "CILO/FIR"), s1 = STRAT2V, s2 = STRAT1V) 
# 
# tmp = datwt(dattmp, var_clas, var_num)
# View(tmp)
# 
