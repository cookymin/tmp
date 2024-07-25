## For Study Affirm

## Seladelpar vs placebo 


N1=128          ##  total #subjects in Sel arm
N2=64           ##  total #subjects in placebo arm

n11=seq(1, N1)  ##  #subjects in Sel in region 1 China
n12=N1-n11      ##  #subjects in Sel in ROW

n21=0.5*n11     ##  #subjects in placebo arm in region 1 China
n22=N2-n21      ##  #subjects in placebo arm in ROW


e_p=0.5         ## event rate for placebo 32/64
e_s=0.2578      ## event rate for Sel 33/128 ## HR 0.5
hr=0.5

                ## same event rate for both regions 

theta=log(0.5)
e1s=n11*e_s
e1p=n21*e_p
e2s=n12*e_s
e2p=n22*e_p

lambda1 = 1/e1s + 1/e1p
lambda2 = 1/e2s + 1/e2p

p1=pnorm((-theta)/sqrt(lambda1))
p2=pnorm((-theta)/sqrt(lambda2))
p=p1*p2

plot(p)
cat("sel vs placebo: find the minimal #subjects(n11) with prob(p) >= 0.8")
cbind(n11,p)
