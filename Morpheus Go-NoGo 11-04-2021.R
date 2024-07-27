rm(list=ls())

path <- "C:/Xiaomin Lu/CSE/Design Studio"
setwd(path)

library(BONUS)

source("Misc Functions.R")


lOut <- CalcProb( vSamp = c(15, 15), dP0 = 0.13, vP1 = c(), dTV = 0.2, dLRV = 0.05, dMid = 0.1, dAlpha = 0.3, dBeta = 0.3, dBetaMid = 0.3,
          vBeta0 = c(1, 1), vBeta1 = c(1, 1) )

dfCutoff <- do.call(cbind, lOut$lCutoff)
dfProb   <- lOut$dfProb

write.csv( dfCutoff, "Morpheus trial Cutoffs.csv", row.names = FALSE )
write.csv( dfProb, "Morpheus trial probs.csv", row.names = FALSE )

dY0 <- 2
dY1 <- 3
vBeta0 <- c(1, 1)
vBeta1 <- c(1, 1)
n1     <- 15
n0     <- 15

cDist2 <- cDist1 <- structure( list(), class = "Beta" )
cDist2$dA  <- vBeta0[1] + dY0
cDist2$dB  <- vBeta0[2] + n0 - dY0

cDist1$dA  <- vBeta1[1] + dY1
cDist1$dB  <- vBeta1[2] + n1 - dY1

BONUS::ProbX1GrtX2PlusDelta( cDist1 = cDist1, cDist2 = cDist2, dDelta = 0.2 )
BONUS::ProbX1GrtX2PlusDelta( cDist1 = cDist1, cDist2 = cDist2, dDelta = 0.1 )
1 - BONUS::ProbX1GrtX2PlusDelta( cDist1 = cDist1, cDist2 = cDist2, dDelta = 0.05 )

# vSamp = c(15, 15); dP0 = 0.13; vP1 = c(0.2, 0.3); dTV = 0.2; dLRV = 0.05; dAlpha = 0.3; dBeta = 0.3; vBeta0 = c(1, 1); vBeta1 = c(1, 1)



CalcProb( vSamp = c(15), dP0 = 0.13, vP1 = c(0.1), dTV = 0.6, dLRV = 0.3, dMid = 0.5, dAlpha = 0.3, dBeta = 0.3, dBetaMid = 0.3,
          vBeta0 = c(1, 1), vBeta1 = c(1, 1) )
