rm( list = ls())

# library(mvtnorm)
# library(mnormt)
# library(Hmisc)
library( BONUS )
library( PPPoS )


setwd("C:/Xiaomin Lu/CSE/Ph2_3 Seamless Design")
source( "event projection - functions.r" )

dLmdC <- -log(1 - 0.05) / 12
fnC  <- Exp_curv( dLmdC )

NEvtPh3 <- ClacEventN( dMedSurvTime0 = 12, dMedSurvTime1 = 18, nTotal = 277, vRatio = c(1,1), dAccrRate = 277/36, dFUMonthPh3 = 12, fnC = fnC )

dMonthGapPh23 <- 8
nPh2          <- 75*2
nPh3          <- 64*2
dAccrRate2    <- 278/36
dAccrRate3    <- 278/36
dFUMonthPh3   <- 12

dDurPh2 <- nPh2 / dAccrRate2 + dMonthGapPh23
dDurPh3 <- nPh3 / dAccrRate3 + dFUMonthPh3

NEvtPh2 <- ClacEventN( dMedSurvTime0 = 12, dMedSurvTime1 = 18, nTotal = nPh2, vRatio = c(1,1), dAccrRate = 277/36, dFUMonthPh3 = dMonthGapPh23 + dDurPh3, fnC = fnC )
NEvtPh3 <- ClacEventN( dMedSurvTime0 = 12, dMedSurvTime1 = 18, nTotal = nPh3, vRatio = c(1,1), dAccrRate = 277/36, dFUMonthPh3 = dFUMonthPh3, fnC = fnC )
nEvt    <- ClacEventNPh23( dMedSurvTime0 = 12, dMedSurvTime1 = 18, nPh2, nPh3, vRatio2 = c(1,1), vRatio3 = c(1,1), dAccrRate2, dAccrRate3, dMonthGapPh23, 
                           dFUMonthPh3, fnC )

NEvtPh2 <- ClacEventN( dMedSurvTime0 = 12, dMedSurvTime1 = 18, nTotal = nPh2, vRatio = c(1,1), dAccrRate = 278/36, dFUMonthPh3 = 6, fnC = fnC )


SimBinSurv <- function( n, dRho, dRespRate0, dLmd )
{
    Z1     <- rnorm( n )
    Z      <- rnorm( n )
    Z2     <- dRho * Z1 + sqrt( 1 - dRho^2 ) * rnorm( n )
    vY     <- pnorm(Z1) > 1 - dRespRate0
    vT     <- -log( 1-pnorm(Z2) ) / dLmd
    
    return( cbind(vY, vT) )
}


OneSim <- function( nPh2, nPh3, vRatio2, vRatio3, dAccrRate2, dAccrRate3, dMonthGapPh23, dFUMonthPh3, dLmdC, dMedSurvTime0, 
                    dMedSurvTime1, dRespRate0, dRespRate1, dRho )
{
    
    dLmd0 <- log(2) / dMedSurvTime0
    dLmd1 <- log(2) / dMedSurvTime1
    
    r2 <- vRatio2[2] / sum( vRatio2 )
    r3 <- vRatio3[2] / sum( vRatio3 )
    
    n21 <- round( nPh2 * r2 )
    n20 <- nPh2 - n21
    n31 <- round( nPh3 * r3 )
    n30 <- nPh3 - n31
    
    dfOut <- NULL
    for( k in 1:nRep )
    {
        mYT0 <- SimBinSurv( n = n20 + n30, dRho = dRho, dRespRate0 = dRespRate0, dLmd = dLmd0 )
        mYT1 <- SimBinSurv( n = n21 + n31, dRho = dRho, dRespRate0 = dRespRate1, dLmd = dLmd1 )
        
        
        vTrt    <- rep( c(0,1), c(n20 + n30, n21 + n31) )
        vT      <- c( mYT0[, "vT"], mYT1[, "vT"] )
        vC      <- rexp( n = nPh2 + nPh3, rate = dLmdC )
        vU      <- c( runif( nPh2, 0, nPh2/dAccrRate2 ), runif( nPh3, 0, nPh3/dAccrRate3 ) )
        
        dDurPh2 <- max( vU[1:nPh2] ) + dMonthGapPh23
        dDurPh3 <- max( vU[(nPh2+1):nPh3] ) + dFUMonthPh3
        
        vDur      <- rep( c(dDurPh2 + dDurPh3, dDurPh3), c(nPh2, nPh3))
        vSurvTime <- pmin( vT, vC, vDur - vU )
        vStatus   <- vT <= pmin( vC, vDur - vU )
        
        dPvalORR <- BinWaldPoolTest(m0 = sum( mYT0[, "vY"]), m1 = sum( mYT1[, "vY"]), n0 = n20 + n30, n1 = n21 + n31 )$pval
        nEvt     <- sum( vStatus )
        dPvalLR  <- logrank1( vSurvTime, vStatus, vTrt )
        
        
        ##  Ph2 
        vTrtPh2      <- rep( c(0,1), c(n20, n21) )
        vTPh2        <- c( mYT0[1:n20, "vT"], mYT1[1:n21, "vT"] )
        vCPh2        <- vC[1:nPh2]
        vSurvTimePh2 <- pmin( vTPh2, vCPh2, max( vU[1:nPh2] ) + 6 - vU[1:nPh2] )
        vStatusPh2   <- vTPh2 <= pmin( vCPh2, max( vU[1:nPh2] ) + 6 - vU[1:nPh2] )
        
        
        dPvalORRPh2 <- BinWaldPoolTest(m0 = sum( mYT0[1:n20, "vY"]), m1 = sum( mYT1[1:n21, "vY"]), n0 = n20, n1 = n21 )$pval
        nEvtPh2     <- sum( vStatusPh2 )
        dPvalLRPh2  <- logrank1( vSurvTimePh2, vStatusPh2, vTrtPh2 )
        
        dfOut   <- rbind( dfOut, data.frame( dPvalORRPh2, nEvtPh2, dPvalLRPh2, dPvalORR, nEvt, dPvalLR ) )
    }
    
    return( dfOut )
}


SummarizeSim <- function( nPh2, nPh3, vRatio2, vRatio3, dAccrRate2, dAccrRate3, dMonthGapPh23, dFUMonthPh3, dLmdC, dMedSurvTime0, 
                          dMedSurvTime1, dRespRate0, dRespRate1, dAlpha, dRho )
{
    set.seed(1234)
    
    dfOut <- OneSim( nPh2, nPh3, vRatio2, vRatio3, dAccrRate2, dAccrRate3, dMonthGapPh23, dFUMonthPh3, dLmdC, dMedSurvTime0, 
                     dMedSurvTime1, dRespRate0, dRespRate1, dRho )
    
    # browser()
    PowerPh2 <- mean( dfOut$dPvalORRPh2 <= dAlpha )
    PowerPh3 <- mean ( dfOut$dPvalORRPh2 <= dAlpha & dfOut$dPvalLR <= dAlpha)
    
    CPPh3givenPh2 <- PowerPh3 / PowerPh2
    MeanEvtPh2 <- mean( dfOut$nEvtPh2 )
    MeanEvtPh3 <- mean( dfOut$nEvt )
    
    PowerORRPh3 <- mean( dfOut$dPvalORR <= dAlpha )
    PowerLRPh2  <- mean( dfOut$dPvalLRPh2 <= dAlpha )
    PowerLRPh3  <- mean( dfOut$dPvalLR <= dAlpha )
    
    return( data.frame( PowerPh2, PowerPh3, CPPh3givenPh2, MeanEvtPh2, MeanEvtPh3, PowerORRPh3, PowerLRPh2, PowerLRPh3 ) )
    
}


nRep = 10000
SummarizeSim( nPh2 = 75*2, nPh3 = 64*2, vRatio2 = c(1, 1), vRatio3 = c(1, 1), dAccrRate2 = 278/36, dAccrRate3 = 278/36, dMonthGapPh23 = 9, dFUMonthPh3 = 12, 
              dLmdC = dLmdC, dMedSurvTime0 = 12, dMedSurvTime1 = 18, dRespRate0 = 0.55, dRespRate1 = 0.7, dAlpha = 0.025, dRho = 0.7 )
    
SummarizeSim( nPh2 = 75*2, nPh3 = 64*2, vRatio2 = c(1, 1), vRatio3 = c(1, 1), dAccrRate2 = 278/36, dAccrRate3 = 278/36, dMonthGapPh23 = 9, dFUMonthPh3 = 12, 
              dLmdC = dLmdC, dMedSurvTime0 = 12, dMedSurvTime1 = 12, dRespRate0 = 0.55, dRespRate1 = 0.55, dAlpha = 0.025, dRho = 0.7 )

