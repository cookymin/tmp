#################################################################################################### .
#   Function Name: VectorToString
#   Author: Xiaomin Lu
#   Description:  retrun a vector of numbers to string
#   Change History:
#       2020/06/10 -
#################################################################################################### .
VectorToString <- function( vx = NULL, sep = ", " )
{
    
    nDim = length(vx)
    
    if (nDim == 0) { retStr <- "" }
    if (nDim == 1) { retStr <- paste0( round(vx, 3) ) }
    
    if (nDim > 1)
    {
        retStr <- "("
        for(i in 1:(nDim-1))
        {
            retStr <- paste0(retStr, round(vx[i], 3), sep)
        }
        retStr <- paste0(retStr, round(vx[nDim], 3), ")")
    }
    
    
    return( retStr )
}




#################################################################################################### .
#   Function Name: SolveY1Baye
#   Author: Xiaomin Lu
#   Description:  Compute smallest p1 such that posterior prob(theta > dVal) = 1 - dPct
#   Change History:
#       2020/07/14 - Created
#################################################################################################### .

SolveY1Baye <- function( dY0, n0, n1, dVal, dPct, vBeta0, vBeta1 )
{
    cDist2 <- cDist1 <- structure( list(), class = "Beta" )
    
    if(round(n0) == 0)
    {
        f <- function( p1 )
        {
            dY1    <- p1 * n1
            vBeta  <- vBeta1 + c(dY1, n1 - dY1)
            
            ret    <- pbeta( q = dVal, shape1 = vBeta[1], shape2 = vBeta[2], lower.tail = FALSE ) - (1 - dPct)
            return( ret )
        }
    }
    else
    {
        cDist2$dA  <- vBeta0[1] + dY0
        cDist2$dB  <- vBeta0[2] + n0 - dY0
        
        f <- function( p1 )
        {
            dY1    <- p1 * n1
            cDist1$dA  <- vBeta1[1] + dY1
            cDist1$dB  <- vBeta1[2] + n1 - dY1
            
            #### TODO: add BONUS:: prior to the following function after BONUS package is updated ####
            ret   <- BONUS::ProbX1GrtX2PlusDelta( cDist1 = cDist1, cDist2 = cDist2, dDelta = dVal ) - (1 - dPct)
            return( ret )
        }
    }
    
    
    if( f( p1 = 1) < 0 )
    {
        ret <- n1 + 2
    }
    else if( f( p1 = 1) == 0 )
    {
        ret <- n1 + 1
    }
    else if ( f( p1 = 0) == 0 )
    {
        ret <- 1
    }
    else if ( f( p1 = 0) > 0 )
    {
        ret <- -1
    }
    else
    {
        p   <- uniroot( f, c(0,1), tol = 1e-7 )$root
        ret <- ceiling( round( n1 * p, 10) )
    }
    
    return( ret )
}





#################################################################################################### .
#   Function Name: GetCutoff
#   Author: Xiaomin Lu
#   Description:  Compute cutoff values for given TV, LRV and expansion value
#   Change History:
#       2021/11/05 - Created
#################################################################################################### .

GetCutoff <- function( vSamp, dTV, dLRV, dMid, dAlpha, dBeta, dBetaMid, vBeta0, vBeta1 )
{
    if(length(vSamp) == 1)
    {
        n1  <- vSamp[1]
        n0  <- 0
        vY0 <- cbind( NA )
    }
    else
    {
        n0  <- vSamp[1]
        n1  <- vSamp[2]
        vY0 <- cbind(0:n0)
    }
    
    vGoCutoff    <- apply( vY0, 1, function(x) SolveY1Baye( dY0 = x, n0 = n0, n1 = n1, dVal = dTV,  dPct = dBeta, vBeta0 = vBeta0, vBeta1 = vBeta1 ) )
    vExpanCutoff <- apply( vY0, 1, function(x) SolveY1Baye( dY0 = x, n0 = n0, n1 = n1, dVal = dMid,  dPct = dBetaMid, vBeta0 = vBeta0, vBeta1 = vBeta1 ) )
    vNoGoCutoff  <- apply( vY0, 1, function(x) SolveY1Baye( dY0 = x, n0 = n0, n1 = n1, dVal = dLRV,  dPct = 1 - dAlpha, vBeta0 = vBeta0, vBeta1 = vBeta1 ) )
    vNoGoCutoff  <- pmin( vNoGoCutoff - 1, 15)
    vNoGoCutoff[vNoGoCutoff < 0] <- NA
    
    lRet    <- list( vY0 = as.vector(vY0), vNoGoCutoffY1 = vNoGoCutoff, vGoCutoffY1= vGoCutoff, vExpanCutoffY1= vExpanCutoff )
    return( lRet )
}





#################################################################################################### .
#   Function Name: CalcProb
#   Author: Xiaomin Lu
#   Description:  Compute prob of go, no-go, expansion and consider zone
#   Change History:
#       2021/11/05 - Created
#################################################################################################### .

CalcProb <- function( vSamp, dP0 = NULL, vP1, dTV, dLRV, dMid, dAlpha, dBeta, dBetaMid, vBeta0, vBeta1 )
{
    lCutoff <- GetCutoff( vSamp = vSamp, dTV = dTV, dLRV = dLRV, dMid = dMid, dAlpha = dAlpha, dBeta = dBeta, dBetaMid = dBetaMid, vBeta0 = vBeta0, vBeta1 = vBeta1 )
    
    vY0            <- lCutoff$vY0
    vGoCutoffY1    <- lCutoff$vGoCutoffY1
    vExpanCutoffY1 <- lCutoff$vExpanCutoffY1
    vNoGoCutoffY1  <- lCutoff$vNoGoCutoffY1
    
    
    if( length(vSamp) == 1 )
    {
        n1           <- vSamp
        dP0          <- NULL
        vP1          <- cbind( unique( c(vP1, dTV, dLRV, dMid ) ) )
        vTrtEff      <- vP1
        dProbGo      <-  apply( cbind(vP1), 1, function(x) ifelse( is.na(vGoCutoffY1), 0, 1 - pbinom(vGoCutoffY1 - 1, n1, x) ) )
        dProbGoExpan <-  apply( cbind(vP1), 1, function(x) ifelse( is.na(vExpanCutoffY1), 0, 1 - pbinom(vExpanCutoffY1 - 1, n1, x) ) )
        dProbNoGo    <-  apply( cbind(vP1), 1, function(x) ifelse( is.na(vNoGoCutoffY1), 0, pbinom( vNoGoCutoffY1, n1, x ) ) )
        
    }
    else
    {
        n0  <- vSamp[1]
        n1  <- vSamp[2]
        
        vP1     <- cbind( unique( c(vP1, dP0 + dTV, dP0 + dLRV, dP0 + dMid ) ) )
        vTrtEff <- vP1 - dP0
        
        dProbGo      <- apply( vP1, 1, function(x) sum( ifelse( is.na(vGoCutoffY1), 0, dbinom(vY0,   n0, dP0) * (1- pbinom(vGoCutoffY1 - 1, n1, x)) ) ) )
        dProbGoExpan <- apply( vP1, 1, function(x) sum( ifelse( is.na(vExpanCutoffY1), 0, dbinom(vY0,   n0, dP0) * (1- pbinom(vExpanCutoffY1 - 1, n1, x)) ) ) )
        dProbNoGo    <- apply( vP1, 1, function(x) sum( ifelse( is.na(vNoGoCutoffY1), 0, dbinom(vY0, n0, dP0) * pbinom(vNoGoCutoffY1, n1, x) ) ) )
        
    }
    
    
    dfProb <- data.frame( vSamp = VectorToString(vSamp), dTV = dTV, dLRV = dLRV, dMid = dMid, dAlpha = dAlpha, dBeta = dBeta, dBetaMid = dBetaMid,
                          vBeta0 = VectorToString(vBeta0), vBeta1 = VectorToString(vBeta1), dP0 = VectorToString(dP0), vP1 = vP1, vTrtEff = vTrtEff,
                          ProbGo = dProbGo, ProbGoExpan = dProbGoExpan, ProbExpan = dProbGoExpan - dProbGo, ProbNoGo = dProbNoGo, 
                          ProbConsider = 1 - dProbGoExpan - dProbNoGo ) 
    
    dfProbCutoff <- NULL
    if( length(vSamp) == 1 )
    {
        dfProbCutoff <- data.frame( dfProb, vGoCutoffY1 = vGoCutoffY1, vExpanCutoffY1 = vExpanCutoffY1, vNoGoCutoffY1 = vNoGoCutoffY1 )
    }
    
    lRet  <- list( lCutoff = lCutoff, dfProb = dfProb, dfProbCutoff = dfProbCutoff )
    
    return( lRet )
}


