###########################################################
#                Time-to-event: Exponential               #
###########################################################
## dist. of time-to-event per option 1 of the spec
Exp_curv <- function( lmd )
{
    Surv <- function(t)
    {
        exp(-lmd*t)
    }
    
    pdf <- function(t)
    {
        lmd*exp(-lmd*t)
    }
    
    return( list(lmd = lmd, Surv = Surv, pdf = pdf) )
}





#######################################################################
##    Total number of events that will be observed by time t         ##
##      TD(t0,t) = D(t0) + ED(t0,t) + AD(t0,t)                       ##
##                                                                   ##
##      D(t0) -    number of events observed by time t0             ##
##      ED(t0,t) -  number of events observed by time t among        ##
##                  those at risk at time t0                         ##
##      AD(t0,t) -  number of subj enrolled and fail in (t0,t)       ##
#######################################################################


## -----------  Calculate ED(t0,t) ----------- ##

ED_0_t <- function( t, Trt, trt, dEnrollTimeRisk, fnC, fnE, dMaxFuTime )
{
    e     <- dEnrollTimeRisk[ Trt==trt ]
    nRisk <- length(e)
    
    intgrd <- function( u )
    {
        fnC$Surv(u) * fnE$pdf(u)
    }
    
    ED <- 0
    for(i in 1:nRisk)
    {
        tmp <- try( integrate(intgrd, lower = 0, upper = min(t-e[i],dMaxFuTime))$value, silent = TRUE )
        if("try-error" %in% class(tmp)) 
        {
            tmp <- integrate( Vectorize(intgrd), lower = 0, upper = min(t-e[i],dMaxFuTime) )$value
        }
        
        ED <- ED + tmp
    }
    
    return( ED )
}


ED_t0_t <- function( t0, t, Trt, trt, dEnrollTimeRisk, fnC, fnE, dMaxFuTime )
{
    e     <- dEnrollTimeRisk[Trt==trt]
    nRisk <- length(e)
    
    intgrd <- function(u) 
    {
        fnC$Surv(u) * fnE$pdf(u)
    }
    
    ED <- 0
    for( i in 1:nRisk )
    {
        tmp <- try( integrate( intgrd, lower = t0-e[i], upper = min(t-e[i],dMaxFuTime) )$value, silent = TRUE )
        
        if( "try-error" %in% class(tmp) )
        {
            tmp <- integrate( Vectorize(intgrd), lower = t0-e[i], upper = min(t-e[i],dMaxFuTime) )$value
        }
        
        ED <- ED + tmp / fnE$Surv(t0-e[i]) / fnC$Surv(t0-e[i])
    }
    
    return( ED )
}


## -----------  Calculate  AD(t0,t) ----------- ##

AD_t0_t <- function( t0, t, fnC, fnE, nTotal, dAccrRate )
{
    intgrd1 <- function(t) 
    {
        fnC$Surv(t) * fnE$pdf(t)
    }
    
    intgrd2 <- function(u)
    {
        tmp <- try( integrate(intgrd1, lower = 0, upper = t-t0-u, abs.tol = 1e-20)$value, silent = TRUE )
        if("try-error" %in% class(tmp))
        {
            tmp <- integrate(Vectorize(intgrd1), lower = 0, upper = t-t0-u, abs.tol = 1e-20)$value
        }
        
        return( accr(u+t0, nTotal, dAccrRate)*tmp )
    } 
    
    AD <- try( integrate(intgrd2,  lower = 0, upper = t-t0, abs.tol = 1e-20)$value, silent = TRUE )
    if("try-error" %in% class(AD))
    {
        AD <- integrate( Vectorize(intgrd2), lower = 0, upper = t-t0, abs.tol = 1e-20 )$value
    }
    
    return( AD )
}


###########################################################
#                   Accrual Function                      #
###########################################################

accr <- function( u, nTotal, dAccrRate )
{
    dAccrDur <- nTotal / dAccrRate
    rate     <- (0 < u & u<= dAccrDur) * dAccrRate
    
    return(rate)
} 




#################################################################
##  Expected number of events for Ph2/3                        ##
##  dMonthGapPh23: time of stopping accrual between Ph2 and Ph3 ##
#################################################################


## Expected events to be observed at a single study
ClacEventN <- function( dMedSurvTime0, dMedSurvTime1, nTotal, vRatio, dAccrRate, dFUMonthPh3, fnC )
{
    dLmd0 <- log(2) / dMedSurvTime0
    dLmd1 <- log(2) / dMedSurvTime1
    
    fnE0 <- Exp_curv( dLmd0 )
    fnE1 <- Exp_curv( dLmd1 )
    
    r <- vRatio[2] / sum( vRatio )

    dEvt <- AD_t0_t( t0 = 0, t = nTotal / dAccrRate + dFUMonthPh3, fnC = fnC, fnE = fnE0, nTotal = nTotal - round(nTotal*r), dAccrRate = dAccrRate * (1-r) ) +
        AD_t0_t( t0 = 0, t = nTotal / dAccrRate + dFUMonthPh3, fnC = fnC, fnE = fnE1, nTotal = round(nTotal*r), dAccrRate = dAccrRate * r )
    
    return( round(dEvt) )
}


## Expected events to be observed for Ph2/3
ClacEventNPh23 <- function( dMedSurvTime0, dMedSurvTime1, nPh2, nPh3, vRatio2, vRatio3, dAccrRate2, dAccrRate3, dMonthGapPh23, dFUMonthPh3, fnC )
{
    dLmd0 <- log(2) / dMedSurvTime0
    dLmd1 <- log(2) / dMedSurvTime1
    
    fnE0 <- Exp_curv( dLmd0 )
    fnE1 <- Exp_curv( dLmd1 )
    
    r2 <- vRatio2[2] / sum( vRatio2 )
    r3 <- vRatio3[2] / sum( vRatio3 )
    
    dDurPh2 <- nPh2 / dAccrRate2 + dMonthGapPh23
    dDurPh3 <- nPh3 / dAccrRate3 + dFUMonthPh3
    
    dEvt <- AD_t0_t( t0 = 0, t = dDurPh2 + dDurPh3, fnC = fnC, fnE = fnE0, nTotal = nPh2 - round(nPh2*r2), dAccrRate = dAccrRate2 * (1-r2) ) +
        AD_t0_t( t0 = 0, t = dDurPh2 + dDurPh3, fnC = fnC, fnE = fnE1, nTotal = round(nPh2*r2), dAccrRate = dAccrRate2 * r2 ) +
        AD_t0_t( t0 = 0, t = dDurPh3, fnC = fnC, fnE = fnE0, nTotal = nPh3 - round(nPh3*r3), dAccrRate = dAccrRate3 * (1-r3) ) +
        AD_t0_t( t0 = 0, t = dDurPh3, fnC = fnC, fnE = fnE1, nTotal = round(nPh3*r3), dAccrRate = dAccrRate3 * r3 )
    
    return( round(dEvt) )
}




logrank1 <- function( time, status, trt )
{
    fit <- survdiff( Surv(time,status) ~ trt )
    p   <- (1 - pchisq(fit$chisq,1) ) / 2
    if ( fit$obs[2] > fit$exp[2] )
    {
        p <- 1-p
    }
    
    return( p )
}
