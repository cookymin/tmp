#################################################################################################### .
#   Program/Function Name: PlotCutoff
#   Author: Xiaomin Lu
#   Description: Generate plot of GoNG cutoffs on number of responders
#   Change History:
#   Last Modified Date: 11/06/2021
#################################################################################################### .

PlotCutoff3 <- function( dfDat, strTitle, strXAxisLabel, strYAxisLabel, vXAxisTick = NULL, vVlineVal = NULL, bGrey = FALSE, gBaseSize = 12 )
{
    colorPalette <- c( "#009E73", "#F0E442", "#D55E00", "#808080",
                       "#000000", "#E69F00", "#56B4E9",
                       "#0072B2", "#CC79A7")
    
    vGoCutoffY1    <- dfDat$vGoCutoffY1
    vNoGoCutoffY1  <- dfDat$vNoGoCutoffY1

    ymin   <- min(vGoCutoffY1, vNoGoCutoffY1, na.rm = TRUE)
    ymax   <- max(vGoCutoffY1, vNoGoCutoffY1, na.rm = TRUE)
    
    dfDat$ConLow <- ifelse(is.na(vNoGoCutoffY1), ymin, vNoGoCutoffY1)
    dfDat$ConUp  <- ifelse(is.na(vGoCutoffY1),   ymax, vGoCutoffY1)

    
    if( is.null(vXAxisTick) )
    {
        vXAxisTick <- dfDat$vX
        nGrid <- length( vXAxisTick )
        while( nGrid > 10 )
        {
            vXAxisTick <- vXAxisTick[ unique( c( seq(1, nGrid, 2), nGrid ) ) ]
            nGrid      <- length( vXAxisTick )
        }
    }
    
    
    if( bGrey )
    {
        color1        <- colorPalette[c(1,4,3)]
        vGrp          <- c( "Go", "Consider", "No Go" )
        names(color1) <- vGrp
        
        grPlot <- ggplot(  dfDat, aes( x = vX )) +
            geom_ribbon( aes(ymin = ConUp,  ymax = ymax,   fill = "Go" ), alpha = 0.4) +
            geom_ribbon( aes(ymin = ConLow, ymax = ConUp,  fill = "Consider" ), alpha = 0.4) +
            geom_ribbon( aes(ymin = ymin,   ymax = ConLow, fill = "No Go" ), alpha = 0.4) +
            geom_line( aes(y = vGoCutoffY1 ),   color = color1["Go"], size = 1.5 ) +
            geom_line( aes(y = vNoGoCutoffY1 ), color = color1["No Go"], size = 1.5 ) +
            theme_minimal(base_size = gBaseSize)  +
            theme(legend.position = "top", plot.title = element_text( hjust = 0.5, size = 16 )) +
            scale_color_manual( "", breaks = vGrp, values = color1 ) +
            scale_fill_manual( "", breaks = vGrp, values = color1 ) +
            geom_vline( xintercept = vVlineVal, lty = "dotted" , col = colorPalette[6], lwd = 1.25) +
            scale_x_continuous(breaks = vXAxisTick ) +
            labs( x = strXAxisLabel , y = strYAxisLabel, hajust = 0.5) +
            strTitle +
            theme( plot.title = element_text( hjust = 0.5, size = 14 ),
                   plot.subtitle = element_text( hjust = 0.5, size = 10 ),
                   panel.grid.major.y = element_line( size = 1.5 ),
                   legend.position ="top", legend.direction="horizontal", legend.title = element_blank(),
                   legend.text = element_text(margin = margin(r = 15, unit = "pt")),
                   plot.margin=unit(c(2,0,0,0),"cm"))
    }
    else
    {
        color1        <- colorPalette[1:3]
        vGrp          <- c( "Go", "Expan", "No Go" )
        names(color1) <- vGrp
        
        grPlot <- ggplot(  dfDat, aes( x = vX )) +
            geom_ribbon( aes(ymin = ConUp,  ymax = ymax,   fill = "Go" ), alpha = 0.4) +
            geom_ribbon( aes(ymin = ConLow, ymax = ConUp,  fill = "Expan" ), alpha = 0.4) +
            geom_ribbon( aes(ymin = ymin,   ymax = ConLow, fill = "No Go" ), alpha = 0.4) +
            geom_line( aes(y = vGoCutoffY1 ),   color = color1["Go"], size = 1.5 ) +
            geom_line( aes(y = vNoGoCutoffY1 ), color = color1["No Go"], size = 1.5 ) +
            theme_minimal(base_size = gBaseSize)  +
            theme(legend.position = "top", plot.title = element_text( hjust = 0.5, size = 16 )) +
            scale_color_manual( "", breaks = vGrp, values = color1 ) +
            scale_fill_manual( "", breaks = vGrp, values = color1 ) +
            geom_vline( xintercept = vVlineVal, lty = "dotted" , col = colorPalette[6], lwd = 1.25) +
            scale_x_continuous(breaks = vXAxisTick ) +
            labs( x = strXAxisLabel , y = strYAxisLabel, hajust = 0.5) +
            strTitle +
            theme( plot.title = element_text( hjust = 0.5, size = 14 ),
                   plot.subtitle = element_text( hjust = 0.5, size = 10 ),
                   panel.grid.major.y = element_line( size = 1.5 ),
                   legend.position ="top", legend.direction="horizontal", legend.title = element_blank(),
                   legend.text = element_text(margin = margin(r = 15, unit = "pt")),
                   plot.margin=unit(c(2,0,0,0),"cm"))
    }
    
    
    return( grPlot )
}





#################################################################################################### .
#   Program/Function Name:
#   Author: Xiaomin Lu
#   Description: This function generate stacked plot of the decision probabilities
#   Change History:
#   Last Modified Date: 11/06/2021
#################################################################################################### .

PlotProbStacked3 <- function( dfDat, strTitle, strXAxisLabel, vXAxisTick = NULL, vVlineVal = NULL, vHlineVal = NULL, bGrey = FALSE, gBaseSize = 12 )
{
    
    colorPalette <- c( "#009E73", "#F0E442", "#D55E00", "#808080",
                       "#000000", "#E69F00", "#56B4E9",
                       "#0072B2", "#CC79A7")
    
    
    if( is.null(vXAxisTick) )
    {
        vXAxisTick <- dfDat$vX
        nGrid <- length( vXAxisTick )
        while( nGrid > 10 )
        {
            vXAxisTick <- vXAxisTick[ unique( c( seq(1, nGrid, 2), nGrid ) ) ]
            nGrid      <- length( vXAxisTick )
        }
    }
    
    
    if( bGrey )
    {
        color1        <- colorPalette[c(1,4,3)]
        vGrp          <- c( "Pr( Go )", "Pr( Consider )", "Pr( No Go )" )
        names(color1) <- vGrp
        
        grPlot <- ggplot(  dfDat, aes( x = vX )) +
            geom_ribbon(aes(ymin = 0, ymax = ProbNoGo, fill = "Pr( No Go )"), alpha = 0.4) +
            geom_ribbon(aes(ymin = ProbNoGo, ymax = 1 - ProbGo, fill = "Pr( Consider )"), alpha = 0.4) +
            geom_ribbon(aes(ymin = 1 - ProbGo, ymax = 1, fill = "Pr( Go )"), alpha = 0.4) +
            theme_minimal(base_size = gBaseSize)  +
            scale_fill_manual( "", breaks = vGrp, values = color1 ) +
            geom_vline( xintercept = vVlineVal, lty = "dotted" , col = colorPalette[6], lwd = 1.25) +
            geom_hline( yintercept = vHlineVal, lty = "dotted" , col = colorPalette[6], lwd = 1.25) +
            scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1) ) +
            scale_x_continuous(breaks = vXAxisTick ) +
            labs( x = strXAxisLabel , y = "Probability", hajust = 0.5) +
            strTitle +
            theme( plot.title = element_text( hjust = 0.5, size = 14 ),
                   plot.subtitle = element_text( hjust = 0.5, size = 10 ),
                   panel.grid.major.y = element_line( size = 1.5 ),
                   legend.position ="top", legend.text = element_text(margin = margin(r = 15, unit = "pt")),
                   legend.direction="horizontal", legend.title = element_blank(),
                   plot.margin = unit(c(2,0,0,0),"cm") ) +
            coord_cartesian(clip = "off")
    }
    else
    {
        color1        <- colorPalette[1:3]
        vGrp          <- c( "Pr( Go )", "Pr( Expan )", "Pr( No Go )" )
        names(color1) <- vGrp
        
        grPlot <- ggplot(  dfDat, aes( x = vX )) +
            geom_ribbon(aes(ymin = 0, ymax = ProbNoGo, fill = "Pr( No Go )"), alpha = 0.4) +
            geom_ribbon(aes(ymin = ProbNoGo, ymax = 1 - ProbGo, fill = "Pr( Expan )"), alpha = 0.4) +
            geom_ribbon(aes(ymin = 1 - ProbGo, ymax = 1, fill = "Pr( Go )"), alpha = 0.4) +
            theme_minimal(base_size = gBaseSize)  +
            scale_fill_manual( "", breaks = vGrp, values = color1 ) +
            geom_vline( xintercept = vVlineVal, lty = "dotted" , col = colorPalette[6], lwd = 1.25) +
            geom_hline( yintercept = vHlineVal, lty = "dotted" , col = colorPalette[6], lwd = 1.25) +
            scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1) ) +
            scale_x_continuous(breaks = vXAxisTick ) +
            labs( x = strXAxisLabel , y = "Probability", hajust = 0.5) +
            strTitle +
            theme( plot.title = element_text( hjust = 0.5, size = 14 ),
                   plot.subtitle = element_text( hjust = 0.5, size = 10 ),
                   panel.grid.major.y = element_line( size = 1.5 ),
                   legend.position ="top", legend.text = element_text(margin = margin(r = 15, unit = "pt")),
                   legend.direction="horizontal", legend.title = element_blank(),
                   plot.margin = unit(c(2,0,0,0),"cm") ) +
            coord_cartesian(clip = "off")
    }
    
    
    
    return( grPlot )
    
}


#################################################################################################### .
#   Program/Function Name:
#   Author: Xiaomin Lu
#   Description: This function generate stacked plot of the decision probabilities
#   Change History:
#   Last Modified Date: 08/14/2020
#################################################################################################### .

PlotProbStackedTVLRV <- function( dfDat, strTitle, strXAxisLabel, bGrey = FALSE, gBaseSize = 12 )
{
    
    colorPalette <- c( "#009E73", "#F0E442", "#D55E00", "#808080",
                       "#000000", "#E69F00", "#56B4E9",
                       "#0072B2", "#CC79A7")
    
    
    vXAxisTick <- dfDat$vX
    nGrid <- length( vXAxisTick )
    while( nGrid > 10 )
    {
        vXAxisTick <- vXAxisTick[ unique( c( seq(1, nGrid, 2), nGrid ) ) ]
        nGrid      <- length( vXAxisTick )
    }
    
    
    text_TV    <- grid::textGrob("TV",  gp = grid::gpar( fontsize = 8, col = colorPalette[5]))
    text_LRV   <- grid::textGrob("LRV", gp = grid::gpar( fontsize = 8, col = colorPalette[5]))

    if( bGrey )
    {
        color1        <- colorPalette[c(1,4,3)]
        vGrp          <- c( "Pr( Go )", "Pr( Consider )", "Pr( No Go )" )
        names(color1) <- vGrp
        
        grPlot <- ggplot(  dfDat, aes( x = vX )) +
        geom_ribbon(aes(ymin = 0, ymax = ProbNoGo, fill = "Pr( No Go )"), alpha = 0.4) +
        geom_ribbon(aes(ymin = ProbNoGo, ymax = 1 - ProbGo, fill = "Pr( Consider )"), alpha = 0.4) +
        geom_ribbon(aes(ymin = 1 - ProbGo, ymax = 1, fill = "Pr( Go )"), alpha = 0.4) +
        theme_minimal(base_size = gBaseSize)  +
        scale_fill_manual( "", breaks = vGrp, values = color1 ) +
        scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1) ) +
        labs( x = strXAxisLabel , y = "Probability", hajust = 0.5) +
        strTitle +
        geom_vline( xintercept = c( dfDat$dTV, dfDat$dLRV ), lty = "dotted" , col = colorPalette[6], lwd = 1.25) +
        annotation_custom(text_TV, xmin  = dfDat$dTV[1], xmax = dfDat$dTV[1], ymin = 1.07, ymax = 1.07) +
        annotation_custom(text_LRV, xmin  = dfDat$dLRV[1], xmax = dfDat$dLRV[1], ymin = 1.07, ymax = 1.07) +
        theme( plot.title = element_text( hjust = 0.5, size = 14 ),
               plot.subtitle = element_text( hjust = 0.5, size = 10 ),
               panel.grid.major.y = element_line( size = 1.5 ),
               legend.position ="top", legend.text = element_text(margin = margin(r = 15, unit = "pt")),
               legend.direction="horizontal", legend.title = element_blank(),
               plot.margin = unit(c(2,0,0,0),"cm") ) +
        coord_cartesian(clip = "off")
    }
    else
    {
        color1        <- colorPalette[1:3]
        vGrp          <- c( "Pr( Go )", "Pr( Expan )", "Pr( No Go )" )
        names(color1) <- vGrp
        
        grPlot <- ggplot(  dfDat, aes( x = vX )) +
            geom_ribbon(aes(ymin = 0, ymax = ProbNoGo, fill = "Pr( No Go )"), alpha = 0.4) +
            geom_ribbon(aes(ymin = ProbNoGo, ymax = 1 - ProbGo, fill = "Pr( Expan )"), alpha = 0.4) +
            geom_ribbon(aes(ymin = 1 - ProbGo, ymax = 1, fill = "Pr( Go )"), alpha = 0.4) +
            theme_minimal(base_size = gBaseSize)  +
            scale_fill_manual( "", breaks = vGrp, values = color1 ) +
            scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1) ) +
            labs( x = strXAxisLabel , y = "Probability", hajust = 0.5) +
            strTitle +
            geom_vline( xintercept = c( dfDat$dTV, dfDat$dLRV ), lty = "dotted" , col = colorPalette[6], lwd = 1.25) +
            annotation_custom(text_TV, xmin  = dfDat$dTV[1], xmax = dfDat$dTV[1], ymin = 1.07, ymax = 1.07) +
            annotation_custom(text_LRV, xmin  = dfDat$dLRV[1], xmax = dfDat$dLRV[1], ymin = 1.07, ymax = 1.07) +
            theme( plot.title = element_text( hjust = 0.5, size = 14 ),
                   plot.subtitle = element_text( hjust = 0.5, size = 10 ),
                   panel.grid.major.y = element_line( size = 1.5 ),
                   legend.position ="top", legend.text = element_text(margin = margin(r = 15, unit = "pt")),
                   legend.direction="horizontal", legend.title = element_blank(),
                   plot.margin = unit(c(2,0,0,0),"cm") ) +
            coord_cartesian(clip = "off")
    }
    return( grPlot )
    
}

################################################################################################### .
#   Program/Function Name: GetPlots
#   Author: Xiaomin Lu
#   Description: Generate plot of cutoffs and probs
#   Change History:
#   Last Modified Date: 11/06/2021
#################################################################################################### .

GetPlotsSamp3 <- function( dfDat, vRatio, dTrueRateSVal = NULL, vTrtEffVal, dTVVal, dLRVVal, vXAxisTick = NULL, vVlineVal = NULL, vHlineTVVal = NULL, 
                           vHlineLRVVal = NULL, dAlphaVal, dBetaVal, strDecisionMethodVal, strTitle1, bGrey = FALSE )
{
    subdat <- filter( dfDat, round(dTV - dTVVal, 8) == 0, round(dLRV - dLRVVal, 8) == 0, round(dAlpha - dAlphaVal, 8) == 0, 
                      round(dBeta - dBetaVal, 8) == 0 )
    
    
    strXAxisLabel <- "Sample Size"
    strYAxisLabel <- "Observed Number of Responders"
    subdat$vX     <- as.numeric( subdat$nTotalSampleSize )
    
    
    
    lPlots   <- list()
    
    if( length(vRatio) == 1 )
    {
        strTitle2   <- paste0( " TV = ", round(dTVVal, 3), ", LRV = ", round(dLRVVal, 3), ", Alpha = ", dAlphaVal, ", Beta = ", dBetaVal )
        
        title1   <- paste0( strTitle1, paste0(": Cutoff (Clopper-Pearson ", strDecisionMethodVal, ")" ) )
        title2   <- strTitle2
        strTitle <- ggtitle(bquote(atop(.(title1), atop(italic(.(title2))))))
        
        subdat1  <- filter( subdat, round(vTrueRateE - dTVVal, 8) == 0 )
        lPlots$pCutoff <- PlotCutoff3( dfDat = subdat1, strTitle = strTitle, strXAxisLabel = strXAxisLabel, strYAxisLabel = strYAxisLabel, 
                                       vXAxisTick = vXAxisTick, vVlineVal = vVlineVal, bGrey = bGrey )
        
        title1     <- paste0( strTitle1, paste0(": Probability (Clopper-Pearson ", strDecisionMethodVal, ")" ) )
        strTitle   <- ggtitle(bquote(atop(.(title1), atop(italic(.(title2))))))
        vTrtEffVal <- unique( c( vTrtEffVal, dTVVal, dLRVVal ) )
        for( i in c(1:length(vTrtEffVal)) )
        {
            Xlabel   <- paste0( strXAxisLabel, " (True Resp Rate = ", round(vTrtEffVal[i], 3), ")" )
            subdat1  <- filter( subdat, round(vTrueRateE - vTrtEffVal[i], 8) == 0 )
            if( round(vTrtEffVal[i] - dTVVal, 8) == 0 ) 
            {
                lPlots[[paste0("pProb", i)]]  <- PlotProbStacked3( dfDat = subdat1, strTitle = strTitle, strXAxisLabel = Xlabel, vXAxisTick = vXAxisTick, 
                                                                   vVlineVal = vVlineVal, vHlineVal = vHlineTVVal, bGrey = bGrey )
            }
            else if( round(vTrtEffVal[i] - dLRVVal, 8) == 0 ) 
            {
                lPlots[[paste0("pProb", i)]]  <- PlotProbStacked3( dfDat = subdat1, strTitle = strTitle, strXAxisLabel = Xlabel, vXAxisTick = vXAxisTick,
                                                                   vVlineVal = vVlineVal, vHlineVal = vHlineLRVVal, bGrey = bGrey )
            }
            else
            {
                lPlots[[paste0("pProb", i)]]  <- PlotProbStacked3( dfDat = subdat1, strTitle = strTitle, strXAxisLabel = Xlabel, vXAxisTick = vXAxisTick, 
                                                                   bGrey = bGrey)
            }
            
        }
    }
    
    if( length(vRatio) == 2 )
    {
        subdat <- filter( subdat, dRatio == vRatio[2] )
        
        strTitle2   <- paste0( "Ctr:Trt = ", vRatio[1], ":", vRatio[2], ", TV = ", round(dTVVal, 3), ", LRV = ", round(dLRVVal, 3), 
                               ", Alpha = ", dAlphaVal, ", Beta = ", dBetaVal )
        
        title1     <- paste0( strTitle1, paste0(": Probability (Approximate ", strDecisionMethodVal, ")" ) )
        title2     <- paste0( strTitle2, ", True Ctr Rate = ", dTrueRateSVal )
        strTitle   <- ggtitle(bquote(atop(.(title1), atop(italic(.(title2))))))
        vTrtEffVal <- unique( round( c( vTrtEffVal, dTVVal, dLRVVal ), 3 ) )
        for( i in c(1:length(vTrtEffVal)) )
        {
            Xlabel   <- paste0( strXAxisLabel, " (True Treatment Effect = ", round(vTrtEffVal[i], 3), ")" )
            subdat1  <- filter( subdat, round(vTrueRateDiff - vTrtEffVal[i], 8) == 0, round(dTrueRateS - dTrueRateSVal, 8) == 0 )
            if( round(vTrtEffVal[i] - dTVVal, 8) == 0 ) 
            {
                lPlots[[paste0("pProb", i)]]  <- PlotProbStacked3( dfDat = subdat1, strTitle = strTitle, strXAxisLabel = Xlabel, vXAxisTick = vXAxisTick, 
                                                                   vVlineVal = vVlineVal, vHlineVal = vHlineTVVal, bGrey = bGrey )
            }
            else if( round(vTrtEffVal[i] - dLRVVal, 8) == 0 ) 
            {
                lPlots[[paste0("pProb", i)]]  <- PlotProbStacked3( dfDat = subdat1, strTitle = strTitle, strXAxisLabel = Xlabel, vXAxisTick = vXAxisTick, 
                                                                   vVlineVal = vVlineVal, vHlineVal = vHlineLRVVal, bGrey = bGrey )
            }
            else
            {
                lPlots[[paste0("pProb", i)]]  <- PlotProbStacked3( dfDat = subdat1, strTitle = strTitle, strXAxisLabel = Xlabel, vXAxisTick = vXAxisTick, 
                                                                   bGrey = bGrey )
            }
        }
    }
    
    
    return( lPlots )
}



GetPlotsTrtEff3 <- function( dTVVal, dTrueRateSVal = NULL, dLRVVal, vSampVal, dAlphaVal, dBetaVal, strDecisionMethodVal, vCutoffVal = NULL, strTitle1,
                             bGrey = FALSE )
{
    lRet  <- list( lPlot = list(), lDat = list() )

    if( length(vSampVal) == 1 )
    {
        subdat <- CalcGoNGBinary( vSampleSize       = vSampVal,
                                  dTV               = dTVVal,
                                  dLRV              = dLRVVal,
                                  vTrueTrtEff       = seq(0, 1, 0.01),
                                  dAlpha            = dAlphaVal,
                                  dBeta             = dBetaVal,
                                  strAnalysisModel  = "clopper-pearson",
                                  strDecisionMethod = strDecisionMethodVal )$dfProbCutoff$dfProb
        
        subdat <- filter( subdat, ProbGo < 0.999, ProbNoGo < 0.999 )
        
        
        strXAxisLabel <- "Treatment Effect"
        subdat$vX     <- subdat$vTrueRateE
        
        strTitle2   <- paste0( "Sample Size = ", vSampVal, ", TV = ", round(dTVVal, 3), ", LRV = ", round(dLRVVal, 3), ", Alpha = ", 
                               dAlphaVal, ", Beta = ", dBetaVal )
        
        
        
        title1      <- paste0( strTitle1, paste0(": Probability (Clopper-Pearson ", strDecisionMethodVal, ")" ) )
        strTitle    <- ggtitle(bquote(atop(.(title1), atop(italic(.(strTitle2))))))
        lRet$lPlot$pProb <- PlotProbStackedTVLRV( dfDat = subdat, strTitle = strTitle, strXAxisLabel = strXAxisLabel )
    }
        
    if( length(vSampVal) == 2 )
    {
        lProbCutoff <- CalcGoNGBinary( vSampleSize       = vSampVal,
                                  dTrueRateS        = dTrueRateSVal,
                                  dTV               = dTVVal,
                                  dLRV              = dLRVVal,
                                  vTrueTrtEff       = seq(- dTrueRateSVal, 1 - dTrueRateSVal, 0.01),
                                  dAlpha            = dAlphaVal,
                                  dBeta             = dBetaVal,
                                  strAnalysisModel  = "approx",
                                  strDecisionMethod = strDecisionMethodVal)$dfProbCutoff
        
        dfProb <- lProbCutoff$dfProb %>% 
            filter( ProbGo < 0.999, ProbNoGo < 0.999 )
        
        dfCutoff <- lProbCutoff$dfCutoff
        
        lRet$lDat <- lProbCutoff
        
        
        strTitle2   <- paste0( "Sample Size = ", VectorToString(vSampVal), ", TV = ", round(dTVVal, 3), ", LRV = ", round(dLRVVal, 3), ", Alpha = ", 
                               dAlphaVal, ", Beta = ", dBetaVal )
        
        
        
        
        ### plot of cutoffs ###
        title1   <- paste0( strTitle1, paste0(": Cutoff (Approximate ", strDecisionMethodVal, ")" ) )
        strTitle <- ggtitle(bquote(atop(.(title1), atop(italic(.(strTitle2))))))
        
        dfCutoff$vX   <- as.numeric( dfCutoff$vY0 )
        strXAxisLabel <- "Ctr: Observed Number of Responders"
        strYAxisLabel <- "Trt: Observed Number of Responders"
        
        lRet$lPlot$pCutoff <- PlotCutoff3( dfDat = dfCutoff, strTitle = strTitle, strXAxisLabel = strXAxisLabel, strYAxisLabel = strYAxisLabel, 
                                      vVlineVal = vCutoffVal, bGrey = bGrey )
        
        
        
        
        ### plot of probs ###
        title1   <- paste0( strTitle1, paste0(": Probability (Approximate ", strDecisionMethodVal, ")" ) )
        strTitle <- ggtitle(bquote(atop(.(title1), atop(italic(.(strTitle2))))))
        
        dfProb$vX     <- dfProb$vTrueRateDiff
        strXAxisLabel <- paste0( "Treatment Effect (True Rate in Ctr = ", dTrueRateSVal, ")" )
        
        lRet$lPlot$pProb   <- PlotProbStackedTVLRV( dfDat = dfProb, strTitle = strTitle, strXAxisLabel = strXAxisLabel, bGrey = bGrey )
    }
    
    return( lRet )
}


