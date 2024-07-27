
#################################################################################################### .
#   Program/Function Name: PlotCutoff
#   Author: Xiaomin Lu
#   Description: Generate plot of GoNG cutoffs on number of responders
#   Change History:
#   Last Modified Date: 11/06/2021
#################################################################################################### .

PlotCutoff <- function( dfDat, strTitle, strXAxisLabel, strYAxisLabel, gBaseSize = 12 )
{
    colorPalette <- c( "#009E73", "#F0E442", "#D55E00", "#808080",
                       "#000000", "#E69F00", "#56B4E9",
                       "#0072B2", "#CC79A7")
    
    color1        <- colorPalette[1:4]
    vGrp          <- c( "Go", "Expan", "No Go", "Grey" )
    names(color1) <- vGrp
    
    vGoCutoffY1    <- dfDat$vGoCutoffY1
    vNoGoCutoffY1  <- dfDat$vNoGoCutoffY1
    vExpanCutoffY1 <- dfDat$vExpanCutoffY1
    
    ymin   <- min(vGoCutoffY1, vNoGoCutoffY1, na.rm = TRUE)
    ymax   <- max(vGoCutoffY1, vNoGoCutoffY1, na.rm = TRUE)
    
    dfDat$ConLow <- ifelse(is.na(vNoGoCutoffY1),  ymin, vNoGoCutoffY1)
    dfDat$ConUp  <- ifelse(is.na(vGoCutoffY1),    ymax, vGoCutoffY1)
    dfDat$ConUp1 <- ifelse(is.na(vExpanCutoffY1), ymax, vExpanCutoffY1)
    
    
    vXAxisTick <- dfDat$vX
    nGrid <- length( vXAxisTick )
    while( nGrid > 10 )
    {
        vXAxisTick <- vXAxisTick[ unique( c( seq(1, nGrid, 2), nGrid ) ) ]
        nGrid      <- length( vXAxisTick )
    }
    
    
    grPlot <- ggplot(  dfDat, aes( x = vX )) +
        geom_ribbon( aes(ymin = ConUp,  ymax = ymax,   fill = "Go" ), alpha = 0.4) +
        geom_ribbon( aes(ymin = ConUp1,  ymax = ConUp,   fill = "Expan" ), alpha = 0.4) +
        geom_ribbon( aes(ymin = ConLow, ymax = ConUp1,  fill = "Grey" ), alpha = 0.4) +
        geom_ribbon( aes(ymin = ymin,   ymax = ConLow, fill = "No Go" ), alpha = 0.4) +
        geom_line( aes(y = vGoCutoffY1 ),   color = color1["Go"], size = 1.5 ) +
        geom_line( aes(y = vExpanCutoffY1 ),   color = color1["Expan"], size = 1.5 ) +
        geom_line( aes(y = vNoGoCutoffY1 ), color = color1["No Go"], size = 1.5 ) +
        theme_minimal(base_size = gBaseSize)  +
        theme(legend.position = "top", plot.title = element_text( hjust = 0.5, size = 16 )) +
        scale_color_manual( "", breaks = vGrp, values = color1 ) +
        scale_fill_manual( "", breaks = vGrp, values = color1 ) +
        scale_x_continuous(breaks = vXAxisTick ) +
        labs( x = strXAxisLabel , y = strYAxisLabel, hajust = 0.5) +
        strTitle +
        theme( plot.title = element_text( hjust = 0.5, size = 14 ),
               plot.subtitle = element_text( hjust = 0.5, size = 10 ),
               panel.grid.major.y = element_line( size = 1.5 ),
               legend.position ="top", legend.direction="horizontal", legend.title = element_blank(),
               legend.text = element_text(margin = margin(r = 15, unit = "pt")),
               plot.margin=unit(c(2,0,0,0),"cm"))
    
    return( grPlot )
}




#################################################################################################### .
#   Program/Function Name:
#   Author: Xiaomin Lu
#   Description: This function generate stacked plot of the decision probabilities
#   Change History:
#   Last Modified Date: 11/06/2021
#################################################################################################### .

PlotProbStacked <- function( dfDat, strTitle, strXAxisLabel, gBaseSize = 12 )
{
    
    colorPalette <- c( "#009E73", "#F0E442", "#D55E00", "#808080",
                       "#000000", "#E69F00", "#56B4E9",
                       "#0072B2", "#CC79A7")
    
    
    color1        <- colorPalette[1:4]
    vGrp          <- c( "Pr( Go )", "Pr( Expan )", "Pr( No Go )", "Pr( Grey )" )
    names(color1) <- vGrp
    
    
    vXAxisTick <- dfDat$vX
    nGrid <- length( vXAxisTick )
    while( nGrid > 10 )
    {
        vXAxisTick <- vXAxisTick[ unique( c( seq(1, nGrid, 2), nGrid ) ) ]
        nGrid      <- length( vXAxisTick )
    }
    
    
    grPlot <- ggplot(  dfDat, aes( x = vX )) +
        geom_ribbon(aes(ymin = 0, ymax = ProbNoGo, fill = "Pr( No Go )"), alpha = 0.4) +
        geom_ribbon(aes(ymin = ProbNoGo, ymax = ProbNoGo + ProbConsider, fill = "Pr( Grey )"), alpha = 0.4) +
        geom_ribbon(aes(ymin = ProbNoGo + ProbConsider, ymax = 1 - ProbGo, fill = "Pr( Expan )"), alpha = 0.4) +
        geom_ribbon(aes(ymin = 1 - ProbGo, ymax = 1, fill = "Pr( Go )"), alpha = 0.4) +
        theme_minimal(base_size = gBaseSize)  +
        scale_fill_manual( "", breaks = vGrp, values = color1 ) +
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
    
    return( grPlot )
    
}




#################################################################################################### .
#   Program/Function Name:
#   Author: Xiaomin Lu
#   Description: This function generate stacked plot of the decision probabilities
#   Change History:
#   Last Modified Date: 08/14/2020
#################################################################################################### .

PlotProbStackedVline <- function( dfDat, strTitle, strXAxisLabel, gBaseSize = 12 )
{
    
    colorPalette <- c( "#009E73", "#F0E442", "#D55E00", "#808080",
                       "#000000", "#E69F00", "#56B4E9",
                       "#0072B2", "#CC79A7")
    
    
    color1        <- colorPalette[1:4]
    vGrp          <- c( "Pr( Go )", "Pr( Expan )", "Pr( No Go )", "Pr( Grey )" )
    names(color1) <- vGrp
    
  
    vXAxisTick <- dfDat$vX
    nGrid <- length( vXAxisTick )
    while( nGrid > 10 )
    {
        vXAxisTick <- vXAxisTick[ unique( c( seq(1, nGrid, 2), nGrid ) ) ]
        nGrid      <- length( vXAxisTick )
    }
    
    
    text_TV    <- grid::textGrob("TV",  gp = grid::gpar( fontsize = 8, col = colorPalette[5]))
    text_LRV   <- grid::textGrob("LRV", gp = grid::gpar( fontsize = 8, col = colorPalette[5]))
    text_Expan <- grid::textGrob("Expan", gp = grid::gpar( fontsize = 8, col = colorPalette[5]))
    
    grPlot <- ggplot(  dfDat, aes( x = vX )) +
        geom_ribbon(aes(ymin = 0, ymax = ProbNoGo, fill = "Pr( No Go )"), alpha = 0.4) +
        geom_ribbon(aes(ymin = ProbNoGo, ymax = ProbNoGo + ProbConsider, fill = "Pr( Grey )"), alpha = 0.4) +
        geom_ribbon(aes(ymin = ProbNoGo + ProbConsider, ymax = 1 - ProbGo, fill = "Pr( Expan )"), alpha = 0.4) +
        geom_ribbon(aes(ymin = 1 - ProbGo, ymax = 1, fill = "Pr( Go )"), alpha = 0.4) +
        theme_minimal(base_size = gBaseSize)  +
        scale_fill_manual( "", breaks = vGrp, values = color1 ) +
        scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1) ) +
        labs( x = strXAxisLabel , y = "Probability", hajust = 0.5) +
        strTitle +
        geom_vline( xintercept = c( dfDat$dTV, dfDat$dLRV, dfDat$dMid ), lty = "dotted" , col = colorPalette[6], lwd = 1.25) +
        annotation_custom(text_TV, xmin  = dfDat$dTV[1], xmax = dfDat$dTV[1], ymin = 1.07, ymax = 1.07) +
        annotation_custom(text_LRV, xmin  = dfDat$dLRV[1], xmax = dfDat$dLRV[1], ymin = 1.07, ymax = 1.07) +
        annotation_custom(text_Expan, xmin  = dfDat$dMid[1], xmax = dfDat$dMid[1], ymin = 1.07, ymax = 1.07) +
        theme( plot.title = element_text( hjust = 0.5, size = 14 ),
               plot.subtitle = element_text( hjust = 0.5, size = 10 ),
               panel.grid.major.y = element_line( size = 1.5 ),
               legend.position ="top", legend.text = element_text(margin = margin(r = 15, unit = "pt")),
               legend.direction="horizontal", legend.title = element_blank(),
               plot.margin = unit(c(2,0,0,0),"cm") ) +
        coord_cartesian(clip = "off")
    
    
    
    return( grPlot )
    
}





################################################################################################### .
#   Program/Function Name: GetPlots
#   Author: Xiaomin Lu
#   Description: Generate plot of cutoffs and probs
#   Change History:
#   Last Modified Date: 11/06/2021
#################################################################################################### .

GetPlotsSamp <- function( dfDat, vTrtEffVal, dTVVal, dLRVVal, dMidVal, dPctVal, strTitle1 )
{
    subdat <- filter( dfDat, round(dTV - dTVVal, 8) == 0, round(dLRV - dLRVVal, 8) == 0, round(dMid - dMidVal, 8) == 0, 
                      round(dAlpha - 1 + dPctVal, 8) == 0 )
    
    
    strXAxisLabel <- "Sample Size"
    strYAxisLabel <- "Observed Number of Responders"
    subdat$vX     <- as.numeric( subdat$vSamp )
    
    strTitle2   <- paste0( "TV = ", round(dTVVal, 3), ", LRV = ", round(dLRVVal, 3), ", Expan Value = ", round(dMidVal, 3), 
                        ", Posterior Prob Threshold = ", dPctVal )
    
    
    
    lPlots   <- list()
    
    title1   <- paste0( strTitle1, ": Cutoff (Morpheus) over Sample Size" )
    title2   <- strTitle2
    strTitle <- ggtitle(bquote(atop(.(title1), atop(italic(.(title2))))))
    
    subdat1  <- filter( subdat, round(vTrtEff - dTVVal, 8) == 0 )
    lPlots$pCutoff <- PlotCutoff( dfDat = subdat1, strTitle = strTitle, strXAxisLabel = strXAxisLabel, strYAxisLabel = strYAxisLabel )
    
    
    
    title1     <- paste0( strTitle1, ": Probabililty (Morpheus) over Sample Size" )
    strTitle   <- ggtitle(bquote(atop(.(title1), atop(italic(.(title2))))))
    vTrtEffVal <- c( vTrtEffVal, dTVVal, dLRVVal, dMidVal )
    for( i in c(1:length(vTrtEffVal)) )
    {
        Xlabel   <- paste0( strXAxisLabel, " (True Resp Rate = ", round(vTrtEffVal[i], 3), ")" )
        subdat1  <- filter( subdat, round(vTrtEff - vTrtEffVal[i], 8) == 0 )
        lPlots[[paste0("pProb", i)]]  <- PlotProbStacked( dfDat = subdat1, strTitle = strTitle, strXAxisLabel = Xlabel )
    }
    
    return( lPlots )
}



GetPlotsPostThreshold <- function( dfDat, vTrtEffVal, dTVVal, dLRVVal, dMidVal, dSampVal, strTitle1 )
{
    subdat <- filter( dfDat, round(dTV - dTVVal, 8) == 0, round(dLRV - dLRVVal, 8) == 0, round(dMid - dMidVal, 8) == 0, 
                      as.numeric(vSamp) == dSampVal )
    
    
    strXAxisLabel <- "Posterior Probability Threshold"
    strYAxisLabel <- "Observed Number of Responders"
    subdat$vX     <- 1 - subdat$dAlpha
    
    strTitle2   <- paste0( "Sample Size = ", dSampVal, ", TV = ", round(dTVVal, 3), ", LRV = ", round(dLRVVal, 3), ", Expan Value = ", round(dMidVal, 3) )
    
    
    
    lPlots   <- list()
    
    title1   <- paste0( strTitle1, ": Cutoff (Morpheus) over Posterior Probability Threshold" )
    title2   <- strTitle2
    strTitle <- ggtitle(bquote(atop(.(title1), atop(italic(.(title2))))))
    
    subdat1  <- filter( subdat, round(vTrtEff - dTVVal, 8) == 0 )
    lPlots$pCutoff <- PlotCutoff( dfDat = subdat1, strTitle = strTitle, strXAxisLabel = strXAxisLabel, strYAxisLabel = strYAxisLabel )
    
    
    
    title1     <- paste0( strTitle1, ": Probabililty (Morpheus) over Posterior Probability Threshold" )
    strTitle   <- ggtitle(bquote(atop(.(title1), atop(italic(.(title2))))))
    vTrtEffVal <- c( vTrtEffVal, dTVVal, dLRVVal, dMidVal )
    for( i in c(1:length(vTrtEffVal)) )
    {
        Xlabel   <- paste0( strXAxisLabel, " (True Resp Rate = ", round(vTrtEffVal[i], 3), ")" )
        subdat1  <- filter( subdat, round(vTrtEff - vTrtEffVal[i], 8) == 0 )
        lPlots[[paste0("pProb", i)]]  <- PlotProbStacked( dfDat = subdat1, strTitle = strTitle, strXAxisLabel = Xlabel )
    }
    
    return( lPlots )
}



GetPlotsTrtEff <- function( dTVVal, dLRVVal, dMidVal, dSampVal, dPctVal, strTitle1 )
{
    subdat <- CalcProb( vSamp = dSampVal, vP1 = seq(0, 1, 0.01), dTV = dTVVal, dLRV = dLRVVal, dMid = dMidVal, dAlpha = 1 - dPctVal, dBeta = 1 - dPctVal, 
              dBetaMid = 1 - dPctVal, vBeta0 = c(1, 1), vBeta1 = c(1, 1) )$dfProbCutoff
    
    subdat <- filter( subdat, ProbGo < 0.999, ProbNoGo < 0.999 )
    

    strXAxisLabel <- "Treatment Effect"
    subdat$vX     <- subdat$vTrtEff
    
    strTitle2   <- paste0( "Sample Size = ", dSampVal, ", TV = ", round(dTVVal, 3), ", LRV = ", round(dLRVVal, 3), ", Expan Value = ", round(dMidVal, 3),
                           ", Posterior Prob Threshold = ", dPctVal )
    
    
    
    title1   <- paste0( strTitle1, ": Probabililty (Morpheus) over Treatment Effect" )
    strTitle <- ggtitle(bquote(atop(.(title1), atop(italic(.(strTitle2))))))
    pProb    <- PlotProbStackedVline( dfDat = subdat, strTitle = strTitle, strXAxisLabel = strXAxisLabel )
    
    return( pProb )
}






