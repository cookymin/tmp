library("ggplot2")

Stop.Go.cutoff <- function(d, TV, LRV, DC_LRV, AR_TV, ratio){
  r = ratio/(ratio+1)
  se = 1/sqrt(d*r*(1-r))
  
  alpha = 1 - DC_LRV; beta = AR_TV
  stop.c = log(TV) + qnorm(1-beta)*se
  go.c = log(LRV) - qnorm(1-alpha)*se
  if(go.c >= stop.c) go.c = stop.c
  
  return(list(go = exp(go.c), stop = exp(stop.c)))
}


Stop.Go.prob <- function(UIV, d, TV, LRV, DC_LRV, AR_TV, ratio){
  cut = Stop.Go.cutoff(d, TV, LRV, DC_LRV, AR_TV, ratio)
  r = ratio/(ratio+1)
  se = 1/sqrt(d*r*(1-r))
  
  go.prob = pnorm((log(cut$go)-log(UIV))/se)
  stop.prob = 1 - pnorm((log(cut$stop)-log(UIV))/se)
  cont.prob = 1 - go.prob - stop.prob
  return(list(go = go.prob, consider = cont.prob, stop = stop.prob))
}



plot.prob <- function(datplot, xvar, xlab, title1, title2, outfile){
  xvar = datplot[, xvar]
  datplot = cbind(datplot, xvar)
  
  x.max = max(xvar)
  x.min = min(xvar)
  diff = x.max - x.min
  grid = round(diff/8)
  npwr10 = 1
  while(diff/8 < 1/npwr10){
    npwr10 = npwr10 * 10
  }
  grid = round(diff/8 * npwr10)/npwr10
  
  out.plot <- ggplot(datplot, aes(x = xvar)) + 
    geom_ribbon(aes(ymin = 1-stop.prob, ymax = 1), fill = "coral2", alpha = 0.9) +
    geom_ribbon(aes(ymin = go.prob, ymax = 1-stop.prob), fill = "yellow", alpha = 0.9) +
    geom_ribbon(aes(ymin = 0, ymax = go.prob), fill = "lawngreen", alpha = 0.9) +
    theme_grey() + theme(legend.position="bottom") +
    labs(y = "Probability", x = paste0("True Treatment Effect (", xlab, ")")) +
    scale_y_continuous(minor_breaks=seq(0, 1, 0.1), breaks=seq(0, 1, 0.2), expand = c(0, 0), limits = c(0, 1)) +
    scale_x_continuous(breaks=seq(ceiling(x.min*npwr10)/npwr10, x.max, grid), expand = c(0, 0), limits = c(x.min, x.max)) +
    ggtitle(bquote(atop(.(title1), atop(italic(.(title2)))))) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.border = element_rect(colour = "gray50", fill=NA),
          #          strip.background =element_rect(fill="grey50", color="grey50"),
          panel.spacing = unit(0, "lines"),
          #          panel.grid.major = element_line(colour="gray80", size=0.1, linetype = "solid"),
          #          panel.grid.minor = element_line(colour="gray90", size=0.1, linetype = "dashed"),
          axis.text.x = element_text(colour="grey40",size=7),
          axis.title.x = element_text(size=10, color = "gray40"),
          axis.text.y = element_text(colour="grey40",size=7),
          axis.title.y = element_text(size=10, color = "gray40")) 
  
  ggsave(outfile, width=9.3, height=6, plot = out.plot)
  
}

plot.criter <- function(datplot, xvar, xlab, left.cut.v, right.cut.v, title1, title2, TV, LRV, DC_LRV, AR_TV, outfile){
  xvar = datplot[, xvar]
  left.cut = datplot[, left.cut.v]
  right.cut = datplot[, right.cut.v]
  datplot = cbind(datplot, xvar, left.cut, right.cut)

  x.max = max(xvar)
  x.min = min(xvar)
  diff = x.max - x.min
  grid = round(diff/8)
  npwr10 = 1
  while(diff/8 < 1/npwr10){
    npwr10 = npwr10 * 10
  }
  grid = round(diff/8 * npwr10)/npwr10
  
  
  if (left.cut.v == "stop.cut"){
    left.col = "coral2"
    left.label = "Stop"
  } 
  if (left.cut.v == "go.cut"){
    left.col = "lawngreen"
    left.label = "Go"
  } 
  
  if (right.cut.v == "stop.cut"){
    right.col = "coral2"
    right.label = "Stop"
  } 
  if (right.cut.v == "go.cut"){
    right.col = "lawngreen"
    right.label = "Go"
  } 
  
  out.plot <- ggplot(datplot, aes(x = xvar)) + 
    geom_rect(mapping=aes(xmin=-Inf, xmax=left.cut, ymin=-Inf, ymax=Inf), fill=left.col, alpha=0.9) +
    geom_rect(mapping=aes(xmin=left.cut, xmax=right.cut, ymin=-Inf, ymax=Inf), fill="yellow", alpha=0.9) +
    geom_rect(mapping=aes(xmin=right.cut, xmax=Inf, ymin=-Inf, ymax=Inf), fill=right.col, alpha=0.9) +
    ggtitle(bquote(atop(.(title1), atop(italic(.(title2)))))) +
    geom_vline(xintercept = LRV, linetype = "dashed", colour = "gray60") +
    geom_vline(xintercept = TV, linetype = "solid", colour = "gray60") +
    geom_text(aes(x=LRV, label="LRV", y = 1), colour = "gray60", hjust = 1, vjust = 1.5, size = 2, angle = 90) +
    geom_text(aes(x=TV, label="TV", y = 1), colour = "gray60", hjust = 1, vjust = 1.5, size = 2, angle = 90) +
    geom_text(aes(x=right.cut, label=right.label, y = 0.5), colour = "gray40", hjust = -1, size = 5) +
    geom_text(aes(x=left.cut, label=left.label, y = 0.5), colour = "gray40", hjust = 1.5,  size = 5) +
    geom_hline(yintercept = AR_TV, linetype = "solid", colour = "red", size = 1) +
    geom_hline(yintercept = DC_LRV, linetype = "solid", colour = "green4", size = 1) +
    theme_grey() + theme(legend.position="bottom") +
    labs(y = "Probability", x = paste0("Observed Treatment Effect (", xlab, ")")) +
    scale_y_continuous(minor_breaks=seq(0, 1, 0.1), breaks=seq(0, 1, 0.1), limits = c(0, 1)) +
    scale_x_continuous(breaks=c(LRV, TV, seq(ceiling(x.min*npwr10)/npwr10, x.max, grid)), limits = c(x.min, x.max)) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.border = element_rect(colour = "gray50", fill=NA),
          #          strip.background =element_rect(fill="grey50", color="grey50"),
          panel.spacing = unit(0, "lines"),
          #          panel.grid.major = element_line(colour="gray80", size=0.1, linetype = "solid"),
          #          panel.grid.minor = element_line(colour="gray90", size=0.1, linetype = "dashed"),
          axis.text.x = element_text(colour="grey40",size=7),
          axis.title.x = element_text(size=10, color = "gray40"),
          axis.text.y = element_text(colour="grey40",size=7),
          axis.title.y = element_text(size=10, color = "gray40")) 
  
  ggsave(outfile, width=9.3, height=6, plot = out.plot)
  
}



## for hazard ratio (HR)
f.HR <- function(d, ratio, TV, LRV, DC_LRV, AR_TV, UIV, plot, no){

  cut = Stop.Go.cutoff(d, TV, LRV, DC_LRV, AR_TV, ratio)
  go.cut = cut$go; stop.cut = cut$stop
  
  if (plot == "Y"){
    stop.prob.fun <- function(x) Stop.Go.prob(x, d, TV, LRV, DC_LRV, AR_TV, ratio)$stop - 0.99
    go.prob.fun <- function(x) Stop.Go.prob(x, d, TV, LRV, DC_LRV, AR_TV, ratio)$go - 0.99
    
    a = 100
    while(stop.prob.fun(a)*stop.prob.fun(1e-8) > 0) a = a*10
    upper = uniroot(stop.prob.fun, c(1e-8, a))$root
    
    a = 100
    while(go.prob.fun(a)*go.prob.fun(1e-8) > 0) a = a*10
    lower = uniroot(go.prob.fun, c(1e-8, a))$root
    
    datplot = NULL
    lower = min(lower, TV, LRV)
    HR = lower + (upper-lower)*c(0:200)/200
    HR = c(TV, LRV, HR, go.cut, stop.cut)
    for(i in 1:length(HR)){
      prob = Stop.Go.prob(HR[i], d, TV, LRV, DC_LRV, AR_TV, ratio)
      datplot = rbind(datplot, c(go.cut, stop.cut, HR[i], prob$go, prob$consider, prob$stop))
    }
    
    colnames(datplot) <- c("go.cut", "stop.cut", "HR", "go.prob", "consider.prob", "stop.prob")
    datplot = data.frame(datplot)
    
    
    title1 = "Survival: Probability of being in each zone"
    title2 = paste0("d = ", d, ", ratio = ", ratio, ", TV = ", TV, " , LRV = ", LRV, " , DC_LRV = ", DC_LRV, " , AR_TV = ", AR_TV)
    outfile = paste0("Survival ", no, " - Probability Zone ", date, ".jpg")
    plot.prob(datplot, xvar = "HR", xlab = "Hazard Ratio", title1, title2, outfile)
    
    title1 = "Survival: Go-NoGo Criteria for Treatment Effect"
    outfile = paste0("Survival ", no, " - Go-NoGo Criteria ", date, ".jpg")
    plot.criter(datplot, xvar = "HR", xlab = "Hazard Ratio", left.cut.v = "go.cut", right.cut.v = "stop.cut", 
                title1, title2, TV, LRV, DC_LRV, AR_TV, outfile)
  }
  
  outdat = NULL
  HR = c(TV, LRV, UIV)
  for(i in 1:length(HR)){
    prob = Stop.Go.prob(HR[i], d, TV, LRV, DC_LRV, AR_TV, ratio)
    outdat = rbind(outdat, c(go.cut, stop.cut, HR[i], prob$go, prob$consider, prob$stop))
  }
  
  colnames(outdat) <- c("go.cut", "stop.cut", "HR", "go.prob", "consider.prob", "stop.prob")
  outdat = data.frame(outdat)

  outdat = cbind(no, d, ratio, TV, LRV, DC_LRV, AR_TV, outdat)
  return(outdat)
}




## plot of prob vs N
plot.prob.N <- function(datplot, val, n.min, n.max, title1, title2, outfile){
  go.prob = datplot[, paste0("go.prob.", val)]
  stop.prob = datplot[, paste0("stop.prob.", val)]
  consider.prob = datplot[, paste0("consider.prob.", val)]
  n = datplot$n
  subdat= data.frame(n, go.prob, stop.prob, consider.prob)
  
  n.30 = NULL
  if (length(n[consider.prob <= 0.3]) > 0) n.30 = min(n[consider.prob <= 0.3])
  
  out.plot <- ggplot(subdat, aes(x = n, y = 1-stop.prob)) + geom_line(aes(y = go.prob)) + 
    geom_ribbon(aes(ymin = 1-stop.prob, ymax = 1), fill = "coral2", alpha = 0.9) +
    geom_ribbon(aes(ymin = go.prob, ymax = 1-stop.prob), fill = "yellow", alpha = 0.9) +
    geom_ribbon(aes(ymin = 0, ymax = go.prob), fill = "lawngreen", alpha = 0.9) +
    geom_vline(xintercept = n.30, linetype = "dotted", colour = "grey80", size = 1) +
    theme_grey() + theme(legend.position="bottom") +
    labs(y = "Probability", x = "Number of Events") +
    scale_y_continuous(minor_breaks=seq(0, 1, 0.1), expand = c(0, 0), breaks=seq(0, 1, 0.2), limits = c(0, 1)) +
    scale_x_continuous(breaks=c(n.30, seq(n.min, n.max, 10)), expand = c(0, 0), limits = c(n.min, n.max)) +
    ggtitle(bquote(atop(.(title1), atop(italic(.(title2)))))) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.border = element_rect(colour = "gray50", fill=NA),
          panel.spacing = unit(0, "lines"),
          axis.text.x = element_text(colour="grey40",size=7),
          axis.title.x = element_text(size=10, color = "gray40"),
          axis.text.y = element_text(colour="grey40",size=7),
          axis.title.y = element_text(size=10, color = "gray40")) 
  
  ggsave(outfile, width=9.3, height=6, plot = out.plot)
}


## plot of criteria over N
plot.criter.N <- function(datplot, n.min, n.max, title1, title2, outfile){
  out.plot <- ggplot(datplot, aes(x = n)) + 
    geom_line(aes(y = go.cut, color = "Go", linetype = "Go")) + geom_line(aes(y = stop.cut, color = "Stop", linetype = "Stop")) + 
    geom_line(aes(y = TV, color = "TV", linetype = "TV")) + geom_line(aes(y = LRV, color = "LRV", linetype = "LRV")) + 
    ggtitle(bquote(atop(.(title1), atop(italic(.(title2)))))) +
    scale_colour_manual("", breaks = c("Go", "Stop", "TV", "LRV"), values = c("Go"="green4", "Stop"="red", "TV" = "black", "LRV" = "blue")) +
    scale_linetype_manual("", breaks = c("Go", "Stop", "TV", "LRV"), values = c("Go"="solid", "Stop"="solid", "TV" = "solid", "LRV" = "dashed")) +
    theme_grey() + theme(legend.position="bottom") +
    labs(y = "Hazard Ratio", x = "Number of Events") +
    # scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(breaks=seq(n.min, n.max, 10), expand = c(0, 0), limits = c(n.min, n.max)) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.border = element_rect(colour = "gray50", fill=NA),
          #          strip.background =element_rect(fill="grey50", color="grey50"),
          panel.spacing = unit(0, "lines"),
          #          panel.grid.major = element_line(colour="gray80", size=0.1, linetype = "solid"),
          #          panel.grid.minor = element_line(colour="gray90", size=0.1, linetype = "dashed"),
          axis.text.x = element_text(colour="grey40",size=7),
          axis.title.x = element_text(size=10, color = "gray40"),
          axis.text.y = element_text(colour="grey40",size=7),
          axis.title.y = element_text(size=10, color = "gray40")) 
  
  ggsave(outfile, width=9.3, height=6, plot = out.plot)
}


plot.N <- function(n.min, n.max, ratio, TV, LRV, UIV, DC_LRV, AR_TV, no){
  datplot = NULL
  for(n in n.min:n.max){
    cut = Stop.Go.cutoff(d = n, TV, LRV, DC_LRV, AR_TV, ratio)
    go.cut = cut$go; stop.cut = cut$stop
    
    prob.TV = Stop.Go.prob(TV, d = n, TV, LRV, DC_LRV, AR_TV, ratio)
    prob.LRV = Stop.Go.prob(LRV, d = n, TV, LRV, DC_LRV, AR_TV, ratio)
    prob.UIV = Stop.Go.prob(UIV, d = n, TV, LRV, DC_LRV, AR_TV, ratio)
    datplot = rbind(datplot, cbind(n, TV, LRV, go.cut, stop.cut, prob.TV$go, prob.TV$consider, prob.TV$stop, 
                                   prob.LRV$go, prob.LRV$consider, prob.LRV$stop, prob.UIV$go, prob.UIV$consider, prob.UIV$stop))
  }
  
  colnames(datplot) <- c("n", "TV", "LRV", "go.cut", "stop.cut", "go.prob.TV", "consider.prob.TV", "stop.prob.TV", 
                         "go.prob.LRV", "consider.prob.LRV", "stop.prob.LRV", "go.prob.UIV", "consider.prob.UIV", "stop.prob.UIV")
  datplot = data.frame(datplot)
  
  
  
  title2 = paste0("Ratio = ", ratio, ", TV = ", TV, " , LRV = ", LRV, " , UIV = ", UIV, " , DC_LRV = ", DC_LRV, " , AR_TV = ", AR_TV)
  val = "TV"
  title1 = paste0("Survival: Probability of being in each zone, given the true value = ", val)
  outfile = paste0("Survival ", no, " - Probability Zone vs N ", val, " ", date, ".jpg")
  plot.prob.N(datplot, val, n.min, n.max, title1, title2, outfile)

  val = "LRV"
  title1 = paste0("Survival: Probability of being in each zone, given the true value = ", val)
  outfile = paste0("Survival ", no, " - Probability Zone vs N ", val, " ", date, ".jpg")
  plot.prob.N(datplot, val, n.min, n.max, title1, title2, outfile)
  
  val = "UIV"
  title1 = paste0("Survival: Probability of being in each zone, given the true value = ", val)
  outfile = paste0("Survival ", no, " - Probability Zone vs N ", val, " ", date, ".jpg")
  plot.prob.N(datplot, val, n.min, n.max, title1, title2, outfile)
  

  title1 = "Survivalf: Go-NoGo Cut Offs vs Number of Events"
  plot.criter.N(datplot, n.min, n.max, title1, title2, outfile = paste0("Survival ", no, " - Go-NoGo Cut Offs vs N ", date, ".jpg"))
  
}


find.N <- function(n.min, n.max, ratio, TV, LRV, UIV, DC_LRV, AR_TV){
  datplot = NULL
  for(n in n.min:n.max){
    cut = Stop.Go.cutoff(d = n, TV, LRV, DC_LRV, AR_TV, ratio)
    go.cut = cut$go; stop.cut = cut$stop
    
    prob.TV = Stop.Go.prob(TV, d = n, TV, LRV, DC_LRV, AR_TV, ratio)
    datplot = rbind(datplot, cbind(n, TV, LRV, go.cut, stop.cut, prob.TV$go, prob.TV$consider, prob.TV$stop))
  }
  
  colnames(datplot) <- c("n", "TV", "LRV", "go.cut", "stop.cut", "go.prob.TV", "consider.prob.TV", "stop.prob.TV")
  datplot = data.frame(datplot)
  
  consider.prob = datplot[, "consider.prob.TV"]
  n = datplot$n
  n.30 = NULL
  if (length(n[consider.prob <= 0.3]) > 0) n.30 = min(n[consider.prob <= 0.3])
  return(n.30)
}






