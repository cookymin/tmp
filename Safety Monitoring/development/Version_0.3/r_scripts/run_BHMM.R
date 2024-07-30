run_BHMM <- function(Nae, N.SOC, SOC, PT, Y, X, exposure_t, exposure_c, model_type, model_file, cutoff) {
    data <- list(Nae=Nae, N.SOC=N.SOC, SOC=SOC, PT=PT, Y=Y, X=X, exposure_t=exposure_t, exposure_c=exposure_c)
    
    init1=list(.RNG.name="base::Mersenne-Twister", .RNG.seed=1)
    init2=list(.RNG.name="base::Wichmann-Hill", .RNG.seed=2)
    init3=list(.RNG.name="base::Marsaglia-Multicarry", .RNG.seed=3)
    inits = list(init1,init2,init3)
    
    n.chains = 3
    
    model.l = jags.model(file=model_file, data, n.chains=n.chains, inits=inits, n.adapt=100)
    
    out_col = c("theta")   # output variables
    nDraw = 10000   # sample size to be simulated
    output = coda.samples(model.l, out_col, nDraw)
    out.mat = as.matrix(output)
    
    burn.in = nDraw/2
    Samp = NULL
    for (i in 1:n.chains)
        Samp = rbind(Samp,out.mat[(i*nDraw+1-burn.in):(i*nDraw),])
    
    probg0 = colMeans(Samp>cutoff)
    
    Prob.G0 = numeric(Nae) ## the posterior probability of theta > 0, where theta is the log of relative risk of trt over ctr after adjusting for exposure time, the higher the more likely to be a signal, the cutoff value can be 0.9 or 0.95
    for (i in 1:Nae)
        Prob.G0[i] = probg0[paste("theta[",SOC[i],",",PT[i],"]",sep="")]
    return(bhmm_poisson = Prob.G0)
    
}