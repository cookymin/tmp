rm( list=ls() )

path = "P:/Xiaomin Lu/CSE/NASH/ATLAS - Ph3"
setwd(path)

outdat <- NULL
for( alpha in c(0.04, 0.05, 0.0008, 0.001)/2 )
    for( Power in c(0.8, 0.85, 0.9) )
        for( HR in c(0.7, 0.75, 0.8, 0.85) )
        {
            NEvt <- round( 4*(qnorm(1-alpha) + qnorm(Power))^2/(log(HR))^2 )
            outdat <- rbind(outdat, data.frame(alpha, Power, HR, NEvt))
        }

write.csv(outdat, "Survival Power 07-15-2020.csv")
View(outdat)
