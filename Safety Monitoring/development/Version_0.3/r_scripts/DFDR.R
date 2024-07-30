p.BH_FDR <- function(p){
    temp = sort(p,index.return=TRUE)
    v = temp$x
    ind = temp$ix
    
    n = length(p)
    p.tld = numeric(n)
    p.tld[temp$ix[n]] = v[n]
    if (n>1){
        for (i in (n-1):1){
            p.tld[temp$ix[i]] = min(p.tld[temp$ix[i+1]],n/i*v[i])
        }
    }
    p.tld
}


DFDR <- function(p,alpha,In.SOC){
    u.SOC = unique(In.SOC)
    n = length(u.SOC)
    p.i = numeric(n)
    for(bl in 1:n){
        p.SOC = p[In.SOC==u.SOC[bl]]
        p.i[bl] = min(p.BH_FDR(p.SOC))

    }
    
    p.i.tld = numeric(n)
    p.i.tld = p.BH_FDR(p.i)
    
    p.F = NULL
    SOC.F = NULL; SOC.ind = NULL
    for(bl in 1:n)
        if (p.i.tld[bl]<=alpha){
            p.F = c(p.F,p[In.SOC==u.SOC[bl]])
            SOC.F = c(SOC.F,In.SOC[In.SOC==u.SOC[bl]])
            SOC.ind = c(SOC.ind,bl)
        } 
    p.DFDR = rep(1,length(In.SOC))
    if (!is.null(p.F)){
        p.F.tld = p.BH_FDR(p.F)
        n.F = length(SOC.ind)
        for (k in 1:n.F)
            p.DFDR[In.SOC==u.SOC[SOC.ind[k]]] = p.F.tld[SOC.F==u.SOC[SOC.ind[k]]]
    }
    p.DFDR
}




# p.BH_FDR <- function(p){
#     temp = sort(p,index.return=TRUE)
#     v = temp$x
#     ind = temp$ix
#     
#     n = length(p)
#     p.tld = numeric(n)
#     p.tld[temp$ix[n]] = v[n]
#     for (i in (n-1):1){
#         p.tld[temp$ix[i]] = min(p.tld[temp$ix[i+1]],n/i*v[i])
#     }
#     p.tld
# }
# 
# 
# # N.SOC, SOC, Nae
# DFDR <- function(p, alpha, N.SOC, SOC, Nae){
#     p.i = numeric(N.SOC)
#     for(bl in 1:N.SOC)
#         p.i[bl] = min(p.BH_FDR(p[SOC==bl]))
#     print(p.i)
#     p.i.tld = p.BH_FDR(p.i)
#     
#     p.F = NULL
#     SOC.F = NULL; SOC.ind = NULL
#     for(bl in 1:N.SOC) {
#         print(p.i.tld)
#         if (p.i.tld[bl]<=alpha){
#             p.F = c(p.F,p[SOC==bl])
#             SOC.F = c(SOC.F,SOC[SOC==bl])
#             SOC.ind = c(SOC.ind,bl)
#         }
#     }
#     p.DFDR = rep(1,Nae)
#     if (!is.null(p.F)){
#         p.F.tld = p.BH_FDR(p.F)
#         n = length(SOC.ind)
#         for (k in 1:n)
#             p.DFDR[SOC==SOC.ind[k]] = p.F.tld[SOC.F==SOC.ind[k]]
#     }
#     p.DFDR
# }

