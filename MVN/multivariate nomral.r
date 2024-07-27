dim = 2; r1 = 0.5; r2 = 0.55

S1 = matrix(rep(r1, dim^2), nrow = dim); diag(S1) = 1
S2 = matrix(rep(r2, dim^2), nrow = dim); diag(S2) = 1
#S2 = S1; S2[3,4] = r2; S2[4,3] = r2


ZU = c(1.26, 2); ZL = rep(-Inf, dim)
pmvnorm(lower=ZL, upper = ZU, mean = rep(0, dim), sigma = S1)[1]
pmvnorm(lower=ZL, upper = ZU, mean = rep(0, dim), sigma = S2)[1]

ZU = rep(Inf, dim); ZL = c(1.26, 2)
pmvnorm(lower=ZL, upper = ZU, mean = rep(0, dim), sigma = S1)[1]
pmvnorm(lower=ZL, upper = ZU, mean = rep(0, dim), sigma = S2)[1]


ZU = rep(Inf, dim); ZL = c(0, 0)
pmvnorm(lower=ZL, upper = ZU, mean = rep(0, dim), sigma = S1)[1]
pmvnorm(lower=ZL, upper = ZU, mean = rep(0, dim), sigma = S2)[1]

ZU = c(0,0); ZL = rep(-Inf, dim)
pmvnorm(lower=ZL, upper = ZU, mean = rep(0, dim), sigma = S1)[1]
pmvnorm(lower=ZL, upper = ZU, mean = rep(0, dim), sigma = S2)[1]
