library(rjags)


N <- 250
alpha <- 10
beta <- 10

y <- c(rep(0, N-139), rep(1, 139))
n <- sum(y)



M <- jags.model('coin_toss_bf.jags', 
                data = list('y'=y, 'N'=N, 'alpha'=alpha, 'beta'=beta),
                n.chains = 3)

update(M, 10000)

S <- coda.samples(M, 
                  variable.names = c('x'),
                  n.iter=10000)

# Examine results 
summary(S)

posterior.of.null <- mean(c(S[[1]], S[[2]], S[[3]])==2)
null.posterior.odds <- posterior.of.null/(1-posterior.of.null)
