library(rjags)

# Simulate N coin flips, where probability of Heads is p.
coin.flips <- function(N, p){
	outcomes <- sample(c(0, 1),
		    size=N,
		    replace=T,
		    prob=c(1-p, p))

	return(outcomes)

}

set.seed(101)

N <- 100
p <- 0.25
alpha <- 1.0
beta <- 1.0

y <- coin.flips(N, p)
n <- sum(y)

M <- jags.model('coin_toss.jags', 
                data = list('y'=y, 'N'=N, 'alpha'=alpha, 'beta'=beta),
                n.chains = 3)

update(M, 10000)

S <- coda.samples(M, 
                  variable.names = c('p'),
                  n.iter=10000)

# Examine results 
summary(S)

HPDinterval(S)

par(mfrow=c(2,1))
# Plot density
plot(S, trace=F, density=T, auto.layout = F, xlim=c(0, 1))

# Compare with analytical solution
beta.plot(alpha + n, beta + (N-n))

