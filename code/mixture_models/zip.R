library(rjags)

set.seed(17)

N <- 100
lambda <- 5.75
beta <- 0.75 # Probability of non-null component

z <- runif(N) < beta
y <- rep(0, N)

for (i in 1:N) {
	if (z[i]) {
	  y[i] <- rpois(1, lambda = lambda)
	} else {
	  y[i] <- 0
	}
}

plot(as.numeric(names(table(y))), 
     as.numeric(table(y)), 
     xlab='number of events',
     ylab='frequency',
     type='h')


M <- jags.model('zip.jags',
                data = list('y' = y, 'N'=N),
                n.chains = 3)

update(M, 10000)
S <- coda.samples(M, variable.names = c('lambda.nonnull', 'beta'), n.iter = 10000)