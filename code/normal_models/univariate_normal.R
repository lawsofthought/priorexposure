library(rjags)

##########################
# Generate synthetic data

set.seed(808)

N <- 50
true.mu <- 2.25
true.sigma <- 1.25

y <- rnorm(N, mean=true.mu, sd=true.sigma)

##########################
# Define your model

M <- jags.model('univariate_normal.jags',
                data = list('y'=y, 'N'=N),
                n.chains = 3)

##########################
# Run the sampler

N.iterations = 1e4

update(M, N.iterations)

S <- coda.samples(M,
                  variable.names = c('mu', 'sigma'),
                  n.iter = N.iterations)

plot(S[,1], trace=FALSE)
