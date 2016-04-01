library(rjags)

##########################
# Generate synthetic data

set.seed(707)

N <- 50
true.mu.x <- 2.25
true.mu.y <- 3.75
true.sigma <- 1.25

x <- rnorm(N, mean=true.mu.x, sd=true.sigma)
y <- rnorm(N, mean=true.mu.y, sd=true.sigma)

##########################
# Define your model

M <- jags.model('bivariate_normal.jags',
                data = list('x'=x, 'y'=y, 'N'=N),
                n.chains = 3)

##########################
# Run the sampler

N.iterations = 1e4

update(M, N.iterations)

S <- coda.samples(M,
                  variable.names = c('d'),
                  n.iter = N.iterations)

plot(S, trace=FALSE)