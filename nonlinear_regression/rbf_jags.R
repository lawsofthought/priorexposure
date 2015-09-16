# A Gaussian radial basis function.
phi <- function(x, center=0, width=1){
  
  basisfunction.output <- exp(-(x-center)^2/width) 
  
  return(basisfunction.output)
}

Phi <- function(x, centers, width=1.0) {
 
  N <- length(x)
  K <- length(centers)
 
  basisfunctions.output <- matrix(0, nrow=K, ncol=N)
  
  for (k in 1:K) {
    basisfunctions.output[k,] <- phi(x, centers[k], width)
  }
  
  return(basisfunctions.output)
  
}

y.stretch <- function(y, stretch.factor){
  y.range.length <- max(y) - min(y)
  y.center <- min(y) + y.range.length/2
  ylims <- y.center + c(-1, 1) * y.range.length/2 * stretch.factor
  return(ylims)
}

library(rjags)

set.seed(100)

N <- 50
K <- 10
width <- 2.25
sigma <- 0.25
w <- rnorm(K, mean=0.0, sd=1.0)

x <- seq(-5, 5, length.out=N)
centers <- seq(-3, 3, length.out=K)

basisfunctions.output <- Phi(x, centers, width)
f <- as.vector(w %*% basisfunctions.output)

y <- f + rnorm(N, mean=0, sd=sigma)

M <- jags.model('rbf.jags', 
                data=list('x'=x, 'y'=y, 'centers'=centers, 'N'=N, 'K'=K),
                n.chains = 3)

update(M, 100000)

S <- coda.samples(M, variable.names = c('width', 'sigma', 'w.tau'), n.iter = 100000)

S.mu <- coda.samples(M, variable.names = c('mu'), n.iter = 10000)
q <- summary(S.mu)

plot(x, q$quantiles[,3], ylim=y.stretch(q$quantiles[,3], 1.25), type='l')
lines(x, q$quantiles[,1], type='l', col='red')
lines(x, q$quantiles[,5], type='l', col='red')
points(x,y)
