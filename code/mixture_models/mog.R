library(rjags)
set.seed(101)
vpi <- rep(1, 3)/3
K <- length(vpi)
mu <- c(5.0, 10.0, 15.0)
sigma <- c(1.0, 1.0, 1.0)

x <- seq(0, 20, length.out=1e3)

par(mfrow=c(1,2))
plot(x=x, 
     y=dnorm(x, mean=mu[1], sd=sigma[1]), 
     xlab='x',
     ylab='p(x)',
     type='l', 
     col='red')

lines(x, dnorm(x, mean=mu[2], sd=sigma[2]), type='l', col='blue')
lines(x, dnorm(x, mean=mu[3], sd=sigma[3]), type='l', col='green')

N <- 100
x <- sample(c(1, 2, 3), size=N, replace = TRUE, prob=vpi)
y <- rep(0, N)
for (i in 1:N){
  y[i] <- rnorm(1, mean=mu[x[i]], sd=sigma[x[i]])
}

hist(y, 25)

M <- jags.model('mog.jags', 
                data=list('y'=y, 'N'=N, 'K'=K),
                n.chains = 3)

update(M, 1e5)
S <- coda.samples(M, variable.names = c('mu', 'sigma', 'vpi'), n.iter = 1e4)

Q.mu <- c()
Q.sd <- c()
Q.vpi <- c()
for (j in 1:3){
  for (i in 1:dim(S[[j]])[1]) {
    I <- order(S[[j]][i, 1:K])
    Q.mu <- cbind(Q.mu, S[[j]][i, 1:K][I])
    Q.sd <- cbind(Q.sd, S[[j]][i, (K+1):(2*K)][I])
    Q.vpi <- cbind(Q.vpi, S[[j]][i, (2*K+1):(3*K)][I])
  }
}

apply(Q.mu, 1, mean)
