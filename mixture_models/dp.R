library('rjags')

rdirich <- function(alpha) {
  g <- rep(0, K)
  for (k in 1:length(alpha)){
    g[k] <- rgamma(1, shape=alpha[k], scale=1)
  }
  return(g/sum(g))
}

set.seed(101)

K <- 3

mu <- seq(-10, 10, length.out = K)
sigma <- sample(c(2, 5, 3.5), K, replace = T)

x.ax <- seq(-25, 25, length.out = 1e3)

plot(
  x = x.ax,
  y = dnorm(x.ax, mean = mu[1], sd = sigma[1]),
  xlab = 'x',
  ylab = 'p(x)',
  type = 'l',
  col = 'blue',
  ylim = c(0, 1.1 * dnorm(0, 0, min(sigma)))
)

for (k in 2:K) {
  lines(x.ax, 
        dnorm(x.ax, mean = mu[k], sd = sigma[k]), 
        type = 'l', 
        col = 'blue')
}

vpi <- rdirich(rep(1, K))

N <- 1e5
x <- sample(seq(K), size = N, replace = TRUE, prob = vpi)
y <- rep(0, N)
for (i in 1:N) {
  y[i] <- rnorm(1, mean = mu[x[i]], sd = sigma[x[i]])
}

hist(y, 100)

N <- 100
y.training <- y[1:N]

M <- jags.model(
  'dp.jags',
  data = list(
    'y' = y.training, 'N' = length(y.training)),
  n.chains = 3
)

update(M, 10000)

S <- coda.samples(M, variable.names = c('alpha'), n.iter=10000)
