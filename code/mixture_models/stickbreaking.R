N <- 100
alpha <- 10.0

v <- rep(0, N)
p <- rep(0, N)

for (k in 1:(N-1)) {
  v[k] <- rbeta(1, 1, alpha)
}

p[1] <- v[1] 
for (k in 2:(N-1)) {
  p[k] <- v[k] * (1-v[k-1]) * p[k-1]/v[k-1]
}

p.sum <- sum(p[1:(N-1)])
p[N] <- 1-p.sum

mu = 0.0 
sigma = 1.0

w <- rep(0, N)
for (i in 1:N){
  w[i] <- rnorm(1, mean=mu, sd=sigma)
}

plot(w, p, type='h', xlim=c(-2, 2))