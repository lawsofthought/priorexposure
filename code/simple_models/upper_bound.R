set.seed(1001)

upper.bound = 100
N <- 10
y <- runif(n=10, min=0, max=upper.bound)

M <- jags.model('upper_bound.jags', 
                data = list('y'=y, 'N'=N, tau=1e-10),
                n.chains = 3)

update(M, 10000)

S <- coda.samples(M, 
                  variable.names = c('upper.bound'),
                  n.iter=10000)

par(mfrow=c(2,1))
plot(S, trace=FALSE, auto.layout = FALSE, xlim=c(0, 200))

xm = max(y)
curve(dpareto(x, scale=xm, shape=N-1),
      xlim=c(0, 200),
      xlab=expression(lambda),
      ylab=expression(P*(lambda*'|'*x)))