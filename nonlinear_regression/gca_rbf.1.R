y.stretch <- function(y, stretch.factor){
  y.range.length <- max(y) - min(y)
  y.center <- min(y) + y.range.length/2
  ylims <- y.center + c(-1, 1) * y.range.length/2 * stretch.factor
  return(ylims)
}

library(rjags)

load('mirman_gca.Rdata')

df <- aggregate(meanFix ~ Time, 
                data=subset(FunctThemePts, Object=='Target'), 
                FUN=mean)
                
centers <- seq(min(df$Time), max(df$Time), by=100)
K <- length(centers)

time <- df$Time
fixation <- df$meanFix
N <- length(time)

M <- jags.model('gca_rbf.1.jags', 
                data=list('x'=time, 
                          'y'=fixation, 
                          'centers'=centers,
                          'N'=N, 
                          'K'=K),
                n.chains = 3)

update(M, 10000)

S <- coda.samples(M, variable.names = c('width', 'sigma', 'w.sigma'), n.iter = 10000)
S.mu <- coda.samples(M, variable.names = c('mu'), n.iter = 10000)
Q <- summary(S.mu)

plot(time, Q$quantiles[,3], ylim=y.stretch(Q$quantiles[,3], 1.25), type='l', col='blue')
lines(time, Q$quantiles[,1], type='l', col='red')
lines(time, Q$quantiles[,5], type='l', col='red')
points(time, fixation, pch=20, cex=0.5)