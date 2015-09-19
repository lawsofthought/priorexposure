y.stretch <- function(y, stretch.factor){
  y.range.length <- max(y) - min(y)
  y.center <- min(y) + y.range.length/2
  ylims <- y.center + c(-1, 1) * y.range.length/2 * stretch.factor
  return(ylims)
}

library(rjags)

load('Examples.Rdata')

df <- aggregate(meanFix ~ Time + Object, 
                data=FunctThemePts, 
                FUN=mean)

condition <- df$Object
levels(condition) <- seq(1, length(levels(condition)))
condition <- as.numeric(levels(condition))[condition] 

J <- max(condition)
N <- length(condition)
fixation <- df$meanFix
time <- df$Time
centers <- seq(min(time), max(time), by=100)
K <- length(centers)

M <- jags.model('gca_rbf.2.jags', 
                data=list('x'=time, 
                          'y'=fixation, 
                          'centers'=centers,
                          'condition'=condition,
                          'J'=J,
                          'N'=N, 
                          'K'=K),
                n.chains = 3)

update(M, 100000)

S <- coda.samples(M, variable.names = c('width', 'sigma', 'w.sigma'), n.iter = 10000)

S <- coda.samples(M, variable.names = c('mu'), n.iter = 10000)
Q <- summary(S)
plot(time[condition==1], Q$quantiles[condition==1,3], type='l', col='red')
lines(time[condition==2], Q$quantiles[condition==2,3], col='blue')
lines(time[condition==3], Q$quantiles[condition==3,3], col='green')
