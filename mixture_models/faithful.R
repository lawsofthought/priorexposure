data("faithful")

N <- dim(faithful)[1]
K <- 2
M <- jags.model('momvg.jags', 
                data=list('y'=faithful, 'N'=N, 'K'=K),
                n.chains = 3)
update(M, 1000)
S <- coda.samples(M, variable.names = c('mu', 'Sigma'), n.iter = 1e3)

plot.new()
par(mfrow=c(3,1))
for (j in 1:3){
  
  centers <- matrix(apply(S[[j]][,9:12], 2, median), 2, 2)
  s <- apply(S[[j]][,1:8], 2, median)
  Sigma <- array(c(s[seq(1,8, by=2)], s[seq(2,8, by=2)]), c(2, 2, 2))
  
  plot(faithful, xlim=c(1, 6), ylim=c(40, 100))
  lines( ellipse(Sigma[,,1], centre = centers[1,]) , col='red')
  lines( ellipse(Sigma[,,2], centre = centers[2,]) , col='blue')
  
}