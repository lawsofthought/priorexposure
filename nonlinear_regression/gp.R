# With some code based on http://www.r-bloggers.com/gaussian-process-regression-with-r/
  
library(MASS)
library(ggplot2)
library(reshape2)

sq.exp.kernel <- function(x1, x2, tau=1) {
  Sigma <- matrix(rep(0, length(x1)*length(x2)), nrow=length(x1), ncol=length(x2))
  
  for (i in 1:nrow(Sigma)) {
    for (j in 1:ncol(Sigma)) {
      Sigma[i,j] <- exp(-(abs(x1[i]-x2[j])/tau)^2)
    }
  }
  return(Sigma)

}

f.sample <- function(n, mu, Sigma){
  
  samples <- matrix(rep(0, length(mu)*n), nrow = length(x), ncol=n)
  for (i in 1:n){
    samples[,i] <- mvrnorm(1, mu, Sigma)
  }
  
  samples.df <- as.data.frame(samples)
  names(samples.df) <- paste('Sample', seq(ncol(samples.df)))
  
  samples <- cbind(x=x, samples.df)
  samples <- melt(samples, id='x', variable.name = 'Sample')
  
  return(samples)
}

plot.samples <- function(x, samples){
  
  gg <- ggplot(samples, aes(x=x, y=value, color=Sample)) + 
    geom_line() +
    labs(x='x', y='f(x)')

  return(gg)
}



conditional.kernel.noise <- function(df, x, kernel, params, sigma.noise=0.1){
  
  k.xx <- kernel(df$x, df$x, params)
  k.xxs <- kernel(df$x, x, params)
  k.xsx <- kernel(x, df$x, params)
  k.xsxs <- kernel(x, x, params)
  
  f.bar.star <- k.xsx%*%solve(k.xx + sigma.noise^2*diag(1, ncol(k.xx)))%*%df$y
  cov.f.star <- k.xsxs - k.xsx%*%solve(k.xx + sigma.noise^2*diag(1, ncol(k.xx)))%*%k.xxs
  
  return(list('f.bar.star'=f.bar.star, 'cov.f.star'=cov.f.star))

}

conditional.kernel <- function(df, x, kernel, params){
  
  k.xx <- kernel(df$x, df$x, params)
  k.xxs <- kernel(df$x, x, params)
  k.xsx <- kernel(x, df$x, params)
  k.xsxs <- kernel(x, x, params)
  
  f.bar.star <- k.xsx%*%solve(k.xx)%*%df$y
  cov.f.star <- k.xsxs - k.xsx%*%solve(k.xx)%*%k.xxs
  
  return(list('f.bar.star'=f.bar.star, 'cov.f.star'=cov.f.star))
}

posterior.plot <- function(df, x, mu, samples){
  
  mu.df <- data.frame(x=x, y=mu)
  
  ggplot(samples, aes(x=x, y=value)) +
    geom_line(aes(group=Sample), colour="grey80") +
    geom_line(data=mu.df, aes(x=x, y=y), colour="red", size=1) + 
    geom_point(data=df, aes(x=x,y=y)) +
    theme_bw() +
    labs(x='x', y='f(x)')
  
}

x <- seq(-10, 10, length.out=1e2)
Sigma <- sq.exp.kernel(x, x, tau=2)
mu <- rep(0, length(x))
samples <- f.sample(3, mu, Sigma)
plot.samples(x, samples)

observed.data <- data.frame(x=c(-6, -5, -2, 2, 5),
                            y=c(-3, -2, 1, 5, -1))

Q <- conditional.kernel(observed.data, x, sq.exp.kernel, params = 2)
samples.posterior <- f.sample(50, Q$f.bar.star, Q$cov.f.star)

posterior.plot(observed.data, x=x, mu=Q$f.bar.star, samples = samples.posterior)

Q.noise <- conditional.kernel.noise(observed.data, x, sq.exp.kernel, params = 2, sigma.noise = 0.5)
samples.posterior.noise <- f.sample(50, Q.noise$f.bar.star, Q.noise$cov.f.star)

posterior.plot(observed.data, x=x, mu=Q.noise$f.bar.star, samples = samples.posterior.noise)

