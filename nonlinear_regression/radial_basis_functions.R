# Some commands and functions for drawing and plotting random samples from 
# Gaussian radial basis function regression models.

# A Gaussian radial basis function.
phi <- function(x, center=0, width=1){

	y <- exp(-(x-center)^2/width) 

	return(y)
}

# Let's look at a example radial basis function.
x <- seq(-2, 2, length.out=1000)

plot(x, phi(x),
     type='l',
     xlab='x', 
     ylab='phi(x)')


# Now let's look at a few rbf's evenly spaced over an interval
x <- seq(-6, 6, length.out=1000)
plot(x, phi(x),
     type='l',
     xlab='x', 
     ylab='phi(x)')

for (mu in seq(-4, 4)) {lines(x, phi(x, center=mu))}

# A function to evaluate a set of radial basis functions with centers = centers.
Phi <- function(x, centers, width=1.0) {
 
  N <- length(x)
  K <- length(centers)
 
  basisfunctions.output <- matrix(0, nrow=K, ncol=N)
  
  for (k in 1:K) {
    basisfunctions.output[k,] <- phi(x, centers[k], width)
  }
  
  return(y)
  
}

# Plot a (random) weighted sum of K radial basis functions
x <- seq(-6, 6, length.out=1000)
centers <- seq(-4, 4)
basisfunctions.output <- Phi(x, centers=centers)

w <- rnorm(length(centers))
y <- w %*% basisfunctions.output
plot(x, y, xlab='x', ylab='y', type='l', main='Weighted sum of radial basis functions')

rbf.prior.samples <- function(x, centers, width, n.samples=5, ylims=NULL) {
  
  basisfunctions.output <- Phi(x, centers=centers, width=width)
  
  y <- matrix(0, nrow=n.samples, ncol=length(x))
  
  for (i in seq(n.samples)) {
    w <- rnorm(length(centers))
    y[i,] <- w %*% basisfunctions.output
    
  }
  
  if (is.null(ylims)) {
    # Try to get good limits for y-axis
    y.range.length <- max(y) - min(y)
    y.center <- min(y) + y.range.length/2
    ylims <- y.center + c(-1, 1) * y.range.length/2 * 1.1 
  }
  
  
  # Set up empty plot
  main.title = sprintf('%d random samples from noiseless rbf regression model: width = %2.2f', n.samples, width)
  plot(1, 1, xlab='x', ylab='y', type='n', xlim=range(x), ylim=ylims, main=main.title)
  
  for (i in seq(n.samples)) {
    lines(x, y[i,], type='l')
  }
}
