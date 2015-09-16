spline.polynomial <- function(x, knot.0, width){
  
  knots <- knot.0 + width * seq(0, 4)
  
	if (x >= knots[1] & x < knots[2]) {

		u <- (x-knots[1])/width
		y <- 1/6 * u^3 
	
	} else if (x >= knots[2] & x < knots[3]) {

		u <- (x-knots[2])/width
		y <- 1/6 * (1 + 3*u + 3*u^2 - 3*u^3)

	} else if (x >= knots[3] & x <= knots[4]) {

		u <- (x-knots[3])/width
		y <- 1/6 * (4 - 6*u^2 + 3*u^3)

	} else if (x >= knots[3] & x <= knots[5]) {

		u <- (x-knots[4])/width
		y <- 1/6 * (1 - 3*u + 3*u^2 - u^3)
	}

	else {
		y <- 0 
	}

	return(y)

}

phi <- function(x, knot.0=0.0, width=1.0) {
  
  y <- sapply(x, spline.polynomial, knot.0, width)
  
  return(y)
}

x <- seq(-5, 5, length=1e3)
basisfunctions.output <- phi(x, knot.0=0, width=1.0)
plot(x, basisfunctions.output, 
     xlab='x', ylab='phi(x)', type='l',
     main='Cubic b-spline basis function')

Phi <- function(x, knots, width=1.0){
  
  N <- length(x)
  K <- length(knots)
  
  basisfunctions.output <- matrix(0, nrow=K, ncol=N)
  
  for (k in 1:K){
    basisfunctions.output[k,] <- phi(x, knots[k], width=width)
  }
  
  return(basisfunctions.output)
  
}

spline.prior.samples <- function(x, knots, width, n.samples=5, ylims=NULL) {
  
  basisfunctions.output <- Phi(x, knots=knots, width=width)
  
 
  y <- matrix(0, nrow=n.samples, ncol=length(x))
  
  for (i in seq(n.samples)) {
    w <- rnorm(length(knots))
    y[i,] <- w %*% basisfunctions.output
  }
  
  if (is.null(ylims)) {
    # Try to get good limits for y-axis
    y.range.length <- max(y) - min(y)
    y.center <- min(y) + y.range.length/2
    ylims <- y.center + c(-1, 1) * y.range.length/2 * 1.1 
  }
  
  
  # Set up empty plot
  main.title = sprintf('%d random samples from noiseless cubic b-spline regression model: width = %2.2f', n.samples, width)
  plot(1, 1, xlab='x', ylab='y', type='n', xlim=range(x), ylim=ylims, main=main.title)
  
  for (i in seq(n.samples)) {
    lines(x, y[i,], type='l')
  }
  
  
}

x <- seq(-5, 5, length.out=1e4)
knots <- seq(-9, 4)

