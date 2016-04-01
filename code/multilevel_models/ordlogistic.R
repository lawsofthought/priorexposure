data <- read.table("forensic_data.txt")
z <- data$V32 - 1 
x <- data$V31

n<-dim(data)[1]

# Downsampling
n <- 100
z <- z[1:n]
x <- x[1:n]

J<-5
K<-3

y<-array(0, dim=c(n,J,K))

# Create 1 in c response vectors
for (i in 1:n ) {
	for (j in 1:J ) {
			if (is.na(data[i,j]) || data[i,j]>=3)
				{ y[i,j,1:K]<-c(NA,NA,NA) }
			else
				{ y[i,j,1+data[i,j]]<-1 }
			}
	}

library(rjags)

M <- jags.model("ordlogistic.jags", 
                data=list('y'=y, 'x'=x, 'z'=z, 'n'=n, 'J'=J, 'K'=K),
                n.chain=3)
update(M, 10000)


S.params <- coda.samples(M,variable.names=c("alpha","beta", "gamma","c.cutoff"), n.iter=2000, thin=2)
S.x <- coda.samples(M, variable.names=c("x"), n.iter=2000, thin=2)
S.lmbda <- coda.samples(M,variable.names=c("lmbda"), n.iter=2000, thin=2)