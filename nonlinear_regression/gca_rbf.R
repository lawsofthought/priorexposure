library(rjags)
load('Examples.Rdata')

df <- subset(FunctThemePts, Object=='Target')

centers <- seq(min(df$Time), max(df$Time), by=100)
K <- length(centers)

subjects <- df$subj
levels(subjects) <- seq(1, length(levels(subjects)))
subjects <- as.numeric(levels(subjects))[subjects] 

J <- max(subjects)
N <- length(subjects)

M <- jags.model('gca_rbf.jags', 
                data=list('x'=df$Time, 
                          'y'=df$meanFix, 
                          'centers'=centers, 
                          'subjects'=subjects,
                          'N'=N, 'K'=K, 'J'=J),
                n.chains = 3)