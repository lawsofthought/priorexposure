load('smoking.Rda')

y <- smoking$cigs
N <- length(y)

M <- jags.model('zip.jags',
                data = list('y' = y, 'N'=N),
                n.chains = 3)

update(M, 10000)
S <- coda.samples(M, variable.names = c('lambda.nonnull', 'beta'), n.iter = 10000)