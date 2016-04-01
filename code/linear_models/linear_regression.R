load('../../data/beautyeval.Rda')

model.data = list('beauty'=beautydata$beauty,
                  'sex'=as.numeric(beautydata$sex)-1,
                  'eval'=beautydata$eval,
                  'N'=dim(beautydata)[1])

M <- jags.model('linear_regression.jags',
                data=model.data,
                n.chains = 3)

update(M, 10000)

S <- coda.samples(M, 
                  variable.names = c('alpha', 
                                     'beta.sex',
                                     'beta.beauty',
                                     'beta.interaction'),
                  10000)


lm.M <- lm(eval ~ beauty*sex, data=beautydata)