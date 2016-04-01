load('../../data/affairs.Rda')

model.data = list('cheat' = Affairs$affairs>0,
                  'years'=Affairs$yearsmarried,
                  'sex'=as.numeric(Affairs$gender)-1,
                  'children'=as.numeric(Affairs$children)-1,
                  'N'=dim(Affairs)[1])

M <- jags.model('affairs.jags',
                data=model.data,
                n.chains = 3)

update(M, 10000)

S <- coda.samples(M, 
                  variable.names = c('alpha', 
                                     'beta.sex',
                                     'beta.children',
                                     'beta.years'),
                  10000)


glm.M <- glm(I(affairs>0) ~ yearsmarried + children + gender, 
             family=binomial,
             data=Affairs)
