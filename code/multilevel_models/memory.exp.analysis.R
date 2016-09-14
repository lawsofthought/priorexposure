library(rjags)

Df <- read.table("memory.experiment.data.txt", header=T)

N<-dim(Df)[1]
text <- Df$text
item <- Df$item
subject <- Df$subject
x <- Df$present
response <- Df$response

J<-length(levels(subject))
K<-length(levels(item))
T<-length(levels(text))

data=list('response' = response,
          'item'=item,
          'text'=text,
          'x'=x,
          'subject'=subject,
          'J'=J,
          'N'=N,
          'K'=K,
          'T'=T)

M<-jags.model("memory.exp.analysis.jags", 
              data=data,
              n.chains=3)

update(M,1000)

S <- coda.samples(M, variable.names = c('psi.beta.text'), 1000)
