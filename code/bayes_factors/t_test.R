library(dplyr)
library(tidyr)
library(BayesFactor)

# Generate some random data 
set.seed(1001)
N <- 50

Df <- data.frame(x = rnorm(N, mean=100, sd=15),
                 y = rnorm(N, mean=110, sd=15)) %>% 
  gather(country, IQ) %>%
  mutate(country = factor(country))

# Save it to file for use elsewhere (e.g. in Jasp)
write.csv(Df, row.names = F, quote = F, file='countryiq.csv')

# Box plot
ggplot(Df, aes(x=country, y=IQ)) + geom_boxplot()

# Descriptive statistics 
group_by(Df, country) %>% 
  summarize(mean=mean(IQ),
            sd=sd(IQ))

# Traditional t-test (Welch t-test)
M.classic <- t.test(IQ ~ country, data=Df)

# t-test using BayesFactor
M.bf <- ttestBF(formula = IQ ~ country, data=Df)

# Sample from posterior
S <- posterior(M.bf, iterations=10000)

summary(S)
plot(S[,1:4])
