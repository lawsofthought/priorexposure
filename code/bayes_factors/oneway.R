library(dplyr)
library(tidyr)
library(BayesFactor)
  
set.seed(42)

Df <- data.frame(x = rnorm(N, mean=100, sd=15),
           y = rnorm(N, mean=100, sd=15),
           z = rnorm(N, mean=110, sd=15)) %>% 
  gather(country, IQ) %>%
  mutate(country = factor(country))

# Save it to file for use elsewhere (e.g. in Jasp)
write.csv(Df, row.names = F, quote = F, file='countryiq3.csv')

# Some descriptives
group_by(Df, country) %>% 
  summarise(mean = mean(IQ),
            sd = sd(IQ))

# Box plot
ggplot(Df, aes(x=country, y=IQ)) + geom_boxplot()

# Classic oneway anova
summary(aov(IQ ~ country, data=Df))

# BayesFactor Anova
M.bf = anovaBF(IQ ~ country, data=Df)

S <- posterior(M.bf, iterations = 10000)
summary(S)
plot(S[,1:4])