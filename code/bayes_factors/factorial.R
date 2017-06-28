library(dplyr)
library(tidyr)

data("ToothGrowth")

Df <- within(ToothGrowth,
             dose <- factor(dose))

# Save it to file for use elsewhere (e.g. in Jasp, even though I think it is there already)
write.csv(Df, row.names = F, quote = F, file='tooth.csv')

# Descriptives 
group_by(Df, dose, supp) %>%
  summarize(mean = mean(len)) %>%
  spread(supp, mean)

# Interaction plot
with(Df, 
     interaction.plot(dose, supp, len, lwd=3))

# dose main effect box plot
ggplot(Df, aes(x=dose, y=len)) + 
  geom_boxplot(width=0.5) +
  xlab('dose') + 
  theme_classic()

# supp main effect box plot
ggplot(Df, aes(x=supp, y=len)) + 
  geom_boxplot(width=0.5) +
  xlab('dose') + 
  theme_classic()

# Interaction box plot
ggplot(Df, aes(x=dose, y=len, col=supp)) + 
  geom_boxplot(width=0.5) +
  xlab('dose') + 
  theme_classic()

# Classical factorial Anova
M <- aov(len ~ dose*supp, data=Df)
summary(M)

# Bayes factor factorial anova 
M.bf = anovaBF(len ~ supp*dose, data=Df)

# Sampling full model
S <- posterior(M.bf, index=4, iterations = 10000)
summary(S)

# Bf of all possible models compared to null
M.bf = anovaBF(len ~ supp*dose, data=Df, whichModels = 'all')

# Compare nested models, e.g. looking for interaction effects
M.bf.additive <- lmBF(len ~ supp + dose, data = Df)
M.bf.full <- lmBF(len ~ supp + dose + supp:dose, data = Df)

# Get the interaction BF
M.bf.interaction <- M.bf.full/M.bf.additive

# Recompute it to get increased accuracy
M.bf.interaction <- recompute(M.bf.interaction, iterations=1e6)
