load('Examples.Rdata')
library(ggplot2)

ggplot(FunctThemePts, 
       aes(Time, meanFix, color=Object)) + 
       stat_summary(fun.y=mean, geom='line') + 
       stat_summary(fun.data=mean_se, geom='pointrange')

ggplot(FunctTheme, aes(Time, meanFix, linetype=Object)) +
  facet_wrap(~ Condition) + 
  stat_summary(fun.y=mean, geom="line") +
  stat_summary(fun.data=mean_se, geom="ribbon", alpha=0.3) +
  labs(x="Time Since Word Onset (ms)",
       y="Fixation Proportion")
