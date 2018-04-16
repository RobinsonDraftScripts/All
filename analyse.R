setwd('/home/robinson/Data/confocal_extensometer/acid_growth')

t<-read.csv('summary.csv')


fit <- aov(strain ~ treatment, data=t) 
TukeyHSD(fit)
