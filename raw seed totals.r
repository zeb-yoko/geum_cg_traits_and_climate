df.all <- read.csv("full_clean_geum_experiment.csv")
library(ggplot2)
str(df.all$Population)
str(df.all$sm2018)
summary(df.all)
str(df.all)
b <- ggplot(df.all, aes(x = Population, y = sm2018/1000))+
	geom_bar(stat = "summary", fun.y = "sum")+ ylab("seedmass (g) total")+
	xlab("Population")+
	theme(axis.text.x = element_text(angle = 90, vjust= -.05))
b				
