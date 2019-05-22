setwd("C:/Users/eckat/Desktop")
stoma <- read.csv("GeumStomata_V9.csv")
str(stoma)
stoma$Region <-gsub("GL Alvar", "GL_Alvar", stoma$Region)
stoma$Region <-gsub("M Alvar", "MB_Alvar", stoma$Region)

iso<-read.csv("Refined_Isotope.csv")
str(iso)
clim <- read.csv("all_climate_data.csv")
str(clim)
clim$Region <-gsub("GL Alvar", "GL_Alvar", clim$Region)
clim$Region <-gsub("M Alvar", "MB_Alvar", clim$Region)

View(clim)
library(dplyr)
df <-iso %>% dplyr::inner_join(clim, by=c("Population", "Region"))
str(df)
View(df)
df.all <- read.csv("full_clean_geum_experiment.csv")
df.all$Region <-gsub("GL_alvar", "GL_Alvar", df.all$Region)
df.all$Region <-gsub("MB_alvar", "MB_Alvar", df.all$Region)
View(df.all)
##select only 2018 fitness data for comparison##
##2018 single year is sm.3; sm2018 is cumulative fitness from 2016-2018##
fits <- select(df.all, Region, Sample.ID, sm.3, No.Fruit.2018)
df <- merge(fits, df, by = c("Sample.ID", "Region"), all.x = T)
str(df)
View(df)
library(ggplot2)
col.bw<- c("black" , "gray52")


#remove manitoba
dfa<-subset(df, subset=Region=='Prairie' | Region=='GL_Alvar')

#-----------------------------------------------------------------
#isotope
#-----------------------------------------------------------------
#fitness x isotope----------------------------------------------------
c<-ggplot(dfa, aes(x=dfa$dC13, y = dfa$sm.3, colour=Region))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y ~ poly(x, 2))+theme_classic()+
  scale_colour_manual(values=col.bw) +
#  annotate("text",label="GLAr^2 == 0.01917",x=135,y=-27.2,parse=TRUE,size=2,colour="darkslategray3")+
 # annotate("text",label="PRAr^2 == 0.03289",x=135,y=-27.4,parse=TRUE,size=2,colour="gold3")+
  #ylab("dC13")+xlab("Spring Climate Moisture Deficit")+ 
#  geom_vline(xintercept=55,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "top")
c

cmodp<-lm(prc$dC13~prc$CMD_sp) 
summary(cmodp) 
#Multiple R-squared:  0.03289,	Adjusted R-squared:  -0.0004569 
#F-statistic: 0.9863 on 1 and 29 DF,  p-value: 0.3289
cmodg<-lm(glc$dC13~glc$CMD_sp) 
summary(cmodg)
#Multiple R-squared:  0.01917,	Adjusted R-squared:  0.0003037 
#F-statistic: 1.016 on 1 and 52 DF,  p-value: 0.3181

########################################

##load stomata into fitness-isotope sheet##
df <- merge(df, stoma, by = c("Sample.ID", "Region"), all = T)
dfb<-subset(df, subset=Region=='Prairie' | Region=='GL_Alvar')
#library(ggpubr)
#col <- ggarrange(c, d, h, i,labels = c("A", "B", "C", "D"),ncol=2,nrow=2)
#col

#bw<-ggarrange(cb,dw,hb,ib, labels = c("A", "B", "C", "D"),ncol=2,nrow=2)
#bw
#subset data to get r2
prc <- subset(dfb, Region == "Prairie")
glc <- subset(dfb, Region =="GL Alvar")


#GCL_B
#####################################
#bw
cb<-ggplot(dfb, aes(x=dfb$Av_GCLengthB, y = dfb$sm.3, colour=Region))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=c("black", "gray48")) +
  #annotate("text",label="GLAr^2 == 0.03704",x=130,y=0.035,parse=TRUE,size=2,colour="black")+
  #annotate("text",label="PRAr^2 == 0.06628",x=130,y=0.0357,parse=TRUE,size=2,colour="gray48")+ annotate("text", label="*", x=88.65124456, y=0.02607884, col="black", size=13)+
#  annotate("text", label="*", x=40.18813232, y=0.02500193, col="black", size=13)+
#  ylab("Abaxial Guard Cell Length")+xlab("Spring Climate Moisture Deficit")+ 
#  geom_vline(xintercept=55,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "top")
cb
#quadratic BEST
cmodp<-lm(prc$Av_GCLengthB~prc$CMD_sp+ I(prc$CMD_sp^2)) 
summary(cmodp) 
#Multiple R-squared:  0.06628,	Adjusted R-squared:  0.05058 
#F-statistic: 4.223 on 2 and 119 DF,  p-value: 0.0169
cmodg<-lm(glc$Av_GCLengthB~glc$CMD_sp+ I(glc$CMD_sp^2)) 
summary(cmodg)
#Multiple R-squared:  0.03704,	Adjusted R-squared:  0.03154 
#F-statistic: 6.732 on 2 and 350 DF,  p-value: 0.001353

#linear
#cmodp<-lm(prc$Av_GCLengthB~prc$CMD_sp) 
#summary(cmodp) 
#Multiple R-squared:  0.03254,	Adjusted R-squared:  0.02448 
#F-statistic: 4.037 on 1 and 120 DF,  p-value: 0.04677
#cmodg<-lm(glc$Av_GCLengthB~glc$CMD_sp) 
#summary(cmodg)
#Multiple R-squared:  0.006082,	Adjusted R-squared:  0.00325 
#F-statistic: 2.148 on 1 and 351 DF,  p-value: 0.1437


#quadratic BEST
dmodpq<-lm(prc$Av_GCLengthB~prc$CMD_sm+ I(prc$CMD_sm^2)) 
summary(dmodpq) 
#Multiple R-squared:  0.07218,	Adjusted R-squared:  0.05659 
#F-statistic: 4.629 on 2 and 119 DF,  p-value: 0.01159
dmodgq<-lm(glc$Av_GCLengthB~glc$CMD_sm+ I(glc$CMD_sm^2)) 
summary(dmodgq)
#Multiple R-squared:  0.02676,	Adjusted R-squared:  0.0212 
#F-statistic: 4.812 on 2 and 350 DF,  p-value: 0.008678



##GCL_T##
##########################################
#bw
cb<-ggplot(dfb, aes(x=dfb$Av_GCLengthT, y = dfb$sm.3, colour=Region))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=c("black", "gray48")) +
  #annotate("text", label="*", x=88.58770541, y=0.02596007, col="black", size=13)+
  #annotate("text", label="*", x=42.87992547, y=0.02192606, col="black", size=13)+
#annotate("text",label="GLAr^2 == 0.09722",x=135,y=0.035,parse=TRUE,size=2,colour="black")+
 # annotate("text",label="PRAr^2 ==  0.08697",x=135,y=0.0354,parse=TRUE,size=2,colour="gray48")+
 # ylab("Adaxial Guard Cell Length")+xlab("Spring Climate Moisture Deficit")+ 
 # geom_vline(xintercept=55,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "top")
cb

#quadratic BEST
cmodp<-lm(prc$Av_GCLengthT~prc$CMD_sp+ I(prc$CMD_sp^2)) 
summary(cmodp) 
#Multiple R-squared:  0.08697,	Adjusted R-squared:  0.07163 
#F-statistic: 5.668 on 2 and 119 DF,  p-value: 0.004454
cmodg<-lm(glc$Av_GCLengthT~glc$CMD_sp+ I(glc$CMD_sp^2)) 
summary(cmodg)
#Multiple R-squared:  0.09722,	Adjusted R-squared:  0.09195 
#F-statistic: 18.47 on 2 and 343 DF,  p-value: 2.414e-08

##Density B
####################################
#bw
cb<-ggplot(dfb, aes(x=dfb$Density_B, y = dfb$sm.3, colour=Region))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=c("black", "gray48")) +
  #annotate("text", label="*", x=85.09458, y=277.30348, col="black", size=13)+
 # annotate("text", label="*", x=42.81667, y=443.14718, col="black", size=13)+
#  annotate("text",label="GLAr^2 == 0.06485",x=130,y=435,parse=TRUE,size=2,colour="black")+
#  annotate("text",label="PRAr^2 ==  0.2094",x=130,y=450,parse=TRUE,size=2,colour="gray48")+
#  ylab("Abaxial Stomatal Density")+xlab("Spring Climate Moisture Deficit")+ 
#  geom_vline(xintercept=55,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "top")
cb

#quadratic BEST
cmodp<-lm(prc$Density_B~prc$CMD_sp+ I(prc$CMD_sp^2)) 
summary(cmodp) 
#Multiple R-squared:  0.2094,	Adjusted R-squared:  0.1962 
#F-statistic: 15.89 on 2 and 120 DF,  p-value: 7.562e-07
cmodg<-lm(glc$Density_B~glc$CMD_sp+ I(glc$CMD_sp^2)) 
summary(cmodg)
#Multiple R-squared:  0.06485,	Adjusted R-squared:  0.05955 
#F-statistic: 12.24 on 2 and 353 DF,  p-value: 7.26e-06


##Density T
####################################
#bw
cb<-ggplot(dfb, aes(x=dfb$Density_T, y = dfb$sm.3, colour=Region))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=c("black", "gray48")) +
  #annotate("text", label="*", x=82.73791, y=222.06189, col="black", size=13)+
  #annotate("text", label="*", x=43.26434, y=255.90579, col="black", size=13)+
  #annotate("text",label="GLAr^2 == 0.06759",x=130,y=435,parse=TRUE,size=2,colour="black")+
  #annotate("text",label="PRAr^2 == 0.1805",x=130,y=420,parse=TRUE,size=2,colour="gray48")+
 # ylab("Adaxial Stomatal Density")+xlab("Spring Climate Moisture Deficit")+ 
 # geom_vline(xintercept=55,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "top")
cb

#quadratic BEST
cmodp<-lm(prc$Density_T~prc$CMD_sp+ I(prc$CMD_sp^2)) 
summary(cmodp) 
#Multiple R-squared:  0.1805,	Adjusted R-squared:  0.1669 
#F-statistic: 13.22 on 2 and 120 DF,  p-value: 6.491e-06
cmodg<-lm(glc$Density_T~glc$CMD_sp+ I(glc$CMD_sp^2)) 
summary(cmodg)
#Multiple R-squared:  0.06759,	Adjusted R-squared:  0.0622 
#F-statistic: 12.54 on 2 and 346 DF,  p-value: 5.521e-06

##SAI_B
####################################
#bw
cb<-ggplot(dfb, aes(x=dfb$SAI_B, y = dfb$sm.3, colour=Region))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=c("black", "gray48"))+
  #annotate("text", label="*", x=85.972450, y=7.155827, col="black", size=13)+
  #annotate("text", label="*", x=42.35001, y=11.21488, col="black", size=13)+
  #annotate("text",label="GLAr^2 == 0.04422",x=130,y=11.7,parse=TRUE,size=2,colour="black")+
  #annotate("text",label="PRAr^2 ==  0.2051",x=130,y=11.5,parse=TRUE,size=2,colour="gray48")+
  #ylab("Abaxial Area Index")+xlab("Spring Climate Moisture Deficit")+ 
  #geom_vline(xintercept=55,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "top")
cb

#quadratic BEST
cmodp<-lm(prc$SAI_B~prc$CMD_sp+ I(prc$CMD_sp^2)) 
summary(cmodp) 
#Multiple R-squared:  0.2051,	Adjusted R-squared:  0.1918 
#F-statistic: 15.36 on 2 and 119 DF,  p-value: 1.168e-06
cmodg<-lm(glc$SAI_B~glc$CMD_sp+ I(glc$CMD_sp^2)) 
summary(cmodg)
#Multiple R-squared:  0.04422,	Adjusted R-squared:  0.03876 
#F-statistic: 8.096 on 2 and 350 DF,  p-value: 0.0003653


##SAI_T
####################################
#bw
cb<-ggplot(dfb, aes(x=dfb$SAI_T, y = sm.3, colour=Region))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=c("black", "gray48"))+
  #annotate("text", label="*", x=83.849163, y=5.694438, col="black", size=13)+
  #annotate("text", label="*", x=6.028046, y=42.424627, col="black", size=13)+
 #annotate("text",label="GLAr^2 == 0.04016",x=130,y=9,parse=TRUE,size=2,colour="black")+
  #annotate("text",label="PRAr^2 ==  0.1504",x=130,y=8.8,parse=TRUE,size=2,colour="gray48")+
 # ylab("Adaxial Area Index")+xlab("Spring Climate Moisture Deficit")+ 
#  geom_vline(xintercept=55,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "top")
cb

#quadratic BEST
cmodp<-lm(prc$SAI_T~prc$CMD_sp+ I(prc$CMD_sp^2)) 
summary(cmodp) 
#Multiple R-squared:  0.1504,	Adjusted R-squared:  0.1361 
#F-statistic: 10.54 on 2 and 119 DF,  p-value: 6.13e-05
cmodg<-lm(glc$SAI_T~glc$CMD_sp+ I(glc$CMD_sp^2)) 
summary(cmodg)
#Multiple R-squared:  0.04016,	Adjusted R-squared:  0.03456 
#F-statistic: 7.175 on 2 and 343 DF,  p-value: 0.0008861


