setwd("C:/Users/eckat/Desktop")
stoma <- read.csv("GeumStomata_V9.csv")
iso<-read.csv("Refined_Isotope.csv")
clim <- read.csv("all_climate_data.csv")
library(dplyr)
df <-iso %>% dplyr::inner_join(clim, by=c("Population"))
library(ggplot2)
col.kat <- c("darkslategray3", "gold3")
col.bw<- c("black" , "gray52")
#
#b<-read.csv("bluestem.csv")
#remove manitoba
dfa<-subset(df, subset=Region.x=='Prairie' | Region.x=='GL Alvar')
#-----------------------------------------------------------------
#isotope
#-----------------------------------------------------------------
#climate-moisture deficit for spring----------------------------------------------------
c<-ggplot(dfa, aes(x=dfa$CMD_sp, y=dfa$dC13, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2)+theme_classic()+
  scale_colour_manual(values=col.kat) +
  annotate("text",label="GLAr^2 == 0.01917",x=135,y=-27.2,parse=TRUE,size=2,colour="darkslategray3")+
  annotate("text",label="PRAr^2 == 0.03289",x=135,y=-27.4,parse=TRUE,size=2,colour="gold3")+
  ylab("dC13")+xlab("Spring Climate Moisture Deficit")+ 
  geom_vline(xintercept=55,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
c
#bw
cb<-ggplot(dfa, aes(x=dfa$CMD_sp, y=dfa$dC13, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2)+theme_classic()+
  scale_colour_manual(values=c("black", "gray48")) +
  annotate("text",label="GLAr^2 == 0.01917",x=135,y=-27.2,parse=TRUE,size=2,colour="black")+
  annotate("text",label="PRAr^2 == 0.03289",x=135,y=-27.4,parse=TRUE,size=2,colour="gray48")+
  ylab("dC13")+xlab("Spring Climate Moisture Deficit")+ 
  geom_vline(xintercept=55,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
cb

cmodp<-lm(prc$dC13~prc$CMD_sp) 
summary(cmodp) 
#Multiple R-squared:  0.03289,	Adjusted R-squared:  -0.0004569 
#F-statistic: 0.9863 on 1 and 29 DF,  p-value: 0.3289
cmodg<-lm(glc$dC13~glc$CMD_sp) 
summary(cmodg)
#Multiple R-squared:  0.01917,	Adjusted R-squared:  0.0003037 
#F-statistic: 1.016 on 1 and 52 DF,  p-value: 0.3181

#climate-moisture deficit for summer----------------------------------------------------
#color
d<-ggplot(dfa, aes(x=dfa$CMD_sm, y=dfa$dC13, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2)+theme_classic()+
  scale_colour_manual(values=col.kat) +
  annotate("text",label="GLAr^2 == 0.1035",x=400,y=-27.2,parse=TRUE,size=2,colour="darkslategray3")+
  annotate("text",label="PRAr^2 == 0.1617",x=400,y=-27.4,parse=TRUE,size=2,colour="gold3")+
  ylab("dC13")+xlab("Summer Climate Moisture Deficit")+ 
  geom_vline(xintercept=155,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
d
#b&w
dw<-ggplot(dfa, aes(x=dfa$CMD_sm, y=dfa$dC13, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2)+theme_classic()+
  scale_colour_manual(values=c("black", "gray48")) +
  annotate("text",label="GLAr^2 == 0.1035",x=400,y=-27.2,parse=TRUE,size=2,colour="black")+
  annotate("text",label="PRAr^2 == 0.1617",x=400,y=-27.4,parse=TRUE,size=2,colour="gray48")+
  ylab("dC13")+xlab("Summer Climate Moisture Deficit")+ 
  geom_vline(xintercept=155,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
dw

dmodp<-lm(prc$dC13~prc$CMD_sm) 
summary(dmodp) 
#Multiple R-squared:  0.1617,	Adjusted R-squared:  0.1328 
#F-statistic: 5.596 on 1 and 29 DF,  p-value: 0.02491
dmodg<-lm(glc$dC13~glc$CMD_sm) 
summary(dmodg)
#Multiple R-squared:  0.1035,	Adjusted R-squared:  0.08629 
#F-statistic: 6.005 on 1 and 52 DF,  p-value: 0.01767

#spring mean maximum temperature (°C)---------------------------------------------------
h<-ggplot(dfa, aes(x=dfa$Tmax_sp, y=dfa$dC13, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2)+theme_classic()+
  scale_colour_manual(values=col.kat) +
  annotate("text",label="GLAr^2 == 0.1225",x=16,y=-27.2,parse=TRUE,size=2,colour="darkslategray3")+
  annotate("text",label="PRAr^2 == 0.0081",x=16,y=-27.4,parse=TRUE,size=2,colour="gold3")+
  ylab("dC13")+xlab("Spring Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=13.7,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")

h
#bw
hb<-ggplot(dfa, aes(x=dfa$Tmax_sp, y=dfa$dC13, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2)+theme_classic()+
  scale_colour_manual(values=c("black", "gray48")) +
  annotate("text",label="GLAr^2 == 0.1225",x=16,y=-27.2,parse=TRUE,size=2,colour="black")+
  annotate("text",label="PRAr^2 == 0.0081",x=16,y=-27.4,parse=TRUE,size=2,colour="gray48")+
  ylab("dC13")+xlab("Spring Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=13.7,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")

hb

hmodp<-lm(prc$dC13~prc$Tmax_sp) 
summary(hmodp) 
#Multiple R-squared:  0.008075,	Adjusted R-squared:  -0.02613 
#F-statistic: 0.2361 on 1 and 29 DF,  p-value: 0.6307
hmodg<-lm(glc$dC13~glc$Tmax_sp) 
summary(hmodg)
#Multiple R-squared:  0.1225,	Adjusted R-squared:  0.1056 
#F-statistic: 7.261 on 1 and 52 DF,  p-value: 0.009468

#summer mean maximum temperature (°C)----------------------------------------------------
i<-ggplot(dfa, aes(x=dfa$Tmax_sm, y=dfa$dC13, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2)+theme_classic()+
  scale_colour_manual(values=col.kat) +
  annotate("text",label="GLAr^2 == 0.09879",x=28.5,y=-27.2,parse=TRUE,size=2,colour="darkslategray3")+
  annotate("text",label="PRAr^2 == 0.00042",x=28.5,y=-27.4,parse=TRUE,size=2,colour="gold3")+
  ylab("dC13")+xlab("Summer Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=27.5,col="gray60",lwd=0.75,lty=2)+theme(legend.position = "none")
                                        
i
#bw
ib<-ggplot(dfa, aes(x=dfa$Tmax_sm, y=dfa$dC13, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2)+theme_classic()+
  scale_colour_manual(values=c("black", "gray48")) +
  annotate("text",label="GLAr^2 == 0.09879",x=28.5,y=-27.2,parse=TRUE,size=2,colour="black")+
  annotate("text",label="PRAr^2 == 0.00042",x=28.5,y=-27.4,parse=TRUE,size=2,colour="gray48")+
  ylab("dC13")+xlab("Summer Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=27.5,col="gray60",lwd=0.75,lty=2)+theme(legend.position = "none")

ib

imodp<-lm(prc$dC13~prc$Tmax_sm) 
summary(imodp) 
#Multiple R-squared:  0.0004289,	Adjusted R-squared:  -0.03404 
#F-statistic: 0.01244 on 1 and 29 DF,  p-value: 0.9119
imodg<-lm(glc$dC13~glc$Tmax_sm) 
summary(imodg)
#Multiple R-squared:  0.09879,	Adjusted R-squared:  0.08146 
#F-statistic:   5.7 on 1 and 52 DF,  p-value: 0.02063

#....................................................................................
#....................................................................................

#load stomata-isotope
stoma <- read.csv("GeumStomata_V9.csv")
df1 <-stoma %>% dplyr::inner_join(clim, by=c("Population"))
dfb<-subset(df1, subset=Region.x=='Prairie' | Region.x=='GL Alvar')
library(ggpubr)
col <- ggarrange(c, d, h, i,labels = c("A", "B", "C", "D"),ncol=2,nrow=2)
col

bw<-ggarrange(cb,dw,hb,ib, labels = c("A", "B", "C", "D"),ncol=2,nrow=2)
bw
#subset data to get r2
prc <- subset(dfb, Region.x == "Prairie")
glc <- subset(dfb, Region.x =="GL Alvar")

library(ggplot2)
#----------------------------------
#GCL_B
#----------------------------------
#climate-moisture deficit for spring--------------------------------------------------
c<-ggplot(dfb, aes(x=dfb$CMD_sp, y=dfb$Av_GCLengthB, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=col.kat) +
  #annotate("text",label="GLAr^2 == 0.03704",x=130,y=0.035,parse=TRUE,size=2,colour="darkslategray3")+
  #annotate("text",label="PRAr^2 == 0.06628",x=130,y=0.0357,parse=TRUE,size=2,colour="gold3")+
  annotate("text", label="*", x=88.65124456, y=0.02607884, col="black", size=13)+
  annotate("text", label="*", x=40.18813232, y=0.02500193, col="black", size=13)+
  ylab("Abaxial Guard Cell Length")+xlab("Spring Climate Moisture Deficit")+ 
  geom_vline(xintercept=55,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
c

#bw
cb<-ggplot(dfb, aes(x=dfb$CMD_sp, y=dfb$Av_GCLengthB, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=c("black", "gray48")) +
  #annotate("text",label="GLAr^2 == 0.03704",x=130,y=0.035,parse=TRUE,size=2,colour="black")+
  #annotate("text",label="PRAr^2 == 0.06628",x=130,y=0.0357,parse=TRUE,size=2,colour="gray48")+ annotate("text", label="*", x=88.65124456, y=0.02607884, col="black", size=13)+
  annotate("text", label="*", x=40.18813232, y=0.02500193, col="black", size=13)+
  ylab("Abaxial Guard Cell Length")+xlab("Spring Climate Moisture Deficit")+ 
  geom_vline(xintercept=55,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
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

#climate-moisture deficit for summer-------------------------------------------------
#color
d<-ggplot(dfb, aes(x=dfb$CMD_sm, y=dfb$Av_GCLengthB, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=col.kat) +
  #annotate("text",label="GLAr^2 == 0.02676",x=400,y=0.035,parse=TRUE,size=2,colour="darkslategray3")+
  #annotate("text",label="PRAr^2 == 0.07218",x=400,y=0.0357,parse=TRUE,size=2,colour="gold3")+ annotate("text", label="*", x=88.65124456, y=0.02607884, col="black", size=13)+
  annotate("text", label="*", x=157.953207, y=0.026614, col="black", size=13)+
  annotate("text", label="*", x=9.543577e+01, y=6.122797e-04, col="black", size=13)+
  ylab("Abaxial Guard Cell Length")+xlab("Summer Climate Moisture Deficit")+ 
  geom_vline(xintercept=155,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
d
#b&w
dw<-ggplot(dfb, aes(x=dfb$CMD_sm, y=dfb$Av_GCLengthB, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=c("black", "gray48")) +
  #annotate("text",label="GLAr^2 == 0.02676",x=400,y=0.035,parse=TRUE,size=2,colour="black")+
  #annotate("text",label="PRAr^2 == 0.07218",x=400,y=0.0357,parse=TRUE,size=2,colour="gray48")+
  annotate("text", label="*", x=157.953207, y=0.026614, col="black", size=13)+
  annotate("text", label="*", x=9.543577e+01, y=6.122797e-04, col="black", size=13)+
  ylab("Abaxial Guard Cell Length")+xlab("Summer Climate Moisture Deficit")+ 
  geom_vline(xintercept=155,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
dw

#quadratic BEST
dmodpq<-lm(prc$Av_GCLengthB~prc$CMD_sm+ I(prc$CMD_sm^2)) 
summary(dmodpq) 
#Multiple R-squared:  0.07218,	Adjusted R-squared:  0.05659 
#F-statistic: 4.629 on 2 and 119 DF,  p-value: 0.01159
dmodgq<-lm(glc$Av_GCLengthB~glc$CMD_sm+ I(glc$CMD_sm^2)) 
summary(dmodgq)
#Multiple R-squared:  0.02676,	Adjusted R-squared:  0.0212 
#F-statistic: 4.812 on 2 and 350 DF,  p-value: 0.008678

#linear
#dmodp<-lm(prc$Av_GCLengthB~prc$CMD_sm) 
#summary(dmodp) 
#Multiple R-squared:  0.03908,	Adjusted R-squared:  0.03107 
#F-statistic:  4.88 on 1 and 120 DF,  p-value: 0.02907
#dmodg<-lm(glc$Av_GCLengthB~glc$CMD_sm) 
#summary(dmodg)
#Multiple R-squared:  0.02675,	Adjusted R-squared:  0.02397 
#F-statistic: 9.646 on 1 and 351 DF,  p-value: 0.002052

#spring mean maximum temperature (°C)----------------------------------------------------
h<-ggplot(dfb, aes(x=dfb$Tmax_sp, y=dfb$Av_GCLengthB, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=col.kat) +
  #annotate("text", label="*", x=13.72070447, y=0.01339613, col="black", size=13)+
  #annotate("text", label="*", x=7.95900851, y=0.06304594, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.01109",x=16,y=0.035,parse=TRUE,size=2, colour="darkslategray3")+
  annotate("text",label="PRAr^2 == 0.06653",x=16,y=0.0357,parse=TRUE,size=2, colour="gold3")+
  ylab("Abaxial Guard Cell Length")+xlab("Spring Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=13.7,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")

h

#bw
hb<-ggplot(dfb, aes(x=dfb$Tmax_sp, y=dfb$Av_GCLengthB, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=c("black", "gray48")) + 
  #annotate("text", label="*", x=13.72070447, y=0.01339613, col="black", size=13)+
  #annotate("text", label="*", x=7.95900851, y=0.06304594, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.01109",x=16,y=0.035,parse=TRUE,size=2, colour="black")+
  annotate("text",label="PRAr^2 == 0.06653",x=16,y=0.0357,parse=TRUE,size=2, colour="gray48")+
  ylab("Abaxial Guard Cell Length")+xlab("Spring Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=13.7,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")

hb

#quadratic BEST
hmodpq<-lm(prc$Av_GCLengthB~prc$Tmax_sp+ I(prc$Tmax_sp^2)) 
summary(hmodpq) 
#Multiple R-squared:  0.06653,	Adjusted R-squared:  0.05084 
#F-statistic: 4.241 on 2 and 119 DF,  p-value: 0.01664
hmodgq<-lm(glc$Av_GCLengthB~glc$Tmax_sp+ I(glc$Tmax_sp^2)) 
summary(hmodgq)
#Multiple R-squared:  0.01109,	Adjusted R-squared:  0.005436 
#F-statistic: 1.962 on 2 and 350 DF,  p-value: 0.1421

#linear
#hmodp<-lm(prc$Av_GCLengthB~prc$Tmax_sp) 
#summary(hmodp) 
#Multiple R-squared:  0.02772,	Adjusted R-squared:  0.01962 
#F-statistic: 3.422 on 1 and 120 DF,  p-value: 0.0668
#hmodg<-lm(glc$Av_GCLengthB~glc$Tmax_sp) 
#summary(hmodg)
#Multiple R-squared:  0.0106,	Adjusted R-squared:  0.007782 
#F-statistic: 3.761 on 1 and 351 DF,  p-value: 0.05327

#summer mean maximum temperature (°C)--------------------------------------------------
i<-ggplot(dfb, aes(x=dfb$Tmax_sm, y=dfb$Av_GCLengthB, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=col.kat) +
  #annotate("text", label="*", x=27.36456423, y=0.02985947, col="black", size=13)+
  #annotate("text", label="*", x=23.82605697, y=0.02051722, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.008851",x=28.5,y=0.034,parse=TRUE,size=2, colour="darkslategray3")+
  annotate("text",label="PRAr^2 == 0.06959",x=28.5,y=0.0344,parse=TRUE,size=2,colour="gold3")+
  ylab("Abaxial Guard Cell Length")+xlab("Summer Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=27.5,col="gray60",lwd=0.75,lty=2)+theme(legend.position = "none")

i
#bw
ib<-ggplot(dfb, aes(x=dfb$Tmax_sm, y=dfb$Av_GCLengthB, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=c("black", "gray48")) + 
  #annotate("text", label="*", x=27.36456423, y=0.02985947, col="black", size=13)+
  #annotate("text", label="*", x=23.82605697, y=0.02051722, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.008851",x=28.5,y=0.034,parse=TRUE,size=2, colour="black")+
  annotate("text",label="PRAr^2 == 0.06959",x=28.5,y=0.0344,parse=TRUE,size=2, colour="gray48")+
  ylab("Abaxial Guard Cell Length")+xlab("Summer Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=27.5,col="gray60",lwd=0.75,lty=2)+theme(legend.position = "none")

ib

#quadratic BEST
imodpq<-lm(prc$Av_GCLengthB~prc$Tmax_sm+ I(prc$Tmax_sm^2)) 
summary(imodpq) 
#Multiple R-squared:  0.06959,	Adjusted R-squared:  0.05395 
#F-statistic:  4.45 on 2 and 119 DF,  p-value: 0.01369
imodgq<-lm(glc$Av_GCLengthB~glc$Tmax_sm+ I(glc$Tmax_sm^2)) 
summary(imodgq)
#Multiple R-squared:  0.008851,	Adjusted R-squared:  0.003188 
#F-statistic: 1.563 on 2 and 350 DF,  p-value: 0.211

#linear
#imodp<-lm(prc$Av_GCLengthB~prc$Tmax_sm) 
#summary(imodp) 
#Multiple R-squared:  0.007659,	Adjusted R-squared:  -0.0006103 
#F-statistic: 0.9262 on 1 and 120 DF,  p-value: 0.3378
#imodg<-lm(glc$Av_GCLengthB~glc$Tmax_sm) 
#summary(imodg)
#Multiple R-squared:  0.004905,	Adjusted R-squared:  0.00207 
#F-statistic:  1.73 on 1 and 351 DF,  p-value: 0.1893

col <- ggarrange(c, d, h, i,labels = c("A", "B", "C", "D"),ncol=2,nrow=2)
col

bw<-ggarrange(cb,dw,hb,ib, labels = c("A", "B", "C", "D"),ncol=2,nrow=2)
bw

##----------------------------------------
#GCL_T
#-----------------------------------------
#climate-moisture deficit for spring-------------------------------------------
c<-ggplot(dfb, aes(x=dfb$CMD_sp, y=dfb$Av_GCLengthT, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=col.kat) +
  #annotate("text", label="*", x=88.58770541, y=0.02596007, col="black", size=13)+
  #annotate("text", label="*", x=42.87992547, y=0.02192606, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.09722",x=135,y=0.035,parse=TRUE,size=2,colour="darkslategray3")+
  annotate("text",label="PRAr^2 ==  0.08697",x=135,y=0.0354,parse=TRUE,size=2,colour="gold3")+
  ylab("Adaxial Guard Cell Length")+xlab("Spring Climate Moisture Deficit")+ 
  geom_vline(xintercept=55,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
c
#bw
cb<-ggplot(dfb, aes(x=dfb$CMD_sp, y=dfb$Av_GCLengthT, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=c("black", "gray48")) +
  #annotate("text", label="*", x=88.58770541, y=0.02596007, col="black", size=13)+
  #annotate("text", label="*", x=42.87992547, y=0.02192606, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.09722",x=135,y=0.035,parse=TRUE,size=2,colour="black")+
  annotate("text",label="PRAr^2 ==  0.08697",x=135,y=0.0354,parse=TRUE,size=2,colour="gray48")+
  ylab("Adaxial Guard Cell Length")+xlab("Spring Climate Moisture Deficit")+ 
  geom_vline(xintercept=55,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
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

#linear
#cmodpl<-lm(prc$Av_GCLengthT~prc$CMD_sp) 
#summary(cmodpl) 
#Multiple R-squared:  0.03719,	Adjusted R-squared:  0.02916 
#F-statistic: 4.635 on 1 and 120 DF,  p-value: 0.03333
#cmodgl<-lm(glc$Av_GCLengthT~glc$CMD_sp) 
#summary(cmodgl) 
#Multiple R-squared:  0.03325,	Adjusted R-squared:  0.03044 
#F-statistic: 11.83 on 1 and 344 DF,  p-value: 0.0006537
#c<-ggplot(dfb, aes(x=dfb$CMD_sp, y=dfb$Av_GCLengthT, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2)+theme_classic()+
  scale_colour_manual(values=col.kat) +
  annotate("text",label="GLAr^2 == 0.03325",x=135,y=0.035,parse=TRUE,size=2,colour="darkslategray3")+
  annotate("text",label="PRAr^2 ==  0.03719",x=135,y=0.0354,parse=TRUE,size=2,colour="gold3")+
  ylab("Adaxial Guard Cell Length")+xlab("Spring Climate Moisture Deficit")+ 
  geom_vline(xintercept=55,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
#c
  
#climate-moisture deficit for summer-------------------------------------------
#color
d<-ggplot(dfb, aes(x=dfb$CMD_sm, y=dfb$Av_GCLengthT, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=col.kat) + 
  #annotate("text", label="*", x=153.65064803, y=0.02621779, col="black", size=13)+
  #annotate("text", label="*", x=4969.650706, y=2.564034, col="black", size=13)+
  annotate("text",label="GLAr^2 ==  0.0677",x=400,y=0.035,parse=TRUE,size=2,colour="darkslategray3")+
  annotate("text",label="PRAr^2 == 0.08849",x=400,y=0.0354,parse=TRUE,size=2,colour="gold3")+
  ylab("Adaxial Guard Cell Length")+xlab("Summer Climate Moisture Deficit")+ 
  geom_vline(xintercept=155,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
d
#b&w
dw<-ggplot(dfb, aes(x=dfb$CMD_sm, y=dfb$Av_GCLengthT, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=c("black", "gray48")) +
  #annotate("text", label="*", x=153.65064803, y=0.02621779, col="black", size=13)+
  #annotate("text", label="*", x=4969.650706, y=2.564034, col="black", size=13)+
  annotate("text",label="GLAr^2 ==  0.0677",x=400,y=0.035,parse=TRUE,size=2,colour="black")+
  annotate("text",label="PRAr^2 == 0.08849",x=400,y=0.0354,parse=TRUE,size=2,colour="gray48")+
  ylab("Adaxial Guard Cell Length")+xlab("Summer Climate Moisture Deficit")+ 
  geom_vline(xintercept=155,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
dw

#quadratic BEST
dmodpq<-lm(prc$Av_GCLengthT~prc$CMD_sm+ I(prc$CMD_sm^2)) 
summary(dmodpq)
#Multiple R-squared:  0.08849,	Adjusted R-squared:  0.07317 
#F-statistic: 5.776 on 2 and 119 DF,  p-value: 0.004035
dmodgq<-lm(glc$Av_GCLengthT~glc$CMD_sm+ I(glc$CMD_sm^2)) 
summary(dmodgq)
#Multiple R-squared:  0.0677,	Adjusted R-squared:  0.06226 
#F-statistic: 12.45 on 2 and 343 DF,  p-value: 6.011e-06

#linear
#dmodp<-lm(prc$Av_GCLengthT~prc$CMD_sm) 
#summary(dmodp) 
#Multiple R-squared:  0.04703,	Adjusted R-squared:  0.03909 
#F-statistic: 5.923 on 1 and 120 DF,  p-value: 0.01642
#dmodg<-lm(glc$Av_GCLengthT~glc$CMD_sm) 
#summary(dmodg)
#Multiple R-squared:  0.06124,	Adjusted R-squared:  0.05851 
#F-statistic: 22.44 on 1 and 344 DF,  p-value: 3.178e-06
d<-ggplot(dfb, aes(x=dfb$CMD_sm, y=dfb$Av_GCLengthT, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2)+theme_classic()+
  scale_colour_manual(values=col.kat) +
  annotate("text",label="GLAr^2 ==  0.04703",x=400,y=0.035,parse=TRUE,size=2,colour="darkslategray3")+
  annotate("text",label="PRAr^2 == 0.06124",x=400,y=0.0354,parse=TRUE,size=2,colour="gold3")+
  ylab("Adaxial Guard Cell Length")+xlab("Summer Climate Moisture Deficit")+ 
  geom_vline(xintercept=155,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
d
#spring mean maximum temperature (°C)-----------------------------------------------
h<-ggplot(dfb, aes(x=dfb$Tmax_sp, y=dfb$Av_GCLengthT, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=col.kat) +
  #annotate("text", label="*", x=12.11180500, y=-0.01467967, col="black", size=13)+
  #annotate("text", label="*", x=9.24224967, y=0.03538059, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.03368",x=16,y=0.035,parse=TRUE,size=2, colour="darkslategray3")+
  annotate("text",label="PRAr^2 == 0.09844",x=16,y=0.0354,parse=TRUE,size=2, colour="gold3")+
  ylab("Adaxial Guard Cell Length")+xlab("Spring Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=13.7,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")

h

#bw
hb<-ggplot(dfb, aes(x=dfb$Tmax_sp, y=dfb$Av_GCLengthT, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=c("black", "gray48")) +
  #annotate("text", label="*", x=12.11180500, y=-0.01467967, col="black", size=13)+
  #annotate("text", label="*", x=9.24224967, y=0.03538059, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.03368",x=16,y=0.035,parse=TRUE,size=2, colour="black")+
  annotate("text",label="PRAr^2 == 0.09844",x=16,y=0.0354,parse=TRUE,size=2, colour="gray48")+
  ylab("Adaxial Guard Cell Length")+xlab("Spring Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=13.7,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")

hb

#quadratic BEST
hmodpq<-lm(prc$Av_GCLengthT~prc$Tmax_sp+ I(prc$Tmax_sp^2)) 
summary(hmodpq)
#Multiple R-squared:  0.09844,	Adjusted R-squared:  0.08329 
#F-statistic: 6.497 on 2 and 119 DF,  p-value: 0.002099
hmodgq<-lm(glc$Av_GCLengthT~glc$Tmax_sp+ I(glc$Tmax_sp^2)) 
summary(hmodgq)
#Multiple R-squared:  0.03368,	Adjusted R-squared:  0.02805 
#F-statistic: 5.978 on 2 and 343 DF,  p-value: 0.002806

#linear
#hmodp<-lm(prc$Av_GCLengthT~prc$Tmax_sp) 
#summary(hmodp) 
#Multiple R-squared:  0.02692,	Adjusted R-squared:  0.01881 
#F-statistic:  3.32 on 1 and 120 DF,  p-value: 0.07094
#hmodg<-lm(glc$Av_GCLengthT~glc$Tmax_sp) 
#summary(hmodg)
#Multiple R-squared:  0.02449,	Adjusted R-squared:  0.02166 
#F-statistic: 8.637 on 1 and 344 DF,  p-value: 0.003517
#h<-ggplot(dfb, aes(x=dfb$Tmax_sp, y=dfb$Av_GCLengthT, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2)+theme_classic()+
  scale_colour_manual(values=col.kat) +
  annotate("text",label="GLAr^2 == 0.02449",x=16,y=0.035,parse=TRUE,size=2, colour="darkslategray3")+
  annotate("text",label="PRAr^2 == 0.02692",x=16,y=0.0354,parse=TRUE,size=2, colour="gold3")+
  ylab("Adaxial Guard Cell Length")+xlab("Spring Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=13.7,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")

#h
#summer mean maximum temperature (°C)-----------------------------------------------
i<-ggplot(dfb, aes(x=dfb$Tmax_sm, y=dfb$Av_GCLengthT, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=col.kat) +
  #annotate("text", label="*", x=27.36139673, y=-0.02939387, col="black", size=13)+
  #annotate("text", label="*", x=22.06674900, y=0.06889961, col="black", size=13)+
  annotate("text",label="GLAr^2 ==  0.02345",x=28.5,y=0.035,parse=TRUE,size=2, colour="darkslategray3")+
  annotate("text",label="PRAr^2 ==  0.08022",x=28.5,y=0.0354,parse=TRUE,size=2,colour="gold3")+
  ylab("Adaxial Guard Cell Length")+xlab("Summer Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=27.5,col="gray60",lwd=0.75,lty=2)+theme(legend.position = "none")

i
#bw
ib<-ggplot(dfb, aes(x=dfb$Tmax_sm, y=dfb$Av_GCLengthT, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=c("black", "gray48"))+
  #annotate("text", label="*", x=27.36139673, y=-0.02939387, col="black", size=13)+
  #annotate("text", label="*", x=22.06674900, y=0.06889961, col="black", size=13)+
  annotate("text",label="GLAr^2 ==  0.02345",x=28.5,y=0.035,parse=TRUE,size=2, colour="black")+
  annotate("text",label="PRAr^2 ==  0.08022",x=28.5,y=0.0354,parse=TRUE,size=2, colour="gray48")+
  ylab("Adaxial Guard Cell Length")+xlab("Summer Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=27.5,col="gray60",lwd=0.75,lty=2)+theme(legend.position = "none")

ib

#quadratic BEST
imodpq<-lm(prc$Av_GCLengthT~prc$Tmax_sm+ I(prc$Tmax_sm^2)) 
summary(imodpq)
#Multiple R-squared:  0.08022,	Adjusted R-squared:  0.06476 
#F-statistic: 5.189 on 2 and 119 DF,  p-value: 0.006905
imodgq<-lm(glc$Av_GCLengthT~glc$Tmax_sm+ I(glc$Tmax_sm^2)) 
summary(imodgq)
#Multiple R-squared:  0.02345,	Adjusted R-squared:  0.01776 
#F-statistic: 4.119 on 2 and 343 DF,  p-value: 0.01707

#linear
#imodp<-lm(prc$Av_GCLengthT~prc$Tmax_sm) 
#summary(imodp) 
#Multiple R-squared:  0.007585,	Adjusted R-squared:  -0.000685 
#F-statistic: 0.9172 on 1 and 120 DF,  p-value: 0.3401
#imodg<-lm(glc$Av_GCLengthT~glc$Tmax_sm) 
#summary(imodg)
#Multiple R-squared:  0.02014,	Adjusted R-squared:  0.0173 
#F-statistic: 7.072 on 1 and 344 DF,  p-value: 0.008197
#i<-ggplot(dfb, aes(x=dfb$Tmax_sm, y=dfb$Av_GCLengthT, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2)+theme_classic()+
  scale_colour_manual(values=col.kat) +
  annotate("text",label="GLAr^2 ==  0.02014",x=28.5,y=0.035,parse=TRUE,size=2, colour="darkslategray3")+
  annotate("text",label="PRAr^2 ==  0.007585",x=28.5,y=0.0354,parse=TRUE,size=2,colour="gold3")+
  ylab("Adaxial Guard Cell Length")+xlab("Summer Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=27.5,col="gray60",lwd=0.75,lty=2)+theme(legend.position = "none")

#i
col <- ggarrange(c, d, h, i,labels = c("A", "B", "C", "D"),ncol=2,nrow=2)
col

bw<-ggarrange(cb,dw,hb,ib, labels = c("A", "B", "C", "D"),ncol=2,nrow=2)
bw
#-----------------------------------------------------------------------------------
#--------------------------------------------------------
#Density B
#--------------------------------------------------------
#climate-moisture deficit for spring-------------------------------------------------
c<-ggplot(dfb, aes(x=dfb$CMD_sp, y=dfb$Density_B, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=col.kat)+
  #annotate("text", label="*", x=85.09458, y=277.30348, col="black", size=13)+
  #annotate("text", label="*", x=42.81667, y=443.14718, col="black", size=13)+
  annotate("text",label="GLAr^2 ==  0.06485",x=130,y=435,parse=TRUE,size=2,colour="darkslategray3")+
  annotate("text",label="PRAr^2 ==  0.2094",x=130,y=450,parse=TRUE,size=2,colour="gold3")+
  ylab("Abaxial Stomatal Density")+xlab("Spring Climate Moisture Deficit")+ 
  geom_vline(xintercept=55,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
c

#bw
cb<-ggplot(dfb, aes(x=dfb$CMD_sp, y=dfb$Density_B, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=c("black", "gray48")) +
  #annotate("text", label="*", x=85.09458, y=277.30348, col="black", size=13)+
 # annotate("text", label="*", x=42.81667, y=443.14718, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.06485",x=130,y=435,parse=TRUE,size=2,colour="black")+
  annotate("text",label="PRAr^2 ==  0.2094",x=130,y=450,parse=TRUE,size=2,colour="gray48")+
  ylab("Abaxial Stomatal Density")+xlab("Spring Climate Moisture Deficit")+ 
  geom_vline(xintercept=55,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
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

#linear
#cmodpl<-lm(prc$Density_B~prc$CMD_sp) 
#summary(cmodpl) 
#Multiple R-squared:  0.05781,	Adjusted R-squared:  0.05002 
#F-statistic: 7.424 on 1 and 121 DF,  p-value: 0.007391
#cmodgl<-lm(glc$Density_B~glc$CMD_sp) 
#summary(cmodgl)
#Multiple R-squared:  0.02417,	Adjusted R-squared:  0.02141 
#F-statistic: 8.766 on 1 and 354 DF,  p-value: 0.003275
#c<-ggplot(dfb, aes(x=dfb$CMD_sp, y=dfb$Density_B, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2)+theme_classic()+
  scale_colour_manual(values=col.kat) +
  annotate("text",label="GLAr^2 == 0.02417",x=130,y=435,parse=TRUE,size=2,colour="darkslategray3")+
  annotate("text",label="PRAr^2 == 0.05781",x=130,y=450,parse=TRUE,size=2,colour="gold3")+
  ylab("Abaxial Stomatal Density")+xlab("Spring Climate Moisture Deficit")+ 
  geom_vline(xintercept=55,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
#c
#climate-moisture deficit for summer--------------------------------------------------
#color
d<-ggplot(dfb, aes(x=dfb$CMD_sm, y=dfb$Density_B, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=col.kat) +
  #annotate("text", label="*", x=97.9731, y=329.6663, col="black", size=13)+
  #annotate("text", label="*", x=30.91213, y=2564.14212, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.03798",x=400,y=435,parse=TRUE,size=2,colour="darkslategray3")+
  annotate("text",label="PRAr^2 == 0.2822",x=400,y=450,parse=TRUE,size=2,colour="gold3")+
  ylab("Abaxial Stomatal Density")+xlab("Summer Climate Moisture Deficit")+ 
  geom_vline(xintercept=155,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
d
#b&w quad
dw<-ggplot(dfb, aes(x=dfb$CMD_sm, y=dfb$Density_B, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=c("black", "gray48")) +
  #annotate("text", label="*", x=97.9731, y=329.6663, col="black", size=13)+
  #annotate("text", label="*", x=30.91213, y=2564.14212, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.03798",x=400,y=435,parse=TRUE,size=2,colour="black")+
  annotate("text",label="PRAr^2 == 0.2822",x=400,y=450,parse=TRUE,size=2,colour="gray48")+
  ylab("Abaxial Stomatal Density")+xlab("Summer Climate Moisture Deficit")+ 
  geom_vline(xintercept=155,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
dw

#quadratic BEST
dmodpq<-lm(prc$Density_B~prc$CMD_sm+ I(prc$CMD_sm^2)) 
summary(dmodpq) 
#Multiple R-squared:  0.2822,	Adjusted R-squared:  0.2702 
#F-statistic: 23.59 on 2 and 120 DF,  p-value: 2.292e-09
dmodgq<-lm(glc$Density_B~glc$CMD_sm+ I(glc$CMD_sm^2)) 
summary(dmodgq)
#Multiple R-squared:  0.03798,	Adjusted R-squared:  0.03253 
#F-statistic: 6.969 on 2 and 353 DF,  p-value: 0.001076

#linear
#dmodp<-lm(prc$Density_B~prc$CMD_sm) 
#summary(dmodp) 
#Multiple R-squared:  0.2805,	Adjusted R-squared:  0.2745 
#F-statistic: 47.16 on 1 and 121 DF,  p-value: 3.014e-10
#dmodg<-lm(glc$Density_B~glc$CMD_sm) 
#summary(dmodg)
#Multiple R-squared:  0.03417,	Adjusted R-squared:  0.03144 
#F-statistic: 12.52 on 1 and 354 DF,  p-value: 0.0004551
#d<-ggplot(dfb, aes(x=dfb$CMD_sm, y=dfb$Density_B, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2)+theme_classic()+
  scale_colour_manual(values=col.kat) +
  annotate("text",label="GLAr^2 == 0.03417",x=400,y=435,parse=TRUE,size=2,colour="darkslategray3")+
  annotate("text",label="PRAr^2 == 0.2805",x=400,y=450,parse=TRUE,size=2,colour="gold3")+
  ylab("Abaxial Stomatal Density")+xlab("Summer Climate Moisture Deficit")+ 
  geom_vline(xintercept=155,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
#d

#spring mean maximum temperature (°C)---------------------------------------------------
#color
h<-ggplot(dfb, aes(x=dfb$Tmax_sp, y=dfb$Density_B, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=col.kat) +
  #annotate("text", label="*", x=14.53504, y=181.99229, col="black", size=13)+
  #annotate("text", label="*", x=8.485958, y=-299.205200, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.02924",x=16.5,y=435,parse=TRUE,size=2,colour="darkslategray3")+
  annotate("text",label="PRAr^2 ==  0.2511",x=16.5,y=450,parse=TRUE,size=2,colour="gold3")+
  ylab("Abaxial Stomatal Density")+xlab("Spring Mean Maximum Temp (°C)")+ 
  geom_vline(xintercept=13.7,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
h
#b&w quad
hw<-ggplot(dfb, aes(x=dfb$Tmax_sp, y=dfb$Density_B, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=c("black", "gray48")) +
  #annotate("text", label="*", x=14.53504, y=181.99229, col="black", size=13)+
  #annotate("text", label="*", x=8.485958, y=-299.205200, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.02924",x=16.5,y=435,parse=TRUE,size=2,colour="black")+
  annotate("text",label="PRAr^2 ==  0.2511",x=16.5,y=450,parse=TRUE,size=2,colour="gray48")+
  ylab("Abaxial Stomatal Density")+xlab("Spring Mean Maximum Temp (°C)")+ 
  geom_vline(xintercept=13.7,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
hw

#quadratic BEST
hmodp<-lm(prc$Density_B~prc$Tmax_sp+ I(prc$Tmax_sp^2)) 
summary(hmodp) 
#Multiple R-squared:  0.2511,	Adjusted R-squared:  0.2386 
#F-statistic: 20.11 on 2 and 120 DF,  p-value: 2.929e-08
hmodg<-lm(glc$Density_B~glc$Tmax_sp+ I(glc$Tmax_sp^2)) 
summary(hmodg)
#Multiple R-squared:  0.02924,	Adjusted R-squared:  0.02374 
#F-statistic: 5.317 on 2 and 353 DF,  p-value: 0.005308

#linear
#hmodpl<-lm(prc$Density_B~prc$Tmax_sp) 
#summary(hmodpl) 
#Multiple R-squared:  0.01081,	Adjusted R-squared:  0.002632 
#F-statistic: 1.322 on 1 and 121 DF,  p-value: 0.2525
#hmodgl<-lm(glc$Density_B~glc$Tmax_sp) 
#summary(hmodgl)
#Multiple R-squared:  0.02916,	Adjusted R-squared:  0.02642 
#F-statistic: 10.63 on 1 and 354 DF,  p-value: 0.001219
#h<-ggplot(dfb, aes(x=dfb$Tmax_sp, y=dfb$Density_B, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2)+theme_classic()+
  scale_colour_manual(values=col.kat) +
  annotate("text",label="GLAr^2 == 0.02449",x=16,y=435,parse=TRUE,size=2, colour="darkslategray3")+
  annotate("text",label="PRAr^2 == 0.02692",x=16,y=450,parse=TRUE,size=2, colour="gold3")+
  ylab("Abaxial Stomatal Density")+xlab("Spring Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=13.7,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")

#h
#summer mean maximum temperature (°C)---------------------------------------------------
i<-ggplot(dfb, aes(x=dfb$Tmax_sm, y=dfb$Density_B, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=col.kat) +
  #annotate("text", label="*", x=22.0724, y=-1457.0552, col="black", size=13)+
  #annotate("text", label="*", x=24.57056, y=947.88424, col="black", size=13)+
  annotate("text",label="GLAr^2 ==  0.03275",x=28.5,y=435,parse=TRUE,size=2, colour="darkslategray3")+
  annotate("text",label="PRAr^2 ==  0.2148",x=28.5,y=450,parse=TRUE,size=2,colour="gold3")+
  ylab("Abaxial Stomatal Density")+xlab("Summer Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=27.5,col="gray60",lwd=0.75,lty=2)+theme(legend.position = "none")

i
#bw
ib<-ggplot(dfb, aes(x=dfb$Tmax_sm, y=dfb$Density_B, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=c("black", "gray48")) +
  #annotate("text", label="*", x=22.0724, y=-1457.0552, col="black", size=13)+
  #annotate("text", label="*", x=24.57056, y=947.88424, col="black", size=13)+
  annotate("text",label="GLAr^2 ==  0.03275",x=28.5,y=435,parse=TRUE,size=2, colour="black")+
  annotate("text",label="PRAr^2 == 0.2148",x=28.5,y=450,parse=TRUE,size=2, colour="gray48")+
  ylab("Abaxial Stomatal Density")+xlab("Summer Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=27.5,col="gray60",lwd=0.75,lty=2)+theme(legend.position = "none")

ib
#quadratic BEST
imodp<-lm(prc$Density_B~prc$Tmax_sm+ I(prc$Tmax_sm^2)) 
summary(imodp) 
#Multiple R-squared:  0.2148,	Adjusted R-squared:  0.2017 
#F-statistic: 16.41 on 2 and 120 DF,  p-value: 4.999e-07
imodg<-lm(glc$Density_B~glc$Tmax_sm+ I(glc$Tmax_sm^2)) 
summary(imodg)
#Multiple R-squared:  0.03275,	Adjusted R-squared:  0.02727 
#F-statistic: 5.975 on 2 and 353 DF,  p-value: 0.002805

#linear
#imodpl<-lm(prc$Density_B~prc$Tmax_sm) 
#summary(imodpl) 
#Multiple R-squared:  0.05447,	Adjusted R-squared:  0.04665 
#F-statistic:  6.97 on 1 and 121 DF,  p-value: 0.00938
#imodgl<-lm(glc$Density_B~glc$Tmax_sm) 
#summary(imodgl)
#Multiple R-squared:  0.02235,	Adjusted R-squared:  0.01958 
#F-statistic: 8.091 on 1 and 354 DF,  p-value: 0.004707
#i<-ggplot(dfb, aes(x=dfb$Tmax_sm, y=dfb$Density_B, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2)+theme_classic()+
  scale_colour_manual(values=col.kat) +
  annotate("text",label="GLAr^2 == 0.02235",x=28.5,y=435,parse=TRUE,size=2, colour="darkslategray3")+
  annotate("text",label="PRAr^2 == 0.05447",x=28.5,y=450,parse=TRUE,size=2, colour="gold3")+
  ylab("Abaxial Stomatal Density")+xlab("Summer Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=27.5,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")

#i
col <- ggarrange(c, d, h, i,labels = c("A", "B", "C", "D"),ncol=2,nrow=2)
col

bw<-ggarrange(cb,dw,hb,ib, labels = c("A", "B", "C", "D"),ncol=2,nrow=2)
bw

#------------------------------------------------------------------------------------
#--------------------------------------------------
#Density T
#--------------------------------------------------
#climate-moisture deficit for spring-------------------------------------------------
#color
c<-ggplot(dfb, aes(x=dfb$CMD_sp, y=dfb$Density_T, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=c("darkslategray3", "gold3")) +
  #annotate("text", label="*", x=82.73791, y=222.06189, col="black", size=13)+
  #annotate("text", label="*", x=43.26434, y=255.90579, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.06759",x=130,y=435,parse=TRUE,size=2,colour="darkslategray3")+
  annotate("text",label="PRAr^2 ==  0.1805",x=130,y=420,parse=TRUE,size=2,colour="gold3")+
  ylab("Adaxial Stomatal Density")+xlab("Spring Climate Moisture Deficit")+ 
  geom_vline(xintercept=55,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
c
#bw
cb<-ggplot(dfb, aes(x=dfb$CMD_sp, y=dfb$Density_T, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=c("black", "gray48")) +
  #annotate("text", label="*", x=82.73791, y=222.06189, col="black", size=13)+
  #annotate("text", label="*", x=43.26434, y=255.90579, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.06759",x=130,y=435,parse=TRUE,size=2,colour="black")+
  annotate("text",label="PRAr^2 == 0.1805",x=130,y=420,parse=TRUE,size=2,colour="gray48")+
  ylab("Adaxial Stomatal Density")+xlab("Spring Climate Moisture Deficit")+ 
  geom_vline(xintercept=55,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
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

#linear
#cmodpl<-lm(prc$Density_T~prc$CMD_sp) 
#summary(cmodpl) 
#Multiple R-squared:  0.1127,	Adjusted R-squared:  0.1054 
#F-statistic: 15.37 on 1 and 121 DF,  p-value: 0.0001469
#cmodgl<-lm(glc$Density_T~glc$CMD_sp) 
#summary(cmodgl)
#Multiple R-squared:  0.03356,	Adjusted R-squared:  0.03077 
#F-statistic: 12.05 on 1 and 347 DF,  p-value: 0.000584
#c<-ggplot(dfb, aes(x=dfb$CMD_sp, y=dfb$Density_T, colour=Region.x))+
geom_point()+stat_smooth(method = 'lm', alpha=0.2)+theme_classic()+
  scale_colour_manual(values=col.kat) +
  annotate("text",label="GLAr^2 == 0.03356",x=130,y=335,parse=TRUE,size=2, colour="darkslategray3")+
  annotate("text",label="PRAr^2 == 0.1127",x=130,y=350,parse=TRUE,size=2, colour="gold3")+
  ylab("Adaxial Stomatal Density")+xlab("Spring Climate Moisture Deficit")+
  geom_vline(xintercept=55,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")

#c
#climate-moisture deficit for summer----------------------------------------------------
#color
d<-ggplot(dfb, aes(x=dfb$CMD_sm, y=dfb$Density_T, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=col.kat) +
  #annotate("text", label="*", x=122.8558, y=218.5621, col="black", size=13)+
 # annotate("text", label="*", x=111.9069, y=369.8539, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.05768",x=400,y=335,parse=TRUE,size=2,colour="darkslategray3")+
  annotate("text",label="PRAr^2 ==  0.1715",x=400,y=350,parse=TRUE,size=2,colour="gold3")+
  ylab("Adaxial Stomatal Density")+xlab("Summer Climate Moisture Deficit")+ 
  geom_vline(xintercept=155,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
d
#b&w
dw<-ggplot(dfb, aes(x=dfb$CMD_sm, y=dfb$Density_T, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=c("black", "gray48")) +
  #annotate("text", label="*", x=122.8558, y=218.5621, col="black", size=13)+
  #annotate("text", label="*", x=111.9069, y=369.8539, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.05768",x=400,y=335,parse=TRUE,size=2,colour="black")+
  annotate("text",label="PRAr^2 ==  0.1715",x=400,y=350,parse=TRUE,size=2,colour="gray48")+
  ylab("Adaxial Stomatal Density")+xlab("Summer Climate Moisture Deficit")+ 
  geom_vline(xintercept=155,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
dw

#quadratic SLIGHTLY BEST
dmodp<-lm(prc$Density_T~prc$CMD_sm+ I(prc$CMD_sm^2)) 
summary(dmodp) 
#Multiple R-squared:  0.1715,	Adjusted R-squared:  0.1577 
#F-statistic: 12.42 on 2 and 120 DF,  p-value: 1.251e-05
dmodg<-lm(glc$Density_T~glc$CMD_sm+ I(glc$CMD_sm^2)) 
summary(dmodg)
#Multiple R-squared:  0.05768,	Adjusted R-squared:  0.05223 
#F-statistic: 10.59 on 2 and 346 DF,  p-value: 3.439e-05

#linear
#dmodpl<-lm(prc$Density_T~prc$CMD_sm) 
#summary(dmodpl) 
#Multiple R-squared:  0.1497,	Adjusted R-squared:  0.1426 
#F-statistic:  21.3 on 1 and 121 DF,  p-value: 9.87e-06
#dmodgl<-lm(glc$Density_T~glc$CMD_sm) 
#summary(dmodgl)
#Multiple R-squared:  0.05763,	Adjusted R-squared:  0.05492 
#F-statistic: 21.22 on 1 and 347 DF,  p-value: 5.753e-06
d<-ggplot(dfb, aes(x=dfb$CMD_sm, y=dfb$Density_B, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2)+theme_classic()+
  scale_colour_manual(values=col.kat) +
  annotate("text",label="GLAr^2 == 0.05763",x=400,y=435,parse=TRUE,size=2, colour="darkslategray3")+
  annotate("text",label="PRAr^2 == 0.1497",x=400,y=450,parse=TRUE,size=2, colour="gold3")+
  ylab("Adaxial Stomatal Density")+xlab("Summer Climate Moisture Deficit")+
  geom_vline(xintercept=155,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")

d
#spring mean maximum temperature (°C)--------------------------------------------------
#color
h<-ggplot(dfb, aes(x=dfb$Tmax_sp, y=dfb$Density_T, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=col.kat) +
  #annotate("text", label="*", x=14.47218, y=166.29841, col="black", size=13)+
  #annotate("text", label="*", x=11.30716, y=407.47429, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.02449",x=16,y=335,parse=TRUE,size=2, colour="darkslategray3")+
  annotate("text",label="PRAr^2 == 0.02692",x=16,y=350,parse=TRUE,size=2, colour="gold3")+
  ylab("Adaxial Stomatal Density")+xlab("Spring Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=13.7,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")

h
#bw
hb<-ggplot(dfb, aes(x=dfb$Tmax_sp, y=dfb$Density_T, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=c("black", "gray48")) +
  #annotate("text", label="*", x=14.47218, y=166.29841, col="black", size=13)+
  #annotate("text", label="*", x=11.30716, y=407.47429, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.05262",x=16,y=335,parse=TRUE,size=2, colour="black")+
  annotate("text",label="PRAr^2 == 0.1821",x=16,y=350,parse=TRUE,size=2, colour="gray48")+
  ylab("Adaxial Stomatal Density")+xlab("Spring Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=13.7,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")

hb
#quadratic BEST
hmodp<-lm(prc$Density_T~prc$Tmax_sp+ I(prc$Tmax_sp^2)) 
summary(hmodp) 
#Multiple R-squared:  0.1821,	Adjusted R-squared:  0.1685 
#F-statistic: 13.36 on 2 and 120 DF,  p-value: 5.765e-06
hmodg<-lm(glc$Density_T~glc$Tmax_sp+ I(glc$Tmax_sp^2)) 
summary(hmodg) 
#Multiple R-squared:  0.05282,	Adjusted R-squared:  0.04734 
#F-statistic: 9.647 on 2 and 346 DF,  p-value: 8.374e-05

#linear
#hmodpl<-lm(prc$Density_T~prc$Tmax_sp) 
#summary(hmodpl)
#Multiple R-squared:  0.00409,	Adjusted R-squared:  -0.004141 
#F-statistic: 0.4969 on 1 and 121 DF,  p-value: 0.4822
#hmodgl<-lm(glc$Density_T~glc$Tmax_sp) 
#summary(hmodgl)
#Multiple R-squared:  0.05262,	Adjusted R-squared:  0.04989 
#F-statistic: 19.27 on 1 and 347 DF,  p-value: 1.505e-05
#h<-ggplot(dfb, aes(x=dfb$Tmax_sp, y=dfb$Density_T, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2)+theme_classic()+
  scale_colour_manual(values=col.kat) +
  annotate("text",label="GLAr^2 == 0.05262",x=16,y=335,parse=TRUE,size=2, colour="darkslategray3")+
  annotate("text",label="PRAr^2 == 0.00409",x=16,y=350,parse=TRUE,size=2, colour="gold3")+
  ylab("Adaxial Stomatal Density")+xlab("Spring Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=13.7,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")

#h
##combined linear and quadratics
#h<- ggplot() +
  geom_point(data=prc, aes(x=prc$Tmax_sp, y=prc$Density_T)) + 
  stat_smooth(data=prc,aes(x=prc$Tmax_sp, y=prc$Density_T,colour=prc$Region.x),method = 'lm',formula=y~x+I(x^2))+
  geom_point(data=glc, aes(x=glc$Tmax_sp, y=glc$Density_T))+
  stat_smooth(data=glc,aes(x=glc$Tmax_sp, y=glc$Density_T,colour=glc$Region.x), method = 'lm') +
  annotate("text",label="GLAr^2 == 0.05262",x=16,y=435,parse=TRUE,size=2, colour="darkslategray3")+
  annotate("text",label="PRAr^2 ==  0.1821",x=16,y=450,parse=TRUE,size=2, colour="gold3")+
  ylab("Adaxial Stomatal Density")+xlab("Spring Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=13.7,col="gray60",lwd=0.75,lty=2)+
  theme_classic()+theme(legend.position="none")
#h
#funkymonkeyb&w
#hb<- ggplot() +
  geom_point(data=prc, aes(x=prc$Tmax_sp, y=prc$Density_T)) + 
  stat_smooth(data=prc,aes(x=prc$Tmax_sp, y=prc$Density_T,colour=prc$Region.x),method = 'lm',formula=y~x+I(x^2))+
  geom_point(data=glc, aes(x=glc$Tmax_sp, y=glc$Density_T)) + 
  stat_smooth(data=glc,aes(x=glc$Tmax_sp, y=glc$Density_T,colour=glc$Region.x), method = 'lm') +
  annotate("text",label="GLAr^2 == 0.05262",x=16,y=435,parse=TRUE,size=2, colour="black")+
  annotate("text",label="PRAr^2 ==  0.1821",x=16,y=450,parse=TRUE,size=2, colour="gray48")+
  ylab("Adaxial Stomatal Density")+xlab("Spring Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=13.7,col="gray60",lwd=0.75,lty=2)+
  theme_classic()+theme(legend.position="none")
#hb

#summer mean maximum temperature (°C)---------------------------------------------------
i<-ggplot(dfb, aes(x=dfb$Tmax_sm, y=dfb$Density_T, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=col.kat) +
  #annotate("text", label="*", x=27.07972, y=124.24815, col="black", size=13)+
  #annotate("text", label="*", x=24.09835, y=313.36490, col="black", size=13)+ 
  annotate("text",label="GLAr^2 ==  0.03762",x=28.5,y=335,parse=TRUE,size=2, colour="darkslategray3")+
  annotate("text",label="PRAr^2 ==  0.1693",x=28.5,y=350,parse=TRUE,size=2,colour="gold3")+
  ylab("Adaxial Stomatal Density")+xlab("Summer Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=27.5,col="gray60",lwd=0.75,lty=2)+theme(legend.position = "none")

i
#bw
ib<-ggplot(dfb, aes(x=dfb$Tmax_sm, y=dfb$Density_T, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=c("black", "gray48"))+
  #annotate("text", label="*", x=27.07972, y=124.24815, col="black", size=13)+
  #annotate("text", label="*", x=24.09835, y=313.36490, col="black", size=13)+ 
  annotate("text",label="GLAr^2 ==  0.03762",x=28.5,y=335,parse=TRUE,size=2, colour="black")+
  annotate("text",label="PRAr^2 == 0.1693",x=28.5,y=350,parse=TRUE,size=2, colour="gray48")+
  ylab("Adaxial Stomatal Density")+xlab("Summer Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=27.5,col="gray60",lwd=0.75,lty=2)+theme(legend.position = "none")

ib

#quadratic BEST
imodp<-lm(prc$Density_T~prc$Tmax_sm+ I(prc$Tmax_sm^2)) 
summary(imodp) 
#Multiple R-squared:  0.1693,	Adjusted R-squared:  0.1555 
#F-statistic: 12.23 on 2 and 120 DF,  p-value: 1.465e-05
imodg<-lm(glc$Density_T~glc$Tmax_sm+ I(glc$Tmax_sm^2)) 
summary(imodg)
#Multiple R-squared:  0.03762,	Adjusted R-squared:  0.03205 
#F-statistic: 6.762 on 2 and 346 DF,  p-value: 0.001316

#linear
#imodpl<-lm(prc$Density_T~prc$Tmax_sm) 
#summary(imodpl)
#Multiple R-squared:  0.01034,	Adjusted R-squared:  0.002161 
#F-statistic: 1.264 on 1 and 121 DF,  p-value: 0.2631
#imodgl<-lm(glc$Density_T~glc$Tmax_sm) 
#summary(imodgl)
#Multiple R-squared:  0.03393,	Adjusted R-squared:  0.03115 
#F-statistic: 12.19 on 1 and 347 DF,  p-value: 0.000543
#i<-ggplot(dfb, aes(x=dfb$Tmax_sm, y=dfb$Density_T, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2)+theme_classic()+
  scale_colour_manual(values=col.kat) +
  annotate("text",label="GLAr^2 ==  0.03393",x=28.5,y=335,parse=TRUE,size=2, colour="darkslategray3")+
  annotate("text",label="PRAr^2 == 0.01034",x=28.5,y=350,parse=TRUE,size=2, colour="gold3")+
  ylab("Adaxial Stomatal Density")+xlab("Summer Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=27.5,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")

#i
col <- ggarrange(c, d, h,i,labels = c("A", "B", "C", "D"),ncol=2,nrow=2)
col

bw<-ggarrange(cb,dw,hb,ib, labels = c("A", "B", "C", "D"),ncol=2,nrow=2)
bw

#-------------------------------------------------
#SAI_B
#-------------------------------------------------
#climate-moisture deficit for spring---------------------------------------------------
c<-ggplot(dfb, aes(x=dfb$CMD_sp, y=dfb$SAI_B, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=col.kat)+
  #annotate("text", label="*", x=85.972450, y=7.155827, col="black", size=13)+
  #annotate("text", label="*", x=42.35001, y=11.21488, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.04422",x=130,y=11.7,parse=TRUE,size=2,colour="darkslategray3")+
  annotate("text",label="PRAr^2 ==  0.2051",x=130,y=11.4,parse=TRUE,size=2,colour="gold3")+
  ylab("Abaxial Area Index")+xlab("Spring Climate Moisture Deficit")+ 
  geom_vline(xintercept=55,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
c
#bw
cb<-ggplot(dfb, aes(x=dfb$CMD_sp, y=dfb$SAI_B, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=c("black", "gray48"))+
  #annotate("text", label="*", x=85.972450, y=7.155827, col="black", size=13)+
  #annotate("text", label="*", x=42.35001, y=11.21488, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.04422",x=130,y=11.7,parse=TRUE,size=2,colour="black")+
  annotate("text",label="PRAr^2 ==  0.2051",x=130,y=11.5,parse=TRUE,size=2,colour="gray48")+
  ylab("Abaxial Area Index")+xlab("Spring Climate Moisture Deficit")+ 
  geom_vline(xintercept=55,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
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

#linear
cmodp<-lm(prc$SAI_B~prc$CMD_sp) 
summary(cmodp) 
#Multiple R-squared:  0.0438,	Adjusted R-squared:  0.03583 
#F-statistic: 5.496 on 1 and 120 DF,  p-value: 0.0207
cmodg<-lm(glc$SAI_B~glc$CMD_sp) 
summary(cmodg)
#Multiple R-squared:  0.02187,	Adjusted R-squared:  0.01909 
#F-statistic:  7.85 on 1 and 351 DF,  p-value: 0.005365
c<-ggplot(dfb, aes(x=dfb$CMD_sp, y=dfb$SAI_B, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=col.kat) +
  annotate("text",label="GLAr^2 == 0.02187",x=130,y=11.7,parse=TRUE,size=2,colour="darkslategray3")+
  annotate("text",label="PRAr^2 ==  0.0438",x=130,y=11.4,parse=TRUE,size=2,colour="gold3")+
  ylab("Abaxial Area Index")+xlab("Spring Climate Moisture Deficit")+ 
  geom_vline(xintercept=55,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
c
#climate-moisture deficit for summer------------------------------------------------
#color
d<-ggplot(dfb, aes(x=dfb$CMD_sm, y=dfb$SAI_B, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=col.kat)+
  #annotate("text", label="*", x=89.40094, y=9.39186, col="black", size=13)+
  #annotate("text", label="*", x=112.73981, y=19.47034, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.02638",x=400,y=11.7,parse=TRUE,size=2,colour="darkslategray3")+
  annotate("text",label="PRAr^2 == 0.3074",x=400,y=11.4,parse=TRUE,size=2,colour="gold3")+
  ylab("Abaxial Area Index")+xlab("Summer Climate Moisture Deficit")+ 
  geom_vline(xintercept=155,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
d
#b&w
dw<-ggplot(dfb, aes(x=dfb$CMD_sm, y=dfb$SAI_B, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=c("black", "gray48"))+
  #annotate("text", label="*", x=89.40094, y=9.39186, col="black", size=13)+
  #annotate("text", label="*", x=112.73981, y=19.47034, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.02638",x=400,y=11.7,parse=TRUE,size=2,colour="black")+
  annotate("text",label="PRAr^2 == 0.3074",x=400,y=11.4,parse=TRUE,size=2,colour="gray48")+
  ylab("Abaxial Area Index")+xlab("Summer Climate Moisture Deficit")+ 
  geom_vline(xintercept=155,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
dw

#quadratic BEST
dmodpq<-lm(prc$SAI_B~prc$CMD_sm+ I(prc$CMD_sm^2)) 
summary(dmodpq) 
#Multiple R-squared:  0.3074,	Adjusted R-squared:  0.2958 
#F-statistic: 26.41 on 2 and 119 DF,  p-value: 3.219e-10
dmodgq<-lm(glc$SAI_B~glc$CMD_sm+ I(glc$CMD_sm^2)) 
summary(dmodgq)
#Multiple R-squared:  0.02638,	Adjusted R-squared:  0.02082 
#F-statistic: 4.742 on 2 and 350 DF,  p-value: 0.00929

#linear
dmodp<-lm(prc$SAI_B~prc$CMD_sm) 
summary(dmodp) 
#Multiple R-squared:  0.2977,	Adjusted R-squared:  0.2918 
#F-statistic: 50.86 on 1 and 120 DF,  p-value: 8.101e-11
dmodg<-lm(glc$SAI_B~glc$CMD_sm) 
summary(dmodg)
#Multiple R-squared:  0.0217,	Adjusted R-squared:  0.01891 
#F-statistic: 7.784 on 1 and 351 DF,  p-value: 0.005559
d<-ggplot(dfb, aes(x=dfb$CMD_sm, y=dfb$SAI_B, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2)+theme_classic()+
  scale_colour_manual(values=col.kat) +
  annotate("text",label="GLAr^2 == 0.0217",x=400,y=11.7,parse=TRUE,size=2,colour="darkslategray3")+
  annotate("text",label="PRAr^2 == 0.2977",x=400,y=11.4,parse=TRUE,size=2,colour="gold3")+
  ylab("Abaxial Area Index")+xlab("Summer Climate Moisture Deficit")+ 
  geom_vline(xintercept=155,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
d

#spring mean maximum temperature (°C)------------------------------------------------
h<-ggplot(dfb, aes(x=dfb$Tmax_sp, y=dfb$SAI_B, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=col.kat)+
  #annotate("text", label="*", x=14.482577, y=4.662539, col="black", size=13)+
  #annotate("text", label="*", x=12.08311, y=29.53010, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.0238",x=16,y=11.7,parse=TRUE,size=2, colour="darkslategray3")+
  annotate("text",label="PRAr^2 == 0.2727",x=16,y=11.4,parse=TRUE,size=2, colour="gold3")+
  ylab("Abaxial Area Index")+xlab("Spring Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=13.7,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")

h
#bw
hb<-ggplot(dfb, aes(x=dfb$Tmax_sp, y=dfb$SAI_B, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=c("black", "gray48"))+
  #annotate("text", label="*", x=14.482577, y=4.662539, col="black", size=13)+
 # annotate("text", label="*", x=12.08311, y=29.53010, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.0238",x=16,y=0.035,parse=TRUE,size=2, colour="black")+
  annotate("text",label="PRAr^2 == 0.2727",x=16,y=0.036,parse=TRUE,size=2, colour="gray48")+
  ylab("Abaxial Area Index")+xlab("Spring Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=13.7,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")

hb


#quadratic BEST
hmodpq<-lm(prc$SAI_B~prc$Tmax_sp+ I(prc$Tmax_sp^2)) 
summary(hmodpq) 
#Multiple R-squared:  0.2727,	Adjusted R-squared:  0.2605 
#F-statistic: 22.31 on 2 and 119 DF,  p-value: 5.912e-09
hmodgq<-lm(glc$SAI_B~glc$Tmax_sp+ I(glc$Tmax_sp^2)) 
summary(hmodgq)
#Multiple R-squared:  0.0238,	Adjusted R-squared:  0.01822 
#F-statistic: 4.266 on 2 and 350 DF,  p-value: 0.01477

#linear
hmodp<-lm(prc$SAI_B~prc$Tmax_sp) 
summary(hmodp) 
#Multiple R-squared:  0.02568,	Adjusted R-squared:  0.01757 
#F-statistic: 3.163 on 1 and 120 DF,  p-value: 0.07784
hmodg<-lm(glc$SAI_B~glc$Tmax_sp) 
summary(hmodg)
#Multiple R-squared:  0.02325,	Adjusted R-squared:  0.02047 
#F-statistic: 8.354 on 1 and 351 DF,  p-value: 0.004087
h<-ggplot(dfb, aes(x=dfb$Tmax_sp, y=dfb$SAI_B, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=col.kat) +
  annotate("text",label="GLAr^2 == 0.02325",x=16,y=11.7,parse=TRUE,size=2, colour="darkslategray3")+
  annotate("text",label="PRAr^2 == 0.02568",x=16,y=11.4,parse=TRUE,size=2, colour="gold3")+
  ylab("Abaxial Area Index")+xlab("Spring Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=13.7,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")

h

#summer mean maximum temperature (°C)------------------------------------------------
i<-ggplot(dfb, aes(x=dfb$Tmax_sm, y=dfb$SAI_B, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=col.kat)+
  #annotate("text", label="*", x=35.97574, y=65.30908, col="black", size=13)+
  #annotate("text", label="*", x=23.96509, y=12.15934, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.03762",x=28.5,y=11.7,parse=TRUE,size=2, colour="darkslategray3")+
  annotate("text",label="PRAr^2 == 0.2219",x=28.5,y=11.4,parse=TRUE,size=2,colour="gold3")+
  ylab("Abaxial Area Index")+xlab("Summer Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=27.5,col="gray60",lwd=0.75,lty=2)+theme(legend.position = "none")

i
#bw
ib<-ggplot(dfb, aes(x=dfb$Tmax_sm, y=dfb$SAI_B, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=c("black", "gray48"))+
  #annotate("text", label="*", x=35.97574, y=65.30908, col="black", size=13)+
 # annotate("text", label="*", x=23.96509, y=12.15934, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.03762",x=28.5,y=11.7,parse=TRUE,size=2, colour="black")+
  annotate("text",label="PRAr^2 == 0.2219",x=28.5,y=11.4,parse=TRUE,size=2, colour="gray48")+
  ylab("Abaxial Area Index")+xlab("Summer Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=27.5,col="gray60",lwd=0.75,lty=2)+theme(legend.position = "none")

ib

#quadratic BEST
imodp<-lm(prc$SAI_B~prc$Tmax_sm+ I(prc$Tmax_sm^2)) 
summary(imodp) 
#Multiple R-squared:  0.2219,	Adjusted R-squared:  0.2089 
#F-statistic: 16.97 on 2 and 119 DF,  p-value: 3.277e-07
imodg<-lm(glc$SAI_B~glc$Tmax_sm I(glc$Tmax_sm^2)) 
summary(imodg)
#MMultiple R-squared:  0.03762,	Adjusted R-squared:  0.03205 
#F-statistic: 6.762 on 2 and 346 DF,  p-value: 0.001316

#linear
imodpl<-lm(prc$SAI_B~prc$Tmax_sm) 
summary(imodpl) 
#Multiple R-squared:  0.07592,	Adjusted R-squared:  0.06822 
#F-statistic: 9.858 on 1 and 120 DF,  p-value: 0.002129
imodgl<-lm(glc$SAI_B~glc$Tmax_sm) 
summary(imodgl)
#Multiple R-squared:  0.01969,	Adjusted R-squared:  0.01689 
#F-statistic: 7.048 on 1 and 351 DF,  p-value: 0.008295
i<-ggplot(dfb, aes(x=dfb$Tmax_sm, y=dfb$SAI_B, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2)+theme_classic()+
  scale_colour_manual(values=col.kat) +
  annotate("text",label="GLAr^2 == 0.01969",x=28.5,y=11.7,parse=TRUE,size=2, colour="darkslategray3")+
  annotate("text",label="PRAr^2 == 0.07592",x=28.5,y=11.4,parse=TRUE,size=2,colour="gold3")+
  ylab("Abaxial Area Index")+xlab("Summer Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=27.5,col="gray60",lwd=0.75,lty=2)+theme(legend.position = "none")

i

col <- ggarrange(c, d, i,labels = c("A", "B", "C", "D"),ncol=2,nrow=2)
col

bw<-ggarrange(cb,dw,ib, labels = c("A", "B", "C", "D"),ncol=2,nrow=2)
bw
#-------------------------------------------------
#SAI_T
#------------------------------------------------
#climate-moisture deficit for spring---------------------------------------------------
c<-ggplot(dfb, aes(x=dfb$CMD_sp, y=dfb$SAI_T, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=col.kat)+
  #annotate("text", label="*", x=83.849163, y=5.694438, col="black", size=13)+
  #annotate("text", label="*", x=6.028046, y=42.424627, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.04016",x=130,y=9,parse=TRUE,size=2,colour="darkslategray3")+
  annotate("text",label="PRAr^2 ==  0.1504",x=130,y=8.8,parse=TRUE,size=2,colour="gold3")+
  ylab("Adaxial Area Index")+xlab("Spring Climate Moisture Deficit")+ 
  geom_vline(xintercept=55,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
c

#bw
cb<-ggplot(dfb, aes(x=dfb$CMD_sp, y=dfb$SAI_T, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=c("black", "gray48"))+
  #annotate("text", label="*", x=83.849163, y=5.694438, col="black", size=13)+
  #annotate("text", label="*", x=6.028046, y=42.424627, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.04016",x=130,y=9,parse=TRUE,size=2,colour="black")+
  annotate("text",label="PRAr^2 ==  0.1504",x=130,y=8.8,parse=TRUE,size=2,colour="gray48")+
  ylab("Adaxial Area Index")+xlab("Spring Climate Moisture Deficit")+ 
  geom_vline(xintercept=55,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
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

#linear
cmodpl<-lm(prc$SAI_T~prc$CMD_sp) 
summary(cmodpl) 
#Multiple R-squared:  0.09644,	Adjusted R-squared:  0.08891 
#F-statistic: 12.81 on 1 and 120 DF,  p-value: 0.000499
cmodgl<-lm(glc$SAI_T~glc$CMD_sp) 
summary(cmodgl)
#Multiple R-squared:  0.02233,	Adjusted R-squared:  0.01949 
#F-statistic: 7.857 on 1 and 344 DF,  p-value: 0.00535
c<-ggplot(dfb, aes(x=dfb$CMD_sp, y=dfb$SAI_T, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2)+theme_classic()+
  scale_colour_manual(values=col.kat) +
  annotate("text",label="GLAr^2 == 0.02233",x=130,y=9,parse=TRUE,size=2,colour="darkslategray3")+
  annotate("text",label="PRAr^2 ==  0.09644",x=130,y=8.8,parse=TRUE,size=2,colour="gold3")+
  ylab("Adaxial Area Index")+xlab("Spring Climate Moisture Deficit")+ 
  geom_vline(xintercept=55,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
c
#climate-moisture deficit for summer---------------------------------------------------
#color
d<-ggplot(dfb, aes(x=dfb$CMD_sm, y=dfb$SAI_T, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=col.kat)+
  #annotate("text", label="*", x=119.819900, y=6.440876, col="black", size=13)+
  #annotate("text", label="*", x=120.608736, y=7.179445, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.03709",x=400,y=9,parse=TRUE,size=2,colour="darkslategray3")+
  annotate("text",label="PRAr^2 == 0.1483",x=400,y=8.8,parse=TRUE,size=2,colour="gold3")+
  ylab("Adaxial Area Index")+xlab("Summer Climate Moisture Deficit")+ 
  geom_vline(xintercept=155,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
d
#b&w
dw<-ggplot(dfb, aes(x=dfb$CMD_sm, y=dfb$SAI_T, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=c("black", "gray48"))+
  #annotate("text", label="*", x=119.819900, y=6.440876, col="black", size=13)+
  #annotate("text", label="*", x=120.608736, y=7.179445, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.03709",x=400,y=9,parse=TRUE,size=2,colour="black")+
  annotate("text",label="PRAr^2 == 0.1483",x=400,y=8.8,parse=TRUE,size=2,colour="gray48")+
  ylab("Adaxial Area Index")+xlab("Summer Climate Moisture Deficit")+ 
  geom_vline(xintercept=155,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
dw

#quadratic BEST
dmodpq<-lm(prc$SAI_T~prc$CMD_sm+ I(prc$CMD_sm^2)) 
summary(dmodpq) 
#Multiple R-squared:  0.1483,	Adjusted R-squared:  0.134 
#F-statistic: 10.36 on 2 and 119 DF,  p-value: 7.093e-05
dmodgq<-lm(glc$SAI_T~glc$CMD_sm+ I(glc$CMD_sm^2)) 
summary(dmodgq)
#Multiple R-squared:  0.03709,	Adjusted R-squared:  0.03148 
#F-statistic: 6.606 on 2 and 343 DF,  p-value: 0.00153


#linear
dmodp<-lm(prc$SAI_T~prc$CMD_sm) 
summary(dmodp) 
#Multiple R-squared:  0.1385,	Adjusted R-squared:  0.1313 
#F-statistic: 19.28 on 1 and 120 DF,  p-value: 2.441e-05
dmodg<-lm(glc$SAI_T~glc$CMD_sm) 
summary(dmodg)
#Multiple R-squared:  0.03606,	Adjusted R-squared:  0.03325 
#F-statistic: 12.87 on 1 and 344 DF,  p-value: 0.0003828
d<-ggplot(dfb, aes(x=dfb$CMD_sm, y=dfb$SAI_T, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2)+theme_classic()+
  scale_colour_manual(values=col.kat) +
  annotate("text",label="GLAr^2 == 0.03606",x=400,y=9,parse=TRUE,size=2,colour="darkslategray3")+
  annotate("text",label="PRAr^2 == 0.1385",x=400,y=8.8,parse=TRUE,size=2,colour="gold3")+
  ylab("Adaxial Area Index")+xlab("Summer Climate Moisture Deficit")+ 
  geom_vline(xintercept=155,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")
d

#spring mean maximum temperature (°C)-----------------------------------------------
h<-ggplot(dfb, aes(x=dfb$Tmax_sp, y=dfb$SAI_T, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=col.kat) +
  #annotate("text", label="*", x=14.47385, y=4.38189, col="black", size=13)+
  #annotate("text", label="*", x=10.736151, y=7.679337, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.04216",x=16,y=11.7,parse=TRUE,size=2, colour="darkslategray3")+
  annotate("text",label="PRAr^2 == 0.1548",x=16,y=11.4,parse=TRUE,size=2, colour="gold3")+
  ylab("Adaxial Area Index")+xlab("Spring Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=13.7,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")

h
#bw
hb<-ggplot(dfb, aes(x=dfb$Tmax_sp, y=dfb$SAI_T, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2)+theme_classic()+
  scale_colour_manual(values=c("black", "gray48")) +
  #annotate("text", label="*", x=14.47385, y=4.38189, col="black", size=13)+
  #annotate("text", label="*", x=10.736151, y=7.679337, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.04216",x=16,y=0.035,parse=TRUE,size=2, colour="black")+
  annotate("text",label="PRAr^2 == 0.1548",x=16,y=0.036,parse=TRUE,size=2, colour="gray48")+
  ylab("Adaxial Area Index")+xlab("Spring Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=13.7,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")

hb

#quadratic BEST
hmodpq<-lm(prc$SAI_T~prc$Tmax_sp+ I(prc$Tmax_sp^2)) 
summary(hmodpq) 
#Multiple R-squared:  0.1548,	Adjusted R-squared:  0.1405 
#F-statistic: 10.89 on 2 and 119 DF,  p-value: 4.525e-05
hmodgq<-lm(glc$SAI_T~glc$Tmax_sp+ I(glc$Tmax_sp^2)) 
summary(hmodgq)
#Multiple R-squared:  0.04216,	Adjusted R-squared:  0.03658 
#F-statistic: 7.549 on 2 and 343 DF,  p-value: 0.0006187

#linear
hmodp<-lm(prc$SAI_T~prc$Tmax_sp) 
summary(hmodp) 
#Multiple R-squared:  0.0004734,	Adjusted R-squared:  -0.007856 
#F-statistic: 0.05684 on 1 and 120 DF,  p-value: 0.812
hmodg<-lm(glc$SAI_T~glc$Tmax_sp) 
summary(hmodg)
#Multiple R-squared:  0.04206,	Adjusted R-squared:  0.03927 
#F-statistic:  15.1 on 1 and 344 DF,  p-value: 0.0001222
h<-ggplot(dfb, aes(x=dfb$Tmax_sp, y=dfb$SAI_T, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=col.kat) +
  annotate("text",label="GLAr^2 == 0.04206",x=16,y=11.7,parse=TRUE,size=2, colour="darkslategray3")+
  annotate("text",label="PRAr^2 == 0.0004734",x=16,y=11.4,parse=TRUE,size=2, colour="gold3")+
  ylab("Adaxial Area Index")+xlab("Spring Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=13.7,col="gray60",lwd=0.75,lty=2)+
  theme(legend.position = "none")

h
#summer mean maximum temperature (°C)-------------------------------------------------
i<-ggplot(dfb, aes(x=dfb$Tmax_sm, y=dfb$SAI_T, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=col.kat) +
  #annotate("text", label="*", x=27.046443, y=3.216157, col="black", size=13)+
  #annotate("text", label="*", x=23.972746, y=6.650427, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.03762",x=28.5,y=9,parse=TRUE,size=2, colour="darkslategray3")+
  annotate("text",label="PRAr^2 == 0.1512",x=28.5,y=8.8,parse=TRUE,size=2,colour="gold3")+
  ylab("Adaxial Area Index")+xlab("Summer Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=27.5,col="gray60",lwd=0.75,lty=2)+theme(legend.position = "none")

i
#bw
ib<-ggplot(dfb, aes(x=dfb$Tmax_sm, y=dfb$SAI_T, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=c("black", "gray48")) +
  #annotate("text", label="*", x=27.046443, y=3.216157, col="black", size=13)+
  #annotate("text", label="*", x=23.972746, y=6.650427, col="black", size=13)+
  annotate("text",label="GLAr^2 == 0.03762",x=28.5,y=9,parse=TRUE,size=2, colour="black")+
  annotate("text",label="PRAr^2 == 0.1512",x=28.5,y=8.8,parse=TRUE,size=2, colour="gray48")+
  ylab("Adaxial Area Index")+xlab("Summer Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=27.5,col="gray60",lwd=0.75,lty=2)+theme(legend.position = "none")

ib

#quadratic BEST
imodp<-lm(prc$SAI_T~prc$Tmax_sm+ I(prc$Tmax_sm^2)) 
summary(imodp) 
#Multiple R-squared:  0.1512,	Adjusted R-squared:  0.137 
#F-statistic:  10.6 on 2 and 119 DF,  p-value: 5.794e-05
imodg<-lm(glc$SAI_T~glc$Tmax_sm I(glc$Tmax_sm^2)) 
summary(imodg)
#Multiple R-squared:  0.03762,	Adjusted R-squared:  0.03205 
#F-statistic: 6.762 on 2 and 346 DF,  p-value: 0.001316

#linear
imodpl<-lm(prc$SAI_T~prc$Tmax_sm) 
summary(imodpl) 
#Multiple R-squared:  0.01859,	Adjusted R-squared:  0.01041 
#F-statistic: 2.273 on 1 and 120 DF,  p-value: 0.1342
imodgl<-lm(glc$SAI_T~glc$Tmax_sm) 
summary(imodgl)
#Multiple R-squared:  0.0255,	Adjusted R-squared:  0.02266 
#F-statistic: 9.001 on 1 and 344 DF,  p-value: 0.002896
i<-ggplot(dfb, aes(x=dfb$Tmax_sm, y=dfb$SAI_T, colour=Region.x))+
  geom_point()+stat_smooth(method = 'lm', alpha=0.2,formula=y~x+I(x^2))+theme_classic()+
  scale_colour_manual(values=col.kat) +
  annotate("text",label="GLAr^2 == 0.0255",x=28.5,y=9,parse=TRUE,size=2, colour="darkslategray3")+
  annotate("text",label="PRAr^2 == 0.01859",x=28.5,y=8.8,parse=TRUE,size=2,colour="gold3")+
  ylab("Adaxial Area Index")+xlab("Summer Mean Maximum Temperature (°C)")+
  geom_vline(xintercept=27.5,col="gray60",lwd=0.75,lty=2)+theme(legend.position = "none")

i
col <- ggarrange(c, d, i,labels = c("A", "B", "C", "D"),ncol=2,nrow=2)
col

bw<-ggarrange(cb,dw,ib, labels = c("A", "B", "C", "D"),ncol=2,nrow=2)
bw
