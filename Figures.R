############# Figures created in the main text and supplementary information
setwd("C:/Users/xiong/Desktop/MS_history of BTF China/Fisheries Research")
### Plot figures

####### Fig. 2
## read data
sh<-read.csv("Catch_all 2020.csv",header=TRUE,sep=',')
str(sh)
#tiff("Figure/Fig.1 within & beyond C4S.tif",width = 12, height = 8.8, units = 'in', res = 300)
par(mar=c(2,2,2,3),mfrow=c(2,2),oma=c(0,0,0,0),las=1)
# a) all sectors
str(sh)
max2<-max(na.omit(sh$H_c1+2*sh$H_c1_se))/1000000
max2
min2<-min(na.omit(sh$H_c1-2*sh$H_c1_se))/1000000
min2
plot(sh$H_c1/1000000~sh$year,type='l',lwd=2,pch=2,col="black",ylab="",xlab="",xlim=c(1950,2018),ylim=c(min2,max2),xaxt='n',yaxt='n')
polygon(x=c(sh$year,rev(sh$year)),
        y=c((sh$H_c1-2*sh$H_c1_se)/1000000, rev((sh$H_c1+2*sh$H_c1_se)/1000000)),
        col=adjustcolor("black",alpha.f = 0.2),border = NA)
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(1950,2018,10)
axis(side = 1, at = ticks,tck=-0.03,labels = ticks)
ticks = seq(0,20,1)
axis(side = 2, at = ticks,tck=-0.01,col.ticks = "black",labels = NA)
ticks = seq(0,20,5)
axis(side = 2, at = ticks,tck=-0.03,labels = ticks)
par(new=T)
max2<-max(na.omit(sh$H_c2+2*sh$H_c2_se2))/1000000
max2
min2<-min(na.omit(sh$H_c2-2*sh$H_c2_se1))/1000000
plot(sh$H_c2/1000000~sh$year,type='l',lwd=2,pch=2,col="red",ylab="",xlab="",xlim=c(1950,2018),ylim=c(min2,max2),xaxt='n',yaxt='n')
polygon(x=c(sh$year,rev(sh$year)),
        y=c((sh$H_c2-2*sh$H_c2_se1)/1000000, rev((sh$H_c2+2*sh$H_c2_se2)/1000000)),
        col=adjustcolor("red",alpha.f = 0.2),border = NA)
ticks = seq(0,120,10)
axis(side = 4, at = ticks,tck=-0.01,col.ticks = "red",labels = NA, col = "red",col.axis="red")
ticks = seq(0,120,30)
axis(side = 4, at = ticks,tck=-0.03,labels = ticks,col.ticks = "red", col = "red",col.axis="red")
mtext("a) All marine fishing sectors in China",side=3,adj=0,cex=1,font=2)
abline(v=c(1985,2002,2006,2015),lty="dotted",col="grey50")
text(x=1985,y=30,labels = "1985:\nVessel privatization\nFishing offshore",pos=2,col="black",cex=1)
text(x=2003,y=15,labels = "2002:\nVessel\nbuyback",pos=2,col="black",cex=1)
text(x=2007,y=108,labels = "2006:\nFuel\nsubsidy",pos=2,col="black",cex=1)
text(x=2015,y=40,labels = "2015:\nFuel\nsubsidy\nreduction",pos=2,col="black",cex=1)
legend("topleft",legend=c("Nominal fishing power (GW)", "Effective fishing power (GW)"),
       text.col=c("black","red"),lty="solid",col=c("black","red"),lwd=2)## Add Legend
## b)
max2<-max(na.omit(sh$H_ind1+2*sh$H_ind1_se))/1000000
max2
min2<-min(na.omit(sh$H_ind1))/1000000
min2
plot(sh$H_ind1/1000000~sh$year,type='l',lwd=2,pch=2,col="black",ylab="",xlab="",xlim=c(1950,2018),ylim=c(min2,max2),xaxt='n',yaxt='n')
polygon(x=c(sh$year,rev(sh$year)),
        y=c((sh$H_ind1-2*sh$H_ind1_se)/1000000, rev((sh$H_ind1+2*sh$H_ind1_se)/1000000)),
        col=adjustcolor("black",alpha.f = 0.2),border = NA)
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(1950,2018,10)
axis(side = 1, at = ticks,tck=-0.03,labels = ticks)
ticks = seq(0,17.00,1)
axis(side = 2, at = ticks,tck=-0.01,col.ticks = "black",labels = NA)
ticks = seq(0,17.00,3.00)
axis(side = 2, at = ticks,tck=-0.03,labels = ticks)
par(new=T)
max2<-max(na.omit(sh$H_ind2+2*sh$H_ind2_se2))/1000000
max2
min2<-min(na.omit(sh$H_ind2))/1000000
plot(sh$H_ind2/1000000~sh$year,type='l',lwd=2,pch=2,col="red",ylab="",xlab="",xlim=c(1950,2018),ylim=c(min2,max2),xaxt='n',yaxt='n')
polygon(x=c(sh$year,rev(sh$year)),
        y=c((sh$H_ind2-2*sh$H_ind2_se1)/1000000, rev((sh$H_ind2+2*sh$H_ind2_se2)/1000000)),
        col=adjustcolor("red",alpha.f = 0.2),border = NA)
ticks = seq(0,110,10)
axis(side = 4, at = ticks,tck=-0.01,col.ticks = "red",labels = NA, col = "red",col.axis="red")
ticks = seq(0,110,20)
axis(side = 4, at = ticks,tck=-0.03,labels = ticks,col.ticks = "red", col = "red",col.axis="red")
mtext("b) Industrial fisheries within C4S",side=3,adj=0,cex=1,font=2)
abline(v=c(1985,1997,2002,2015),lty="dotted",col="grey50")


# c) Small-scale
str(sh)
max2<-max(na.omit(sh$H_nin1+2*sh$H_nin1_se))/1000000
max2
min2<-min(na.omit(sh$H_nin1-2*sh$H_nin1_se))/1000000
min2
plot(sh$H_nin1/1000000~sh$year,type='l',lwd=2,pch=2,col="black",ylab="",xlab="",xlim=c(1950,2018),ylim=c(min2,max2),xaxt='n',yaxt='n')
polygon(x=c(sh$year,rev(sh$year)),
        y=c((sh$H_nin1-2*sh$H_nin1_se)/1000000, rev((sh$H_nin1+2*sh$H_nin1_se)/1000000)),
        col=adjustcolor("black",alpha.f = 0.2),border = NA)
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(1950,2018,10)
axis(side = 1, at = ticks,tck=-0.03,labels = ticks)
ticks = seq(0,4.00,.1)
axis(side = 2, at = ticks,tck=-0.01,col.ticks = "black",labels = NA)
ticks = seq(0,4,1)
axis(side = 2, at = ticks,tck=-0.03,labels = ticks)
par(new=T)
max2<-max(na.omit(sh$H_nin2+2*sh$H_nin2_se2))/1000000
max2
min2<-min(na.omit(sh$H_nin2-2*sh$H_nin2_se1))/1000000
plot(sh$H_nin2/1000000~sh$year,type='l',lwd=2,pch=2,col="red",ylab="",xlab="",xlim=c(1950,2018),ylim=c(min2,max2),xaxt='n',yaxt='n')
polygon(x=c(sh$year,rev(sh$year)),
        y=c((sh$H_nin2-2*sh$H_nin2_se1)/1000000, rev((sh$H_nin2+2*sh$H_nin2_se2)/1000000)),
        col=adjustcolor("red",alpha.f = 0.2),border = NA)
ticks = seq(0,18,1)
axis(side = 4, at = ticks,tck=-0.01,col.ticks = "red",labels = NA, col = "red",col.axis="red")
ticks = seq(0,18,3)
axis(side = 4, at = ticks,tck=-0.03,labels = ticks,col.ticks = "red", col = "red",col.axis="red")
mtext("c) Small-scale fisheries within C4S",side=3,adj=0,cex=1,font=2)
abline(v=c(1985,1997,2002),lty="dotted",col="grey50")
text(x=1997,y=2,labels = "1997:\nDouble\ncontrol",pos=2,col="black",cex=1)

# d) distant water 
max<-max(na.omit(sh$H_dwc1))/1000000
max
min<-min(na.omit(sh$H_dwc1))/1000000
min
plot(sh$H_dwc1/1000000~sh$year,type='l',lwd=2,pch=1,col="black",xlim=c(1984,2018),ylim=c(min,max),ylab="",xlab="",axes=F)
box()
ticks = seq(0,2.7,.1)
axis(side = 2, at = ticks,tck=-0.01,col.ticks = "black",labels = NA)
ticks = round(seq(0,2.7,0.5),1)
axis(side = 2, at = ticks,tck=-0.03,labels = sprintf("%0.1f", ticks))
par(new=T)
max<-max(na.omit(sh$H_dwc2+2*sh$H_dwc2_se2))/1000000
max
min<-min(na.omit(sh$H_dwc2))/1000000
min
plot(sh$H_dwc2/1000000~sh$year,type='l',lwd=2,pch=1,col="red",xlim=c(1984,2018),ylim=c(min,max),xlab='',ylab='',axes=F)
polygon(x=c(sh$year,rev(sh$year)),
        y=c((sh$H_dwc2-2*sh$H_dwc2_se1)/1000000, rev((sh$H_dwc2+2*sh$H_dwc2_se2)/1000000)),
        col=adjustcolor("red",alpha.f = 0.2),border = NA)

ticks = seq(1985,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(1985,2018,10)
axis(side = 1, at = ticks,tck=-0.03,labels = ticks)
ticks = seq(0,10,1)
axis(side = 4, at = ticks,tck=-0.01,col.ticks = "red",col = "red",col.axis="red",labels = NA)
ticks = seq(0,10,2)
axis(side = 4, at = ticks,tck=-0.03,labels = ticks,col.ticks = "red", col = "red",col.axis="red")
abline(v=c(1985,1998,2006,2013),lty="dotted",col="grey50")
text(x=1985,y=2.4,labels = "1985:\nDeveloping\ndistant-water\nfisheries",pos=4,col="black",cex=1)
text(x=1998,y=6,labels = "1998:\nAsian\nfinancial\ncrisis",pos=4,col="black",cex=1)
text(x=2006,y=0.3,labels = "2006:\nFuel subsidy",pos=2,col="black",cex=1)
text(x=2013,y=7,labels = "2013:\nAccelerating\nfisheries\nupgrading",pos=2,col="black",cex=1)
mtext("d) Distant-water fisheries beyond C4S",side=3,adj=0,cex=1,font=2)

########## Fig. 3
## read data
sh<-read.csv("H_composition.csv",header=TRUE,sep=',')
str(sh)
library(reshape2)
df<-melt(sh,id="year")
str(df)
unique(df$variable)
library(ggplot2)
p1<-
  ggplot(df,aes(x = year, y = value*100, fill=variable))+
  geom_area(alpha=1 , size=.2, colour="white")+
  labs(x = "Year", y = "Stacked percentage of nominal fishing power (%)",face="bold")+
  scale_x_continuous(breaks=seq(1950,2018,10)
  )+
  scale_fill_manual("Fishing sectors:",
                    labels = c("Industrial fisheries within C4S","Powered small-scale fisheries","Unpowered small-scale fisheries","Distant-water fisheries beyond C4S"),
                    values = c("brown","green","green3","blue"))+
  geom_vline(xintercept = c(1953,1978,1985,1990,2008),colour="black",linetype="dotted",lwd=1.5)+
  theme(legend.position = "top",
        axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(color = "black", size = 11, face = "bold"),
        axis.title.y = element_text(color = "black", size = 11, face = "bold"),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11, face = "bold"))
p1

library(scales)
p1+guides(fill = guide_legend(nrow = 2,ncol=3))
####### Fig. 4 
## read data
sh<-read.csv("C_stacked.csv",header=TRUE,sep=',')
str(sh)
library(reshape2)
df<-melt(sh[,1:4],id="year")
str(df)
unique(df$variable)
df2<-melt(sh[,c(1,6:8)],id="year")
str(df2)
df3<-sh[,c(1,9)]
str(df3)
unique(df2$variable)

library(ggplot2)
df$variable <- factor(df$variable , levels=c("C_ind","C_nin","C_dw") )
p1<-ggplot()+
  geom_area(data=df,aes(x = year, y = value/1000000, fill=variable),alpha=1 , size=.2, colour="white")+
  labs(x = "", y = "Stacked marine catch by China (Mt)",face="bold")+
  scale_x_continuous(breaks=seq(1950,2018,10)
  )+
  scale_fill_manual("Fishing sectors:",
                    labels = c("Industrial fisheries within C4S","Small-scale fisheries within C4S","Distant-water fisheries beyond C4S"),
                    values = c("brown","green","blue"))+
  geom_vline(xintercept = c(1985,1999,2000,2013,2015),colour="black",linetype="dotted",lwd=1.5)+
  theme(legend.position = "top",
        axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(color = "black", size = 11, face = "bold"),
        axis.title.y = element_text(color = "black", size = 11, face = "bold",margin = margin(t = 0, r = 15, b = 0, l = 0)),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 10, face = "bold"),
        legend.justification = "top")+
  geom_line(data=df3,aes(x = year, y = C_all_FAO/1000000),lwd=1)+
  annotate("segment", x = 1970, xend = 1976, y = 4.5, yend = 3.5, colour = "black") +
  annotate("text", x=1970, y=5, label= "Reported catch (from FAO)")
p1
df2$variable <- factor(df2$variable , levels=c("R_ind","R_nin","R_dw") )
p2<-ggplot(df2,aes(x = year, y = value*100, fill=variable))+
  geom_area(alpha=1 , size=.2, colour="white")+
  scale_x_continuous(breaks=seq(1950,2018,10)
  )+
  scale_fill_manual("Fishing sectors:",
                    labels = c("Industrial (within C4S)","Small-scale (within C4S)","Distant-water (beyond C4S)"),
                    values = c("brown","green","blue"))+
  labs(x = "Year", y = "Stacked percentage of catch (%)",face="bold")+
  geom_vline(xintercept = c(1985,1996),colour="black",linetype="dotted",lwd=1.5)+
  theme(legend.position = "top",
        axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(color = "black", size = 11, face = "bold"),
        axis.title.y = element_text(color = "black", size = 11, face = "bold",margin = margin(t = 0, r = 15, b = 0, l = 0)),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 10, face = "bold"),
        legend.justification = "top")
p2

library(ggpubr)
## For Figure S2.11
ggarrange(p1,p2,ncol=1,nrow=2,common.legend = T,
          labels=c("a)", "b)")
)

####### Fig. 5 CPUP
## read data
sh<-read.csv("Catch_all 2020.csv",header=TRUE,sep=',')
str(sh)
#tiff("Figure/Fig.2 beyond C4S.tif",width = 12, height = 4.4, units = 'in', res = 300)
par(mfrow=c(2,2),mar=c(2,2,2,1))
# a) all
max<-max(na.omit(sh$CPUP_n+2*sh$CPUP_n_se))
max
min<-min(na.omit(sh$CPUP_e-2*sh$CPUP_e_se1))
min
plot(sh$CPUP_n~sh$year,type='l',lwd=2,col="black",xlim=c(1950,2018),ylim=c(min,max),ylab="",xlab="",axes=F)
polygon(x=c(sh$year,rev(sh$year)),
        y=c(sh$CPUP_n-2*sh$CPUP_n_se, rev(sh$CPUP_n+2*sh$CPUP_n_se)),
        col=adjustcolor("black",alpha.f = 0.2),border = NA)
box()
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(1950,2018,10)
axis(side = 1, at = ticks)
ticks = seq(0,15,1)
axis(side = 2, at = ticks,tck=-0.01,labels = NA)
ticks = seq(0,15,3)
axis(side = 2, at = ticks, tck=-0.03,labels = ticks)
par(new=T)
plot(sh$CPUP_e~sh$year,type='l',lwd=2,col="red",xlim=c(1950,2018),ylim=c(min,max),ylab="",xlab="",axes=F)
polygon(x=c(sh$year,rev(sh$year)),
        y=c(sh$CPUP_e-2*sh$CPUP_e_se1, rev(sh$CPUP_e+2*sh$CPUP_e_se2)),
        col=adjustcolor("red",alpha.f = 0.2),border = NA)
legend("topright",legend=c(as.expression("Nominal CPUE (t·"~kW^-1~"·"~year^-1~")"), as.expression("Effective CPUE (t·"~kW^-1~"·"~year^-1~")")),
       text.col=c("black","red"),pch=NA,lty="solid",col=c("black","red"))## Add Legend
mtext("a) All marine fishing sectors in China",side=3,adj=0,cex=1,font=2)

# b)
max<-max(na.omit(sh$CPUP_i_n+2*sh$CPUP_i_n_se))
max
min<-min(na.omit(sh$CPUP_i_e-2*sh$CPUP_i_e_se1))
min
plot(sh$CPUP_i_n~sh$year,type='l',lwd=2,col="black",xlim=c(1950,2018),ylim=c(min,max),ylab="",xlab="",axes=F)
polygon(x=c(sh$year,rev(sh$year)),
        y=c(sh$CPUP_i_n-2*sh$CPUP_i_n_se, rev(sh$CPUP_i_n+2*sh$CPUP_i_n_se)),
        col=adjustcolor("black",alpha.f = 0.2),border = NA)
box()
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(1950,2018,10)
axis(side = 1, at = ticks)
ticks = seq(0,9,1)
axis(side = 2, at = ticks,tck=-0.01,labels = NA)
ticks = seq(0,9,2)
axis(side = 2, at = ticks, tck=-0.03,labels = ticks)
par(new=T)
plot(sh$CPUP_i_e~sh$year,type='l',lwd=2,col="red",xlim=c(1950,2018),ylim=c(min,max),ylab="",xlab="",axes=F)
polygon(x=c(sh$year,rev(sh$year)),
        y=c(sh$CPUP_i_e-2*sh$CPUP_i_e_se1, rev(sh$CPUP_i_e+2*sh$CPUP_i_e_se2)),
        col=adjustcolor("red",alpha.f = 0.2),border = NA)
abline(v=c(1981,1999,2009),lty="dotted",col="grey50")
text(x=1981,y=3,labels = "1981:\nInitial\nsummer\nmoratorium\n(Yellow Sea &\nEast China Sea)",pos=2,col="black",cex=1)
text(x=1999,y=2,labels = "1999:\nSummer\nmoratorium\n(across C4S)",pos=2,col="black",cex=1)
text(x=2009,y=5,labels = "2009:\nSummer\nmoratorium\n(extend by\nhalf month)",pos=2,col="black",cex=1)
mtext("b) Industrial fisheries within C4S",side=3,adj=0,cex=1,font=2)

## c)
max<-max(na.omit(sh$CPUP_ni_n+2*sh$CPUP_ni_n_se))
max
min<-min(na.omit(sh$CPUP_ni_e-2*sh$CPUP_ni_e_se1))
min
plot(sh$CPUP_ni_n~sh$year,type='l',lwd=2,col="black",xlim=c(1950,2018),ylim=c(min,max),ylab="",xlab="",axes=F)
polygon(x=c(sh$year,rev(sh$year)),
        y=c(sh$CPUP_ni_n-2*sh$CPUP_ni_n_se, rev(sh$CPUP_ni_n+2*sh$CPUP_ni_n_se)),
        col=adjustcolor("black",alpha.f = 0.2),border = NA)
box()
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(1950,2018,10)
axis(side = 1, at = ticks)
ticks = seq(0,21,1)
axis(side = 2, at = ticks,tck=-0.01,labels = NA)
ticks = seq(0,21,3)
axis(side = 2, at = ticks, tck=-0.03,labels = ticks)
par(new=T)
plot(sh$CPUP_ni_e~sh$year,type='l',lwd=2,col="red",xlim=c(1950,2018),ylim=c(min,max),ylab="",xlab="",axes=F)
polygon(x=c(sh$year,rev(sh$year)),
        y=c(sh$CPUP_ni_e-2*sh$CPUP_ni_e_se1, rev(sh$CPUP_ni_e+2*sh$CPUP_ni_e_se2)),
        col=adjustcolor("red",alpha.f = 0.2),border = NA)
abline(v=c(1981,1999,2009),lty="dotted",col="grey50")
mtext("c) Small-scale fisheries within C4S",side=3,adj=0,cex=1,font=2)


## d)
max<-max(na.omit(sh$CPUP_dw_n+2*sh$CPUP_dw_n_se))
max
min<-min(na.omit(sh$CPUP_dw_e-2*sh$CPUP_dw_e_se1))
min
plot(sh$CPUP_dw_n~sh$year,type='l',lwd=2,col="black",xlim=c(1985,2018),ylim=c(min,max),ylab="",xlab="",axes=F)
polygon(x=c(sh$year,rev(sh$year)),
        y=c(sh$CPUP_dw_n-2*sh$CPUP_dw_n_se, rev(sh$CPUP_dw_n+2*sh$CPUP_dw_n_se)),
        col=adjustcolor("black",alpha.f = 0.2),border = NA)
box()
ticks = seq(1985,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(1985,2018,5)
axis(side = 1, at = ticks)
ticks = seq(0,67,1)
axis(side = 2, at = ticks,tck=-0.01,labels = NA)
ticks = seq(0,67,10)
axis(side = 2, at = ticks, tck=-0.03,labels = ticks)
par(new=T)
plot(sh$CPUP_dw_e~sh$year,type='l',lwd=2,col="red",xlim=c(1985,2018),ylim=c(min,max),ylab="",xlab="",axes=F)
polygon(x=c(sh$year,rev(sh$year)),
        y=c(sh$CPUP_dw_e-2*sh$CPUP_dw_e_se1, rev(sh$CPUP_dw_e+2*sh$CPUP_dw_e_se2)),
        col=adjustcolor("red",alpha.f = 0.2),border = NA)
mtext("d) Distant-water fisheries beyond C4S",side=3,adj=0,cex=1,font=2)
#dev.off()

#### Fig. 6
## read data
sh<-read.csv("CPUE_compare.csv",header=TRUE,sep=',')
str(sh)
#tiff("Figure/Fig.1 within & beyond C4S.tif",width = 12, height = 8.8, units = 'in', res = 300)
par(mar=c(2,3.5,1,1),mfrow=c(2,1),oma=c(0,0,0,0),las=1)
# a) all sectors
str(sh)
max2<-max(na.omit(sh$RCPUE_ni_i+2*sh$RCPUE_ni_i_se))
max2
min2<-min(na.omit(sh$RCPUE_ni_i-2*sh$RCPUE_ni_i_se))
min2
plot(sh$RCPUE_ni_i~sh$year,type='l',lwd=2,pch=2,col="black",ylab="",xlab="",xlim=c(1950,2018),ylim=c(min2,max2),xaxt='n',yaxt='n')
polygon(x=c(sh$year,rev(sh$year)),
        y=c((sh$RCPUE_ni_i-2*sh$RCPUE_ni_i_se), rev((sh$RCPUE_ni_i+2*sh$RCPUE_ni_i_se))),
        col=adjustcolor("black",alpha.f = 0.2),border = NA)
abline(h=1,lty="dotted",col="blue")
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(1950,2018,10)
axis(side = 1, at = ticks,tck=-0.03,labels = ticks)
ticks = seq(0,20,1)
axis(side = 2, at = ticks,tck=-0.01,col.ticks = "black",labels = NA)
ticks = seq(0,20,3)
axis(side = 2, at = ticks,tck=-0.03,labels = ticks)
mtext("CPUE ratio",side=2,cex=1,font=2,line=2.5,las=0)
par(new=T)
plot(sh$RCPUEE_ni_i~sh$year,type='l',lwd=2,pch=2,col="red",ylab="",xlab="",xlim=c(1950,2018),ylim=c(min2,max2),xaxt='n',yaxt='n')
polygon(x=c(sh$year,rev(sh$year)),
        y=c((sh$RCPUEE_ni_i-2*sh$RCPUEE_ni_i_se1), rev((sh$RCPUEE_ni_i+2*sh$RCPUEE_ni_i_se2))),
        col=adjustcolor("red",alpha.f = 0.2),border = NA)
legend("topright",legend=c("Nominal", "Effective"),
       text.col=c("black","red"),pch=NA,lty="solid",col=c("black","red"))## Add Legend

mtext("a) Small-scale fisheries vs. industrial fisheries within C4S",side=3,adj=0,cex=1,font=2)

##b)
max2<-max(na.omit(sh$RCPUEE_di_dw+2*sh$RCPUEE_di_dw_se2))
max2
min2<-min(na.omit(sh$RCPUE_di_dw-2*sh$RCPUE_di_dw_se))
plot(sh$RCPUE_di_dw~sh$year,type='l',lwd=2,pch=2,col="black",ylab="",xlab="",xlim=c(1985,2018),ylim=c(min2,max2),xaxt='n',yaxt='n')
polygon(x=c(sh$year,rev(sh$year)),
        y=c((sh$RCPUE_di_dw-2*sh$RCPUE_di_dw_se), rev((sh$RCPUE_di_dw+2*sh$RCPUE_di_dw_se))),
        col=adjustcolor("black",alpha.f = 0.2),border = NA)
abline(h=1,lty="dotted",col="blue")
ticks = seq(1985,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(1985,2018,10)
axis(side = 1, at = ticks,tck=-0.03,labels = ticks)
ticks = seq(0,220,10)
axis(side = 2, at = ticks,tck=-0.01,col.ticks = "black",labels = NA, col = "black",col.axis="black")
ticks = seq(0,220,40)
axis(side = 2, at = ticks,tck=-0.03,labels = ticks,col.ticks = "black", col = "black",col.axis="black")
par(new=T)
plot(sh$RCPUEE_di_dw~sh$year,type='l',lwd=2,pch=2,col="red",ylab="",xlab="",xlim=c(1985,2018),ylim=c(min2,max2),xaxt='n',yaxt='n')
polygon(x=c(sh$year,rev(sh$year)),
        y=c((sh$RCPUEE_di_dw-2*sh$RCPUEE_di_dw_se1), rev((sh$RCPUEE_di_dw+2*sh$RCPUEE_di_dw_se2))),
        col=adjustcolor("red",alpha.f = 0.2),border = NA)
mtext("b) Distant-water fisheries beyond C4S vs. industrial fisheries within C4S",side=3,adj=0,cex=1,font=2)
mtext("CPUE ratio",side=2,cex=1,font=2,line=2.5,las=0)


#### Fig. A9
## read data
sh<-read.csv("H_compare2.csv",header=TRUE,sep=',')
str(sh)
sh2<-sh[1:39,]
sh2<-sh[39:69,]
#tiff("Figure/Fig.1 within & beyond C4S.tif",width = 12, height = 8.8, units = 'in', res = 300)
par(mar=c(2,3.5,1,1),mfrow=c(2,1),oma=c(0,0,0,0),las=1)
# a) all sectors
str(sh)
max2<-max(sh1$TH)/1000000
max2
min2<-min(na.omit(sh1$H_i-2*sh1$H_i_se))/1000000
min2
plot(sh1$TH/1000000~sh1$Year,type='l',lwd=2,col="black",ylab="",xlab="",xlim=c(1950,1988),ylim=c(min2,max2),xaxt='n',yaxt='n')
lines(sh1$Total/1000000~sh1$Year,type='l',lty="dashed", lwd=1,col="black")

lines(sh1$H_ind1/1000000~sh1$Year,type='l',lwd=2,pch=2,col="red")
lines(sh1$H_ind/1000000~sh1$Year,type='l',lty="dashed",lwd=1,pch=2,col="red")

lines(sh1$H_art1/1000000~sh1$Year,type='l',lwd=2,pch=2,col="green3")
lines(sh1$H_art/1000000~sh1$Year,type='l',lty="dashed",lwd=1,pch=2,col="green3")

lines(sh1$H_up/1000000~sh1$Year,type='l',lwd=2,pch=2,col="grey50")

ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(1950,2018,10)
axis(side = 1, at = ticks,tck=-0.03,labels = ticks)
ticks = seq(0,6,.1)
axis(side = 2, at = ticks,tck=-0.01,col.ticks = "black",labels = NA)
ticks = seq(0,6,1)
axis(side = 2, at = ticks,tck=-0.03,labels = ticks)
mtext("Engine power (GW)",side=2,cex=1,font=2,line=2.5,las=0)
legend("topleft",legend=c("Industrail + powered small-scale (this study)", "Industrail + powered small-scale (Rousseau et al. 2019)",
                          "Industrial (this study)","Industrial (Rousseau et al. 2019)",
                          "Powered small-scale (this study)","Powered small-scale (Rouseau et a. 2019)",
                          "Unpowered small-scale (this study)"),
       text.col=c("black","black","red","red","green3","green3","grey50"),pch=NA,lty=c("solid","dashed","solid","dashed","solid","dashed","solid"),col=c("black","black","red","red","green3","green3","grey50"))## Add Legend

mtext("a) Comparison from 1950 to 1988",side=3,adj=0,cex=1,font=2)

##b)
max2<-max(sh2$TH)/1000000
max2
min2<-min(na.omit(sh2$H_art),sh2$H_up)/1000000
min2
sh2
plot(sh2$TH/1000000~sh2$Year,type='l',lwd=2,col="black",ylab="",xlab="",xlim=c(1988,2018),ylim=c(min2,max2),xaxt='n',yaxt='n')
lines(sh2$Total/1000000~sh2$Year,type='l',lty="dashed", lwd=1,col="black")

lines(sh2$H_ind1/1000000~sh2$Year,type='l',lwd=2,pch=2,col="red")
lines(sh2$H_ind/1000000~sh2$Year,type='l',lty="dashed",lwd=1,pch=2,col="red")

lines(sh2$H_art1/1000000~sh2$Year,type='l',lwd=2,pch=2,col="green3")
lines(sh2$H_art/1000000~sh2$Year,type='l',lty="dashed",lwd=1,pch=2,col="green3")

lines(sh2$H_up/1000000~sh2$Year,type='l',lwd=2,pch=2,col="grey50")

ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(1950,2018,10)
axis(side = 1, at = ticks,tck=-0.03,labels = ticks)
ticks = seq(0,18,1)
axis(side = 2, at = ticks,tck=-0.01,col.ticks = "black",labels = NA)
ticks = seq(0,18,3)
axis(side = 2, at = ticks,tck=-0.03,labels = ticks)
mtext("Engine power (GW)",side=2,cex=1,font=2,line=2.5,las=0)
mtext("b) Comparison from 1988 to 2019",side=3,adj=0,cex=1,font=2)


### The End ###########
