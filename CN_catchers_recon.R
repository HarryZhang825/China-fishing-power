
##################################################################################################################################################

### From here, we start to estimate fishing capacity of Chinese motorized catchers (hereafter, catchers)
#################### estimate catchers from trawlers from 1950 - 2002
setwd("C:/Users/xiong/Desktop/MS_history of BTF China/Fisheries Research")
#################### estimate catchers from production vessels from 1950 - 2002
df<-read.csv("FP_mv_pv.csv",header=TRUE,sep=',')
str(df)
head(df)
### arima for R_pm (Ratio between production vessels and catchers by number)
df3<-df[seq(dim(df)[1],1),]
library(forecast)
df3
ari0<-auto.arima(df3$R_pm[1:39],trace = T) # 
summary(ari0)
ari0$residuals
arim3<-forecast(ari0,h=30)
arim3
df3$R_pm_ar<-0
df3$R_pm_ar[1:39]<-ari0$fitted[1:39]
df3$R_pm_ar[40:69]<-arim3$mean
df3$R_pm_ar_se<-0
df3$R_pm_ar_se[40:69]<-(arim3$mean-arim3$lower[,2])/2
df3
df3<-df3[seq(dim(df3)[1],1),]
df3
### loess for R_pm
library(fANCOVA)
los<-loess.as(df$Year[31:69],df$R_pm[31:69],criterion = c("aicc", "gcv")[1],control = loess.control(surface = "direct")) # change N_YL to H_YL for create model for the latter
s2<-summary(los)$p$span
d2<-summary(los)$p$degree
los2<-loess(df$R_pm~df$Year,degree=d2,span=s2,control = loess.control(surface = "direct")) # change N_YL to H_YL for create model for the latter
fit2<-predict(los2,df,se=T)$fit
fit2_se<-predict(los2,df,se=T)$se.fit
ss.dist <- sum(scale(df$R_pm[31:69], scale=FALSE)^2) # change N_YL to H_YL for create model for the latter
ss.resid <- sum(resid(los2)^2)
r.sq2<- 1-ss.resid/ss.dist
fit2
fit2_se
r.sq2
df$R_pm_los<-fit2
df$R_pm_se<-fit2_se

# gam model for R_pm
library(mgcv)
df$R_pm[1]<-1
gam3<-gam(R_pm~s(Year),data=df, select=TRUE, method='GCV.Cp');BIC(gam1)
summary(gam3) # 0.975,
pred3<-predict(gam3,df,se=T)
pred3
df$R_pm_gam<-pred3$fit
df$R_pm_gam_se<-pred3$se.fit
head(df)
## for output
df0<-cbind(df,df3[,c(5,6)])
str(df0)
write.csv(df0,"FP_mv_pv_estimated.csv",col.names = F)
## Fig. A.2 plot R_pm estimates
par(mfrow=c(2,1),mar=c(3,4,1,1))
max<-max(df0$R_pm_ar+2*df0$R_pm_ar_se);max
min<-min(df0$R_pm_ar-2*df0$R_pm_ar_se,df0$R_pm_los-2*df0$R_pm_se);min
plot(df0$R_pm~df0$Year,pch=1,type="p",col="black",xlim=c(1950,2018),ylim=c(min,max),xaxt="n",xlab="",ylab="",las=1)
lines(df0$R_pm_ar~df0$Year,df,lty="solid",col="black")
polygon(x=c(df0$Year,rev(df0$Year)),
        y=c(df0$R_pm_ar-2*df0$R_pm_ar_se, rev(df0$R_pm_ar+2*df0$R_pm_ar_se)),
        col=adjustcolor("black",alpha.f = 0.2),border = NA)
lines(df$R_pm_los~df$Year,df,lty="solid",col="blue")
polygon(x=c(df0$Year,rev(df0$Year)),
        y=c(df0$R_pm_los-2*df0$R_pm_se, rev(df0$R_pm_los+2*df0$R_pm_se)),
        col=adjustcolor("blue",alpha.f = 0.2),border = NA)

Axis(side=1,at=seq(1950,2017,by=10))
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext("Ratio between FPVs and MFVs in China",side=2,col="black",line=3,las=0) 
legend("topright",legend=c("Original data", "ARIMA","LOESS"),
       text.col=c("black","black","blue"),pch=c(1,NA,NA),lty=c(NA, 'solid','solid'),col=c("black","black","blue"))## Add Legend
mtext("a)",side=3,adj=0,cex=1)
####

max<-max(df0$R_pm_gam+2*df0$R_pm_gam_se);max
min<-min(df0$R_pm_gam-2*df0$R_pm_gam_se,df0$R_pm_los-2*df0$R_pm_se);min
plot(df$R_pm~df$Year,pch=1,type="p",col="black",xlim=c(1950,2018),ylim=c(min,max),xaxt="n",xlab="",ylab="",las=1)
lines(df0$R_pm_gam~df0$Year,df,lty="solid",col="red")
polygon(x=c(df0$Year,rev(df0$Year)),
        y=c(df0$R_pm_gam-2*df0$R_pm_gam_se, rev(df0$R_pm_gam+2*df0$R_pm_gam_se)),
        col=adjustcolor("red",alpha.f = 0.2),border = NA)
Axis(side=1,at=seq(1950,2018,by=10))
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Ratio between FPVs and MFVs in China",side=2,col="black",line=3,las=0) 
legend("topright",legend=c("Original data", "GAM"),
       text.col=c("black","red"),pch=c(1,NA),lty=c(NA, 'solid'),col=c("black","red"))## Add Legend
mtext("b)",side=3,adj=0,cex=1)
#########################################################################################
### Fig. A.3. GAM for Ratio between production vessels and catchers by horsepower
df<-read.csv("FP_pv_c.csv",header=TRUE,sep=',')
str(df)
### gam for RN_c2 and R_cp between catchers and production vessels
library(mgcv)
# for R_cp
gam3<-gam(R_cp~s(Year),data=df[27:69,], select=TRUE, method='GCV.Cp');BIC(gam1)
summary(gam3) # .938, n = 36
pred3<-predict(gam3,df[27:69,],se=T)
pred3

##
## for plot
df$R_cp_gam<-NA
df$R_cp_gam_se<-NA
df$R_cp_gam[27:69]<-pred3$fit
df$R_cp_gam_se[27:69]<-pred3$se.fit
head(df)
write.csv(df,"FP_catcher_estimated.csv")
### plot the ratio
##
par(mfrow=c(1,1))
max<-max(pred3$fit+2*pred3$se.fit);max
min<-min(pred3$fit-2*pred3$se.fit);min
plot(df$R_cp~df$Year,pch=1,type="p",col="black",xlim=c(1950,2018),ylim=c(min,max),xaxt="n",xlab="",ylab="",las=1)
lines(df$R_cp_gam~df$Year,df,lty="solid",col="black")
polygon(x=c(df$Year,rev(df$Year)),
        y=c(df$R_cp_gam-2*df$R_cp_gam_se, rev(df$R_cp_gam+2*df$R_cp_gam_se)),
        col=adjustcolor("black",alpha.f = 0.2),border = NA)
Axis(side=1,at=seq(1950,2018,by=10))
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Ratio between catchers and FPVs in China (by horsepower)",side=2,col="black",line=3,las=0) 
legend("topright",legend=c("Original data", "GAM"),
       text.col=c("black","black"),pch=c(1,NA),lty=c(NA, 'solid'),col=c("black","black"))## Add Legend


### Fig. A.4 validation
df<-read.csv("FP_catcher_estimated.csv",header=TRUE,sep=',')
str(df)
#
par(mfrow=c(1,1),mar=c(4,4,1,1))
max<-max(df$H_c+2*df$H_c_se,na.omit(df$H_mv))/1000000;max
min<-min(df$H_c-2*df$H_c_se,na.omit(df$H_bt))/1000000;min
plot(df$H_c[54:69]/1000000~df$Year[54:69],pch=1,type="p",col="black",xlim=c(1950,2018),ylim=c(min,max),xlab="",ylab="",las=1)
lines((df$H_mv)/1000000~df$Year,pch=1,df,xlim=c(1950,2018),lty="solid",col="red")
lines((df$H_c_G)/1000000~df$Year,pch=1,df,xlim=c(1950,2018),lty="solid",col="blue")
lines((df$H_bt)/1000000~df$Year,pch=1,df,lty="solid",col="green")
lines(df$H_c[1:53]/1000000~df$Year[1:53],pch=1,df,xlim=c(1950,2018),lwd=1,lty="solid",col="black")
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Total engine power of vessels (GW)",side=2,col="black",line=3,las=0) 
legend("topleft",legend=c("All marine fishery vessels","Catchers (original)","Catchers (estimated)","Trawlers", "Catchers in Guangdong"),
       text.col=c("red","black","black","green","blue"),pch=c(NA,1,NA,NA,NA),lty=c("solid",NA,"solid","solid","solid"),col=c("red","black","black","green","blue"))## Add Legend

#######################################################################################
############ unpowered fishing vessels
### Fig. A.3. GAM for Ratio between production vessels and catchers by horsepower
df<-read.csv("T_H_conversion.csv",header=TRUE,sep=',')
str(df)
plot(H~T,df)
### linear model for to convert tonnage to engine power
# for
lm3<-lm(H~T-1,data=df);BIC(lm1)
summary(lm3) # .9993,
pred2<-predict(lm3,df,se=T)
pred2

newd <- data.frame(T=df$T_up[1:69]);newd
pred3<-predict(lm3,newdata = newd,vcov.=vcov,se=T)
pred3

##
## for plot
df$H_lm<-NA
df$H_lm_se<-NA
df$H_lm<-pred2$fit
df$H_lm_se<-pred2$se.fit
df$H_up[1:69]<-pred3$fit
df$H_up_se[1:69]<-pred3$se.fit
head(df)
write.csv(df,"Unpower_estimated.csv")
### plot the ratio
##
df<-df[1:47,];df
df$T<-df$T/100000
df$H<-df$H/1000000
df$H_lm<-df$H_lm/1000000
df$H_lm_se<-df$H_lm_se/1000000
par(mfrow=c(1,1),mar=c(3,4,.1,.1))
max<-max(df$T)/100000;max
max<-max(na.omit(pred2$fit+2*pred2$se.fit))/1000000;max
min<-min(na.omit(pred2$fit-2*pred2$se.fit))/1000000;min
plot(df$H~df$T,pch=1,type="p",col="black",xlab="",ylab="",las=1)
lines(df$H_lm~df$T,lty="solid",col="red")
polygon(x=c(df$T,rev(df$T)),
        y=c(df$H_lm-2*df$H_lm_se, rev(df$H_lm+2*df$H_lm_se)),
        col=adjustcolor("red",alpha.f = 0.2),border = NA)
mtext("Tonnage (x 100,000)",side=1,col="black",line=2,las=0) 
mtext("Engine power (GW)",side=2,col="black",line=3,las=0) 
legend("topleft",legend=c("Original data", "Linear regression"),
       text.col=c("black","red"),pch=c(1,NA),lty=c(NA, 'solid'),col=c("black","red"))## Add Legend


####### From here, we start to estimate fishing capacity for distant-water catchers
################### estimation for distant water vessels ################################
###
setwd("C:/Users/xiong/Desktop/MS_history of BTF China")
df<-read.csv("CN_dv.csv",header=TRUE,sep=',')
str(df)
head(df)
### arima for RN_dvc (Ratio between distant-water catchers and distant-water vessels, by number)
df3<-df
library(forecast)
df3
ari0<-auto.arima(df3$RN_dvc[1:18],trace = T) # 
summary(ari0)
arim3<-forecast(ari0,h=16)
arim3
df$RN_dvc_ar_m<-0
df$RN_dvc_ar_m[1:18]<-ari0$fitted[1:18]
df$RN_dvc_ar_m[19:34]<-arim3$mean
df$RN_dvc_ar_se<-0
df$RN_dvc_ar_se[19:34]<-(arim3$mean-arim3$lower[,2])/2
df


### arima for RH_dvc (Ratio between distant-water catchers and distant-water vessels, by horsepower)
library(forecast)
df3
ari0<-auto.arima(df3$RH_dvc[1:18],trace = T) # 
summary(ari0)
ari0$fitted
arim3<-forecast(ari0,h=16)
arim3
df3$RH_dvc_ar_m<-0
df3$RH_dvc_ar_m[1:18]<-ari0$fitted[1:18]
df3$RH_dvc_ar_m[19:34]<-arim3$mean
df3$RH_dvc_ar_se<-0
df3$RH_dvc_ar_se[19:34]<-(arim3$mean-arim3$lower[,2])/2
df3
df3

## for output
str(df)
write.csv(df3,"CN_dvc.csv")
## Fig A21 plot RH_dvc estimates
par(mfrow=c(1,1),mar=c(4,4,1,1))
####
max<-max(df3$RH_dvc_ar_m+2*df3$RH_dvc_ar_se);max
min<-min(df3$RH_dvc_ar_m-2*df3$RH_dvc_ar_se);min
plot(df3$RH_dvc~df3$Year,pch=1,type="p",col="black",xlim=c(1985,2018),ylim=c(min,max+0.04),xaxt="n",xlab="",ylab="",las=1)
lines(df3$RH_dvc_ar_m~df3$Year,df,lty="solid",col="black")
lines((df3$RH_dvc_ar_m+2*df3$RH_dvc_ar_se)~df$Year,df,lty="dashed",col="red")
lines((df3$RH_dvc_ar_m-2*df3$RH_dvc_ar_se)~df$Year,df,lty="dashed",col="red")
abline(h=1,lty="dashed",col="grey50")
Axis(side=1,at=seq(1985,2018,by=10))
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Ratio between DWCs and DWVs in China based on ARIMA",side=2,col="black",line=3,las=0) 
legend("topright",legend=c("Mean", "95% confidence intervals"),
       text.col=c("black","red"),lty=c("solid","dashed"),col=c("black","red"))## Add Legend

#### loess for RN_dvc
library(fANCOVA)
los<-loess.as(df$Year[1:18],df$RN_dvc[1:18],criterion = c("aicc", "gcv")[1],control = loess.control(surface = "direct")) # change N_YL to H_YL for create model for the latter
s2<-summary(los)$p$span
d2<-summary(los)$p$degree
los2<-loess(df$RN_dvc~df$Year,degree=d2,span=s2,control = loess.control(surface = "direct")) # change N_YL to H_YL for create model for the latter
fit2<-predict(los2,df,se=T)$fit
fit2_se<-predict(los2,df,se=T)$se.fit
ss.dist <- sum(scale(df$RN_dvc[1:18], scale=FALSE)^2) # change N_YL to H_YL for create model for the latter
ss.resid <- sum(resid(los2)^2)
r.sq2<- 1-ss.resid/ss.dist
fit2
fit2_se
r.sq2
## for plot
df$RN_dvc_mean<-fit2
df$RN_dvc_se<-fit2_se

### loess for RH_dvc
library(fANCOVA)
los<-loess.as(df$Year[1:18],df$RH_dvc[1:18],criterion = c("aicc", "gcv")[1],control = loess.control(surface = "direct")) # change N_YL to H_YL for create model for the latter
s2<-summary(los)$p$span
d2<-summary(los)$p$degree
los2<-loess(df$RH_dvc~df$Year,degree=d2,span=s2,control = loess.control(surface = "direct")) # change N_YL to H_YL for create model for the latter
fit2<-predict(los2,df,se=T)$fit
fit2_se<-predict(los2,df,se=T)$se.fit
ss.dist <- sum(scale(df$RH_dvc[1:18], scale=FALSE)^2) # change N_YL to H_YL for create model for the latter
ss.resid <- sum(resid(los2)^2)
r.sq2<- 1-ss.resid/ss.dist
fit2
fit2_se
r.sq2
## for plot
df$RH_dvc_mean<-fit2
df$RH_dvc_se<-fit2_se
df
## Fig A22 plot RH_dvc estimates
par(mfrow=c(1,1),mar=c(4,4,1,1))
####
max<-max(df$RH_dvc_mean+2*df$RH_dvc_se,na.omit(df$RH_dvc));max
min<-min(df$RH_dvc_mean-2*df$RH_dvc_se);min
plot(df$RH_dvc~df3$Year,pch=1,type="p",col="black",xlim=c(1985,2018),ylim=c(min,1.1),xaxt="n",xlab="",ylab="",las=1)
lines(df$RH_dvc_mean~df$Year,df,lty="solid",col="black")
lines((df$RH_dvc_mean+2*df$RH_dvc_se)~df$Year,df,lty="dashed",col="red")
lines((df$RH_dvc_mean-2*df$RH_dvc_se)~df$Year,df,lty="dashed",col="red")
abline(h=1,lty="dashed",col="grey50")
abline(v=2015,lty="dashed",col="grey50")
Axis(side=1,at=seq(1985,2018,by=10))
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Ratio between DWCs and DWVs in China based on LOESS",side=2,col="black",line=3,las=0) 
legend("topleft",legend=c("Mean", "95% confidence intervals"),
       text.col=c("black","red"),lty=c("solid","dashed"),col=c("black","red"))## Add Legend
## calcuate mean and se directly for RH_dvc
RN_mean<-mean(na.omit(df$RN_dvc))
RN_se<-sd(na.omit(df$RN_dvc))/sqrt(18)
RN_mean
RN_se
RH_mean<-mean(na.omit(df$RH_dvc))
RH_se<-sd(na.omit(df$RH_dvc))/sqrt(18)
df$N_dvc_m<-df$N_dvc
df$N_dvc_se<-0
df$H_dvc_m<-df$H_dvc
df$H_dvc_se<-0
df
for(i in 1:34){
  if(is.na(df$N_dvc_m[i])){
    df$N_dvc_m[i]<-df$N_dv[i]*RN_mean
    df$N_dvc_se[i]<-df$N_dv[i]*RN_se
    df$H_dvc_m[i]<-df$H_dv[i]*RH_mean
    df$H_dvc_se[i]<-df$H_dv[i]*RH_se
  }
}
df

## for output
str(df)
write.csv(df,"CN_dvc_estimated2.csv")
df3<-df
####
max<-max(df3$H_dvc_m+2*df3$H_dvc_se,df$H_dv)/1000000;max
min<-min(df3$H_dvc_m-2*df3$H_dvc_se)/1000000;min
plot(df3$H_dvc/1000000~df3$Year,pch=1,type="p",col="black",xlim=c(1985,2018),ylim=c(min,max+0.04),xaxt="n",xlab="",ylab="",las=1)
lines(df3$H_dvc_m/1000000~df3$Year,df,lty="solid",col="black")
lines((df3$H_dvc_m+2*df3$H_dvc_se)[18:34]/1000000~df$Year[18:34],df,lty="dashed",col="red")
lines((df3$H_dvc_m-2*df3$H_dvc_se)[18:34]/1000000~df$Year[18:34],df,lty="dashed",col="red")
lines(df3$H_dv/1000000~df$Year,df,lty="solid",col="blue")
Axis(side=1,at=seq(1985,2018,by=10))
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Total horsepower of vessels (million kW)",side=2,col="black",line=3,las=0) 
legend("topleft",legend=c("Distant-water vessels (reported)","Distant-water catchers (Mean, estimated)", "Distant-water catchers (95% CI, estimated)"),
       text.col=c("blue","black","red"),lty=c("solid","solid","dashed"),col=c("blue","black","red"))## Add Legend
#################################################################################################################################
#############################################################################################################################
