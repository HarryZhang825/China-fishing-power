######### building up statistics from SAUP datasets (two)
# download catch datasets from 'LME' = Yellow Sea, East China Sea, and South China Sea, respectively
# unpack the file and stored it in a local space, here "C:/Users/xiong/Desktop/BTF_China/SAU C4S"
### Section 1: extract all industrial catch and industrial catch from China's four seas
setwd("C:/Users/xiong/Desktop/BTF_China")
df1<-read.csv("SAU C4S/SAU LME 36 v48-0.csv",header=TRUE,sep=',')
df2<-read.csv("SAU C4S/SAU LME 47 v48-0.csv",header=TRUE,sep=',')
df3<-read.csv("SAU C4S/SAU LME 48 v48-0.csv",header=TRUE,sep=',')
df4<-read.csv("SAU China/SAU FishingEntity 31 v48-0.csv",header=TRUE,sep=',')
str(df1);str(df2);str(df3)
df<-rbind(df1,df2,df3)
str(df)
library(data.table)
df_c4s<-subset(df,((df[,9]=="Industrial") & (df[,8]=="China")))[,c(3,14)];str(df_c4s)
str(df4)
df_cn<-subset(df4,((df4[,8]=="Industrial") & (df4[,3]>1984)))[,c(3,13)];str(df_cn)
## integrate data by year
#subset data
tm<-0
j<-0
vm<-0
head(df_c4s)
for(i in 1:67){
  j=1949+i
  tm[i]<-sum(subset(df_c4s[,2],df_c4s[,1]==j))
}
y<-seq(1950,2016,1)
tm
df<-data.frame(year=y,C_dom=tm)
head(df)
write.csv(df,"CN_dom_stat.csv") ## output
## for non-industrial catch within C4S
df_c4s<-subset(df,((df[,9]!="Industrial") & (df[,8]=="China")))[,c(3,14)];str(df_c4s)
## integrate data by year
#subset data
tm<-0
j<-0
vm<-0
head(df_c4s)
for(i in 1:67){
  j=1949+i
  tm[i]<-sum(subset(df_c4s[,2],df_c4s[,1]==j))
}
y<-seq(1950,2016,1)
tm
df<-data.frame(year=y,C_dom=tm)
head(df)
write.csv(df,"CN_dom_art_stat.csv") ## output

#subset data
tm<-0
j<-0
vm<-0
head(df_cn)
for(i in 1:32){
  j=1984+i
  tm[i]<-sum(subset(df_cn[,2],df_cn[,1]==j))
}
y<-seq(1985,2016,1)
tm
df<-data.frame(year=y,C_dom=tm)
head(df)
write.csv(df,"CN_stat 1985-2016.csv") ## output
## extrapolate based on ARIMA
## mannually add reported catch by FAO to estimate ratio between industrial catch and the reported catch, named it as "CN_Ratio.csv", and extended the year column value to 2018
df<-read.csv("CN_Ratio.csv",header=TRUE,sep=',')
str(df)
library(forecast)
## arima for Ratio1
ari0<-auto.arima(df$Ratio1,trace = T) # change N_YL to H_YL for create model for the latter
summary(ari0) # ARIMA (1,1,0) with drift 
r0<-forecast(ari0,h=2)
r0
(r0$upper[,2]-r0$mean)/2
## arima for Ratio2
ari0<-auto.arima(df$Ratio2,trace = T) # change N_YL to H_YL for create model for the latter
summary(ari0) # ARIMA (1,1,0) with drift 
r0<-forecast(ari0,h=2)
r0
(r0$upper[,2]-r0$mean)/2
## arima for Ratio3
ari0<-auto.arima(df$Ratio3,trace = T) # change N_YL to H_YL for create model for the latter
summary(ari0) # ARIMA (1,1,0) with drift 
r0<-forecast(ari0,h=2)
r0
(r0$upper[,2]-r0$mean)/2
## loess
library(fANCOVA)
los<-loess.as(df$Year[1:67],df$Ratio3[1:67],criterion = c("aicc", "gcv")[1],control = loess.control(surface = "direct")) # change N_YL to H_YL for create model for the latter
s2<-summary(los)$p$span
d2<-summary(los)$p$degree
los2<-loess(df$Ratio3~df$Year,degree=d2,span=s2,control = loess.control(surface = "direct")) # change N_YL to H_YL for create model for the latter
fit2<-predict(los2,df,se=T)$fit
fit2_se<-predict(los2,df,se=T)$se.fit
ss.dist <- sum(scale(df$Ratio3[1:67], scale=FALSE)^2) # change N_YL to H_YL for create model for the latter
ss.resid <- sum(resid(los2)^2)
r.sq2<- 1-ss.resid/ss.dist
fit2
fit2_se

