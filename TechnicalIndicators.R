#Create Indicators
library(TTR)
# For testing, load prices for a stock
data.env<-readRDS("./data/qmoddata.rds")
ticker<-"JPM"
p<-data.env[[ticker]]
colnames(p)<-c("Open","High","Low","Close","Volume","Adjusted")

# The following 4 lines are to make O/H/L/C consistent with Adjusted prices so
# that the med, typ and wc values are not impacted by splits and dividends.
p[,"Open"]<-p[,"Open"]*(p[,"Adjusted"]/p[,"Close"])
p[,"High"]<-p[,"High"]*(p[,"Adjusted"]/p[,"Close"])
p[,"Low"]<-p[,"Low"]*(p[,"Adjusted"]/p[,"Close"])
p[,"Close"]<-p[,"Adjusted"] # p[,"Close"]*(p[,"Adjusted"]/p[,"Close"])
# med price = (high + low)/2
# typical price = (high + low + close)/3
# Weighted Close is calculated as:       (High + Low + Close * 2 ) / 4
p$med<-(Hi(p)+Lo(p))/2
p$typ<-(Hi(p)+Lo(p)+Cl(p))/3
p$wc<-(Hi(p)+Lo(p)+2*Cl(p))/4

result<-data.frame(Y=rep(NA,nrow(p)),row.names = index(p))

temp<-EMA(Ad(p),n=10,ratio=0.9)
result$EMA1Ad<-temp/Ad(p)
result$EMA1Med<-temp/p$med
result$EMA1Typ<-temp/p$typ
result$EMA1WC<-temp/p$wc

temp<-EMA(Ad(p),n=10,ratio=0.84)
result$EMA2Ad<-temp/Ad(p)
result$EMA2Med<-temp/p$med
result$EMA2Typ<-temp/p$typ
result$EMA2WC<-temp/p$wc

temp<-EMA(Ad(p),n=10,ratio=0.78)
result$EMA3Ad<-temp/Ad(p)
result$EMA3Med<-temp/p$med
result$EMA3Typ<-temp/p$typ
result$EMA3WC<-temp/p$wc

result$SMA1<-SMA(Ad(p),10)/Ad(p)
result$SMA2<-SMA(Ad(p),16)/Ad(p)
result$SMA3<-SMA(Ad(p),22)/Ad(p)

#Bollinger Bands
# PctB = (Cl-dn)/(up-dn)
# BBandRules are categorical
# slightly different from paper. Price was above bottom, now below is a buy +1.
# and price was below top, now above is a sell -1; otherwise hold 0
temp<-BBands(Ad(p),n=20,sd=2)
result$BBands1PctB<-temp$pctB  # Not in paper, adding anyway
result$BBand1Rule<-0 # hold 
idx<- lag(Ad(p))>=temp$dn & Ad(p)<temp$dn 
result$BBand1Rule[idx]<-1 # buy
idx<- lag(Ad(p))<=temp$up & Ad(p)>temp$up 
result$BBand1Rule[idx]<- -1 # sell

temp<-BBands(Ad(p),n=26,sd=2)
result$BBands2PctB<-temp$pctB  # Not in paper, adding anyway
result$BBand2Rule<-0 # hold 
idx<- lag(Ad(p))>=temp$dn & Ad(p)<temp$dn 
result$BBand2Rule[idx]<-1 # buy
idx<- lag(Ad(p))<=temp$up & Ad(p)>temp$up 
result$BBand2Rule[idx]<- -1 # sell

temp<-BBands(Ad(p),n=32,sd=2)
result$BBands3PctB<-temp$pctB  # Not in paper, adding anyway
result$BBand3Rule<-0 # hold 
idx<- lag(Ad(p))>=temp$dn & Ad(p)<temp$dn 
result$BBand3Rule[idx]<-1 # buy
idx<- lag(Ad(p))<=temp$up & Ad(p)>temp$up 
result$BBand3Rule[idx]<- -1 # sell
