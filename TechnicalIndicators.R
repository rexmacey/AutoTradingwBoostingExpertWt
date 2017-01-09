#Create Indicators
library(TTR)

# Crossover signal with 2 series (e.g. Price and one moving average)
# Zero creates an additional signal when series 1 crosses above or below zero
xo_signal_1<-function(series1,series2,Zero=FALSE){
    out<-rep("Hold",nrow(series1))
    idx <- (lag(series1) <= lag(series2)) & (series1 > series2)
    out[idx]<-"xabove"
    idx <- (lag(series1) >= lag(series2)) & (series1 < series2)
    out[idx]<-"xbelow"
    if(Zero){
        idx <- (lag(series1) <= 0) & (series1 > 0)
        out[idx]<-"xabove0"
        idx <- (lag(series1) >= 0) & (series1 < 0)
        out[idx]<-"xbelow0"
    }
    out<-factor(out,ordered = FALSE)
    return(out)
}
# Crossover signal with 3 series (e.g. Price and two Bollinger bands)
# Buy when 1 crosses below 2; Sell when 1 crosses above 3
# If CrossBack then additional signals are generated with series one crosses back
xo_signal_2<-function(series1,seriesLow,seriesHigh,CrossBack=FALSE){
    out<-rep("Hold",nrow(series1))
    idx <- (lag(series1) >= lag(seriesLow)) & (series1 < seriesLow)
    out[idx]<-"xbelowbot"  # cross bottom from top
    idx <- (lag(series1) <= lag(seriesHigh)) & (series1 > seriesHigh)
    out[idx]<-"xabovetop" # cross top from below
    if(CrossBack){
        idx <- (lag(series1) < lag(seriesLow)) & (series1 >= seriesLow)
        out[idx]<-"xabovebot" # cross bottom from below
        idx <- (lag(series1) > lag(seriesHigh)) & (series1 <= seriesHigh)
        out[idx]<-"xbelowtop" # cross top from above
    }
    out<-factor(out,ordered = FALSE)
    return(out)
}

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
Adjp<-Ad(p) # This is to avoid repeated calls to the Ad function
result<-data.frame(Y=rep(NA,nrow(p)),row.names = index(p))

# For EMA, trying different lookbacks using Ad for all
result$EMA1<-EMA(Adjp,n=10,ratio=0.9)/Adjp
result$EMA2<-EMA(Adjp,n=16,ratio=0.9)/Adjp
result$EMA3<-EMA(Adjp,n=22,ratio=0.9)/Adjp
result$EMA4<-EMA(Adjp,n=10,ratio=0.84)/Adjp
result$EMA5<-EMA(Adjp,n=16,ratio=0.84)/Adjp
result$EMA6<-EMA(Adjp,n=22,ratio=0.84)/Adjp
result$EMA7<-EMA(Adjp,n=10,ratio=0.78)/Adjp
result$EMA8<-EMA(Adjp,n=16,ratio=0.78)/Adjp
result$EMA9<-EMA(Adjp,n=22,ratio=0.78)/Adjp

result$SMA1<-SMA(Adjp,10)/Adjp
result$SMA2<-SMA(Adjp,16)/Adjp
result$SMA3<-SMA(Adjp,22)/Adjp

#Bollinger Bands
# PctB = (Cl-dn)/(up-dn)
# BBandRules are categorical
# slightly different from paper. Price was above bottom, now below is a buy +1.
# and price was below top, now above is a sell -1; otherwise hold 0
temp<-BBands(Adjp,n=20,sd=2)
result$BBands1PctB<-temp$pctB  # Not in paper, adding anyway
result$PBoll1up<-Adjp/temp$up
result$PBoll1dn<-Adjp/temp$dn
result$BBand1Rule<-xo_signal_2(Adjp,temp$dn,temp$up,TRUE)

temp<-BBands(Adjp,n=26,sd=2)
result$BBands2PctB<-temp$pctB  # Not in paper, adding anyway
result$PBoll2up<-Adjp/temp$up
result$PBoll2dn<-Adjp/temp$dn
result$BBand2Rule<-xo_signal_2(Adjp,temp$dn,temp$up,TRUE)

temp<-BBands(Adjp,n=32,sd=2)
result$BBands3PctB<-temp$pctB  # Not in paper, adding anyway
result$PBoll3up<-Adjp/temp$up
result$PBoll3dn<-Adjp/temp$dn
result$BBand3Rule<-xo_signal_2(Adjp,temp$dn,temp$up,TRUE) 

temp<-momentum(Adjp,n=12)
temp1<-EMA(temp,n=12,ratio=0.75)
acc<-temp-lag(temp)
result$MomEMA1<-temp/temp1
result$MomRule1<-xo_signal_1(temp,temp1,FALSE)
result$AccRule1<- xo_signal_1(acc,rep(0,nrow(acc)))

temp<-momentum(Adjp,n=18)
temp1<-EMA(temp,n=18,ratio=0.75)
acc<-temp-lag(temp)
result$MomEMA2<-temp/temp1
result$MomRule2<-xo_signal_1(temp,temp1,FALSE)
result$AccRule2<- xo_signal_1(acc,rep(0,nrow(acc)))

temp<-momentum(Adjp,n=24)
temp1<-EMA(temp,n=24,ratio=0.75)
acc<-temp-lag(temp)
result$MomEMA3<-temp/temp1
result$MomRule3<-xo_signal_1(temp,temp1,FALSE)
result$AccRule3<- xo_signal_1(acc,rep(0,nrow(acc)))

temp <- ROC(Adjp,10)
result$ROC1<-temp
result$ROCRule1<-xo_signal_1(temp,rep(0,nrow(temp)))

temp <- ROC(Adjp,16)
result$ROC2<-temp
result$ROCRule2<-xo_signal_1(temp,rep(0,nrow(temp)))

temp <- ROC(Adjp,22)
result$ROC3<-temp
result$ROCRule3<-xo_signal_1(temp,rep(0,nrow(temp)))

result$MACD1<-MACD(Adjp,nFast = 12,nSlow=18,nsig=9,percent = TRUE)
temp<-MACD(Adjp,nFast = 12,nSlow=18,nsig=9,percent = FALSE)
result$MACDR1 <- temp$macd / temp$signal
result$MACDRule1<-xo_signal_1(temp$macd,temp$signal,TRUE)

result$MACD2<-MACD(Adjp,nFast = 12,nSlow=24,nsig=9,percent = TRUE)
temp<-MACD(Adjp,nFast = 12,nSlow=24,nsig=9,percent = FALSE)
result$MACDR2 <- temp$macd / temp$signal
result$MACDRule2<-xo_signal_1(temp$macd,temp$signal,TRUE)

result$MACD3<-MACD(Adjp,nFast = 12,nSlow=30,nsig=9,percent = TRUE)
temp<-MACD(Adjp,nFast = 12,nSlow=30,nsig=9,percent = FALSE)
result$MACDR3 <- temp$macd / temp$signal
result$MACDRule3<-xo_signal_1(temp$macd,temp$signal,TRUE)

temp<-RSI(Adjp,8)
result$RSI1<-temp
result$RSIRule1<-xo_signal_2(temp,rep(30,nrow(temp)),rep(70,nrow(temp)))

temp<-RSI(Adjp,14)
result$RSI2<-temp
result$RSIRule2<-0
result$RSIRule2<-xo_signal_2(temp,rep(30,nrow(temp)),rep(70,nrow(temp)))

temp<-RSI(Adjp,20)
result$RSI3<-temp
result$RSIRule3<-xo_signal_2(temp,rep(30,nrow(temp)),rep(70,nrow(temp)))

temp<-stoch(p[,c("High","Low","Close")],nFastK=12,nSlowD = 3)
result$fastk1<-temp$fastK
result$fastD1<-temp$fastD
result$slowD1<-temp$slowD
result$fastRule1<-xo_signal_1(temp$fastK,temp$fastD)
result$slowRule1<-xo_signal_1(temp$fastK,temp$slowD)
result$slowRatio1<- temp$fastK/temp$slowD
result$fastRatio1<- temp$fastK/temp$fastD

temp<-stoch(p[,c("High","Low","Close")],nFastK=18,nSlowD = 3)
result$fastk2<-temp$fastK
result$fastD2<-temp$fastD
result$slowD2<-temp$slowD
result$fastRule2<-xo_signal_1(temp$fastK,temp$fastD)
result$slowRule2<-xo_signal_1(temp$fastK,temp$slowD)
result$slowRatio2<- temp$fastK/temp$slowD
result$fastRatio2<- temp$fastK/temp$fastD

temp<-stoch(p[,c("High","Low","Close")],nFastK=24,nSlowD = 3)
result$fastk3<-temp$fastK
result$fastD3<-temp$fastD
result$slowD3<-temp$slowD
result$fastRule3<-xo_signal_1(temp$fastK,temp$fastD)
result$slowRule3<-xo_signal_1(temp$fastK,temp$slowD)
result$slowRatio3<- temp$fastK/temp$slowD
result$fastRatio3<- temp$fastK/temp$fastD