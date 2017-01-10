# Calculate Betas for Stocks
# Using Cov(s,m)/var(m)

# Calculate beta from two price series
calc_beta<-function(pStk,pMkt,n=30){
    require(TTR)
    r<-ROC(pStk)
    m<-ROC(pMkt)
    rollcov<-rollapply(merge(r,m),width=n,FUN=cov, by.column = FALSE)
    rollvar<-rollapply(m, width=n, var)
    return(rollcov[,2]/rollvar)
}

# Calculate betas for all tickers in data.env
data.env<-readRDS("./data/qmoddata.rds")
market<-readRDS("./data/SPY.rds")
colnames(market)<-c("Open","High","Low","Close","Volume","Adjusted")
m<-ROC(market$Adjusted)
calc_all_betas<-function(data.env,m,n=30){
    require(TTR)
    rollvar<-rollapply(m, width=n, var)
    tickers<-ls(data.env)
    out<-list()
    for(t in tickers){
        p<-data.env[[ticker]]
        colnames(p)<-c("Open","High","Low","Close","Volume","Adjusted")
        r<-ROC(p$Adjusted)
        rollcov<-rollapply(merge(r,m),width=n,FUN=cov, by.column = FALSE)
        out[[t]]<-rollcov[,2]/rollvar
    }
    return(out)
}

betas<-calc_all_betas(data.env,m,30)
saveRDS(betas,file="./data/betas.rds")
