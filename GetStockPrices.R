# Script to get historical data for a set of tickers

tickers<-read.csv("./data/tickers.csv",stringsAsFactors = FALSE)

# # Quandl version
# library("Quandl")
# Quandl.api_key(Sys.getenv("QUANDL_TOKEN"))
# sdate<-as.Date("2011/12/31","%Y/%m/%d")
# edate<-as.Date("2016/12/31","%Y/%m/%d")
# qcodes<-paste0("YAHOO/",tickers[,1])
# 
# if(!file.exists("./data/quandldata.rds")){
#     # The following will put all the data into one data frame and save it
#     qdata <- Quandl(qcodes,type="xts",start_date=sdate,end_date=edate)
#     saveRDS(qdata,"./data/quandldata.rds")    
# } else {
#     qdata<-readRDS("./data/quandldata.rds")
# }

# Quantmod version
sdate<-"2011-12-30"
edate<-"2016-12-31"
library(quantmod)
if(!file.exists("./data/qmoddata.rds")){
    data.env <- new.env()
    tickers.missing=character()
    for(i in tickers[,1]){
        print(i)
        tryCatch({
            getSymbols(i, env=data.env,auto.assign = TRUE, from=sdate, to=edate)
        }, warning=function(w){
            tickers.missing<-c(tickers.missing,i)
        },
        error=function(e){
            tickers.missing<-c(tickers.missing,i)
        })
    }
    saveRDS(data.env,"./data/qmoddata.rds")
} else {
    data.env<-readRDS("./data/qmoddata.rds")    
}

tickers.found<-ls(data.env)
tickers.missing<-tickers[sapply(tickers[,1],function(x) length(grep(x,tickers.found)))==0,1]

# We can get an idea of hom many stocks have how many prices
number.of.prices<-sapply(tickers.found,function(x) nrow(data.env[[x]]))
summary(number.of.prices)

idx<-number.of.prices<max(number.of.prices)
sum(idx)
tickers.found[idx]
