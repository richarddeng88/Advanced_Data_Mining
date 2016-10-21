library(dplyr)
### read the data first
card = read.table("data/fraud/card.asc",sep = ";", header=T)
account = read.table("data/fraud/account.asc",sep = ";", header=T)
client = read.table("data/fraud/client.asc",sep = ";", header=T)
disposition = read.table("data/fraud/disp.asc",sep = ";", header=T)
district = read.table("data/fraud/district.asc",sep = ";", header=T)
order = read.table("data/fraud/order.asc",sep = ";", header=T)
trans = read.table("data/fraud/trans.asc",sep = ";", header=T)


write.csv(card, file = "data/fraud/card.csv",row.names = F)
write.csv(account, file = "data/fraud/account.csv",row.names = F)
write.csv(client, file = "data/fraud/client.csv",row.names = F)
write.csv(disposition, file = "data/fraud/disposition.csv",row.names = F)
write.csv(district, file = "data/fraud/district.csv",row.names = F)
write.csv(order, file = "data/fraud/order.csv",row.names = F)
write.csv(transaction, file = "data/fraud/transaction.csv",row.names = F)

## transform
    ## client - parse birth-info into year, month, day and gender. 
    client$year <- floor(client$birth_number/10000) + 1900
    client$month <- floor(client$birth_number%%10000/100)
    client$day <- floor(client$birth_number%%100)
    client$gender <- 0
    client[client$month>12,]$gender <- "Female"
    client[client$month<=12,]$gender <- 'Male'
    client[client$month>12,]$month <- client[client$month>12,]$month - 50


# merge data set
    a = merge(disposition, account, by= "account_id")
    b = merge(a,client,by=c("client_id","district_id"))
    df = merge(b, card, by="disp_id", all = T)
    
## count the missing value
    sum(is.na(df$card_id))
    sapply(df, function(x){sum(is.na(x))})

    
    a
    
    
    
    
    
    
    
    
    
    
    
library(arules)    
trans <- random.transactions(nItems = 200, nTrans = 1000, 
                                 lambda = 5, iProb = seq(0.2,0.0001, length=200))
## size distribution
size(trans)
trasummary(size(trans))
## display random data set
image(trans)

## use the method by Agrawal and Srikant (1994) to simulate transactions 
## which contains correlated items. This should create data similar to
## T10I4D100K (we just create 100 transactions here to speed things up).
patterns <- random.patterns(nItems = 1000)
summary(patterns)


library(foreign)
credit <- read.arff("data/fraud2/credit-g.arff")

german <- read.csv("data/fraud/german_credit.csv")

