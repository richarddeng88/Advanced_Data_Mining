### read the data first
card = read.table("data/fraud/card.asc",sep = ";", header=T)
account = read.table("data/fraud/account.asc",sep = ";", header=T)
        # frequency stands for:         
            # "POPLATEK MESICNE" stands for monthlyissuance
            # "POPLATEK TYDNE" stands for weekly issuance
            # "POPLATEK PO OBRATU" stands for issuance

client = read.table("data/fraud/client.asc",sep = ";", header=T)
disposition = read.table("data/fraud/disp.asc",sep = ";", header=T)
district = read.table("data/fraud/district.asc",sep = ";", header=T)
order = read.table("data/fraud/order.asc",sep = ";", header=T)
transaction = read.table("data/fraud/trans.asc",sep = ";", header=T)

## transform
    ## client - parse birth-info into year, month, day and gender. 
    client$year <- floor(client$birth_number/10000) + 1900
    client$month <- floor(client$birth_number%%10000/100)
    client$day <- floor(client$birth_number%%100)
    client$gender <- 0
    client[client$month>12,]$gender <- "Female"
    client[client$month<=12,]$gender <- 'Male'
    client[client$month>12,]$month <- client[client$month>12,]$month - 50

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
## simulate transaction data
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





german <- read.csv("data/fraud/german_credit.csv")

