library(caret)
library(tm)

sms <- read.table("data/machine_learning_w_r/SMSSpamCollection.txt",
                  header=FALSE, sep="\t", quote="", stringsAsFactors=FALSE) 
# sep="\t" tells R that the file is tab-delimited (use " " for space delimited and "," for comma delimited; use "," for a .csv file).

names(sms) <- c("type","text")
sms$type <- factor(sms$type)
table(sms$type)

sms_corpus <- VCorpus(VectorSource(sms))
print(sms_corpus)




















