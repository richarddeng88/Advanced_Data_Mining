# libraries needed by caret
library(klaR)
library(MASS)
# for the Naive Bayes modelling
library(caret)
# to process the text into a corpus
library(tm)
# to get nice looking tables
library(pander)
# to simplify selections
library(dplyr)
library(doMC)


if (!file.exists("data/machine_learning_w_r/smsspamcollection.zip")) {
    download.file(url="http://www.dt.fee.unicamp.br/~tiago/smsspamcollection/smsspamcollection.zip",
                  destfile="data/machine_learning_w_r/smsspamcollection.zip")
}

sms_raw <- read.table(unzip("data/machine_learning_w_r/smsspamcollection.zip","SMSSpamCollection.txt"),
                      header=FALSE, sep="\t", quote="", stringsAsFactors=FALSE)
colnames(sms_raw) <- c("type", "text")
sms_raw$type <- factor(sms_raw$type)
# randomize it a bit
set.seed(12358)
sms_raw <- sms_raw[sample(nrow(sms_raw)),]
str(sms_raw)



