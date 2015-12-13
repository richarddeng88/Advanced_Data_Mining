credit <- read.csv("data/Machine-Learning-with-R-datasets/credit.csv")
str(credit)
credit$default <- factor(credit$default, levels=c(1,2), labels = c("yes","no"))


table(credit$checking_balance)
table(credit$savings_balance)
table(credit$default)

















