## you already have a model, but every model has its own parameters setting. 
## How to automate model performance tuning by searching for the optimal set of training conditions?
library(caret)
modelLookup("C5.0")  # use modelLookup() function to find the tuning parameters. 
modelLookup("rpart")
modelLookup("glm")

## Creating a simple tuned model
credit <- read.csv("data/Machine-Learning-with-R-datasets/credit.csv")
credit$default <- factor(credit$default, levels=c(1,2), labels = c("yes","no"))

library(caret)
set.seed(300)
m <- train(default ~ ., data = credit, method = "C5.0")














