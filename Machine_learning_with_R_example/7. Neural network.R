concrete <- read.csv("data/Machine-Learning-with-R-datasets/concrete.csv")

normalize <- function(x) {return((x-min(x))/(max(x)-min(x)))}

concrete_norm <- data.frame(sapply(concrete, normalize))

summary(concrete_norm)

## DATA SPLITTING
library(caret)
intrain <- createDataPartition(y=concrete_norm$strength, p=0.75, list = F)
training <- concrete_norm[intrain, ]
testing <- concrete_norm[-intrain,]

## TRAINING THE MODEL
library(neuralnet)
concrete_model <- neuralnet(strength~cement + slag + ash + water+ superplastic + 
                                coarseagg + fineagg + age, data=training)
plot(concrete_model)

## EVALUATING MODEL PERFORMANCE
model_result <- compute(concrete_model, testing[,1:8])
pred <- model_result$net.result
# because it's not classification problem, we use correlation instead of confusion matrix. 
cor(pred, testing$strength)

## IMPROVING PERFORMANCE
concrete_model <- neuralnet(strength~cement + slag + ash + water+ superplastic + 
                                    coarseagg + fineagg + age, data=training, hidden = 5)
plot(concrete_model)

model_result <- compute(concrete_model, testing[,1:8])
pred <- model_result$net.result
cor(pred, testing$strength)








