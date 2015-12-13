wine <- read.csv("data/winequality-white.CSV", header = T, sep = ";")

hist(wine$quality)

# DATA SPLITTING
library(caret)
intrain <- createDataPartition(y=wine$quality, p=0.8, list = F)
wine_train <- wine[intrain,]
wine_test <- wine[-intrain,]

# TRAIN THE TREE MODEL USING "rpart" package
    library(rpart)
    m.rpart <- rpart(quality~., data=wine_train)
    
    # ploting the tree
    library(rpart.plot)
    rpart.plot(m.rpart, digits=3)
    rpart.plot(m.rpart, digits=3, fallen.leaves = T, type=1)
    
    pre_rpart <- predict(m.rpart, wine_test)

    cor(pre_rpart, wine_test$quality)

    MAE <- function(actual, predicted) {mean(abs(actual - predicted))}
    MAE(pre_rpart, wine_test$quality)

## IMPROVING MODEL PERFORMANCE
    library(RWeka)
    m.m5p <- M5P(quality ~., data=wine_train)
    
    pre_m5p <- predict(m.m5p, wine_test)
    cor(pre_m5p, wine_test$quality)
    MAE(pre_m5p, wine_test$quality)