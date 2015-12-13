## you already have a model, but every model has its own parameters setting. 
## How to automate model performance tuning by searching for the optimal set of training conditions?
library(caret)
modelLookup("C5.0")  # use modelLookup() function to find the tuning parameters. 
modelLookup("rpart")
modelLookup("glm")


credit <- read.csv("data/Machine-Learning-with-R-datasets/credit.csv")
credit$default <- factor(credit$default, levels = c(1,2), labels = c("yes", "no"))

# RUN C5.0 TREE MODEL
    library(caret)
    set.seed(300)
    m <- train(default ~ ., data = credit, method = "C5.0")
    
    p <- predict(m, credit)
    p <- predict(m, credit, type = "prob")
    table( predict(m, credit), credit$default)

# CHANGE TUNING PARAMETERS - APPLY 10-FOLD CV AND DIFFERNET SELECTION RULE.
    ## to create a control object named ctrl that uses 10-fold CV and the oneSE secetion function. 
    ctrl <- trainControl(method = "cv", number = 10,selectionFunction = "oneSE") ## oneSE rule, "best" rule 
    
    ##The grid must include a column named for each parameter in the desired model, prefixed by a period.
    grid <- expand.grid(.model=c("tree","rules"), .trials=c(1,5,10,15,20,25,30,35), .winnow="FALSE")
    
    # let's now run a customized train() experiment. 
    # metric = "kappa", indecate that the statistics to be used by the model evaluation function 
    set.seed(300)
    mm <- train(default ~. , data = credit, method="C5.0", 
                metric="Kappa",
                trControl = ctrl,
                tuneGrid = grid)

## bagging
    # BAGGING FROM "ipred" PACKAGE
    library(ipred)
    set.seed(300)
    mybag <- bagging(default ~ ., data = credit, nbagg = 25)
    
    credit_pred <- predict(mybag, credit)
    table(credit_pred, credit$default)

    # CHANGE TUNING PARAMETERS - 10 FOLD CV
    library(caret)
    set.seed(300)
    ctrl <- trainControl(method = "cv", number = 10)
    train(default ~ ., data = credit, method = "treebag", trControl = ctrl)

# BAGED SVM MODEL 
    #one for fitting the model, one for making predictions and one for aggregating the votes. 
    # see the details for each function, use the following code. 
    str(svmBag)
    svmBag$fit
    svmBag$pred
    svmBag$aggregate
    
    # Applying the three functions in the svmBag list, we can create a bagging control object:
    bagctrl <- bagControl(fit = svmBag$fit,
                          predict = svmBag$pred,
                          aggregate = svmBag$aggregate)
    
    #
    set.seed(300)
    svmbag <- train(default ~ ., data = credit, "bag",
                      trControl = ctrl, bagControl = bagctrl)
    svmbag

# BOOSTING
library("adabag")
set.seed(300)
m_adaboost <- boosting(default ~ ., data = credit)
p_adaboost <- predict(m_adaboost, credit)    
p_adaboost$confusion

# 10 fold CV
set.seed(300)
adaboost_cv <- boosting.cv(default ~ ., data = credit)
adaboost_cv$confusion

library(vcd)
Kappa(adaboost_cv$confusion)


## RANDOM FOREST MODEL - ENSEMBLE-BASED METHOD 
#By default, the randomForest() function creates an ensemble of 500 trees that considervsqrt(p) random features at each split
library(randomForest)
set.seed(300)
rf <- randomForest(default ~ ., data = credit)
rf


# comparing RF and TREE models, to see which one is better? 
library(caret)
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
grid_rf <- expand.grid(.mtry = c(2, 4, 8, 16))

set.seed(300)
m_rf <- train(default ~ ., data = credit, method = "rf", metric = "Kappa", trControl = ctrl, 
              tuneGrid = grid_rf)

grid_c50 <- expand.grid(.model = "tree", .trials = c(10, 20, 30, 40), .winnow = "FALSE")
set.seed(300)
m_c50 <- train(default ~ ., data = credit, method = "C5.0", metric = "Kappa", trControl = ctrl,
                 tuneGrid = grid_c50)


m_rf
m_c50


