library(xgboost)
data("agaricus.test"); data("agaricus.train")
train <- agaricus.train ; test <- agaricus.test

# train the model 
bstSparse <- xgboost(data = train$data, 
                     label = train$label, 
                     max.depth = 2, #the trees won't be deep, because our case is very simple ;
                     eta = 1, 
                     nthread = 2,   #the number of cpu threads we are going to use;
                     nround = 2, 
                     objective = "binary:logistic")

dtrain <- xgb.DMatrix(data = train$data, label = train$label)
bst <- xgboost(data = dtrain, max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "binary:logistic")

# prediction 
pred <- predict(bst, test$data)

# size of the prediction vector
length(pred)
head(pred)

# transform to a binary classification
prediction <- as.numeric(pred > 0.5)
print(head(prediction))

# evaluating the performance
err <- mean(as.numeric(pred > 0.5) != test$label)
print(paste("test-error=", err))








