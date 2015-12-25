letters <- read.csv("data/Machine-Learning-with-R-datasets/letterdata.csv")
letters_train <- letters[1:16000, ]
letters_test <- letters[16001:20000, ]

# TRAIN THE MODEL
library(kernlab)
letter_classifier <- ksvm(letter ~ ., data = letters_train, kernel = "vanilladot")
letter_classifier

# EVALUATING PERFORMANCE
letter_predictions <- predict(letter_classifier, letters_test)
table(letter_predictions, letters_test$letter)

agreement <- letter_predictions == letters_test$letter
table(agreement)
prop.table(table(agreement))

# IMPROVE THE PERFORMANCE
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train,kernel = "rbfdot")
letter_predictions_rbf <- predict(letter_classifier_rbf,letters_test)
agreement_rbf <- letter_predictions_rbf == letters_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))
