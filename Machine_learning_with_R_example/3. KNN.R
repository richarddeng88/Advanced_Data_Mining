## DATA READING ==========================================================
url <-"https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
download.file(url,"data/machine_learning_w_r/wdbc.data")
wbcd <- read.table("data/machine_learning_w_r/wdbc.data", sep = ",", header = F, stringsAsFactors = F) 

names(wbcd)[1:2] <- c("id", "diagnosis")
wbcd <- wbcd[-1]  # drop the "id" variable 
table(wbcd$diagnosis)

wbcd$diagnosis <- factor(wbcd$diagnosis, levels=c("B", "M"), labels = c("benign", "malignat"))


round(prop.table(table(wbcd$diagnosis))*100, digits=2)

## DATA PREPARATION -- NORMALIZE ==========================================================
# To normalize these features, we need to create a normalize() function in R.
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x)))}

# normalize(c(10, 20, 30, 40, 50))

wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
wbcd_n <- cbind(wbcd[1], wbcd_n)
summary(wbcd_n)

## DATA PREPARATION -- CREATING TRAINING AND TESTING DATASETS ===============================
intrain <- sample(dim(wbcd_n)[1], 0.8*dim(wbcd_n)[1])

wbcd_train <- wbcd_n[intrain,-1]
wbcd_test <- wbcd_n[-intrain,-1]
wbcd_train_label <- wbcd_n[intrain,1]
wbcd_test_label <- wbcd_n[-intrain,1]


## 3. TRAINING A MODEL ON THE DATA
library(class)
wbcd_pred <- knn(train=wbcd_train, test = wbcd_test, wbcd_train_label, k=20) # k = sqrt(455)


## 4. EVALUATING MODEL PERFORMANCE

table( wbcd_pred,wbcd_test_label)
mean(wbcd_pred==wbcd_test_label) ## corret rate = 0.9473684

## 5. IMPROVING MODEL PERFORMANCE
        # We will attempt two simple variations on our previous classifier. First, we will
        # employ an alternative method for rescaling our numeric features. Second, we
        # will try several different values for k.
    
    # TRANSFORMATION - Z=SCORE STANDARDIZATION 
        # Since the z-score standardized values have no predefined minimum and maximum, extreme values are not compressed towards the center.
        # use scale() to rescales values using the z-score standardization.

wbcd_z <- as.data.frame(scale(wbcd[-1]))
wbcd_z <- cbind(wbcd[1], wbcd_z)
summary(wbcd_z)

intrain <- sample(dim(wbcd_z)[1], 0.8*dim(wbcd_z)[1])

wbcd_train <- wbcd_z[intrain,-1]
wbcd_test <- wbcd_z[-intrain,-1]
wbcd_train_label <- wbcd_z[intrain,1]
wbcd_test_label <- wbcd_z[-intrain,1]

library(class)
wbcd_pred <- knn(train=wbcd_train, test = wbcd_test, wbcd_train_label, k=21) # k = sqrt(455)

table( wbcd_pred,wbcd_test_label)
mean(wbcd_pred==wbcd_test_label) ## corret rate = 0.9385965

    # testing different k values. 