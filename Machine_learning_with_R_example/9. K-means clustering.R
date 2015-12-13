## DATA READING
teens <- read.csv("data/Machine-Learning-with-R-datasets/snsdata.csv")

sapply(teens, function(x){sum(is.na(x))})

table(teens$gender, useNA = "ifany")

## DATA PREPROCESSIN - dummy coding missing values
    summary(teens$age)
    teens$age <- ifelse(teens$age >= 13 & teens$age < 20, teens$age, NA)
    summary(teens$age)
    
    teens$female <- ifelse(teens$gender == "F" & !is.na(teens$gender), 1, 0)
    teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)
    
    table(teens$gender, useNA = "ifany")
    table(teens$female, useNA = "ifany")
    table(teens$no_gender, useNA = "ifany")

## TRAIN MODEL
    interests <- teens[5:40]
    interests_z <- as.data.frame(lapply(interests, scale))
    
    set.seed(2345)
    teen_clusters <- kmeans(interests_z, 5)
    
## EVALUATING THE PERFORMANCE
teen_clusters$size
teen_clusters$centers

## IMPROVING MODEL PERFORMANCE
teens$cluster <- teen_clusters$cluster
teens[1:25, c("cluster", "gender", "age", "friends")]
aggregate(data = teens, age ~ cluster, mean)
aggregate(data = teens, female ~ cluster, mean)
aggregate(data = teens, friends ~ cluster, mean)

    
    
    