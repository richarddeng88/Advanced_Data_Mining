## =======================THE VALIDATION SET APPROACh=====================================
library(ISLR)
set.seed(3)
train <- sample(392,196)
auto <- Auto

# calculate the MSE of the 196 observations in the validation set. 

lm.fit <- lm(mpg~horsepower, data=auto, subset=train)
mean((auto$mpg-predict(lm.fit, auto))[-train]^2)

lm.fit2 <- lm(mpg~poly(horsepower,2), data=auto,subset=train)
mean((auto$mpg-predict(lm.fit2, auto))[-train]^2)

lm.fit3 <- lm(mpg~poly(horsepower,3), data=auto,subset=train)
mean((auto$mpg-predict(lm.fit3, auto))[-train]^2)

lm.fit4 <- lm(mpg~poly(horsepower,4), data=auto,subset=train)
mean((auto$mpg-predict(lm.fit4, auto))[-train]^2)

##===========================leave-one-out cross-validation=======================================
glm.fit <- glm(mpg~horsepower, data=auto)
coef(glm.fit)

library(boot)
glm.fit <- glm(mpg~horsepower, data=auto)
cv.err <- cv.glm(auto,glm.fit)
cv.err$delta

n <- 10
cv_error <- rep(0,n)
tt <- rep(0,n)
for (i in 1:n) {
    glm.fit <- glm(mpg ~ poly(horsepower,i), data=auto)
    cv_error[i] <- cv.glm(auto, glm.fit)$delta[1]
    tt[i] <- cv.glm(auto,glm.fit)$delta[2]
}
cv_error
tt

##==================================k-fold cross-validation=======================================
set.seed(17)
n = 10
cv_error1 <- rep(0,n)
tt <- rep(0,n)
for (i in 1:n){
    glm.fit <- glm(mpg ~ poly(horsepower,i), data=auto)
    cv_error1[i] <- cv.glm(auto,glm.fit, K=10)$delta[1]
    tt[i] <- cv.glm(auto,glm.fit, K=10)$delta[2]
}
cv_error1
tt

##=================================KNN corss-validation===========================================





########========================= bootstrapp ================================================
### Estimating the Accuracy of a Statistic of Interest
    library(boot)
    alpha.fn=function(data,index){
        X=data$X[index]
        Y=data$Y[index]
        return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
    }
    
    # the follwoing command tells R to estimate alpha to use all 100 observation.. 
    alpha.fn(Portfolio,1:100)
    alpha.fn(Portfolio,sample(100,100,replace=T)) 
            ## this is resampling with replacement, so every time the result is different. 
    boot(Portfolio,alpha.fn,R=1000)

## Estimating the Accuracy of a Linear Regression Model
    boot.fn = function(data,index){
        return(coef(lm(mpg~horsepower,data=data,subset=index)))
        }
    boot.fn(Auto,1:392)
    
    set.seed(1)
    boot.fn(Auto,sample(392,392,replace=T))
    boot(Auto,boot.fn,1000)
        ## This indicates that the bootstrap estimate for beta0 is 0.86. for beta1, it's 0.007

    summary(lm(mpg~horsepower,data=Auto))$coef
        ## if we use the origianl data set to run linear regress, we get the coefficent estimate. 
        ## but they are a little different with these we get from bootstrap methods.
    
    
    ## we fit a ploynomial regression model
    boot.fn=function(data,index)
         coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))
    set.seed(1)
    boot(Auto,boot.fn,1000)

    summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef


