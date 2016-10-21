# http://pages.stat.wisc.edu/~gvludwig/327-5/groupwork1/Credit_pod3.html

rm(list = ls())
options(width = 80)
credit <- read.csv("http://www.stat.wisc.edu/~gvludwig/327-5/groupwork1/Credit.csv", header = TRUE)
str(credit)

plot(credit)

# Full model with all variables included
out = lm(credit$Rating ~ credit$Income + credit$Cards + credit$Age + credit$Gender + 
             credit$Education + credit$Student + credit$Married + credit$Ethnicity + credit$Balance)

summary(out)

# Use AIC as the critera to build the optimal model
AIC(out)

step(out)

# Best model returned via the AIC
out = lm(credit$Rating ~ credit$Age + credit$Student + credit$Income + credit$Balance)

# Plot residuals
plot(fitted(out), resid(out), main = "Residual plot for no transform")
abline(0, 0)

# Square root the balance variable
sqrtBal = sqrt(credit$Balance)

# Try refitting model with square root of balance
out = lm(credit$Rating ~ credit$Income + credit$Cards + credit$Age + credit$Gender + 
             credit$Education + credit$Student + credit$Married + credit$Ethnicity + sqrtBal)
AIC(out)
step(out)




out = lm(credit$Rating ~ credit$Ethnicity + credit$Age + credit$Student + credit$Income + sqrtBal)

# View new model with square root balance
plot(fitted(out), resid(out), main = "Residual plot for square root balance")
abline(0, 0)
plot(out)



# Remove possible outliers with high-leverage
creditno = credit[-c(192, 324, 29), ]
boxplot(credit$Balance)


sqrtBal = sqrt(creditno$Balance)
outNoOutliers = lm(creditno$Rating ~ creditno$Ethnicity + creditno$Age + creditno$Student + 
                       creditno$Income + sqrtBal)

plot(fitted(outNoOutliers), resid(outNoOutliers))



sqrtBal = sqrt(credit$Balance)
incomeBal = credit$Income * sqrtBal
out = lm(credit$Rating ~ credit$Income + credit$Cards + credit$Age + credit$Gender + 
             credit$Education + credit$Student + credit$Married + credit$Ethnicity + sqrtBal + 
             incomeBal)
AIC(out)
