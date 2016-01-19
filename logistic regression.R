# draw logistic regression curve
        x <- seq(-10, 10, by=0.01)
        y <- exp(x)/(1+exp(x))
        
        library(ggplot2);library(plotly)
        ggplot(data=NULL, mapping = aes(x=x, y=y))+geom_line(colour="blue")+ggtitle("Logistic Curve")
        g<-ggplot(data=NULL, mapping = aes(x=x, y=y))+geom_line(colour="blue")+ggtitle("Logistic Curve")
        ggplotly(g)
        
        
# real example
        library(C50)
        data(churn); train<-churnTrain; test<-churnTest
        #take out one variable 
            train<-train[,-3]; test<- test[,-3]
        # oreorder the response
            train$churn <- factor(train$churn, levels = c("no","yes"),order=T)
            test$churn <- factor(test$churn, levels = c("no","yes"),order=T)
        # contruct model 
        model <- glm(formula= churn~., 
                     data=train,
                     family="binomial")
        summary(model)
        # many variables are not significant in logistic regression.
        
        # step function
            model2 <- step(object = model, trace=1)
            summary(model2)
        
        # anova testing
            anova(object=model2, test="Chisq")
        
        # 
            library(sjmisc)
            HL_test <- hoslem_gof(x=model)
            
        
        # prediction
            prob <- predict(object=model2, newdata=test, type="response")
            pred <- ifelse(prob>=0.5, "yes","no")
            pred <- factor(pred, levels = c("no","yes"), order=T)
            table(test$churn, pred)
            
        # ROC curve
            library(pROC)
            roc_curve <- roc(test$churn, prob)
            names(roc_curve)
            x <- 1-roc_curve$specificities
            y <- roc_curve$sensitivities
            
            library(ggplot2)
            p <- ggplot(data=NULL,mapping = aes(x=x, y=y)) + geom_line(color="red")+geom_abline(intercept=0, slope=1)
            p <- p + annotate("text", x=0.6, y=0.5, label=paste("AUC=", round(roc_curve$auc,2)))
            p + labs(x="1-specificities", y="sensitivies", title="ROC curve")
            
            ggplotly(p)
            
            
            
            
        
        