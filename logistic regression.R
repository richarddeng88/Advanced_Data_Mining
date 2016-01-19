# draw logistic regression curve
        x <- seq(-10, 10, by=0.01)
        y <- exp(x)/(1+exp(x))
        
        library(ggplot2);library(plotly)
        ggplot(data=NULL, mapping = aes(x=x, y=y))+geom_line(colour="blue")+ggtitle("Logistic Curve")
        #g<-ggplot(data=NULL, mapping = aes(x=x, y=y))+geom_line(colour="blue")+ggtitle("Logistic Curve")
        #ggplotly(g)
        
        
# real example
        library(C50)
        data(churn); train<-churnTrain; test<-churnTest
        
        
        
        
        
        
        
        
        
        
        
        