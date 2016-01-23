library(ggplot2);library(gcookbook)
ggplot(BOD, aes(x=Time, y=demand)) + geom_line()

# if x is categorical, then we need to add group = 1
        BOD1 <- BOD # Make a copy of the data
        BOD1$Time <- factor(BOD1$Time)
        ggplot(BOD1, aes(x=Time, y=demand, group=1)) + geom_line()

# expand the y lim range
        ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + ylim(0, max(BOD$demand))

# adding point to graph line
        ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + geom_point()

# the interval between each data points are not consistent. 
# the estimates are not as fequent as they are in the recent past. 
        ggplot(worldpop, aes(x=Year, y=Population)) + geom_line() + geom_point(size=2,shape=2)
# with the log y-axis, we see that the rate of proportional change has increased in the last thousand year. 
# in the most recent 1000 years, the population has increased at a much faster rate. 
        ggplot(worldpop, aes(x=Year, y=Population)) + geom_line() + geom_point() + scale_y_log10()

# 3. make multi line in a graph
        library(plyr)
        tg <- ddply(ToothGrowth, c("supp", "dose"), summarise, length=mean(len))
        ggplot(tg, aes(x=dose, y=length, colour=supp)) + geom_line()
        ggplot(tg, aes(x=dose, y=length, linetype=supp)) + geom_line()
        
        ggplot(tg, aes(x=dose, y=length, shape=supp)) + geom_line() + geom_point(size=4,shape=18)
        ggplot(tg, aes(x=dose, y=length, fill=supp)) + geom_line() + geom_point(size=4, shape=21)
        
        #dodge the point that overlap
        ggplot(tg, aes(x=dose, y=length, shape=supp)) +
                geom_line(position=position_dodge(0.1)) + # Dodge lines by 0.2
                geom_point(position=position_dodge(0.1), size=4)
        
# 4. change the appearance of the line
        ggplot(BOD, aes(x=Time, y=demand)) + geom_line(linetype="dashed", size=0.7, colour="blue")
        
        tg <- ddply(ToothGrowth, c("supp", "dose"), summarise, length=mean(len))
        ggplot(tg, aes(x=dose, y=length, colour=supp)) + geom_line() + scale_colour_brewer(palette="Set1")
        
        # specify the coulour in the geom_line() function
        ggplot(tg, aes(x=dose, y=length, group=supp)) + geom_line(colour="darkgreen", size=1.5)
        
        ggplot(tg, aes(x=dose, y=length, colour=supp)) +
                geom_line(linetype="dashed") +
                geom_point(shape=22, size=3, fill="white")
        
        
# 5. change the appearence of points
        ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + 
                geom_point(size=4, shape=22, colour="darkred", fill="red")
        
        ggplot(tg, aes(x=dose, y=length, fill=supp)) +
                geom_line(position=pd) +
                geom_point(shape=21, size=3, position=position_dodge(0.2)) +
                scale_fill_manual(values=c("black","white"))
        
# 6.Making a Graph with a Shaded Area
        sunspotyear <- data.frame(
                Year = as.numeric(time(sunspot.year)),
                Sunspots = as.numeric(sunspot.year)
        )
        ggplot(sunspotyear, aes(x=Year, y=Sunspots)) + geom_area(colour="black", fill="blue", alpha=.2)#80% transparent by setting alpha to 0.2.
        
        ggplot(sunspotyear, aes(x=Year, y=Sunspots)) +geom_area(fill="blue", alpha=.2) +geom_line()
        
# 7. Making a Stacked Area Graph        
        # stacked line graph
        ggplot(uspopage, aes(x=Year, y=Thousands,color=AgeGroup)) + geom_line(position = "stack")
        ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + geom_area(alpha=0.9)
        ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + geom_area(alpha=0.3,colour="black")
        
        ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) +
                geom_area(colour="black", size=.2, alpha=.8) +
                scale_fill_brewer(palette="Reds", breaks=rev(levels(uspopage$AgeGroup)))
        
        # change the order
        ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup, order=desc(AgeGroup))) +
                geom_area(colour="black", size=.2, alpha=.4) +
                scale_fill_brewer(palette="Blues")
        
        # take away the outline
        ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup, order=desc(AgeGroup))) +
                geom_area(colour=NA, alpha=.4) +
                scale_fill_brewer(palette="Blues") +
                geom_line(position="stack", size=.2)
        
# 8. Making a Proportional Stacked Area Graph        
        # Convert Thousands to Percent
        uspopage_prop <- ddply(uspopage, "Year", transform,Percent = Thousands / sum(Thousands) * 100)
        ggplot(uspopage_prop, aes(x=Year, y=Percent, fill=AgeGroup)) +
                geom_area(colour="black", size=.2, alpha=.4) +
                scale_fill_brewer(palette="Blues", breaks=rev(levels(uspopage$AgeGroup)))
        
# 9. Adding a Confidence Region        
        clim <- subset(climate, Source == "Berkeley", select=c("Year", "Anomaly10y", "Unc10y"))
        clim   
        ggplot(clim, aes(x=Year, y=Anomaly10y)) + 
                geom_ribbon(aes(ymin=Anomaly10y-Unc10y, ymax=Anomaly10y+Unc10y), alpha=0.4)+
                geom_line() 
        
        # With a dotted line for upper and lower bounds
        ggplot(clim, aes(x=Year, y=Anomaly10y)) +
                geom_line(aes(y=Anomaly10y-Unc10y), colour="grey50", linetype="dashed") +
                geom_line(aes(y=Anomaly10y+Unc10y), colour="grey50", linetype="dotted") +
                geom_line()
        
        
        
        
        
        