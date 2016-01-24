library(gcookbook);library(ggplot2)
# Making a Basic Scatter Plot
heightweight[, c("ageYear", "heightIn")]
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point()
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point(shape=8,size=2)
ggplot(heightweight, aes(x=ageYear, y=heightIn,col=sex)) + geom_point(shape=21,size=2)

# 2. Grouping Data Points by a Variable Using Shape or Color
ggplot(heightweight, aes(x=ageYear, y=heightIn,shape=sex, col=sex)) + geom_point(size=3)+
        scale_shape_manual(values=c(1,2)) + # change the shape of the points
        scale_colour_brewer(palette="Set1")

# 3. looks messy in this case.
hw <- heightweight
# Categorize into <100 and >=100 groups
hw$weightGroup <- cut(hw$weightLb, breaks=c(-Inf, 100, Inf), labels=c("< 100", ">= 100"))

ggplot(hw, aes(x=ageYear, y=heightIn, shape=sex, fill=weightGroup)) +
        geom_point(size=2.5) +
        scale_shape_manual(values=c(21, 24)) +
        scale_fill_manual(values=c(NA, "black"), guide=guide_legend(override.aes=list(shape=21)))

# 4. Mapping a Continuous Variable to Color or Size. (mapped to other aesthetics: size and/or color)
heightweight[, c("sex", "ageYear", "heightIn", "weightLb")]
ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=weightLb)) + geom_point()
ggplot(heightweight, aes(x=ageYear, y=heightIn, size=weightLb)) + geom_point()

ggplot(heightweight, aes(x=weightLb, y=heightIn, fill=ageYear)) +
        geom_point(shape=21, size=2.5) +
        scale_fill_gradient(low="blue", high="red")

ggplot(heightweight, aes(x=weightLb, y=heightIn, fill=ageYear)) +
        geom_point(shape=21, size=2.5) +
        scale_fill_gradient(low="black", high="white", breaks=12:17,guide=guide_legend())

ggplot(heightweight, aes(x=ageYear, y=heightIn, size=weightLb, colour=sex)) +
        geom_point(alpha=.5) +
        scale_size_area() + # Make area proportional to numeric value
        scale_colour_brewer(palette="Set1")

# 5. Dealing with Overplotting
#If there's a high degree of overplotting, there are a number of possible solutions:
#        . Make the points semitransparent
#. Bin the data into rectangles (better for quantitative analysis)
#. Bin the data into hexagons
#. Use box plots
sp <- ggplot(diamonds, aes(x=carat, y=price))
sp + geom_point()
sp + geom_point(alpha=.1)
sp + geom_point(alpha=.01)
# Now we can see that there are vertical bands at nice round values of carats, indicating
#that diamonds tend to be cut to those sizes.

#Another solution is to bin the points into rectangles and map the density of the points
#to the fill color of the rectangles
sp + stat_bin2d() #by default, bin2d() divides the space into 30 groups
sp + stat_bin2d(bins=80) + scale_fill_gradient(low="lightblue", high="red", limits=c(0, 6000))

library(hexbin)
sp + stat_binhex() + scale_fill_gradient(low="lightblue", high="red", limits=c(0, 8000))

sp + stat_binhex() + scale_fill_gradient(low="lightblue", high="red",
                            breaks=c(0, 250, 500, 1000, 2000, 4000, 6000),
                            limits=c(0, 6000))

sp1 <- ggplot(ChickWeight, aes(x=Time, y=weight))
sp1 + geom_point()+ geom_jitter()
sp1 + geom_point(position="jitter")
# Could also use geom_jitter(), which is equivalent
sp1 + geom_point(position=position_jitter(width=.5, height=0))

# When the data has one discrete axis and one continuous axis, it might make sense to use box plots,
sp1 + geom_boxplot(aes(group=Time))

# 6. Adding Fitted Regression Model Lines
sp <- ggplot(heightweight, aes(x=ageYear, y=heightIn))
sp + geom_point() + stat_smooth(method=lm)

# 99% confidence region
sp + geom_point() + stat_smooth(method=lm, level=0.99)

# No confidence region
sp + geom_point() + stat_smooth(method=lm, se=FALSE)

# change the default regression line color
sp + geom_point(colour="grey60") + stat_smooth(method=lm, se=FALSE, colour="black")

# a loess (locally weighted polynomial) curve,
sp + geom_point(colour="grey60") + stat_smooth(method=loess)
        
        # logsitic regression line
        library(MASS) # For the data set
        b <- biopsy
        b$classn[b$class=="benign"] <- 0
        b$classn[b$class=="malignant"] <- 1
        ggplot(b, aes(x=V1, y=classn)) +
                geom_point(position=position_jitter(width=0.3, height=0.06), alpha=0.4,shape=21, size=1.5) +
                stat_smooth(method=glm, family=binomial)

# if grouped, one fit line will be drawn for each group
sps <- ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) +
        geom_point() +
        scale_colour_brewer(palette="Set1")
sps + geom_smooth()
sps + geom_smooth(method=lm, se=FALSE, fullrange=TRUE) # fullrange allow line outside the x-axis range. 

# 7. Adding Fitted Lines from an Existing Model












