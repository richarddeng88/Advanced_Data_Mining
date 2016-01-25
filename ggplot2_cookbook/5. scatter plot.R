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
# Sometimes, you may want to create the model yourself and then add it to your graph.
model <- lm(heightIn ~ ageYear + I(ageYear^2), heightweight)
model
# Create a data frame with ageYear column, interpolating across range
xmin <- min(heightweight$ageYear)
xmax <- max(heightweight$ageYear)
predicted <- data.frame(ageYear=seq(xmin, xmax, length.out=100))
# Calculate predicted values of heightIn
predicted$heightIn <- predict(model, predicted)
predicted
sp <- ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point(colour="grey40")
sp + geom_line(data=predicted, size=1,color="blue")

# here we use predictives method, which can be applied to any model objects. 
        predictvals <- function(model, xvar, yvar, xrange=NULL, samples=100, ...) {
                # If xrange isn't passed in, determine xrange from the models.
                # Different ways of extracting the x range, depending on model type
                if (is.null(xrange)) {
                        if (any(class(model) %in% c("lm", "glm")))
                                xrange <- range(model$model[[xvar]])
                        else if (any(class(model) %in% "loess"))
                                xrange <- range(model$x)
                }
                newdata <- data.frame(x = seq(xrange[1], xrange[2], length.out = samples))
                names(newdata) <- xvar
                newdata[[yvar]] <- predict(model, newdata = newdata, ...)
                newdata
        }
        
        #make a linear model with lm() and a LOESS model with loess()
        modlinear <- lm(heightIn ~ ageYear, heightweight)
        modloess <- loess(heightIn ~ ageYear, heightweight)
        
        #call predictvals() on each model, and pass the resulting data frames to geom_line()
        lm_predicted <- predictvals(modlinear, "ageYear", "heightIn")
        loess_predicted <- predictvals(modloess, "ageYear", "heightIn")
        
        sp + geom_line(data=lm_predicted, colour="red", size=.8) +
                geom_line(data=loess_predicted, colour="blue", size=.8)

        # for the logistic regression example
        library(MASS) # For the data set
        b <- biopsy
        b$classn[b$class=="benign"] <- 0
        b$classn[b$class=="malignant"] <- 1
        fitlogistic <- glm(classn ~ V1, b, family=binomial)
        glm_predicted <- predictvals(fitlogistic, "V1", "classn", type="response") # here need to add "response"
        ggplot(b, aes(x=V1, y=classn)) +
                geom_point(position=position_jitter(width=.3, height=.08), alpha=0.4,shape=21, size=1.5) +
                geom_line(data=glm_predicted, colour="#1177FF", size=1)

# 8.Adding Fitted Lines from Multiple Existing Models
make_model <- function(data) {
        lm(heightIn ~ ageYear, data)
}        
# The dlply() and ldply() calls are used for splitting the data into parts, running functions
# on those parts, and then reassembling the output.
library(plyr)
models <- dlply(heightweight, "sex", .fun = make_model)        
models        
predvals <- ldply(models, .fun=predictvals, xvar="ageYear", yvar="heightIn")
predvals        
ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) + geom_point() + geom_line(data=predvals)        
        
# 9. Adding Annotations with Model Coefficients        
model <- lm(heightIn ~ ageYear, heightweight)
summary(model)        
pred <- predictvals(model, "ageYear", "heightIn")
sp <- ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point() + geom_line(data=pred)
sp + annotate("text", label="r^2=0.42", x=16.5, y=52)               
sp + annotate("text", label="r^2 == 0.42", parse = TRUE, x=16.5, y=52)        
        
eqn <- as.character(as.expression(
        substitute(italic(y) == a + b * italic(x) * "," ~~ italic(r)^2 ~ "=" ~ r2,
                   list(a = format(coef(model)[1], digits=3),
                        b = format(coef(model)[2], digits=3),
                        r2 = format(summary(model)$r.squared, digits=2)
                   ))))
eqn        
parse(text=eqn) # Parsing turns it into an expression        
sp + annotate("text", label=eqn, parse=TRUE, x=Inf, y=-Inf, hjust=1.1, vjust=-.5)

# 10. Adding Marginal Rugs to a Scatter Plot
# A marginal rug plot is essentially a one-dimensional scatter plot that can be used to
# visualize the distribution of data on each axis.
ggplot(faithful, aes(x=eruptions, y=waiting)) + geom_point() + geom_rug()

#The resolution of the waiting variable is in whole minutes, and there are a lot of overlapping.
# hence, the marginal rug in this case is not as informative as it could be
ggplot(faithful, aes(x=eruptions, y=waiting)) + geom_point() + geom_rug(position="jitter", size=.2)

# 11. Labeling Points in a Scatter Plot
#For annotating just one or a few points, you can use annotate() or geom_text().
subset(countries, Year==2009 & healthexp>2000)
sp <- ggplot(subset(countries, Year==2009 & healthexp>2000),aes(x=healthexp, y=infmortality)) + 
        geom_point()
sp + annotate("text", x=4350, y=5.4, label="Canada") + annotate("text", x=7400, y=6.8, label="USA")
sp + geom_text(aes(label=Name), size=4)
sp + geom_text(aes(label=Name), size=4, vjust=2)
sp + geom_text(aes(y=infmortality+.1, label=Name), size=4, vjust=0)
sp + geom_text(aes(label=Name), size=4, hjust=0)
sp + geom_text(aes(x=healthexp+100, label=Name), size=4, hjust=0)
cdat <- subset(countries, Year==2009 & healthexp>2000)
cdat$Name1 <- cdat$Name
idx <- cdat$Name1 %in% c("Canada", "Ireland", "United Kingdom", "United States",
                         "New Zealand", "Iceland", "Japan", "Luxembourg",
                         "Netherlands", "Switzerland")
idx
cdat$Name1[!idx] <- NA
ggplot(cdat, aes(x=healthexp, y=infmortality)) +
        geom_point() +
        geom_text(aes(x=healthexp+100, label=Name1), size=4, hjust=0) +
        xlim(2000, 10000)

# 12. Creating a Balloon Plot
cdat <- subset(countries, Year==2009 &
                       Name %in% c("Canada", "Ireland", "United Kingdom", "United States",
                                   "New Zealand", "Iceland", "Japan", "Luxembourg",
                                   "Netherlands", "Switzerland"))
cdat
p <- ggplot(cdat, aes(x=healthexp, y=infmortality, size=GDP)) +
        geom_point(shape=21, colour="black", fill="cornsilk")
p + scale_size_area(max_size=15)

hec <- HairEyeColor[,,"Male"] + HairEyeColor[,,"Female"]
library(reshape2)
hec <- melt(hec, value.name="count")
ggplot(hec, aes(x=Eye, y=Hair)) +
        geom_point(aes(size=count), shape=21, colour="black", fill="cornsilk") +
        scale_size_area(max_size=20, guide=FALSE) +
        geom_text(aes(y=as.numeric(Hair)-sqrt(count)/22, label=count), vjust=1,colour="grey60", size=4)

# 13. Making a Scatter Plot Matrix
#A scatter plot matrix is an excellent way of visualizing the pairwise relationships among several variables.
c2009 <- subset(countries, Year==2009, select=c(Name, GDP, laborrate, healthexp, infmortality))
pairs(c2009[,2:5])

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
        usr <- par("usr")
        on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r <- abs(cor(x, y, use="complete.obs"))
        txt <- format(c(r, 0.123456789), digits=digits)[1]
        txt <- paste(prefix, txt, sep="")
        if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
        text(0.5, 0.5, txt, cex = cex.cor * (1 + r) / 2)
}

panel.hist <- function(x, ...) {
        usr <- par("usr")
        on.exit(par(usr))
        par(usr = c(usr[1:2], 0, 1.5) )
        h <- hist(x, plot = FALSE)
        breaks <- h$breaks
        nB <- length(breaks)
        y <- h$counts
        y <- y/max(y)
        rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}

# change the loss() line to regression line
pairs(c2009[,2:5], upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

panel.lm <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                      cex = 1, col.smooth = "black", ...) {
        points(x, y, pch = pch, col = col, bg = bg, cex = cex)
        abline(stats::lm(y ~ x), col = col.smooth, ...)
}

pairs(c2009[,2:5], pch=".", upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.lm)
