library(gcookbook);library(ggplot2)
# 1. Making a Basic Histogram
ggplot(faithful, aes(x=waiting)) + geom_histogram()
    # Store the values in a simple vector
    w <- faithful$waiting
    ggplot(NULL, aes(x=w)) + geom_histogram()

    # Set the width of each bin to 5
    ggplot(faithful, aes(x=waiting)) + geom_histogram(binwidth=3, fill="white", colour="black")
    
    # Divide the x range into 15 bins
    binsize <- diff(range(faithful$waiting))/15
    ggplot(faithful, aes(x=waiting)) + geom_histogram(binwidth=binsize, fill="white", colour="black")
    
    # Different appearance of histograms with the origin at 31 and 35
    h <- ggplot(faithful, aes(x=waiting)) # Save the base object for reuse
    h + geom_histogram(binwidth=8, fill="white", colour="black", origin=31)
    h + geom_histogram(binwidth=8, fill="white", colour="black", origin=35)

# 2. Making Multiple Histograms from Grouped Data
    library(MASS)
    ggplot(birthwt, aes(x=bwt)) + geom_histogram(fill="white", colour="black") + facet_grid(smoke ~ .)
    
    birthwt1 <- birthwt 
    birthwt1$smoke <- factor(birthwt1$smoke)
    # Map smoke to fill, make the bars NOT stacked, and make them semitransparent
    ggplot(birthwt1, aes(x=bwt, fill=smoke)) + geom_histogram(position="identity", alpha=0.4)

# 3. Making a Density Curve
    ggplot(faithful, aes(x=waiting)) + geom_density()
    ggplot(faithful, aes(x=waiting)) + geom_line(stat="density") + expand_limits(y=0)

    ggplot(faithful, aes(x=waiting)) +
        geom_line(stat="density", adjust=.25, colour="red") +
        geom_line(stat="density") +
        geom_line(stat="density", adjust=2, colour="blue")

    ggplot(faithful, aes(x=waiting)) + geom_density(fill="blue", alpha=.2) + xlim(35, 105)
    ggplot(faithful, aes(x=waiting)) +
        geom_density(fill="blue", colour=NA, alpha=.2) +
        geom_line(stat="density") +
        xlim(35, 105)
    
    ggplot(faithful, aes(x=waiting, y=..density..)) +
        geom_histogram(fill="cornsilk", colour="grey60", size=.2) +
        geom_density() +
        xlim(35, 105)
    
# 4. Making Multiple Density Curves from Grouped Data    
    library(MASS)
    birthwt1 <- birthwt;birthwt1$smoke <- factor(birthwt1$smoke)
    ggplot(birthwt1, aes(x=bwt, colour=smoke)) + geom_density()
    ggplot(birthwt1, aes(x=bwt, fill=smoke)) + geom_density(alpha=.3)
    ggplot(birthwt1, aes(x=bwt)) + geom_density() + facet_grid(smoke ~ .)
    
    ggplot(birthwt1, aes(x=bwt, y=..density..)) +
            geom_histogram(binwidth=200, fill="cornsilk", colour="grey60", size=.2) +
            geom_density() +
            facet_grid(smoke ~ .)
    
# 5. Making a Frequency Polygon    
    #A frequency polygon appears similar to a kernel density estimate curve, but it shows the
    #same information as a histogram.
    ggplot(faithful, aes(x=waiting)) + geom_freqpoly()
    ggplot(faithful, aes(x=waiting)) + geom_freqpoly(binwidth=4)
    
    binsize <- diff(range(faithful$waiting))/15
    ggplot(faithful, aes(x=waiting)) + geom_freqpoly(binwidth=binsize)
    
# 6. Making a Basic Box Plot    
    ggplot(birthwt, aes(x=factor(race), y=bwt)) + geom_boxplot(width=.5, notch=T)
    ggplot(birthwt, aes(x=factor(race), y=bwt)) + geom_boxplot(width=.3,outlier.size=5, outlier.shape=19)
    
    # make a box plot of a single boxplot. make x=1, and remove x-axis tick markers and label. 
    ggplot(birthwt, aes(x=1, y=bwt)) + geom_boxplot() +
            scale_x_continuous(breaks=NULL) +
            theme(axis.title.x = element_blank())
    
# 7. Adding Notches to a Box Plot 
    #Notches are used in box plots to help visually assess whether the medians of distributions
    #differ. If the notches do not overlap, this is evidence that the medians are different.
    ggplot(birthwt, aes(x=factor(race), y=bwt)) + geom_boxplot(notch=T)
    
# 8. Adding Means to a Box Plot
    ggplot(birthwt, aes(x=factor(race), y=bwt)) + geom_boxplot() +
            stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")
    
# 9. Making a Violin Plot    
    #Violin plots are a way of comparing multiple data distributions. With ordinary density
    #curves, it is difficult to compare more than just a few distributions because the lines
    #visually interfere with each other. With a violin plot, it's easier to compare several distributions
    #since they're placed side by side.
    p <- ggplot(heightweight, aes(x=sex, y=heightIn))
    p + geom_violin()
    
    p + geom_violin() + geom_boxplot(width=.1, fill="black", outlier.colour=NA) +
            stat_summary(fun.y=median, geom="point", fill="white", shape=21, size=2.5)
    
    p + geom_violin(trim=FALSE)
    p + geom_violin(scale="count")
    
# 10. Making a Dot Plot    
    countries2009 <- subset(countries, Year==2009 & healthexp>2000)
    p <- ggplot(countries2009, aes(x=infmortality))
    p + geom_dotplot()
    
    p + geom_dotplot(binwidth=.25) + geom_rug() +
            scale_y_continuous(breaks=NULL) + # Remove tick markers
            theme(axis.title.y=element_blank()) # Remove axis label
    
    p + geom_dotplot(method="histodot", binwidth=.25) + geom_rug() +
            scale_y_continuous(breaks=NULL) + theme(axis.title.y=element_blank())
    
    p + geom_dotplot(binwidth=.25, stackdir="center")
    scale_y_continuous(breaks=NULL) + theme(axis.title.y=element_blank())
    p + geom_dotplot(binwidth=.25, stackdir="centerwhole")
    scale_y_continuous(breaks=NULL) + theme(axis.title.y=element_blank())
    
# 11. Making Multiple Dot Plots for Grouped Data    
    ggplot(heightweight, aes(x=sex, y=heightIn)) +
            geom_dotplot(binaxis="y", binwidth=.5, stackdir="center")
    
    ggplot(heightweight, aes(x=sex, y=heightIn)) +
            geom_boxplot(outlier.colour=NA, width=.4) +
            geom_dotplot(binaxis="y", binwidth=.5, stackdir="center", fill=NA)
    
    ggplot(heightweight, aes(x=sex, y=heightIn)) +
            geom_boxplot(aes(x=as.numeric(sex) + .2, group=sex), width=.25) +
            geom_dotplot(aes(x=as.numeric(sex) - .2, group=sex), binaxis="y",
                         binwidth=.5, stackdir="center") +
            scale_x_continuous(breaks=1:nlevels(heightweight$sex),
                               labels=levels(heightweight$sex))
    
#    6.12. Making a Density Plot of Two-Dimensional Data    
    p <- ggplot(faithful, aes(x=eruptions, y=waiting))
    p + geom_point() + stat_density2d()
    p + stat_density2d(aes(colour=..level..))
    
    # Map density estimate to fill color
    p + stat_density2d(aes(fill=..density..), geom="raster", contour=FALSE)
    # With points, and map density estimate to alpha
    p + geom_point() + stat_density2d(aes(alpha=..density..), geom="tile", contour=FALSE)
    
    p + stat_density2d(aes(fill=..density..), geom="raster", contour=FALSE, h=c(.5,5))
    
    
    
    
    
    
    
    
    
    