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
    
    
    
    
    
    