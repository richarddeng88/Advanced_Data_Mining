library(gcookbook); library(ggplot2)
ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity")

ggplot(BOD, aes(x=Time, y=demand)) + geom_bar(stat="identity")
ggplot(BOD, aes(x=factor(Time), y=demand)) + geom_bar(stat="identity")

ggplot(pg_mean, aes(x=group, y=weight)) +
    geom_bar(stat="identity", fill="lightgreen", colour="black")

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat="identity",position="dodge")

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
    geom_bar(stat="identity",position="dodge", colour="black") +
    scale_fill_brewer(palette = "Pastel1")

# make bar graph count
ggplot(diamonds, aes(x=cut)) + geom_bar()    # x is a facotr
ggplot(diamonds, aes(x=carat)) + geom_bar()  # x is numeric
ggplot(diamonds, aes(x=carat)) + geom_histogram(binwidth = 0.1)

# using color in a bar graph
upc <- subset(uspopchange, rank(Change)>40)
ggplot(upc, aes(x=Abb, y=Change, fill=Region)) + geom_bar(stat="identity",color="black")+xlab("State")+
    scale_fill_brewer(palette = "Pastel1")

ggplot(upc, aes(x=reorder(Abb, Change), y=Change, fill=Region)) +
    geom_bar(stat="identity", colour="black") +
    scale_fill_manual(values=c("#669933", "#FFCC66")) +
    xlab("State")

# coloring negative and positive bars differently
csub <- subset(climate, Source=="Berkeley" & Year >= 1900)
csub$pos <- csub$Anomaly10y >= 0
ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) + geom_bar(stat="identity", position="identity")

ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) +
    geom_bar(stat="identity", position="identity", colour="black", size=0.5) +
    scale_fill_manual(values=c("#CCEEFF", "#FFDDDD"), guide=FALSE)

# adjusting bar with width and spacing
    ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity")
    ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity", width=0.2)
    
    ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
        geom_bar(stat="identity", width=0.9, position="dodge")
    
# making a stacked bar graph
    ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + 
        geom_bar(stat="identity") +
        guides(fill=guide_legend(reverse = T))+
        scale_fill_brewer(palette="Pastel1")
    
# add label to a bar graph
    ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=Weight), vjust=-0.5, colour="black") +
        ylim(0, max(cabbage_exp$Weight) * 1.05)
    
    ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar))  +
        geom_bar(stat="identity") +
        geom_text(aes(label=Weight), vjust=-0.5, colour="black") +
        ylim(0, max(cabbage_exp$Weight) * 1.05)
    
    ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
        geom_bar(stat="identity", position="dodge") +
        geom_text(aes(label=Weight), vjust=1.5, colour="white",
                  position=position_dodge(.9), size=5)
    
    ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=Weight), vjust=1.5, colour="white")
    
# make a point plot
    tophit <- tophitters2001[1:25, ] # Take the top 25 from the tophitters data set
    ggplot(tophit, aes(x=avg, y=name)) + geom_point(size=3)
    
    ggplot(tophit, aes(x=avg, y=reorder(name, avg))) +
        geom_point(size=3) + # Use a larger dot
        theme_bw() +
        theme(panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))
    
    ggplot(tophit, aes(x=reorder(name, avg), y=avg)) +
        geom_point(size=3) + # Use a larger dot
        theme_bw() +
        theme(axis.text.x = element_text(angle=60, hjust=1),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_line(colour="grey60", linetype="dashed"))
    
    ggplot(tophit, aes(x=avg, y=name)) +
        geom_segment(aes(yend=name), xend=0, colour="grey50") +
        geom_point(size=3, aes(colour=lg)) +
        scale_colour_brewer(palette="Set1", limits=c("NL","AL")) +
        theme_bw() +
        theme(panel.grid.major.y = element_blank(), # No horizontal grid lines
              legend.position=c(1, 0.55), # Put legend inside plot area
              legend.justification=c(1, 0.5))
    
    ggplot(tophit, aes(x=avg, y=name)) +
        geom_segment(aes(yend=name), xend=0, colour="grey50") +
        geom_point(size=3, aes(colour=lg)) +
        scale_colour_brewer(palette="Set1", limits=c("NL","AL"), guide=FALSE) +
        theme_bw() +
        theme(panel.grid.major.y = element_blank()) +
        facet_grid(lg ~ ., scales="free_y", space="free_y")