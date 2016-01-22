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