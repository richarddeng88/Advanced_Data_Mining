# line pllot 
library(TSA)
ggplot(pressure, aes(x=temperature, y=pressure)) + geom_line() + geom_point()

# bar chart plot
table(mtcars$cyl)
barplot(table(mtcars$cyl))
ggplot(mtcars, aes(x=factor(cyl))) + geom_bar()

ggplot(BOD, aes(x=Time, y=demand)) + geom_bar(stat="identity")
