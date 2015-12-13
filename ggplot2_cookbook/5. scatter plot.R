library(gcookbook);library(ggplot2)
data(heightweight)
heightweight[, c("ageYear", "heightIn")]
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point()

ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point(shape=8,size=2)

ggplot(heightweight, aes(x=ageYear, y=heightIn,col=sex)) + geom_point(shape=21,size=2)

# using shape and color
ggplot(heightweight, aes(x=ageYear, y=heightIn,shape=sex, col=sex)) + geom_point()












