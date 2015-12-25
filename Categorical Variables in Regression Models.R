hsb2 <- read.csv("http://www.ats.ucla.edu/stat/data/hsb2.csv")
# factor function
        hsb2$race.f <- factor(hsb2$race)
        is.factor(hsb2$race.f)
        
        summary(lm(write ~ race.f, data = hsb2))
        

# C function
        # treatment
        hsb2 <- within(hsb2, {
                race.ct <- C(race.f, treatment)
                print(attributes(race.ct))
        })
        summary(lm(write ~ race.ct, data = hsb2))
        
        # helmert
        hsb2 <- within(hsb2, {
                race.ch <- C(race.f, helmert)
                print(attributes(race.ch))
        })
        summary(lm(write ~ race.ch, data = hsb2))
        
        # helmert 3
        hsb2 <- within(hsb2, {
                race.ch1 <- C(race.f, helmert, 3)
                print(attributes(race.ch1))
        })
        summary(lm(write ~ race.ch1, data = hsb2))
        
# ######
        
        cuse <- read.table("http://data.princeton.edu/wws509/datasets/cuse.dat", header=TRUE)
        attach(cuse)
        lrfit <- glm( cbind(using, notUsing) ~ age + education + wantsMore , family = binomial)
        summary(lrfit)
        
        
        
        
        
        
        
        
        
        
        
        