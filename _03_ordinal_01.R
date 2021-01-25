

options(stringAsFactor=FALSE, width=300)

library(MASS)

xbool_save_file <- FALSE


set.seed(777)


########## two cats

n <- 1000

probCOs <- c(0, 0.5, 1)
ordinalDom <- c(0, 1)

actualProbs <- runif(n, 0, 1)

xndx <- findInterval(actualProbs, probCOs)

y <- as.factor(ordinalDom[ xndx ])

table(y)
###### keep in mind that y is 'ranked'
###### if someone lands in the 'top' group . . .
###### what percentile are they ?

xpolr <- polr(y ~ 1)
summary(xpolr)

############# ???




#####################

n <- 1000

probCOs <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
ordinalDom <- c(1, 2, 3, 4, 5)

ordinalDom <- c("a Very Unawesome", "b Somewhat Unawesome", "c Neutral", "d Somewhat Awesome", "e Very Awesome")

actualProbs <- runif(n, 0, 1)

xndx <- findInterval(actualProbs, probCOs)

y <- as.factor(ordinalDom[ xndx ])

table(y)
###### keep in mind that y is 'ranked'
###### if someone lands in the 'top' group . . .
###### what percentile are they ?

xpolr <- polr(y ~ 1)
summary(xpolr)

########## recover the partition

xx <- summary(xpolr)$coefficients[ , "Value" ]
1 / (1 + exp(-xx))

xx <- summary(xpolr)$zeta
1 / (1 + exp(-xx))


################### stop here
################### stop here
################### stop here
################### stop here
################### stop here
################### stop here



##################################### with numeric predictor

x <- qnorm(actualProbs)

y <- x + rnorm(n)

y <- floor((y - min(y)) * (5 / diff(range(y)))) + 1
table(y)


y <- as.factor(y)

xpolr <- polr(y ~ 1)
summary(xpolr)
xx <- summary(xpolr)$zeta
1 / (1 + exp(-xx))


xpolr <- polr(y ~ x)
summary(xpolr)


xx <- summary(xpolr)$zeta
1 / (1 + exp(-xx))


xxcoef <- summary(xpolr)$coefficients[ "x", "Value" ]
1 / (1 + exp(-xxcoef * (-1))) #### x is -1
1 / (1 + exp(-xxcoef * (+1))) #### x is 1


########### note the symmetry
###### the polr function estimates this coefficient for centered sigmoid
###### the intercept terms above 'shift' this effect to account for baserate probs
1 / (1 + exp(-xxcoef * (-1))) +
1 / (1 + exp(-xxcoef * (+1)))



############# make some predictions

xdf <- data.frame(y, x)

xpolr <- polr(y ~ x, data=xdf)
summary(xpolr)

xdf0 <- data.frame("y"=NA, "x"=10)
predict(xpolr, newdata=xdf0)



xdf0 <- data.frame("y"=NA, "x"=-10)
predict(xpolr, newdata=xdf0)
