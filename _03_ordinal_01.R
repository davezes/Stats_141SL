

options(stringAsFactor=FALSE, width=300)

library(MASS)

xbool_save_file <- FALSE


set.seed(777)


########## two cats

n <- 1000

probCOs <- c(0, 0.5, 1)
ordinalDom <- c("a Unawesome", "b Awesome")

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

ordinalDom <- c("a Not at all Awesome", "b Somewhat Unawesome", "c Neutral", "d Somewhat Awesome", "e Very Awesome")

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

set.seed(777)

n <- 1000

probCOs <- c(-Inf, -1, -0.3, 0.3, 1, Inf)

ordinalDom <- c("aNotAtAllAwesome", "bSomewhatUnawesome", "cNeutral", "dSomewhatAwesome", "eVeryAwesome")

x <- rnorm(n, 0, 1)

xvals <- x + rnorm(n, 0, 1/2)

xndx <- findInterval(xvals, probCOs)

y <- ordinalDom[ xndx ]



table(y)

table(y) / n

y <- as.factor(y)

xpolr <- polr(y ~ 1)
summary(xpolr)
xx <- summary(xpolr)$zeta
1 / (1 + exp(-xx))


xpolr2 <- polr(y ~ x)
summary(xpolr2)


xxcoef <- summary(xpolr2)$coefficients[ "x", "Value" ] ; xxcoef
xx <- summary(xpolr2)$zeta ; xx

################################# examples

########## what is the interpretation of the x coef ?????
########## FAILURE!

x0 <- 0
1 / (1 + exp(- ( xx - x0 * xxcoef ) ) )



x0 <- -1
1 / (1 + exp( - ( xx - x0 * xxcoef ) ) )


x0 <- +1
1 / (1 + exp(- ( xx - x0 * xxcoef ) ) )



###### Pr( Y > 1 | x = x0 ) / Pr( Y <= 1 | x = x0 ) = exp( x0 * xxcoef ) / exp( xx[1] )

xsplit <- c(1,2,3,4)


#################
x0 <- -1
exp( x0 * xxcoef ) / exp( xx[ xsplit ] )

y_star <- log( exp( x0 * (xxcoef) ) / exp( xx[ xsplit ] ) ) ; y_star

1 / (1 + exp( y_star ) )

#### Cf
1 / (1 + exp( - ( xx - x0 * xxcoef ) ) )





x0 <- +1
exp( x0 * xxcoef ) / exp( xx[ xsplit ] )

y_star <- log( exp( x0 * (xxcoef) ) / exp( xx[ xsplit ] ) ) ; y_star

1 / (1 + exp( y_star ) )

#### Cf
1 / (1 + exp( - ( xx - x0 * xxcoef ) ) )






xxcoef <- summary(xpolr)$coefficients[ "x", "Value" ]
1 / (1 + exp(-xxcoef * (-1))) #### x is -1
1 / (1 + exp(-xxcoef * (+1))) #### x is 1


########### note the symmetry
###### the polr function estimates this coefficient for centered sigmoid
###### the intercept terms above 'shift' this effect to account for baserate probs
1 / (1 + exp(-xxcoef * (-1))) +
1 / (1 + exp(-xxcoef * (+1)))


### names(summary(xpolr))



############# make some predictions

xdf <- data.frame(y, x)

xpolrP <- polr(y ~ x, data=xdf)
summary(xpolrP)

xdf0 <- data.frame("y"=NA, "x"=-1)
predict(xpolrP, newdata=xdf0)



xdf0 <- data.frame("y"=NA, "x"=1)
predict(xpolrP, newdata=xdf0)
