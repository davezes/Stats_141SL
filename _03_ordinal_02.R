

options(stringAsFactor=FALSE, width=300)

library(MASS)

xbool_save_file <- FALSE


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


######################
table(x > 0, y)




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



###### Pr( Y > 1 | x = x0 ) / Pr( Y <= 1 | x = x0 ) = exp( x0 * xxcoef - xx[1] )

#### pJ = Pr[ Y > J ]
#### logit(pJ) = b x0 - eta0J

Js <- c(1,2,3,4)


#################
x0 <- -1
y_star_m1 <- x0 * xxcoef - xx[ Js ] ; y_star_m1
1 / ( 1 + exp( -y_star_m1 ) )



x0 <- 0
y_star_z <- x0 * xxcoef - xx[ Js ] ; y_star_z
1 / ( 1 + exp( -y_star_z ) )

####  table(x > -0.3 & x < 0.3, y) ## just for fun



x0 <- +1
y_star_p1 <- x0 * xxcoef - xx[ Js ]  ; y_star_p1
1 / ( 1 + exp( -y_star_p1 ) )



##############

( 1 * xxcoef - xx[ Js ] ) - ( 0 * xxcoef - xx[ Js ] )


log( 1 / ( 1 + exp( -y_star_z ) ) ) - log( 1 - 1 / ( 1 + exp( -y_star_z ) ) )


log( 1 / ( 1 + exp( -y_star_p1 ) ) ) - log( 1 - 1 / ( 1 + exp( -y_star_p1 ) ) )


##############################
##############################


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



