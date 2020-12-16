

options(stringAsFactor=FALSE, width=300)

library(lme4)

xbool_save_file <- FALSE


n <- 100

set.seed(777)

xid <- rep( I(1:50), each=2 )

xtxdom <- c("Before", "xAfter")
xrepmeas <- rep( xtxdom, 50 )

xmeasureInd <- c(0, 1)[ match(xrepmeas, xtxdom) ]






 #### true standard deviation of random effect
#### try different values
xsigRndEff <- 2/3
indiv_intercept <- rep(rnorm(50, 0, xsigRndEff), each=2)

xerrs <- rnorm(n)

xbeta <- 0.5 ##### fixed slope

y_true <- indiv_intercept + xbeta * xmeasureInd + xerrs


## xdf <- data.frame("id"=xid, "measure"=xrepmeas, "y"=y_true)
xdf <- data.frame("id"=as.character(xid), "measure"=xrepmeas, "y"=y_true)



xpalette <- rep(rainbow(70)[1:50], each=2)

ydf <- xdf
ydf[ , "measure"] <- c(0, 1)[ match(xdf[ , "measure"], c("Before", "xAfter")) ]


if(xbool_save_file) {
    png(file.path("~", "Desktop", "randomIntDataSim_01.png"), width=1000, height=1000, pointsize=24)
}
plot( ydf[ , "measure"],  ydf[ , "y"], col=xpalette, cex=3, lwd=4, ylab="y", xlab="Measure (0=Before, 1=After)")
for(i in 1:(nrow(ydf)/2)) {
    segments(x0=0, y0=ydf[ i*2-1, "y"], x1=1, y1=ydf[ i*2, "y"], col=xpalette[i*2-1], lwd=4)
}
if(xbool_save_file) { dev.off() }





xlmer <- lmer(y ~ measure + ( 1 | id ), data=xdf)
summary(xlmer)
#anova(xlmer)

# attributes(xlmer)

#xresids <- y_true - predict(xlmer)
#xvarResids <- mean( xresids^2 ) ; xvarResids
#xvarIntercept <- attr(summary(xlmer)$varcor$id, "stddev")^2 ; xvarIntercept



xlm2 <- lm(y ~ measure, data=xdf)
summary(xlm2)

########### is the random intercept model significantly better than using fixed effect only?
########### we're actually testing null that xsigRndEff = 0 (given 'measure')
anova(xlmer, xlm2)



#####################nlme

library(nlme)

xnlme <- lme(y ~ measure, random= ~ 1 | id, data = xdf)

summary(xnlme)

#anova(xnlme)

########### is the random intercept model significantly better than using fixed effect only?
########### we're actually testing null that xsigRndEff = 0 (given 'measure')
anova(xnlme, xlm2)


