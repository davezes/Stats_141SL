

options(stringAsFactor=FALSE, width=300)

library(lme4)

xbool_save_file <- FALSE


n <- 60

set.seed(777)

xid <- rep( I(1:20), each=3 )

xtxdom <- c(1, 2, 3)
xrepmeas <- rep( xtxdom, 20 )

xmeasureInd <- xtxdom






 #### true standard deviation of random effect
#### try different values
xsigRndEff <- 2/3
indiv_slope <- rep(rnorm(20, 0, xsigRndEff), each=3)

xerrs <- rnorm(n, 0, 1.0)

indiv_intercept <- 0.5 ##### fixed slope

xbeta <- 0.5

y_true <- indiv_intercept + xbeta * xmeasureInd +    indiv_slope * xmeasureInd + xerrs


## xdf <- data.frame("id"=xid, "measure"=xrepmeas, "y"=y_true)
xdf <- data.frame("id"=as.character(xid), "measure"=xrepmeas, "y"=y_true)



xpalette <- rep(rainbow(40)[1:20], each=3)

ydf <- xdf


if(xbool_save_file) {
    png(file.path("~", "Desktop", "randomSlopeDataSim_01.png"), width=1000, height=1000, pointsize=24)
}
plot( ydf[ , "measure"],  ydf[ , "y"], col=xpalette, cex=3, lwd=4, ylab="y", xlab="Measure")
for(i in 1:(nrow(ydf)/3)) {
    abline(a=indiv_intercept, b=indiv_slope[i*3-1] + xbeta, col=xpalette[i*3-1], lwd=4)
    #segments(x0=0, y0=ydf[ i*2-1, "y"], x1=1, y1=ydf[ i*2, "y"], col=xpalette[i*2-1], lwd=4)
}
if(xbool_save_file) { dev.off() }





xlmer <- lmer(y ~ measure + ( measure | id ), data=xdf)
summary(xlmer)
#anova(xlmer)




xlm2 <- lm(y ~ measure, data=xdf)
summary(xlm2)

########### is the random slope model significantly better than using fixed effect only?
########### we're actually testing null that xsigRndEff = 0 (given 'measure')
anova(xlmer, xlm2)


