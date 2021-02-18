

options(stringAsFactor=FALSE, width=300)

library(lme4)

xbool_save_file <- FALSE


n <- 15 #### must be an integer multiple of 3

set.seed(778)

xid <- rep( I(1:(n/3)), each=3 )

xtxdom <- c(1, 2, 3)
xrepmeas <- rep( xtxdom, n/3 )

xmeasureInd <- xtxdom






 #### true standard deviation of random effect
#### try different values
xsigRndEff <- 1
indiv_slope <- rep(rnorm(n/3, 0, xsigRndEff), each=3)

xerrs <- rnorm(n, 0, 1.0)

xsigRndInt <- 3
indiv_intercept <- rep(rnorm(n/3, 0, xsigRndInt), each=3) ##### fixed intercept

xbeta <- 1

y_true <- indiv_intercept + xbeta * xmeasureInd +    indiv_slope * xmeasureInd + xerrs


## xdf <- data.frame("id"=xid, "measure"=xrepmeas, "y"=y_true)
xdf <- data.frame("id"=as.character(xid), "measure"=xrepmeas, "y"=y_true)



xpalette <- rep(rainbow(2*n/3)[1:(n/3)], each=3)

xpalette <- rep(rainbow(n/3), each=3)

ydf <- xdf

ydf


if(xbool_save_file) {
    png(file.path("~", "Desktop", "randomIntAndSlopeDataSimDataOnly_01.png"), width=1000, height=1000, pointsize=24)
}
plot( ydf[ , "measure"],  ydf[ , "y"], col=xpalette, cex=3, lwd=7, ylab="y", xlab="Measure")
if(xbool_save_file) { dev.off() }




xlmer <- lmer(y ~ measure + ( measure | id ) + ( 1 | id ), data=ydf)
summary(xlmer)
#anova(xlmer)



summary(xlmer)$coefficients

xfixed_slope <- summary(xlmer)$coefficients[ 2, 1 ] ; xfixed_slope

xfixed_intercept <- summary(xlmer)$coefficients[ 1, 1 ] ; xfixed_intercept

########## is it possible to recover actual fitted intercepts ????

########## YES!

coef(xlmer)$id ### already have constant, fixed effect baked in
xxslope <- coef(xlmer)$id[ , "measure"] ; xxslope
xxint <- coef(xlmer)$id[ , "(Intercept)"] ; xxint

xxint_adj <- xxint
xxslope_adj <- xxslope

########### or, alternately
### ranef(xlmer)$id[ , "measure"] + xfixed_slope








if(xbool_save_file) {
    png(file.path("~", "Desktop", "randomSlopeAndIntDataSimFit_01.png"), width=1000, height=1000, pointsize=24)
}
plot( ydf[ , "measure"],  ydf[ , "y"], col=xpalette, cex=3, lwd=7, ylab="y", xlab="Measure")
for(i in 1:(nrow(ydf)/3)) {
    #abline(a=indiv_intercept, b=indiv_slope[i*3-1] + xbeta, col=xpalette[i*3-1], lwd=7)
    
    segments(x0=1.0, y0=xxint_adj[i] + (1*xxslope_adj[i]), x1=3.0, y1=xxint_adj[i] + (3*xxslope_adj[i]), col=xpalette[i*3-2], lwd=7)
    
    #segments(x0=0, y0=ydf[ i*2-1, "y"], x1=1, y1=ydf[ i*2, "y"], col=xpalette[i*2-1], lwd=4)
}
if(xbool_save_file) { dev.off() }







summary(xlmer)

xlm2 <- lm(y ~ measure, data=ydf)
summary(xlm2)

########### is the random slope model significantly better than using fixed effect only?
########### we're actually testing null that xsigRndEff = 0 (given 'measure')
anova(xlmer, xlm2)


###################


xlm3 <- lm(y ~ measure*id, data=ydf)
summary(xlm3)

yhat <- predict(xlm3)


if(xbool_save_file) {
    png(file.path("~", "Desktop", "randomSlopeAndIntDataSimInteractionFit_01.png"), width=1000, height=1000, pointsize=24)
}
plot( ydf[ , "measure"],  ydf[ , "y"], col=xpalette, cex=3, lwd=7, ylab="y", xlab="Measure", main="Fixed Interaction Model")
for(i in 1:(nrow(ydf)/3)) {
    #abline(a=indiv_intercept, b=indiv_slope[i*3-1] + xbeta, col=xpalette[i*3-1], lwd=7)
    
    segments(x0=1.0, y0=yhat[i*3-2], x1=3.0, y1=yhat[i*3], col=xpalette[i*3-2], lwd=7)
    
    #segments(x0=0, y0=ydf[ i*2-1, "y"], x1=1, y1=ydf[ i*2, "y"], col=xpalette[i*2-1], lwd=4)
}
if(xbool_save_file) { dev.off() }








anova(xlmer, xlm3)


