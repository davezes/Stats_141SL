

options(stringAsFactor=FALSE, width=300)

library(lme4)

xbool_save_file <- FALSE


n <- 10

set.seed(778)

xid <- rep( I(1:(n/2)), each=2 )

xtxdom <- c("Before", "xAfter")
xrepmeas <- rep( xtxdom, n/2 )

xmeasureInd <- c(0, 1)[ match(xrepmeas, xtxdom) ]






 #### true standard deviation of random effect
#### try different values
xsigRndEff <- 2/3
xsigRndEff <- 10 #### more pronounced

indiv_intercept <- rep(rnorm(n/2, 0, xsigRndEff), each=2)

xerrs <- rnorm(n)

xbeta <- 0.5 ##### fixed slope
xbeta <- 5 ##### fixed slope #### more pronounced



y_true <- indiv_intercept + xbeta * xmeasureInd + xerrs


## xdf <- data.frame("id"=xid, "measure"=xrepmeas, "y"=y_true)
xdf <- data.frame("id"=as.character(xid), "measure"=xrepmeas, "y"=y_true)



xpalette <- rep(rainbow(n)[1:(n/2)], each=2)

ydf <- xdf
ydf[ , "measure"] <- c(0, 1)[ match(xdf[ , "measure"], c("Before", "xAfter")) ]


########### look at data set
head(ydf, 10)



####################### plot points

if(xbool_save_file) {
    png(file.path("~", "Desktop", "randomIntDataSimIntsDataOnly_01.png"), width=1000, height=1000, pointsize=24)
}
plot( ydf[ , "measure"],  ydf[ , "y"], col=xpalette, cex=3, lwd=7, ylab="y", xlab="Tx (0=Before, 1=After)", main="Standardized Muscle Mass Increase ~vs~ Tx")
text(x=0.1, y=ydf[ seq(1, n, by=2), "y" ], ydf[ seq(1, n, by=2), "id" ] )
if(xbool_save_file) { dev.off() }





xlmer <- lmer(y ~ measure + ( 1 | id ), data=xdf)
summary(xlmer)
#anova(xlmer)

# attributes(xlmer)

#xresids <- y_true - predict(xlmer)
#xvarResids <- mean( xresids^2 ) ; xvarResids
#xvarIntercept <- attr(summary(xlmer)$varcor$id, "stddev")^2 ; xvarIntercept

### xxu <- attr(xlmer, "u") ; xxu

summary(xlmer)$coefficients

xfixed_slope <- summary(xlmer)$coefficients[ 2, 1 ] ; xfixed_slope

xfixed_intercept <- summary(xlmer)$coefficients[ 1, 1 ] ; xfixed_intercept

########## is it possible to recover actual fitted intercepts ????

########## YES!
coef(xlmer)$id
xxu <- coef(xlmer)$id[ , 1] ; xxu

########### or, alternately
ranef(xlmer)$id + xfixed_intercept



if(xbool_save_file) {
    png(file.path("~", "Desktop", "randomIntDataSimInts_01.png"), width=1000, height=1000, pointsize=24)
}
plot( ydf[ , "measure"],  ydf[ , "y"], col=xpalette, cex=3, lwd=7, ylab="y", xlab="Tx (0=Before, 1=After)", main="Standardized Muscle Mass Increase ~vs~ Tx")
for(i in 1:(nrow(ydf)/2)) {
    segments(x0=0, y0=xxu[i], x1=1, y1=xxu[i] + 1*xfixed_slope, col=xpalette[i*2-1], lwd=7)
}
if(xbool_save_file) { dev.off() }




####################### ignore id
xlm2 <- lm(y ~ measure, data=xdf)
summary(xlm2)


################ whats this?
if(xbool_save_file) {
    png(file.path("~", "Desktop", "constantIntDataSim_01.png"), width=1000, height=1000, pointsize=24)
}
plot( ydf[ , "measure"],  ydf[ , "y"], col="#333333", cex=3, lwd=4, ylab="y", xlab="Tx (0=Before, 1=After)", main="Standardized Muscle Mass Increase ~vs~ Tx")
abline(xlm2, lwd=4)
if(xbool_save_file) { dev.off() }


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


