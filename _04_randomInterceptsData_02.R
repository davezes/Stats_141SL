

options(stringAsFactor=FALSE, width=300)

library(lme4)

xbool_save_file <- FALSE


xdf <- read.table("data_RndIntPlus_01.tsv", sep="\t", header=TRUE)

ydf <- xdf
ydf[ , "SnackEatenInd"] <- c(0, 1)[ match(xdf[ , "SnackEaten"], c("fruit", "confection")) ]


#### plot( SweetCraving ~ SnackEatenInd + Person, data=ydf)

#### look at make believe data:
ydf

### Person is a potential blocking or nesting factor


xpalette <- rep(rainbow(10)[1:5], each=2)


if(xbool_save_file) {
    png(file.path("~", "Desktop", "randomIntData_01.png"), width=1000, height=1000, pointsize=24)
}
plot( ydf[ , "SnackEatenInd"],  ydf[ , "SweetCraving"], col=xpalette, cex=3, lwd=4, ylab="Craving Sweets", xlab="Snack Eaten (0=fruit, 1=confection)")
for(i in 1:(nrow(ydf)/2)) {
    segments(x0=0, y0=ydf[ i*2-1, "SweetCraving"], x1=1, y1=ydf[ i*2, "SweetCraving"], col=xpalette[i*2-1], lwd=4)
}
if(xbool_save_file) { dev.off() }


xlmer <- lmer(SweetCraving ~ SnackEatenInd + ( 1 | Person ), data=ydf)
summary(xlmer)
#anova(xlmer)

# attributes(xlmer)



xlm2 <- lm(SweetCraving ~ SnackEatenInd, data=ydf)
summary(xlm2)

########### is the random intercept model significantly better than using fixed effect only?
########### we're actually testing null that xsigRndEff = 0 (given 'measure')
anova(xlmer, xlm2)



summary(lm(SweetCraving ~ SnackEatenInd + Person, data=ydf))


#####################nlme

library(nlme)

xnlme <- lme(SweetCraving ~ SnackEatenInd, random= ~ 1 | Person, data = ydf)

summary(xnlme)

#anova(xnlme)

########### is the random intercept model significantly better than using fixed effect only?
########### we're actually testing null that xsigRndEff = 0 (given 'snack eaten')
anova(xnlme, xlm2)


