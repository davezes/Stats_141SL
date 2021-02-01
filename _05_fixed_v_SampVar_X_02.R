


options(stringsAsFactors=FALSE)

xbool_save_file <- TRUE

##################### barchart for proportions

nn <- 2 * 10^(5)


n <- 30

set.seed(780)

x <- rcauchy(n)

hist(x)

xsig <- 3

b_1 <- 1


y <- b_1 * x + rnorm(n, 0, xsig)

if(xbool_save_file) {
    png(file.path("~", "Desktop", "highLeverageSim_01.png"), width=1000, height=1000, pointsize=24)
}
plot(x,y, cex=1.4, lwd=2)
if(xbool_save_file) { dev.off() }



xlm <- lm(y~x-1)

summary(xlm)

xsig_hat <- sqrt( sum( (y - xlm$fitted)^2 ) / (n-1) ) ; xsig_hat
xSEest <- xsig_hat / sqrt(sum(x^2)) ; xSEest

xSEtrue <- xsig / sqrt(sum(x^2)) ; xSEtrue


##################### sim fixed x

xSEs_vec <- numeric(nn)
xbetas_vec <- numeric(nn)

kk <- 1
while(kk <= nn) {
    
    y <- b_1 * x + rnorm(n, 0, xsig)
    xlm <- lm(y~x-1)
    
    xSEs_vec[kk] <- summary(xlm)$coef[1,2]
    xbetas_vec[kk] <- summary(xlm)$coef[1,1]
    
    kk <- kk + 1
    
    if(kk %% 10000 == 0) { cat(kk, "\n") }
}

if(xbool_save_file) {
    png(file.path("~", "Desktop", "distributionSimBhat_fixedX_01.png"), width=1000, height=1000, pointsize=24)
}
hist(xbetas_vec, lwd=1.4)
abline(v=b_1, lwd=3)
if(xbool_save_file) { dev.off() }


mean(xbetas_vec)
sd(xbetas_vec)
xSEtrue




########################


xSEs_vec2 <- numeric(nn)
xbetas_vec2 <- numeric(nn)

kk <- 1
while(kk <= nn) {
    
    x <- rcauchy(n)
    
    y <- b_1 * x + rnorm(n, 0, xsig)
    xlm <- lm(y~x-1)
    
    xSEs_vec2[kk] <- summary(xlm)$coef[1,2]
    xbetas_vec2[kk] <- summary(xlm)$coef[1, 1]
    
    kk <- kk + 1
    
    if(kk %% 10000 == 0) { cat(kk, "\n") }
}

if(xbool_save_file) {
    png(file.path("~", "Desktop", "distributionSimBhat_rndX__01.png"), width=1000, height=1000, pointsize=24)
}
hist(xbetas_vec2, lwd=1.4)
abline(v=b_1, lwd=3)
if(xbool_save_file) { dev.off() }


mean(xSEs_vec2)
xSEtrue




xbrks <- seq(-0.8, 2.8, by=0.02)

if(xbool_save_file) {
    png(file.path("~", "Desktop", "distributionSimBhat_both_01.png"), width=1600, height=1000, pointsize=24)
}
par(mfrow=c(2, 1), mar=c(2, 4, 4, 1))
hist(xbetas_vec, lwd=1.4, main="Sim betas, Fixed x", breaks=xbrks)
abline(v=b_1, lwd=5, col="#33DD33")
hist(xbetas_vec2, lwd=1.4, main="Sim betas, x Subject to Cauchy Sampling Var", breaks=xbrks)
abline(v=b_1, lwd=5, col="#33DD33")
if(xbool_save_file) { dev.off() }



