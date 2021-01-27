


options(stringsAsFactors=FALSE)

xbool_save_file <- FALSE

##################### barchart for proportions

nn <- 2 * 10^(5)


n <- 30

set.seed(780)

x <- rcauchy(n)

hist(x)

xsig <- 3

y <- x + rnorm(n, 0, xsig)

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

kk <- 1
while(kk <= nn) {
    
    y <- x + rnorm(n, 0, xsig)
    xlm <- lm(y~x-1)
    
    xSEs_vec[kk] <- summary(xlm)$coef[1,2]
    
    kk <- kk + 1
    
    if(kk %% 10000 == 0) { cat(kk, "\n") }
}

if(xbool_save_file) {
    png(file.path("~", "Desktop", "distributionSimBhat_01.png"), width=1000, height=1000, pointsize=24)
}
hist(xSEs_vec, lwd=1.4)
abline(v=xSEtrue, lwd=3)
if(xbool_save_file) { dev.off() }


mean(xSEs_vec)
xSEtrue




########################


xSEs_vec2 <- numeric(nn)

kk <- 1
while(kk <= nn) {
    
    x <- rcauchy(n)
    
    y <- x + rnorm(n, 0, xsig)
    xlm <- lm(y~x-1)
    
    xSEs_vec2[kk] <- summary(xlm)$coef[1,2]
    
    kk <- kk + 1
    
    if(kk %% 10000 == 0) { cat(kk, "\n") }
}

if(xbool_save_file) {
    png(file.path("~", "Desktop", "distributionSimBhat_01.png"), width=1000, height=1000, pointsize=24)
}
hist(xSEs_vec2, lwd=1.4)
abline(v=xSEtrue, lwd=3)
if(xbool_save_file) { dev.off() }


mean(xSEs_vec2)
xSEtrue




if(xbool_save_file) {
    png(file.path("~", "Desktop", "distributionSimBhat_both_01.png"), width=2000, height=1000, pointsize=24)
}
par(mfrow=c(1, 2))
hist(xSEs_vec, lwd=1.4, main="Sim SEs, Fixed x")
abline(v=xSEtrue, lwd=5, col="#33DD33")
hist(xSEs_vec2, lwd=1.4, main="Sim SEs, x Subject to Cauchy Sampling Var")
abline(v=xSEtrue, lwd=5, col="#33DD33")
if(xbool_save_file) { dev.off() }



