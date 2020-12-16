

options(stringsAsFactors=FALSE, width=350)


####### make sure in working directory
source("___f_funs.R")


xbool_save_file <- FALSE


############# one-sample test of mean -- known variance -- z-test
############# SIMULATION
############# try comparing this result with that using G*Power

set.seed(777)


nn <- 300000

H0mu <- 0
xmu <- 2
xsigma <- 2 #### known
n <- 20

xzvals <- rep(NA, nn)

for(ii in 1:nn) {
    
    x <- rnorm(n, xmu, xsigma)
    
    zval <- (mean(x) - H0mu) / sqrt( xsigma^2 / n ) ### correct
    
    xzvals[ii] <- zval
    
}



hist(xzvals)

cohensd <- (xmu - H0mu) / xsigma ; cohensd

noncent <- cohensd * sqrt(n) ; noncent ### use this value for non-centrality parameter in G*Power

### agrees with
mean(xzvals)


xalpha <- 0.01 #### here

zcut <-  - qnorm(xalpha) ; zcut



###### empiric estimate of test power
sum(xzvals > zcut) / nn


d <- density(xzvals)
xlim <- c( min( c(-5, xzvals) ), max( c(5, max(xzvals) ) ) ) ; xdom <- seq(xlim[1], xlim[2], length=1000)

if(xbool_save_file) {
    png(file.path("~", "Desktop", "z_power_example_01.png"), width=1500, height=480, pointsize=24)
}
par(mar=c(4,4,2,1))
plot(xdom, dnorm(xdom), main="One Sample Test of Mean, Sim Example", col="#AA0000", lwd=2, type="l", ylab="density", xlab="z")
points(d, col="#0000AA", lwd=2, type="l")
abline(v=zcut)
if(xbool_save_file) { dev.off() }

