

options(stringsAsFactors=FALSE, width=350)

rm(list=ls())


####### make sure in working directory
source("___f_funs.R")

xbool_save_file <- FALSE


############# goodness of fit.  Coin flipping, 2 groups, 1 df
############# SIMULATION
############# try comparing this result with that using G*Power

## set.seed(777)

nn <- 10^6

H0mu <- 50/100
xmu <- 64/100 ######## true p heads
n <- 200

ee1 <- H0mu * n ; ee1
ee2 <- (1-H0mu) * n ; ee2


xtestvals <- rep(NA, nn)

for(ii in 1:nn) {
    
    x <- rbinom(1, size=n, prob=xmu) ; x
    

    
    testval <- (x - ee1)^2/ee1 + (n-x - ee2)^2/ee2 ; testval ### correct
    
    xtestvals[ii] <- testval
    
}

mean(xtestvals)

############### here's a simple empiric way to get to lambda, knowing that mean(X) = df + lambda

cat("est of noncentrality parameter, lambda:", mean(xtestvals) - 1, "\n")

hist(xtestvals)



##### what is this?
xw <- sqrt( (mean(xtestvals)-1) / n ) ; xw
### note that for 2-cells, this is approximately 2 * abs(xmu - H0mu)


xalpha <- 0.01 #### here

xdf <- 1 ; xdf #### here

############### crit cut-off under NULL
testcut <-  qchisq(xalpha, df=xdf, lower.tail=FALSE) ; testcut


###### empiric estimate of test power
sum(xtestvals > testcut) / nn


d <- density(xtestvals)
xlim <- c( 0.1, max( c(5, max(xtestvals) ) ) ) ; xdom <- seq(xlim[1], xlim[2], length=1000)

if(xbool_save_file) {
    png(file.path("~", "Desktop", "chisq_power_example_01.png"), width=1500, height=480, pointsize=24)
}
par(mar=c(4,4,2,1))
plot(xdom, dchisq(xdom, df=xdf), main="ChiSq Goodness of Fit, 2 Cells, Sim Example", col="#AA0000", lwd=2, type="l", ylab="density", xlab="chisq")
points(d, col="#0000AA", lwd=2, type="l")
abline(v=testcut)
if(xbool_save_file) { dev.off() }

