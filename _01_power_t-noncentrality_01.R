

options(stringsAsFactors=FALSE, width=350)

rm(list=ls())

####### make sure in working directory
source("___f_funs.R")


xbool_save_file <- FALSE


############# one-sample test of mean -- unknown variance -- t-test
############# SIMULATION
############# try comparing this result with that using G*Power

set.seed(777)


nn <- 300000

H0mu <- 0
xmu <- 2
xsigma <- 2
n <- 21

xtvals <- rep(NA, nn)

for(ii in 1:nn) {
    
    x <- rnorm(n, xmu, xsigma)
    
    tval <- (mean(x) - H0mu) / sqrt( var(x) / n ) ### correct
    
    xtvals[ii] <- tval
    
}

mean(xtvals)

hist(xtvals)

cohensd <- (xmu - H0mu) / xsigma ; cohensd

noncent <- cohensd * sqrt(n) ; noncent ### use this value for non-centrality parameter in G*Power


xalpha <- 0.01 #### here

##### critical cut off under NULL
tcut <-  - qt(xalpha, df=n-1) ; tcut

xdf <- n - 1 ; xdf #### here


###### empiric estimate of test power
sum(xtvals > tcut) / nn


d <- density(xtvals)
xlim <- c( min( c(-5, xtvals) ), max( c(5, max(xtvals) ) ) ) ; xdom <- seq(xlim[1], xlim[2], length=1000)

if(xbool_save_file) {
    png(file.path("~", "Desktop", "t_power_example_01.png"), width=1500, height=480, pointsize=24)
}
par(mar=c(4,4,2,1))
plot(xdom, dt(xdom, df= xdf), main="One Sample Test of Mean, Sim Example", col="#AA0000", lwd=2, type="l", ylab="density", xlab="t")
points(d, col="#0000AA", lwd=2, type="l")
abline(v=tcut)
if(xbool_save_file) { dev.off() }

