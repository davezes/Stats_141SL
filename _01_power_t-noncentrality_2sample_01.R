

options(stringsAsFactors=FALSE, width=350)

rm(list=ls())

####### make sure in working directory
source("___f_funs.R")


xbool_save_file <- FALSE


############# two-sample test of mean -- unknown but equal variances -- t-test
############# SIMULATION
############# try comparing this result with that using G*Power

set.seed(777)


nn <- 300000

H0delta <- 0
xmu1 <- 0.25
xmu2 <- -0.25
xsigma1 <- 1
xsigma2 <- 1
n1 <- 50
n2 <- 50

xtvals <- rep(NA, nn)

for(ii in 1:nn) {
    
    x1 <- rnorm(n1, xmu1, xsigma1)
    x2 <- rnorm(n2, xmu2, xsigma2)
    
    s2pooled <- ((n1-1)*var(x1) + (n2-1)*var(x2)) / (n1 + n2 - 2)
    
    tval <- (mean(x1) - mean(x2) - H0delta) / sqrt( s2pooled * (1/n1 + 1/n2) ) ### correct
    
    xtvals[ii] <- tval
    
}

mean(xtvals)

hist(xtvals)

############ equal variances -- note that even though population var is not known, we still measure effect size using it
cohensd <- (xmu1 - xmu2) / xsigma1 ; cohensd

noncent <- cohensd * sqrt( n1*n2 / (n1+n2) ) ; noncent ###  this value for non-centrality calculated in G*Power

xdf <- n1 + n2 - 2 ; xdf #### here

xalpha <- 0.01 #### here

tcut <-  - qt(xalpha, df=xdf) ; tcut




###### empiric estimate of test power
sum(xtvals > tcut) / nn


d <- density(xtvals)
xlim <- c( min( c(-5, xtvals) ), max( c(5, max(xtvals) ) ) ) ; xdom <- seq(xlim[1], xlim[2], length=1000)

if(xbool_save_file) {
    png(file.path("~", "Desktop", "t_power_2sample_example_01.png"), width=1500, height=480, pointsize=24)
}
par(mar=c(4,4,2,1))
plot(xdom, dt(xdom, df= xdf), main="Two Sample Test of Means, Sim Example", col="#AA0000", lwd=2, type="l", ylab="density", xlab="t")
points(d, col="#0000AA", lwd=2, type="l")
abline(v=tcut)
if(xbool_save_file) { dev.off() }

