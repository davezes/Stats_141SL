

options(stringsAsFactors=FALSE, width=350)

rm(list=ls())

####### make sure in working directory
source("___f_funs.R")


xbool_save_file <- FALSE


############# simple two-way ANOVA F-test
############# SIMULATION
############# try comparing this result with that using G*Power

set.seed(777)


nn <- 300000

n <- 100 ### must be divisible by total number of groups

x1dom <- c("A", "B", "C", "D")
x2dom <- c("c", "d")

x1 <- rep(x1dom, each=n/length(x1dom))
x2 <- rep(x2dom, n/length(x2dom))

table(x1, x2)


xAmask <- x1 %in% c("A")
xBmask <- x1 %in% c("B")
xCmask <- x1 %in% c("C")
xDmask <- x1 %in% c("D")

xgrandmu <- 0

xgrandsigma <- 1
xsigma <- xgrandsigma

H0ABdelta <- 1 / xsigma ##### 

### H0ABdelta <- 0.5 / xsigma #####


xsigB <-
sd(
c(H0ABdelta / 2, -H0ABdelta / 2, 0, 0, 0)
)

ff <- xsigB / xsigma ; ff ### f in G*Power


xfvals <- rep(NA, nn)

for(ii in 1:nn) {
    
    y <- rnorm(n, xgrandmu, xsigma)
    

    
    y[ xAmask ] <- y[ xAmask ] + H0ABdelta / 2
    y[ xBmask ] <- y[ xBmask ] - H0ABdelta / 2
    
    var(y)
    
    ybargrand <- mean(y)
    
    yAmean <-  mean(y[ xAmask ]) ; yAmean
    yBmean <-  mean(y[ xBmask ]) ; yBmean
    yCmean <-  mean(y[ xCmask ]) ; yCmean
    yDmean <-  mean(y[ xDmask ]) ; yDmean
    
    SSb <-
    sum(xAmask) * ( yAmean - ybargrand )^2 +
    sum(xBmask) * ( yBmean - ybargrand )^2 +
    sum(xCmask) * ( yCmean - ybargrand )^2 +
    sum(xDmask) * ( yDmean - ybargrand )^2     ; SSb
    
    SSw <-
    sum( (y[ xAmask ] - yAmean)^2 ) +
    sum( (y[ xBmask ] - yBmean)^2 ) +
    sum( (y[ xCmask ] - yCmean)^2 ) +
    sum( (y[ xDmask ] - yDmean)^2 )    ; SSw
    
    
    SStot <- sum( (y - ybargrand)^2 ) ; SStot
    SSb + SSw
    
    MSb <- SSb / (4-1)
    MSw <- SSw / (n-4)
    
    #### s2pooled <- ((n1-1)*var(x1) + (n2-1)*var(x2)) / (n1 + n2 - 2)
    
    fval <- MSb / MSw
    
    xfvals[ii] <- fval
    
}

#### MSb / MStot

#### non-centrality
xlambda <- sum( c( H0ABdelta/2, -H0ABdelta/2, 0, 0 )^2 ) / ( xsigma^2 / 25) ; xlambda



mean(xfvals)

hist(xfvals)


xalpha <- 0.01 #### here

fcut <-  qf(xalpha, df1=3, df2=96, lower.tail=FALSE) ; fcut




###### empiric estimate of test power
sum(xfvals > fcut) / nn


d <- density(xfvals)
xlim <- c( min( c(0, xfvals) ), max( c(5, max(xfvals) ) ) ) ; xdom <- seq(xlim[1], xlim[2], length=1000)

if(xbool_save_file) {
    png(file.path("~", "Desktop", "f_power_anova_example_02.png"), width=1500, height=480, pointsize=24)
}
par(mar=c(4,4,2,1))
plot(xdom, df(xdom, df1=3, df2=96), main="ANOVA, 4 Groups, Sim Example", col="#AA0000", lwd=2, type="l", ylab="density", xlab="F")
points(d, col="#0000AA", lwd=2, type="l")
abline(v=fcut)
if(xbool_save_file) { dev.off() }

