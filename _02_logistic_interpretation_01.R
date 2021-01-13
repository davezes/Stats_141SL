

options(stringsAsFactors=FALSE, width=350)

rm(list=ls())

####### make sure in working directory
source("___f_funs.R")


xbool_save_file <- FALSE




n <- 240 ### must be divisible by total number of groups

x1dom <- c("A", "B", "C", "D")
x2dom <- c("yes", "no")

x1 <- rep(x1dom, each=60)

x2 <-
c(
### distribution of yes/no for Group A
rep("yes", 35),
rep("no", 25),

### distribution of yes/no for Group B
rep("yes", 20),
rep("no", 40),

### distribution of yes/no for Group C
rep("yes", 10),
rep("no", 50),

### distribution of yes/no for Group D
rep("yes", 55),
rep("no", 5)
)

table(x1, x2)




ybin <- as.integer(x2 %in% "yes") ; ybin

xdf <- data.frame("y"=ybin, "x"=x1)
xdf

xglm <- glm(y~x, data=xdf, family=binomial(link="logit"))

summary(xglm)



log( (30/60) / (30/60) ) ### for-total for A, divided by against-total for A

log( (20/40) / (35/25) ) ### for-against for B, divided by for-against for A

log( (10/50) / (35/25) ) ### for-against for C, divided by for-against for A

log( (55/5) / (35/25) ) ### for-against for D, divided by for-against for A




