

options(stringsAsFactors=FALSE, width=350)


####### make sure in working directory
source("___f_funs.R")



############# sim data

set.seed(777)

x1 <- c( rep("A", 50), rep("B", 50) )
x2 <- c( rep("c", 25), rep("b", 25), rep("c", 25), rep("b", 25) )

y <- c( rnorm(75), rnorm(25, 1, 1) )


xlm <- lm(y ~ x1 + x2 )

summary(xlm)




xlm <- lm(y ~ x1:x2 )
summary(xlm)

###### same predictive model as

xlm <- lm(y ~ x1*x2 )
summary(xlm)


