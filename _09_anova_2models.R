

########### from Q2, g


x1 <- rep( c("Y", "N"), 500 )
x2 <- rep( c("a", "b"), each=500 )

############ the interaction contributes to y
y_star <- as.integer( x1 == "Y" & x2 == "a" ) * 2

p_true <- 1 / ( 1 + exp( - y_star ) )

y <- rbinom( length(x1), size=1, prob=p_true )


m1 <- glm(y~x1*x2, family=binomial(link="logit"))
summary(m1)

m2 <- glm(y~x1+x2, family=binomial(link="logit"))
summary(m2)


anova(m1, m2, test="Chisq")


anova(m2, m1, test="Chisq")





