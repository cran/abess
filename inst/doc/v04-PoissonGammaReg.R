## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = F, message = F)

## -----------------------------------------------------------------------------
library(abess)
dat <- generate.data(n = 100, p = 6, support.size = 3,family = "poisson")
colnames(dat$x) <- c("age", "sex", "job", 
                     "education", "region", "marriage")
dat$beta
head(dat$x)
complain <- data.frame('count'=dat$y, dat$x)

## -----------------------------------------------------------------------------
library(abess)
abess_fit <- abess(x = dat$x, y = dat$y, family = "poisson")
abess_fit <- abess(count ~ ., complain, family = "poisson")
class(abess_fit)

## -----------------------------------------------------------------------------
# draw the estimated coefficients on all candidate support size
coef(abess_fit)

# get the deviance of the estimated model on all candidate support size
deviance(abess_fit)

# print the fitted model
print(abess_fit)

## -----------------------------------------------------------------------------
head(predict(abess_fit, newx = dat$x, support.size = c(3, 4)))

## -----------------------------------------------------------------------------
plot(abess_fit, label = TRUE)

## -----------------------------------------------------------------------------
plot(abess_fit, type = "tune")

## -----------------------------------------------------------------------------
abess_fit$support.size[which.min(abess_fit$tune.value)]

## -----------------------------------------------------------------------------
best.model = extract(abess_fit, support.size = 3)
str(best.model)
best.model$beta

## -----------------------------------------------------------------------------
# generate data
dat <- generate.data(n = 100, p = 6, support.size = 3, family = "gamma")
colnames(dat$x) <- c("age", "sex", "job", 
                     "education", "region", "marriage")
dat$beta
head(dat$x)
complain <- data.frame('count'=dat$y, dat$x)

abess_fit <- abess(count~., complain, family = "gamma", tune.type ="cv")

# draw the estimated coefficients on all candidate support size
coef(abess_fit)
# get the deviance of the estimated model on all candidate support size
deviance(abess_fit)

# print the fitted model
print(abess_fit)

# predict results for given support sizes
head(predict(abess_fit, newx = dat$x, support.size = c(3, 4)))

plot(abess_fit, label = TRUE)

# tuning plot
plot(abess_fit, type = "tune")

# extract fitted model
best.model = extract(abess_fit, support.size = 3)
str(best.model)

# estimated coefficients
best.model$beta

