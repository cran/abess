## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------
knitr::opts_chunk$set(warning = FALSE, eval = TRUE, message = FALSE)
Sys.setenv("OMP_THREAD_LIMIT" = 2)

## -----------------------------------------------------------------------------
library(abess)
synthetic_data <- generate.data(n = 300, p = 1000, 
                                beta = c(3, 1.5, 0, 0, 2, rep(0, 995)))
dim(synthetic_data[["x"]])
head(synthetic_data[["y"]])
dat <- cbind.data.frame("y" = synthetic_data[["y"]], 
                        synthetic_data[["x"]])

## -----------------------------------------------------------------------------
abess_fit <- abess(y ~ ., data = dat, support.size = 3)

## -----------------------------------------------------------------------------
head(coef(abess_fit, sparse = FALSE))

## -----------------------------------------------------------------------------
lm(y ~ ., data = dat[, c(1, c(1, 2, 5) + 1)])

## -----------------------------------------------------------------------------
abess_fit <- abess(y ~ ., data = dat)

## -----------------------------------------------------------------------------
best_size <- abess_fit[["best.size"]]
print(best_size)
head(coef(abess_fit, support.size = best_size, sparse = FALSE))

## -----------------------------------------------------------------------------
Hitters <- read.csv("Hitters.csv", header = TRUE)
head(Hitters)
dim(Hitters)
sum(is.na(Hitters))

## -----------------------------------------------------------------------------
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

## -----------------------------------------------------------------------------
Hitters <- model.matrix(~., Hitters)[, -1]
Hitters <- as.data.frame(Hitters)

## -----------------------------------------------------------------------------
library(abess)
abess_fit <- abess(Salary ~ ., Hitters)
abess_fit <- abess(Hitters[, -which(colnames(Hitters) == "Salary")], Hitters$Salary)
class(abess_fit)

## -----------------------------------------------------------------------------
# draw the estimated coefficients on all candidate support size
coef(abess_fit)

# get the deviance of the estimated model on all candidate support size
deviance(abess_fit)

# print the fitted model
print(abess_fit)

## -----------------------------------------------------------------------------
hitters_pred <- predict(abess_fit, 
                        newx = Hitters[, -which(colnames(Hitters) == "Salary")], 
                        support.size = c(3, 4))
head(hitters_pred)

## -----------------------------------------------------------------------------
plot(abess_fit, label = TRUE)

## -----------------------------------------------------------------------------
plot(abess_fit, type = "tune")

## -----------------------------------------------------------------------------
extract(abess_fit)[["support.size"]]

## -----------------------------------------------------------------------------
best.model <- extract(abess_fit, support.size = 6)
str(best.model)

## -----------------------------------------------------------------------------
working_directory <- getwd()
if (file.exists("crime.rda")) {
  load("crime.rda")
} else {
  crime_data_url <- "https://github.com/abess-team/abess/raw/master/R-package/data-raw/crime.rda"
  download.file(crime_data_url, "crime.rda")
  load(file.path(working_directory, "crime.rda"))
}

## -----------------------------------------------------------------------------
dim(crime)

## -----------------------------------------------------------------------------
abess_fit <- abess(y ~ ., data = crime, screening.num = 1000)
str(abess_fit)

## -----------------------------------------------------------------------------
head(abess_fit[["screening.vars"]])

## -----------------------------------------------------------------------------
best_model <- extract(abess_fit)
str(best_model)
best_vars <- best_model[["support.vars"]]
best_vars

## ---- eval=FALSE, echo=FALSE--------------------------------------------------
#  lm_dat <- cbind.data.frame(crime[, c("y", best_vars)])
#  lm_fit <- lm(y ~ ., data = lm_dat)
#  summary(lm_fit)

## ---- fig.height=3, fig.width=6, echo=FALSE, eval=FALSE-----------------------
#  library(reshape2)
#  library(ggplot2)
#  pdat <- crime[, c("y",
#                    "pctMaleDivorc:pctKidsBornNevrMarr",
#                    "pct65up:pctPopDenseHous")]
#  pdat <- melt(pdat, id.vars = "y")
#  p <- ggplot(pdat) + geom_point(aes(value, y), size = 0.3) +
#    geom_smooth(aes(value, y), method = "lm", se = FALSE) +
#    facet_wrap(. ~ variable, scales = "free_x") +
#    xlab("") + ylab("Per capita violent crimes")
#  p
#  ggsave(p, filename = "crime.jpg", height = 3, width = 6)

## ---- fig.height=6, fig.width=9, echo=FALSE, eval=FALSE-----------------------
#  library(reshape2)
#  library(ggplot2)
#  pdat <- melt(lm_dat, id.vars = "y")
#  p <- ggplot(pdat) + geom_point(aes(value, y), size = 0.3) +
#    geom_smooth(aes(value, y), method = "lm", se = FALSE) +
#    facet_wrap(. ~ variable, scales = "free_x") +
#    xlab("") + ylab("Per capita violent crimes")
#  p

## ---- echo=FALSE, eval=FALSE--------------------------------------------------
#  working_directory
#  file.remove("crime.rda")

