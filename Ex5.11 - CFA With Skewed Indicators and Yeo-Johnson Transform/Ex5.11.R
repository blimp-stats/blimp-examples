# load packages
library(semTools)
library(lavaan)
library(mitml)

# set working directory
fdir::set()

# read data from working directory
imps <- read.table("imps.dat")
names(imps) <- c("Imputation","x1","x2","x3","y1","y2","y3","latentx","latenty","x2norm","y1norm")

# plot original and normalized variables
hist(imps$x2)
hist(imps$x2norm)
hist(imps$y1)
hist(imps$y1norm)
  
# specify lavaan model
ylatent <- paste("ylatent =~ x1 + x2norm + x3")
xlatent <- paste("xlatent =~ y1norm + y2 + y3")
model <- c(ylatent,xlatent)

# fit model with semtools and lavaan
implist <- as.mitml.list(split(imps, imps$Imputation))
analysis <- cfa.mi(model, data = implist, estimator = "ml")
summary(analysis, standardized = T, fit = T)

# imputation-based modification indices
modindices.mi(analysis, op = c("~~","=~"), minimum.value = 3, sort. = T)

