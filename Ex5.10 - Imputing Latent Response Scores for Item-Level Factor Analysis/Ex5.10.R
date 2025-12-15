# load packages
library(semTools)
library(lavaan)
library(mitml)

# set working directory
fdir::set()

# read data from working directory
imps <- read.table("imps.dat")
names(imps) <- c("Imputation","id",paste0("v",seq(1:8)),
                 paste0("y",seq(1:6)), paste0("v",seq(9:15)),
                 paste0("x",seq(1:6)),paste0("laty",seq(1:6)),paste0("latx",seq(1:6)))
  
# specify lavaan model
ylatent <- paste("ylatent =~", paste0("laty", 1:6, collapse = " + "))
xlatent <- paste("xlatent =~", paste0("latx", 1:6, collapse = " + "))
model <- c(ylatent,xlatent)

# fit model with semtools and lavaan
implist <- as.mitml.list(split(imps, imps$Imputation))
analysis <- cfa.mi(model, data = implist, estimator = "ml")
summary(analysis, standardized = T, fit = T)

# imputation-based modification indices
modindices.mi(analysis, op = c("~~","=~"), minimum.value = 3, sort. = T)