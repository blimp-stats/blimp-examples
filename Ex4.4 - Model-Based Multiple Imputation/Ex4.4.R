# set working directory 
fdir::set()

# read data from working directory
imps <- read.table("imps.dat")
names(imps) <- c("imputation","id","n1","d1","o1","y","x1",
                 "d","x2","x3")

# center predictors
imps$x1_cgm <- imps$x1 - mean(imps$x1)
imps$x2_cgm <- imps$x2 - mean(imps$x2)

# analysis and pooling with mitml
implist <- mitml::as.mitml.list(split(imps, imps$imputation))
results <- with(implist, lm(y ~ x1_cgm  + x2_cgm + d))
mitml::testEstimates(results, extra.pars = T, df.com = 626)
