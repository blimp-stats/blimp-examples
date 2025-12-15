# set working directory
fdir::set()

# read data from working directory
imps <- read.table("imps.dat")
names(imps) <- c("imputation","id","y","v1","x1","d","v2","v3","x2","v4","ytransform","d1.latent")

# plot raw and transformed scores
hist(imps$y)
hist(imps$ytransform)

# center predictors
imps$x1_cgm <- imps$x1 - mean(imps$x1)
imps$x2_cgm <- imps$x2 - mean(imps$x2)

# analysis and pooling with mitml
implist <- mitml::as.mitml.list(split(imps, imps$imputation))

# analyze skewed outcome
results <- with(implist, lm(y ~ x1_cgm  + x2_cgm + d))
mitml::testEstimates(results, extra.pars = T, df.com = 1996)

# analyze transformed outcome
results <- with(implist, lm(ytransform ~ x1_cgm  + x2_cgm + d))
mitml::testEstimates(results, extra.pars = T, df.com = 1996)
