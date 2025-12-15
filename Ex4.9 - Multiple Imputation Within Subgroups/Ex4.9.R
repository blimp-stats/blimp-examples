# set working directory
fdir::set()

# read data from working directory
imps <- read.table("imps.dat")
names(imps) <- c("imputation","id","v1","v2","v3","y","x","v4","v5","d","m",paste0("v", 6:24))

# center predictors
imps$x_cgm <- imps$x - mean(imps$x)
imps$d_cgm <- imps$d - mean(imps$d)

# analysis and pooling with mitml
implist <- mitml::as.mitml.list(split(imps, imps$imputation))
results <- with(implist, lm(y ~ x_cgm  + m + x_cgm*m + d_cgm))
mitml::testEstimates(results, extra.pars = T, df.com = 295)
