# set working directory
fdir::set()

# read data from working directory
imps <- read.table("imps.dat")
names(imps) <- c("imputation",'level1id','level2id','v1_i','v2_i','d1_i','v3_i','x1_i',
                 'v4_i','v5_i','x2_i','y_i','d2_j','x3_j','v6_j') 

# center predictors
imps$x1_i.cgm <- imps$x1_i - mean(imps$x1_i)
imps$x2_i.cgm <- imps$x2_i - mean(imps$x2_i)
imps$d1_i.cgm <- imps$d1_i - mean(imps$d1_i)
imps$x3_j.cgm <- imps$x3_j - mean(imps$x3_j)

# analysis and pooling
implist <- mitml::as.mitml.list(split(imps, imps$imputation))
model <- "y_i ~ x1_i.cgm + x2_i.cgm + d1_i.cgm + x3_j.cgm + d2_j + (1|level2id)"
ddf <- 23
results <- with(implist, lme4::lmer(model, REML = T))
mitml::testEstimates(results, extra.pars = T, df.com = ddf)