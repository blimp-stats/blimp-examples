# set working directory
fdir::set()

# read data from working directory
imps <- read.table('imps.dat')
names(imps) <- c('imputation','level1id','level2id','x1_i','x2_i','y_i',
                 'v1_i','v2_i','d_j','v3_j','v4_j','v5_j','x3_j','v6_j','v7_j',
                 'y_i.ranicept','x1_i.ranslope','x1_i.mean','x2_i.mean')

# center predictors
imps$x1_i.cwc <- imps$x1_i - imps$x1_i.mean
imps$x2_i.cgm <- imps$x2_i - mean(imps$x2_i)
imps$x3_j.cgm <- imps$x3_j - mean(imps$x3_j)
imps$d_j.cgm <- imps$d_j - mean(imps$d_j)

# analysis and pooling
implist <- mitml::as.mitml.list(split(imps, imps$imputation))
model <- "y_i ~ x1_i.cwc + x2_i.cgm + x3_j.cgm + d_j + (1 + x1_i.cwc|level2id)"
ddf <- 127
results <- with(implist, lme4::lmer(model, REML = T))
mitml::testEstimates(results, extra.pars = T, df.com = ddf)
