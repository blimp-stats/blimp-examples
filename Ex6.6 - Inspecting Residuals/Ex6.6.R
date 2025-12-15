# set working directory
fdir::set()

# read data from working directory
imps <- read.table('imps.dat')
names(imps) <- c('imputation','level1id','level2id','x1_i','x2_i','y_i','v1_i','v2_i',
                 'd_j','v3_j','v4_j','v5_j','x3_j','v6_j','v7_j',
                 'y_i.ranicept','x1_i.ranslope','x1_i.mean','x2_i.mean','y_i.residual')

# skewness and kurtosis of residuals
rockchalk::skewness(imps$y_i.ranicept)
rockchalk::kurtosis(imps$y_i.ranicept)
rockchalk::skewness(imps$x1_i.ranslope)
rockchalk::kurtosis(imps$x1_i.ranslope)
rockchalk::skewness(imps$y_i.residual)
rockchalk::kurtosis(imps$y_i.residual)

# plot distribution of level-1 residuals
hist(imps$y_i.residual)
plot(density(imps$y_i.residual))

# plot distribution of level-2 random effects
hist(imps$y_i.ranicept)
hist(imps$x1_i.ranslope)
plot(density(imps$y_i.ranicept))
plot(density(imps$x1_i.ranslope))

# qq plots
qqnorm(imps$y_i.ranicept); qqline(imps$y_i.ranicept)
qqnorm(imps$x1_i.ranslope); qqline(imps$x1_i.ranslope)