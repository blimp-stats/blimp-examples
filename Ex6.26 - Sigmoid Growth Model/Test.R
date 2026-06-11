fdir::set()

library(ggplot2)
library(dplyr)
library(rblimp)
set_blimp('/applications/blimp/blimp-nightly')

dat <- read.csv('sigmoid_growth.csv')

test <- rblimp(
  data      = dat,
  clusterid = 'id',
  latent    = 'id = alpha_i lambda_i',
  center    = 'grandmean = age',
  model     = '
    # level-2 random effects: each latent has a random intercept,
    # with timing and tempo allowed to covary
    alpha_i  ~ 1@a_prior%1.0;
    lambda_i ~ 1@l_prior%0;
    # alpha_i ~~ lambda_i;

    # definition variable: the part of the conditional mean that depends
    # on the latents. (4 - 1) is the PDS range; sigm() is Blimp built-in
    # 1 / (1 + exp(-x)). So nlfunc = 3 / (1 + exp(-alpha_i*(age - lambda_i))).
    nlfunc = ( 4 - 1 ) * sigm( alpha_i * ( age - lambda_i ) );

    # level-1 model: pds = lower + 1 * nlfunc + e
    # intercept fixed at 1 (lower asymptote); slope on nlfunc fixed at 1
    pds <- 1@1 nlfunc@1;',
  parameters = '
    # alpha mean: ~1.0 per year (true value); SD 0.3 keeps it firmly positive
    a_prior ~ normal( 1.0, 0.3 )%1.0;
    # lambda mean: 0 because age is grand-mean centered.
    # true λ = 11.5 in raw years -> 0 in centered years.
    l_prior ~ normal( 0, 2 )%0;
  ',
  seed    = 90291,
  chains  = 4,
  burn    = 250000,
  iter    = 250000,
  options = 'pinfo prior2')
output(test)

# names(test@average_imp)
# test@estimates
# 
# dim(test@iterations)
# head(test@iterations)

arr <- as.array(test)
dim(arr)
dimnames(arr)$parameters

param <- "alpha_i residual variance"
mat   <- arr[, , param]
n_iter   <- nrow(mat)
n_chains <- ncol(mat)

trace <- data.frame(
  iteration = rep(seq_len(n_iter), times = n_chains),
  chain     = factor(rep(seq_len(n_chains), each = n_iter)),
  value     = as.vector(mat)
)

ggplot(trace, aes(iteration, value)) +
  geom_line(linewidth = 0.2, alpha = 0.7) +
  geom_hline(yintercept = median(trace$value), color = "firebrick",
             linetype = "dashed", linewidth = 0.4) +
  facet_wrap(~ chain, ncol = 1, labeller = label_both) +
  labs(x = "Iteration", y = "alpha_i residual variance",
       title = "Per-chain trace, posterior median in red") +
  theme_minimal()

# ACF

param <- "alpha_i residual variance"

# ---- per-chain ACF ----
acf_data <- lapply(1:4, function(k) {
  ac <- acf(arr[, k, param], lag.max = 500, plot = FALSE)
  data.frame(lag = as.vector(ac$lag),
             acf = as.vector(ac$acf),
             chain = factor(k))
}) |> bind_rows()

# 95% white-noise band
n_per_chain <- dim(arr)[1]
ci <- qnorm(0.975) / sqrt(n_per_chain)

ggplot(acf_data, aes(lag, acf)) +
  geom_hline(yintercept = c(-ci, ci),
             linetype = "dashed", color = "blue", alpha = 0.6) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  geom_segment(aes(xend = lag, yend = 0), linewidth = 0.3) +
  facet_wrap(~ chain, ncol = 2, labeller = label_both) +
  labs(x = "Lag", y = "Autocorrelation",
       title = paste0("ACF per chain: ", param),
       subtitle = "blue dashed = 95% white-noise band") +
  theme_minimal()

library(coda)

# convert the four chains to an mcmc.list
chains <- lapply(1:4, function(k) mcmc(arr[, k, param]))
chains <- mcmc.list(chains)

# coda's autocorrelation-based effective size
coda_ess <- effectiveSize(chains)
cat("coda effectiveSize:", coda_ess, "\n")

# posterior package (uses the same Stan-style rank-normalized ESS that papers cite)
# install.packages("posterior") if needed
library(posterior)
draws <- posterior::as_draws_array(arr[, , param, drop = FALSE])
cat("posterior::ess_bulk:", ess_bulk(draws), "\n")
cat("posterior::ess_tail:", ess_tail(draws), "\n")

library(ggplot2)

param <- "alpha_i residual variance"

# ---- batch means ESS ----
# stack all 4 chains end-to-end
all_draws <- as.vector(arr[, , param])
N <- length(all_draws)

# try several batch sizes; ESS should stabilize as batch size grows
batch_sizes <- c(50, 100, 250, 500, 1000, 2500, 5000, 10000, 25000)

batch_ess <- sapply(batch_sizes, function(bs) {
  n_batches <- floor(N / bs)
  trimmed   <- all_draws[1:(n_batches * bs)]
  batches   <- matrix(trimmed, nrow = bs, ncol = n_batches)
  batch_means <- colMeans(batches)
  
  overall_var <- var(all_draws)
  batch_var   <- var(batch_means) * bs   # asymptotic variance of the mean estimator
  ess         <- N * overall_var / batch_var
  ess
})

results <- data.frame(batch_size = batch_sizes, ess = batch_ess)
print(results)

ggplot(results, aes(batch_size, ess)) +
  geom_point() + geom_line() +
  scale_x_log10() + scale_y_log10() +
  geom_hline(yintercept = c(17, 24, 18484),
             linetype = "dashed",
             color = c("red", "orange", "blue")) +
  annotate("text", x = max(batch_sizes), y = 17,    label = "Blimp 17",     hjust = 1) +
  annotate("text", x = max(batch_sizes), y = 24,    label = "posterior 24", hjust = 1) +
  annotate("text", x = max(batch_sizes), y = 18484, label = "coda 18,484",  hjust = 1) +
  labs(x = "Batch size", y = "Batch-means ESS",
       title = "Effective sample size from non-overlapping batch means") +
  theme_minimal()
