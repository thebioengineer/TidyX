## TidyX Episode 171
## Bayes Regression in 20min

library(tidyverse)
library(bayesplot)
library(tidybayes)
library(broom.mixed)
library(rstanarm)

### Get data
d <- mtcars
d %>%
  head()

## Fit model with default priors (weakly informative)
fit_bayes <- stan_glm(mpg ~ wt + disp + drat, data = d)
print(fit_bayes, digit = 4)

## credible intervals for the model coefficients
tidy(fit_bayes,
     conf.int = TRUE,
     conf.level = 0.90)

## what were the priors
prior_summary(fit_bayes)

# trace plots
mcmc_trace(fit_bayes, size = 0.1)

# density
mcmc_dens_overlay(fit_bayes)

# or for single variables
pp_check(fit_bayes, nreps = 50) +
  xlab("wt")


# exploring the posterior distribution
post_coefs <- as.matrix(fit_bayes)
head(post_coefs)

par(mfrow = c(1,2))
hist(post_coefs[, 1], main = "Posterior Draws for Intercept")
hist(post_coefs[, 2], main = "Posterior Draws for Wt")


apply(X = post_coefs, MARGIN = 2, FUN = mean)
apply(X = post_coefs, MARGIN = 2, FUN = sd)

post_coefs %>%
  as.data.frame() %>%
  pivot_longer(cols = everything()) %>%
  group_by(name) %>%
  reframe(pct_tile = c(0.05, 0.5, 0.95),
            val = quantile(value, probs = pct_tile))


# get a few random parameters from the posterior distribution
random_draw <- post_coefs %>%
  as.data.frame() %>%
  rename(intercept = '(Intercept)') %>%
  slice_sample(n = 100)

d %>%
  ggplot(aes(x = wt, y = mpg)) +
  geom_jitter(size = 3) +
  geom_abline(aes(intercept = intercept,
                  slope = wt), 
              data = random_draw,
              color = 'light blue',
              alpha = 0.7)

dev.off()

## Making a prediction
## new point
new_dat <- data.frame(wt = 3.2)

## predict with predict()
y_point_pred <- predict(fit_bayes, newdata = new_dat)
y_point_pred

## or by hand
intercept <- coef(fit_bayes)[1]
wt_coef <- coef(fit_bayes)[2]
y_point_pred_by_hand <- intercept + wt_coef * new_dat$wt
y_point_pred_by_hand


## Prediction with uncertainty (Credible Intervals)
y_uncertain <- posterior_linpred(fit_bayes, newdata = new_dat)
head(y_uncertain)

hist(y_uncertain)
mean(y_uncertain)
sd(y_uncertain)
quantile(y_uncertain, probs = c(0.05, 0.5, 0.95))

posterior_interval(y_uncertain, prob = 0.9)

# now by hand approach
y_uncertain2 <- post_coefs[,1] + post_coefs[,2] * rep(new_dat$wt, nrow(post_coefs))

head(y_uncertain2)

hist(y_uncertain2)
mean(y_uncertain2)
sd(y_uncertain2)
quantile(y_uncertain2, probs = c(0.05, 0.5, 0.95))

## posterior predictive distribution for a new observation
# create a vector of predictive uncertainty in a single forty
y_pred <- posterior_predict(fit_bayes, newdata = new_dat)
head(y_pred)

hist(y_pred)
mean(y_pred)
sd(y_pred)
quantile(y_pred, probs = c(0.05, 0.5, 0.95))

posterior_interval(y_pred, prob = 0.9)

# obtain results by hand using the simulations and add an error term to the computation
n_sims <- nrow(post_coefs)
sigma <- post_coefs[, 3]

y_pred2 <- post_coefs[,1] + post_coefs[,2] * rep(new_dat$wt, nrow(post_coefs)) + rnorm(n_sims, mean = 0, sd = sigma)

head(y_pred2)

hist(y_pred2)
mean(y_pred2)
sd(y_pred2)
quantile(y_pred2, probs = c(0.05, 0.5, 0.95))
