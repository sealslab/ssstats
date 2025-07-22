## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE,
  fig.align = "center",     
  fig.width = 6,            
  fig.height = 4            
)

## -----------------------------------------------------------------------------
library(ssstats)
library(palmerpenguins)

## -----------------------------------------------------------------------------
mean_median(penguins, bill_length_mm, bill_depth_mm, flipper_length_mm)
normality_correlation(penguins, method = "spearman")

## -----------------------------------------------------------------------------
penguins_clean <- na.omit(penguins)
normality_correlation(penguins_clean, method = "spearman")

## -----------------------------------------------------------------------------
n_pct(penguins_clean, sex)

## -----------------------------------------------------------------------------
n_pct(penguins_clean, row_var = year, col_var = species)

## -----------------------------------------------------------------------------
one_mean_HT(penguins_clean, continuous = bill_length_mm, mu = 40, alternative = "greater", alpha = 0.05)

## -----------------------------------------------------------------------------
one_mean_CI(data = penguins_clean, continuous = bill_length_mm, confidence = 0.95)

## -----------------------------------------------------------------------------
one_prop_HT(data = penguins_clean, grouping = species, success = "Adelie", p = 0.5, alternative = "two.sided", alpha = 0.05)

## -----------------------------------------------------------------------------
one_prop_CI(data = penguins_clean, grouping = species, success = "Adelie", confidence = 0.95)

## -----------------------------------------------------------------------------
independent_qq(penguins_clean, continuous = bill_length_mm, grouping = sex)

## -----------------------------------------------------------------------------
independent_mean_HT(
  penguins_clean,
  continuous = bill_length_mm,
  grouping = sex,
  alternative = "two.sided",
  alpha = 0.05,
  variance = "equal"
)

## -----------------------------------------------------------------------------
independent_mean_CI(
  penguins_clean,
  grouping = "sex",
  continuous = "bill_length_mm",
  confidence = 0.95
)

## -----------------------------------------------------------------------------
model <- lm(bill_length_mm ~ sex + species, data = penguins_clean)

## -----------------------------------------------------------------------------
anova_check(model)

## -----------------------------------------------------------------------------
outlier_count(penguins_clean, model)
outlier_graph(df = penguins_clean, model = model, 
              x_var = bill_depth_mm, y_var = bill_length_mm) +
  ggplot2::aes(shape = species) +
  ggplot2::scale_shape_manual(values = c(16, 17, 15))

## -----------------------------------------------------------------------------
cooks(model, show.threshold = T, n_labels_desired = 3)

## -----------------------------------------------------------------------------
summary(model)$coef

## -----------------------------------------------------------------------------
set.seed(123)  

# Number of observations
n <- 100

# Mean and standard deviation for pre and post
mu_pre <- 0
mu_post <- 1
sd_pre <- 1
sd_post <- 1

# correlation between pre and post
rho <- 0.6 

# Define variance-covariance matrix for bivariate normal

Sigma <- matrix(c(sd_pre^2, rho * sd_pre * sd_post,
                  rho * sd_pre * sd_post, sd_post^2), 
                nrow = 2)

bivn <- MASS::mvrnorm(n = n, mu = c(mu_pre, mu_post), Sigma = Sigma)
pre <- bivn[, 1]
post <- bivn[, 2]
id <- 1:n

data <- data.frame(id = id, pre = pre, post = post)

## -----------------------------------------------------------------------------
# Compare means and medians of pre and post scores
dependent_mean_median(data, col1 = pre, col2 = post, accuracy = 3)

# Examine QQ plot of the difference (post - pre)
dependent_qq(data, col1 = pre, col2 = post)

## -----------------------------------------------------------------------------
dependent_mean_HT(data = data, col1 = pre, col2 = post, alternative = "two.sided", alpha = 0.05, mu = 0)

## -----------------------------------------------------------------------------
dependent_mean_CI(data = data, col1 = pre, col2 = post, confidence = 0.95)

