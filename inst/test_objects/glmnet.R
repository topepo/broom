library(glmnet)
library(glmnetUtils)

si <- sessionInfo()

glmnet_si <- si$otherPkgs$glmnet$Version
glmnetUtils_si <- si$otherPkgs$glmnetUtils$Version
glmnet_date <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")


# ------------------------------------------------------------------------------
# For test-glmnet.R:

set.seed(27)

x <- matrix(rnorm(100 * 20), 100, 20)
y <- rnorm(100)
g <- sample(1:4, 100, replace = TRUE)

xy_fit_gaussian    <- glmnet(x, y)
xy_fit_multinomial <- glmnet(x, g, family = "multinomial")

xy_cv_fit_gaussian    <- cv.glmnet(x, y)
xy_cv_fit_multinomial <- cv.glmnet(x, g, family = "multinomial")

# ------------------------------------------------------------------------------
# For test-glmnetUitls.R

set.seed(27)

form_fit_gaussian    <- glmnet(formula = mpg ~ ., data = mtcars)
form_fit_multinomial <- glmnet(formula = Species ~ ., data = iris, family = "multinomial")

# ------------------------------------------------------------------------------

save(
  xy_fit_gaussian,
  xy_fit_multinomial,
  xy_cv_fit_gaussian,
  xy_cv_fit_multinomial,
  form_fit_gaussian,
  form_fit_multinomial,
  glmnet_si,
  glmnetUtils_si,
  glmnet_date,
  file = "../../tests/testthat/cache/glmnet.RData",
  version = 2,
  compress = "xz"
)
