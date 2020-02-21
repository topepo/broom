context("glmnet")

skip_if_not_installed("modeltests")
library(modeltests)

skip_if_not_installed("glmnet")

# ------------------------------------------------------------------------------

load(test_path("cache/glmnet.RData"))

# ------------------------------------------------------------------------------

test_that("glmnet tidier arguments", {
  check_arguments(tidy.glmnet)
  check_arguments(glance.glmnet)
  
  check_arguments(tidy.cv.glmnet)
  check_arguments(glance.cv.glmnet)
})

test_that("tidy.glmnet", {
  
  td <- tidy(xy_fit_gaussian)
  tdz <- tidy(xy_fit_gaussian, return_zeros = TRUE)
  
  check_tidy_output(td)
  check_tidy_output(tdz)
  
  check_dims(td, expected_cols = 5)
  check_dims(tdz, expected_cols = 5)
  
  expect_true(all(td$estimate != 0))
  expect_true(any(tdz$estimate == 0))
  
  # multinomial
  
  td2 <- tidy(xy_fit_multinomial)
  td2z <- tidy(xy_fit_multinomial, return_zeros = TRUE)
  
  check_tidy_output(td2)
  check_tidy_output(td2z)
  
  expect_true(all(td2$estimate != 0))
  expect_true(any(td2z$estimate == 0))
  
  # regression tests
  expect_true(is.numeric(td$step) && !any(is.na(td$step)))
  expect_true(is.numeric(td2$step) && !any(is.na(td2$step)))
})

test_that("glance.glmnet", {
  gl <- glance(xy_fit_gaussian)
  gl2 <- glance(xy_fit_multinomial)
  
  check_glance_outputs(gl, gl2)
})

test_that("tidy.cv.glmnet", {
  
  td <- tidy(xy_cv_fit_gaussian)
  
  check_tidy_output(td)
  check_dims(td, expected_cols = 6)
  
  # multinomial
  
  td2 <- tidy(xy_cv_fit_multinomial)
  
  check_tidy_output(td2)
  check_dims(td2, expected_cols = 6)
})

test_that("glance.cv.glmnet", {
  gl <- glance(xy_cv_fit_gaussian)
  gl2 <- glance(xy_cv_fit_multinomial)
  
  check_glance_outputs(gl, gl2)
})
