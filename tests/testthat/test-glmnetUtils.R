context("glmnetUtils")

skip_if_not_installed("modeltests")
library(modeltests)

skip_if_not_installed("glmnet")
skip_if_not_installed("glmnetUtils")

# ------------------------------------------------------------------------------

load(test_path("cache/glmnet.RData"))

# ------------------------------------------------------------------------------


test_that("glmnet.formula tidier arguments", {
  check_arguments(tidy.glmnet)
  check_arguments(glance.glmnet)

  check_arguments(tidy.cv.glmnet)
  check_arguments(glance.cv.glmnet)
})

test_that("tidy.glmnet.formula", {
  td <- tidy(form_fit_gaussian)
  tdz <- tidy(form_fit_gaussian, return_zeros = TRUE)

  check_tidy_output(td)
  check_tidy_output(tdz)

  check_dims(td, expected_cols = 5)
  check_dims(tdz, expected_cols = 5)

  expect_true(all(td$estimate != 0))
  expect_true(any(tdz$estimate == 0))

  # multinomial

  td2 <- tidy(form_fit_multinomial)
  td2z <- tidy(form_fit_multinomial, return_zeros = TRUE)

  check_tidy_output(td2)
  check_tidy_output(td2z)

  expect_is(td2, "tbl_df")

  expect_equal(dim(td2), c(839L, 6L))
  expect_equal(dim(td2z), c(1500L, 6L))

  expect_true(all(td2$estimate != 0))
  expect_true(any(td2z$estimate == 0))

  # regression tests
  expect_true(is.numeric(td$step) && !any(is.na(td$step)))
  expect_true(is.numeric(td2$step) && !any(is.na(td2$step)))
})

test_that("glance.glmnet.formula", {
  gl <- glance(form_fit_gaussian)
  gl2 <- glance(form_fit_multinomial)

  check_glance_outputs(gl, gl2)
  
  expect_is(gl, "tbl_df")
  expect_equal(dim(gl), c(1L, 3L))
})
