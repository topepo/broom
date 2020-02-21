#' @templateVar class cv.glmnet
#' @template title_desc_tidy
#' 
#' @param x A `cv.glmnet` object returned from [glmnet::cv.glmnet()].
#' @template param_unused_dots
#' 
#' @evalRd return_tidy(
#'   "lambda",
#'   "std.error",
#'   "nzero",
#'   conf.low = "lower bound on confidence interval for cross-validation
#'     estimated loss.",
#'   conf.high = "upper bound on confidence interval for cross-validation
#'     estimated loss.",
#'   estimate = "Median loss across all cross-validation folds for a given
#'     lamdba"
#' )
#' 
#' @includeRmd man/pre-rendered/glmnet-cv-glmnet-tidiers.Rmd
#' @export
#' @family glmnet tidiers
#' @seealso [tidy()], [glmnet::cv.glmnet()]
tidy.cv.glmnet <- function(x, ...) {
  with(
    x,
    tibble(
      lambda = lambda,
      estimate = cvm,
      std.error = cvsd,
      conf.low = cvlo,
      conf.high = cvup,
      nzero = nzero
    )
  )
}

#' @templateVar class cv.glmnet
#' @template title_desc_glance
#' 
#' @inherit tidy.cv.glmnet params examples
#' 
#' @evalRd return_glance("lambda.min", "lambda.1se", "nobs")
#'
#' @export
#' @seealso [glance()], [glmnet::cv.glmnet()]
#' @family glmnet tidiers
glance.cv.glmnet <- function(x, ...) {
  tibble(lambda.min = x$lambda.min, 
         lambda.1se = x$lambda.1se,
         nobs = stats::nobs(x))
}
