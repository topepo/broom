#' @templateVar class glmnet
#' @template title_desc_tidy
#'
#' @param x A `glmnet` object returned from [glmnet::glmnet()].
#' @param return_zeros Logical indicating whether coefficients with value zero
#'   zero should be included in the results. Defaults to `FALSE`.
#' @template param_unused_dots
#' 
#' @evalRd return_tidy(
#'   "term",
#'   "step",
#'   "estimate",
#'   "lambda",
#'   "dev.ratio"
#' )
#'   
#' @details Note that while this representation of GLMs is much easier
#'   to plot and combine than the default structure, it is also much
#'   more memory-intensive. Do not use for large, sparse matrices.
#'
#'   No `augment` method is yet provided even though the model produces
#'   predictions, because the input data is not tidy (it is a matrix that
#'   may be very wide) and therefore combining predictions with it is not
#'   logical. Furthermore, predictions make sense only with a specific
#'   choice of lambda.
#' @includeRmd man/pre-rendered/glmnet-glmnet-tidiers.Rmd
#' @export
#' @aliases glmnet_tidiers
#' @family glmnet tidiers
#' @seealso [tidy()], [glmnet::glmnet()]
tidy.glmnet <- function(x, return_zeros = FALSE, ...) {
  beta <- coef(x)
  
  if (inherits(x, "multnet")) {
    beta_d <- purrr::map_df(beta, function(b) {
      fix_data_frame(as.matrix(b), newnames = 1:ncol(b), newcol = "term")
    }, .id = "class")
    ret <- beta_d %>% 
      tidyr::gather(step, estimate, -term, -class)
  } else {
    beta_d <- fix_data_frame(
      as.matrix(beta),
      newnames = 1:ncol(beta),
      newcol = "term"
    )
    ret <- tidyr::gather(beta_d, step, estimate, -term)
  }
  # add values specific to each step
  ret <- ret %>%
    mutate(
      step = as.numeric(step),
      lambda = x$lambda[step],
      dev.ratio = x$dev.ratio[step]
    )
  
  if (!return_zeros) {
    ret <- filter(ret, estimate != 0)
  }
  
  as_tibble(ret)
}


#' @templateVar class glmnet
#' @template title_desc_glance
#' 
#' @inherit tidy.glmnet params examples
#' 
#' @evalRd return_glance("nulldev", "npasses", "nobs")
#'
#' @export
#' @family glmnet tidiers
#' @seealso [glance()], [glmnet::glmnet()]
glance.glmnet <- function(x, ...) {
  tibble(nulldev = x$nulldev, 
         npasses = x$npasses,
         nobs = stats::nobs(x))
}
