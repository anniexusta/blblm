#' @import purrr
#' @import stats
#' @import furrr
#' @import future
#' @importFrom magrittr %>%
#' @details
#'
#'
#' Linear Regression with Little Bag of Bootstraps
"_PACKAGE"


## quiets concerns of R CMD check re: the .'s that appear in pipelines
# from https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
utils::globalVariables(c("."))

#' The following package is used to fit linear models
#' @param formula the formula to use for the function; an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted
#' @param data an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model
#' @param m factor. Number of splits
#' @param B factor. Number of bootstraps
#' @param parallel logical. Specifies whether to use parallelization. If TRUE, parallelization is used.
#'
#' @return object of class "blblm"
#' @export
#' @examples
#' blblm(mpg ~wt *hp, data = mtcars, m = 3, B = 100, parallel = TRUE)
blblm <- function(formula, data, m = 10, B = 5000, parallel = TRUE) {
  set.seed(10)
  if (parallel == TRUE){
    plan(multiprocess, workers = 8)
    data_list <- split_data(data, m)
    estimates <- future_map(
      data_list,
      ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B),
      .options = furrr_options(seed = TRUE))
    res <- list(estimates = estimates, formula = formula)
  }
  else{
    plan(multiprocess, workers = 1)
    data_list <- split_data(data, m)
    estimates <- map(
      data_list,
      ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
    res <- list(estimates = estimates, formula = formula)
  }
  class(res) <- "blblm"
  invisible(res)
}




#' split data into m parts of approximated equal sizes
split_data <- function(data, m) {
  idx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(idx)
}


#' compute the estimates
lm_each_subsample <- function(formula, data, n, B) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wrong variable from the global scope.
  environment(formula) <- environment()
  m <- model.frame(formula, data)
  X <- model.matrix(formula, m)
  y <- model.response(m)
  replicate(B, lm1(X, y, n), simplify = FALSE)
}


#' compute the regression estimates for a blb dataset
lm1 <- function(X, y, n) {
  freqs <- as.vector(rmultinom(1, n, rep(1, nrow(X))))
  fit <- lm.wfit(X, y, freqs)
  list(coef = blbcoef(fit), sigma = blbsigma(fit))
}


#' compute the coefficients from fit
blbcoef <- function(fit) {
  coef(fit)
}


#' compute sigma from fit
blbsigma <- function(fit) {
  p <- fit$rank
  e <- fit$residuals
  w <- fit$weights
  sqrt(sum(w * (e^2)) / (sum(w) - p))
}

#' Used to print blblm
#' @param x blblm function
#' @param ...
#'
#' @return Outputs the concatenated object of class "blblm"
#' @export
#' @method print blblm
print.blblm <- function(x, ...) {
  cat("blblm model:", capture.output(x$formula))
  cat("\n")
}

#' Used to find the sigma of blblm
#' @param object blblm function
#' @param confidence logical. If confidence = TRUE, we will use alpha in the calculations.
#' @param level confidence level. Set at 0.95.
#' @param ...
#'
#' @return object of class "blblm"

#' @export
#' @method sigma blblm
sigma.blblm <- function(object, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  sigma <- mean(map_dbl(est, ~ mean(map_dbl(., "sigma"))))
  if (confidence) {
    alpha <- 1 - 0.95
    limits <- est %>%
      map_mean(~ quantile(map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2))) %>%
      set_names(NULL)
    return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
  } else {
    return(sigma)
  }
}

#' Used to find the coef of blblm
#' @param object blblm function
#' @param ...
#'
#' @return object of class "blblm" (intercepts)

#' @export
#' @method coef blblm
coef.blblm <- function(object, ...) {
  est <- object$estimates
  map_mean(est, ~ map_cbind(., "coef") %>% rowMeans())
}


#' Used to find the confint of blblm
#' @param object blblm function
#' @param parm logical. Specify estimated model parameters if parm is not NULL.
#' @param level confidence level. Set at 0.95.
#' @param ...
#'
#' @return object of class "blblm"

#' @export
#' @method confint blblm
confint.blblm <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(parm)) {
    parm <- attr(terms(object$formula), "term.labels")
  }
  alpha <- 1 - level
  est <- object$estimates
  out <- map_rbind(parm, function(p) {
    map_mean(est, ~ map_dbl(., list("coef", p)) %>% quantile(c(alpha / 2, 1 - alpha / 2)))
  })
  if (is.vector(out)) {
    out <- as.matrix(t(out))
  }
  dimnames(out)[[1]] <- parm
  out
}

#' Predict blblm
#' @param object blblm object.
#' @param new_data data frame.
#' @param confidence logical. Specifies whether to use confidence.
#' @param level factor. level of the confidence. Set at 0.95.
#' @param ... R objects.
#'
#' @return object of class "blblogreg"
#' @export
#' @method predict blblm
predict.blblm <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
  if (confidence) {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>%
      apply(1, mean_lwr_upr, level = level) %>%
      t())
  } else {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>% rowMeans())
  }
}


mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(fit = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>% set_names(c("lwr", "upr")))
}

map_mean <- function(.x, .f, ...) {
  (map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}

map_cbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(cbind)
}

map_rbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(rbind)
}



#' The following package is used to fit logistics models
#' @param formula the formula to use for the function; an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted
#' @param data an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model
#' @param m factor. Number of splits
#' @param B factor. Number of bootstraps
#' @param family specify which glm family is used.Use binomial for logistic regression.
#' @param parallel logical. Specifies whether to use parallelization. If TRUE, parallelization is used.
#'
#' @return object of class "blblogreg"
#' @export
#' @examples
#' blblogreg(mpg ~wt *hp, data = mtcars, m = 3, B = 100, family = binomial(), parallel = TRUE)
blblogreg = function(formula, data, m = 10, B = 5000, family, parallel = TRUE) {

  if (parallel == TRUE){
    plan(multiprocess, workers = 8)
    data_list <- split_data(data, m)
    estimates <- future_map(
      data_list,
      ~ glm_each_subsample(formula = formula, data = ., n = nrow(data), B = B, family),
      .options = furrr_options(seed = TRUE))
    res <- list(estimates = estimates, formula = formula)
  }
  else{
    plan(multiprocess, workers = 1)
    data_list <- split_data(data, m)
    estimates <- map(
      data_list,
      ~ glm_each_subsample(formula = formula, data = ., n = nrow(data), B = B, family))
    res <- list(estimates = estimates, formula = formula)
  }
  class(res) <- "blblogreg"
  invisible(res)
}



#' split data into m parts of approximated equal sizes
split_data <- function(data, m) {
  idx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(idx)
}


#' compute the estimates
glm_each_subsample <- function(formula, data, n, B, family) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wrong variable from the global scope.
  environment(formula) <- environment()
  m <- model.frame(formula, data)
  X <- model.matrix(formula, m)
  y <- model.response(m)
  replicate(B, glm1(X, y, n, family), simplify = FALSE)
}


#' compute the regression estimates for a blb dataset
glm1 <- function(X, y, n, family) {
  freqs <- rmultinom(1, n, rep(1, nrow(X)))
  fit <- glm(X, y, family = family, weights = freqs)
  list(coef = blbcoef(fit), sigma = blbsigma(fit))
}


#' compute the coefficients from fit
blbcoef <- function(fit) {
  coef(fit)
}


#' compute sigma from fit
blbsigma <- function(fit) {
  p <- fit$rank
  e <- fit$residuals
  w <- fit$weights
  sqrt(sum(w * (e^2)) / (sum(w) - p))
}

#' Used to print blblogreg
#' @param x blblogreg function
#' @param ...
#'
#' @return Outputs the concatenated object of class "blblogreg"
#' @export
#' @method print blblogreg
print.blblogreg <- function(x, ...) {
  cat("blblogreg model:", capture.output(x$formula))
  cat("\n")
}

#' Used to find the sigma of blblogreg
#' @param object blblogreg function
#' @param confidence logical. If confidence = TRUE, we will use alpha in the calculations.
#' @param level confidence level. Set at 0.95.
#' @param ...
#'
#' @return object of class "blblogreg"

#' @export
#' @method sigma blblogreg
sigma.blblogreg <- function(object, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  sigma <- mean(map_dbl(est, ~ mean(map_dbl(., "sigma"))))
  if (confidence) {
    alpha <- 1 - 0.95
    limits <- est %>%
      map_mean(~ quantile(map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2))) %>%
      set_names(NULL)
    return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
  } else {
    return(sigma)
  }
}

#' Used to find the coef of blblogreg
#' @param object blblogreg function
#' @param ...
#'
#' @return object of class "blblogreg" (intercepts)

#' @export
#' @method coef blblogreg
coef.blblogreg <- function(object, ...) {
  est <- object$estimates
  map_mean(est, ~ map_cbind(., "coef") %>% rowMeans())
}


#' Used to find the confint of blblogreg
#' @param object blblogreg function
#' @param parm logical. Specify estimated model parameters if parm is not NULL.
#' @param level confidence level. Set at 0.95.
#' @param ...
#'
#' @return object of class "blblogreg"

#' @export
#' @method confint blblogreg
confint.blblogreg <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(parm)) {
    parm <- attr(terms(object$formula), "term.labels")
  }
  alpha <- 1 - level
  est <- object$estimates
  out <- map_rbind(parm, function(p) {
    map_mean(est, ~ map_dbl(., list("coef", p)) %>% quantile(c(alpha / 2, 1 - alpha / 2)))
  })
  if (is.vector(out)) {
    out <- as.matrix(t(out))
  }
  dimnames(out)[[1]] <- parm
  out
}

#' Predict blblogreg
#' @param object blblogreg object.
#' @param new_data data frame.
#' @param confidence logical. Specifies whether to use confidence.
#' @param level factor. level of the confidence. Set at 0.95.
#' @param ... R objects.
#'
#' @return object of class "blblogreg"
#' @export
#' @method predict blblogreg
predict.blblogreg <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
  if (confidence) {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>%
               apply(1, mean_lwr_upr, level = level) %>%
               t())
  } else {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>% rowMeans())
  }
}


mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(fit = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>% set_names(c("lwr", "upr")))
}

map_mean <- function(.x, .f, ...) {
  (map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}

map_cbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(cbind)
}

map_rbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(rbind)
}