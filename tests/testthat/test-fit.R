test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

#using blblm
test_that("blblm coef works", {
  fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, parallel = FALSE)
  fit1 <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, parallel = TRUE)
  z = length(coef(fit))
  z1 = length(coef(fit1))
  expect_equal(z, z1)
})

test_that("blblm coef works", {
  fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, parallel = FALSE)
  fit1 <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, parallel = TRUE)
  z = class(coef(fit))
  z1 = class(coef(fit1))
  expect_equal(z, z1)
})


test_that("blblm confint works", {
  fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, parallel = FALSE)
  fit1 <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, parallel = TRUE)
  z = length(confint(fit, c("hp")))
  z1 = length(confint(fit, c("hp")))
  expect_equal(z, z1)
})

test_that("blblm confint works", {
  fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, parallel = FALSE)
  fit1 <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, parallel = TRUE)
  z = class(confint(fit, c("hp")))
  z1 = class(confint(fit, c("hp")))
  expect_equal(z, z1)
})


test_that("blblm sigma works", {
  fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, parallel = FALSE)
  fit1 <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, parallel = TRUE)
  z = length(sigma(fit, confidence = TRUE))
  z1 = length(sigma(fit, confidence = TRUE))
  expect_equal(z, z1)
})

test_that("blblm sigma works", {
  fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, parallel = FALSE)
  fit1 <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, parallel = TRUE)
  z = class(sigma(fit, confidence = TRUE))
  z1 = class(sigma(fit, confidence = TRUE))
  expect_equal(z, z1)
})


test_that("blblm predict works", {
  fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, parallel = FALSE)
  fit1 <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, parallel = TRUE)
  z = length(predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = TRUE))
  z1 = length(predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = TRUE))
  expect_equal(z, z1)
})

test_that("blblm predict works", {
  fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, parallel = FALSE)
  fit1 <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, parallel = TRUE)
  z = class(predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = TRUE))
  z1 = class(predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = TRUE))
  expect_equal(z, z1)
})

test_that("blblm works", {
  fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, parallel = FALSE)
  fit1 <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, parallel = TRUE)
  expect_equal(length(coef(fit)), length(coef(fit1)))
  expect_equal(class(coef(fit)), class(coef(fit1)))
  expect_equal(length(confint(fit, c("hp"))), length(confint(fit, c("hp"))))
  expect_equal(class(confint(fit, c("hp"))), class(confint(fit, c("hp"))))
  expect_equal(length(sigma(fit, confidence = TRUE)), length(sigma(fit, confidence = TRUE)))
  expect_equal(class(sigma(fit, confidence = TRUE)), class(sigma(fit, confidence = TRUE)))
  expect_equal(length(predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = TRUE)), length(predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = TRUE)))
  expect_equal(class(predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = TRUE)), class(predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = TRUE)))
})



#using blblogreg
test_that("blblm works", {
  fit <- blblogreg(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, family = binomial, parallel = FALSE)
  fit1 <- blblogreg(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, family = binomial, parallel = TRUE)
  expect_equal(length(coef(fit)), length(coef(fit1)))
  expect_equal(class(coef(fit)), class(coef(fit1)))
  expect_equal(length(confint(fit, c("hp"))), length(confint(fit, c("hp"))))
  expect_equal(class(confint(fit, c("hp"))), class(confint(fit, c("hp"))))
  expect_equal(length(sigma(fit, confidence = TRUE)), length(sigma(fit, confidence = TRUE)))
  expect_equal(class(sigma(fit, confidence = TRUE)), class(sigma(fit, confidence = TRUE)))
  expect_equal(length(predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = TRUE)), length(predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = TRUE)))
  expect_equal(class(predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = TRUE)), class(predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = TRUE)))
})