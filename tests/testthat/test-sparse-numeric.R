library(testthat)

test_that("check validity method exists", {
  expect_false({
    validity_method <- getValidity(getClassDef("sparse_numeric"))
    is.null(validity_method)
  })
})

test_that("check validity method", {
  expect_true({
    x <- new("sparse_numeric",
             value = c(1, 2, 3, 1),
             pos = c(1L, 2L, 3L, 5L),
             length = 5L)
    validObject(x)
  })
})

test_that("check validity method 2", {
  expect_error({
    x <- new("sparse_numeric",
             value = c(1, 2, 3, 1),
             pos = c(1L, 2L, 3L, 5L),
             length = 5L)
    x@length <- 2L
    validObject(x)
  })
})

test_that("check coercion return class", {
  expect_s4_class({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  }, "sparse_numeric")
})

test_that("check for show method", {
  expect_no_error({
    getMethod("show", "sparse_numeric")
  })
})

test_that("check for plot method", {
  expect_no_error({
    getMethod("plot", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for + method", {
  expect_no_error({
    getMethod("+", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for - method", {
  expect_no_error({
    getMethod("-", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for * method", {
  expect_no_error({
    getMethod("*", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("sparse add generic", expect_true(isGeneric("sparse_add")))
test_that("sparse mult generic", expect_true(isGeneric("sparse_mult")))
test_that("sparse sub generic", expect_true(isGeneric("sparse_sub")))
test_that("sparse crossprod generic", expect_true(isGeneric("sparse_crossprod")))

test_that("sparse add formals", {
  expect_true(length(formals(sparse_add)) >= 2L)
})

test_that("sparse mult formals", {
  expect_true(length(formals(sparse_mult)) >= 2L)
})

test_that("sparse sub formals", {
  expect_true(length(formals(sparse_sub)) >= 2L)
})

test_that("sparse crossprod formals", {
  expect_true(length(formals(sparse_crossprod)) >= 2L)
})

test_that("check returned class for add", {
  expect_s4_class({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_add(x, y)
  }, "sparse_numeric")
})

test_that("sparse_add", {
  result <- as(c(1, 1, 0, 1, 6), "sparse_numeric")
  expect_equal({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_add(x, y)
  }, result)
})

test_that("sparse add dense", {
  result <- as(c(2, 4, 6, 10, 12), "sparse_numeric")
  expect_equal({
    x <- as(c(1, 3, 4, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 2, 9, 10), "sparse_numeric")
    sparse_add(x, y)
  }, result)
})

test_that("all zero wrong length", {
  expect_error({
    x <- as(rep(0, 10), "sparse_numeric")
    y <- as(rep(0, 9), "sparse_numeric")
    sparse_add(x, y)
  })
})

#### my tests
test_that("coercion to numeric returns correct vector", {
  x <- as(c(0, 5, 0, 3), "sparse_numeric")
  expect_equal(as(x, "numeric"), c(0, 5, 0, 3))
})

test_that("coercion round trip works", {
  x0 <- c(0, 2, 0, 0, 9)
  x <- as(x0, "sparse_numeric")
  expect_equal(as(x, "numeric"), x0)
})

test_that("sparse subtraction produces correct result", {
  x <- as(c(5, 0, 2, 0, 1), "sparse_numeric")
  y <- as(c(3, 0, 1, 0, 5), "sparse_numeric")
  result <- x - y
  expect_equal(as(result, "numeric"), c(2, 0, 1, 0, -4))
})

test_that("sparse multiplication produces correct result", {
  x <- as(c(2, 0, 3, 0, 4), "sparse_numeric")
  y <- as(c(5, 0, 0, 0, 2), "sparse_numeric")
  result <- x * y
  expect_equal(as(result, "numeric"), c(10, 0, 0, 0, 8))
})

test_that("multiplying vectors with no overlapping nonzero positions returns all zeros", {
  x <- as(c(1, 0, 2, 0), "sparse_numeric")
  y <- as(c(0, 3, 0, 4), "sparse_numeric")
  result <- x * y
  expect_equal(as(result, "numeric"), c(0, 0, 0, 0))
})

test_that("sparse_crossprod returns correct value", {
  x <- as(c(1, 0, 3), "sparse_numeric")
  y <- as(c(4, 0, 2), "sparse_numeric")
  expect_equal(sparse_crossprod(x, y), 1*4 + 3*2)
})

test_that("sparse_crossprod with no overlap returns 0", {
  x <- as(c(1, 0, 3), "sparse_numeric")
  y <- as(c(0, 2, 0), "sparse_numeric")
  expect_equal(sparse_crossprod(x, y), 0)
})

test_that("sparse_crossprod mismatched length triggers error", {
  x <- as(c(1,2,3), "sparse_numeric")
  y <- as(c(1,2), "sparse_numeric")
  expect_error(sparse_crossprod(x, y))
})

test_that("sparse_norm computes Euclidean norm", {
  x <- as(c(3, 4), "sparse_numeric")
  expect_equal(sparse_norm(x), 5)  # sqrt(3^2 + 4^2)
})

test_that("sparse_norm with all zeros returns zero", {
  x <- as(c(0, 0, 0, 0), "sparse_numeric")
  expect_equal(sparse_norm(x), 0)
})

test_that("plot method works (no error)", {
  expect_no_error({
    x <- as(c(1, 0, 2, 0), "sparse_numeric")
    y <- as(c(1, 0, 3, 0), "sparse_numeric")
    plot(x, y)
  })
})

test_that("plot throws error when no overlapping non-zero positions", {
  x <- as(c(1, 0, 2), "sparse_numeric")
  y <- as(c(0, 3, 0), "sparse_numeric")
  expect_error(plot(x, y))
})

test_that("validity fails when pos contains duplicates", {
  expect_error(
    new("sparse_numeric",
        value = c(1, 2),
        pos = c(2L, 2L),
        length = 5L
    )
  )
})

test_that("validity fails when pos is out of range", {
  expect_error(
    new("sparse_numeric",
        value = 1,
        pos = 10L,
        length = 5L
    )
  )
})

test_that("validity fails when value and pos lengths mismatch", {
  expect_error(
    new("sparse_numeric",
        value = c(1, 2),
        pos = 1L,
        length = 5L
    )
  )
})

test_that("all-zero sparse vector is constructed correctly", {
  x <- as(c(0,0,0,0,0), "sparse_numeric")
  expect_equal(x@value, numeric(0))
  expect_equal(x@pos, integer(0))
  expect_equal(x@length, 5L)
})

test_that("mean counts zeros", {
  x <- as(c(0, 2, 0, 3), "sparse_numeric")
  expect_equal(mean(x), (2 + 3) / 4)
})

test_that("norm equals sqrt of sum squares", {
  x <- as(c(3, 0, 4, 0), "sparse_numeric")
  expect_equal(norm(x), sqrt(3^2 + 4^2))
})

test_that("standardize raises on constant vector", {
  x <- as(rep(5, 5), "sparse_numeric")
  expect_error(standardize(x))
})

test_that("standardize returns correct mean and sd for small vector", {
  v <- c(0, 2, 0, 4)
  x <- as(v, "sparse_numeric")
  z <- standardize(x)

  dense_out <- as(z, "numeric")

  # mean should be 0 (within numeric tolerance)
  expect_equal(round(mean(dense_out), 8), 0)

  # sd should match sample sd
  expect_equal(round(sd(dense_out), 8), 1)
})

test_that("standardize only drops exact zeros after standardization", {
  v <- c(1, 0, -1, 0)
  x <- as(v, "sparse_numeric")
  z <- standardize(x)
  dense <- as(z, "numeric")

  # True standardized values
  true <- (v - mean(v)) / sd(v)

  # Positions that should be kept (exact nonzero)
  expect_equal(which(true != 0), z@pos)

  # Dense reconstruction must match
  expect_equal(dense, true)
})

test_that("mean on all zeros returns 0", {
  x <- as(rep(0,10), "sparse_numeric")
  expect_equal(mean(x), 0)
})

test_that("norm errors for unsupported type", {
  x <- as(c(1,2,3), "sparse_numeric")
  expect_error(norm(x, type="1"))
})

test_that("plot errors when lengths differ", {
  x <- as(c(1,0,2), "sparse_numeric")
  y <- as(c(1,0), "sparse_numeric")
  expect_error(plot(x, y))
})

test_that(".merge_sparse errors on length mismatch", {
  x <- as(c(1,0,1), "sparse_numeric")
  y <- as(c(1,0), "sparse_numeric")
  expect_error(sparse_add(x, y))
})

test_that("standardize errors when length < 2", {
  x <- new("sparse_numeric", value=1, pos=1L, length=1L)
  expect_error(standardize(x))
})

