# -------------------------------------------------------------------------

# sparse_numeric S4 class

# -------------------------------------------------------------------------

#' sparse_numeric S4 Class
#'
#' Stores a numeric vector in sparse form using nonzero values and positions.
#'
#' @slot value Numeric vector of non-zero values.
#' @slot pos Integer vector of positions (1-based indices).
#' @slot length Integer total length of the vector.
#'
#' @name sparse_numeric-class
#' @rdname sparse_numeric-class
#' @exportClass sparse_numeric
setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos = "integer",
    length = "integer"
  )
)

# -------------------------------------------------------------------------

# Validity

# -------------------------------------------------------------------------

#' Validity check for sparse_numeric objects
#'
#' Ensures positions are valid, lengths match, and values are consistent.
#'
#' @param object A sparse_numeric object
#'
#' @name sparse_numeric-validity
#' @rdname sparse_numeric-validity
setValidity("sparse_numeric", function(object) {
  if (!is.integer(object@pos)) return("pos must be integer")
  if (!is.numeric(object@value)) return("value must be numeric")
  if (!is.integer(object@length) || length(object@length) != 1)
    return("length must be an integer scalar")
  if (any(object@pos < 1 | object@pos > object@length))
    return("positions out of range")
  if (length(object@value) != length(object@pos))
    return("value and pos must have same length")
  if (any(duplicated(object@pos)))
    return("positions must be unique")
  TRUE
})

# -------------------------------------------------------------------------

# Coercion

# -------------------------------------------------------------------------

#' @importFrom methods new
setAs("numeric", "sparse_numeric", function(from) {
  pos <- which(from != 0)
  new("sparse_numeric",
      value = as.numeric(from[pos]),
      pos = as.integer(pos),
      length = as.integer(length(from)))
})

setAs("sparse_numeric", "numeric", function(from) {
  out <- numeric(from@length)
  if (length(from@pos)) out[from@pos] <- from@value
  out
})

# -------------------------------------------------------------------------

# show

# -------------------------------------------------------------------------

#' @importFrom methods show
#' @rdname sparse_numeric-class
#' @param object A sparse_numeric object
#' @exportMethod show
setMethod("show", "sparse_numeric", function(object) {
  cat("An object of class 'sparse_numeric'\n")
  cat(" Length:", object@length, "\n")
  cat(" Non-zero positions:", if (length(object@pos)) object@pos else "none", "\n")
  cat(" Values:", if (length(object@value)) object@value else "none", "\n")
})

# -------------------------------------------------------------------------

# Internal merge helper for elementwise operations

# -------------------------------------------------------------------------

.merge_sparse <- function(x, y, FUN) {
  if (x@length != y@length) stop("sparse vectors must have same length")
  all_pos <- sort(unique(c(x@pos, y@pos)))
  if (!length(all_pos))
    return(new("sparse_numeric", value = numeric(0), pos = integer(0), length = x@length))
  x_vals <- numeric(length(all_pos))
  y_vals <- numeric(length(all_pos))
  if (length(x@pos)) x_vals[match(x@pos, all_pos)] <- x@value
  if (length(y@pos)) y_vals[match(y@pos, all_pos)] <- y@value
  result_vals <- FUN(x_vals, y_vals)
  nz <- which(result_vals != 0)
  if (!length(nz))
    return(new("sparse_numeric", value = numeric(0), pos = integer(0), length = x@length))
  new("sparse_numeric",
      value = result_vals[nz],
      pos = as.integer(all_pos[nz]),
      length = x@length)
}

# -------------------------------------------------------------------------

# Arithmetic generics/methods

# -------------------------------------------------------------------------

#' Arithmetic operations for sparse_numeric
#'
#' S4 methods for `+`, `-`, and `*` applied to sparse_numeric objects.
#'
#' @param x A sparse_numeric object
#' @param y A sparse_numeric object
#' @param e1 A sparse_numeric object (used for S4 +, -, * methods)
#' @param e2 A sparse_numeric object (used for S4 +, -, * methods)
#' @param ... Additional arguments (currently unused)
#'
#' @name sparse_arithmetic
#' @rdname sparse_arithmetic
NULL

#' @rdname sparse_arithmetic
#' @export
setGeneric("sparse_add", function(x, y, ...) standardGeneric("sparse_add"))
#' @rdname sparse_arithmetic
#' @exportMethod sparse_add
setMethod("sparse_add", c("sparse_numeric", "sparse_numeric"),
          function(x, y, ...) .merge_sparse(x, y, `+`))
#' @rdname sparse_arithmetic
#' @exportMethod +
setMethod("+", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_add(e1, e2))

#' @rdname sparse_arithmetic
#' @export
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))
#' @rdname sparse_arithmetic
#' @exportMethod sparse_mult
setMethod("sparse_mult", c("sparse_numeric", "sparse_numeric"),
          function(x, y, ...) .merge_sparse(x, y, `*`))
#' @rdname sparse_arithmetic
#' @exportMethod *
setMethod("*", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_mult(e1, e2))

#' @rdname sparse_arithmetic
#' @export
setGeneric("sparse_sub", function(x, y, ...) standardGeneric("sparse_sub"))
#' @rdname sparse_arithmetic
#' @exportMethod sparse_sub
setMethod("sparse_sub", c("sparse_numeric", "sparse_numeric"),
          function(x, y, ...) .merge_sparse(x, y, `-`))
#' @rdname sparse_arithmetic
#' @exportMethod -
setMethod("-", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_sub(e1, e2))

# -------------------------------------------------------------------------

# Dot product

# -------------------------------------------------------------------------

setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))
setMethod("sparse_crossprod", c("sparse_numeric", "sparse_numeric"), function(x, y, ...) {
  if (x@length != y@length) stop("sparse vectors must have same length")
  common <- intersect(x@pos, y@pos)
  if (!length(common)) return(0)
  sum(x@value[match(common, x@pos)] * y@value[match(common, y@pos)])
})

# -------------------------------------------------------------------------

# Plot

# -------------------------------------------------------------------------

#' @importFrom graphics abline
setMethod("plot", c("sparse_numeric", "sparse_numeric"), function(x, y, ...) {
  if (x@length != y@length) stop("Sparse vectors must have same length.")
  common <- intersect(x@pos, y@pos)
  if (!length(common)) stop("No overlapping non-zero positions to plot.")
  xv <- x@value[match(common, x@pos)]
  yv <- y@value[match(common, y@pos)]
  plot(xv, yv, xlab = "x values", ylab = "y values", main = "Overlapping Non-Zero Elements", ...)
  abline(0, 1, lty = 2)
})

# -------------------------------------------------------------------------

# Mean

# -------------------------------------------------------------------------

setMethod("mean", "sparse_numeric", function(x, ...) {
  sum(x@value) / x@length
})

# -------------------------------------------------------------------------

# Norm

# -------------------------------------------------------------------------

setGeneric("norm")
setMethod("norm", "sparse_numeric", function(x, type = "2", ...) {
  if (!missing(type) && !is.null(type) && type != "2")
    stop("Only Euclidean norm supported for sparse_numeric.")
  sqrt(sum(x@value^2))
})
sparse_norm <- function(x) norm(x)

# -------------------------------------------------------------------------

# Standardize

# -------------------------------------------------------------------------

setGeneric("standardize", function(x, ...) standardGeneric("standardize"))

setMethod("standardize", "sparse_numeric", function(x, ...) {
  n <- x@length
  if (n < 2L) stop("length must be >= 2 to compute sample sd")
  mu <- sum(x@value) / n
  k <- length(x@value)
  ss_nonzero <- if (k) sum((x@value - mu)^2) else 0
  ss_zero <- (n - k) * mu^2
  sample_var <- (ss_nonzero + ss_zero) / (n - 1)
  sd_val <- sqrt(sample_var)
  if (sd_val == 0) stop("Cannot standardize: standard deviation is zero.")
  std_vals <- if (k) (x@value - mu) / sd_val else numeric(0)
  zero_std <- (-mu) / sd_val
  all_pos <- seq_len(n)
  full <- numeric(n)
  full[x@pos] <- std_vals
  full[setdiff(all_pos, x@pos)] <- zero_std
  nz <- which(full != 0)
  if (!length(nz))
    return(new("sparse_numeric", value = numeric(0), pos = integer(0), length = n))
  new("sparse_numeric", value = full[nz], pos = as.integer(nz), length = n)
})
