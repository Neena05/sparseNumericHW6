
# sparseNumericHW6

<!-- badges: start -->

<!-- badges: end -->

sparseNumericHW6 provides an S4 class sparse_numeric for representing
sparse numeric vectors in a memory-efficient way. The package supports:

- Conversion between dense R numeric vectors and sparse representation

- Arithmetic operations (+, -, \*) on sparse vectors

- Dot product (sparse_crossprod)

- Euclidean norm (norm)

- Standardization (standardize)

- Plotting overlapping non-zero elements

- A validity system enforcing data consistency

This package is useful for teaching S4 object systems and sparse
representation concepts.

## Installation

You can install the development version of sparseNumericHW6 like so:

``` r
# install directly from local folder
install.packages("path/to/sparseNumericHW6", repos = NULL, type = "source")
```

``` r
# remotes::install_github("yourusername/sparseNumericHW6")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(sparseNumericHW6)

# Create a dense numeric vector

x <- c(0, 3, 0, 5, 0)

# Convert to sparse_numeric

sx <- as(x, "sparse_numeric")
sx
```

Arithmetic operations on sparse vectors:

``` r
y <- as(c(1, 0, 2, 0, 4), "sparse_numeric")

sx + y
sx - y
sx * y
```

Dot product:

``` r
sparse_crossprod(sx, y)
```

Standardization:

``` r
standardize(sx)
```

Plotting overlapping non-zero elements:

``` r
plot(sx, y)
```

Why use README.Rmd?

- Using README.Rmd instead of plain README.md lets you:

- Include executable R code chunks

- Automatically generate output tables and plots

- Guarantee documentation always stays up-to-date

- Knit the file into a clean GitHub-ready Markdown README
