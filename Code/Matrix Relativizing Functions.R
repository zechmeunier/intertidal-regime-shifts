#title: Matrix Relativizing Functions
#author: Zechariah Meunier

## Name: col.max
# About: Calculates the column maxima
# Input: mat - matrix with any dimensions
#Output: colmaxes - vector of column maxima
col.max <- function(mat) {
  colmaxes <- as.numeric(sapply(mat, max))
  colmaxes  }

## Name: rel.col.max
# About: Relativizes matrix by its column maxima
# Input: mat - matrix with any dimensions
#Output: relmat - relativized matrix
rel.col.max <- function(mat) {
  colmaxes <- as.numeric(sapply(mat, max))
  relmat <- mat/matrix(colmaxes, nrow = nrow(mat), ncol = ncol(mat), byrow = T)
  relmat[is.na(relmat)] <- 0 #replaces undefined values with 0
  relmat  }