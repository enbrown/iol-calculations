Power.functions$SRK <- function(L, K, A, ELP) {
  args <- list(L = L, K = K)
  if (missing(A) || ! is.finite(A)) {
    A <- (ELP + 63.896) / 0.58357
    args$ELP <- ELP
    warning("Using Holladay approximation for SRK's A from ELP constant: ", A)
  } else {
    args$A <- A
  }
  P <- A - 2.5 * L - 0.9 * K
  attr(P, 'parameters') <- args
  return(P)
}