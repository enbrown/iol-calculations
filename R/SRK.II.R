Power.functions$SRK.II <- function(L, K, A, ELP) {
  args <- list(L = L, K = K)
  if (missing(A) || ! is.finite(A)) {
    print(str(match.call()))
    A <- (ELP + 63.896) / 0.58357
    args$ELP <- ELP
    warning("Using Holladay approximation for SRK II's A from ELP constant: ", A)
  } else {
    args$A <- A
  }
  A2 <- A
  if (A < 20) A2 <- A + 3
  if (A >= 20 & A < 21) A2 <- A + 2
  if (A >= 21 & A < 22) A2 <- A + 1
  if (A >= 24.5) A2 <- A - 0.5
  P <- A2 - 2.5 * L - 0.9 * K
  attr(P, 'parameters') <- args
  return(P)
}