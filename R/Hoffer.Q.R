ELP.functions$Hoffer.Q <- function(L, K, A, pACD) {
  args <- list(L = L, K = K)
  if (missing(pACD) || ! is.finite(pACD)) {
    pACD <- ELP.functions$Holladay(A)
    args$A <- A
    warning("Using Holladay approximation for Hoffer Q's pACD from A constant: ", pACD, "mm")
  } else {
    args$pACD <- pACD
  }
  if (L > 23) {
    M <- -1
    G <- 23.5
  } else {
    M <- +1
    G <- 28
  }
  if (L < 18.5) L <- 18.5
  if (L > 31) L <- 31
  ELP <- pACD +
    0.3 * (L - 23.5) +
    (tan(K * pi / 180))^2 +
    ((0.1 * M * (23.5 - L)^2) *
       tan(0.1 * (G - L)^2 * pi / 180)) -
    0.99166
  result <- ELP
  attr(result, 'parameters') <- args
  return(result)
}

Power.functions$Hoffer.Q <- function(L, K, ELP, Rx = 0, V = 13) {
  args <- list(L = L, K = K, ELP = ELP, Rx = Rx, V = V)
  R <- Rx / (1 - 0.012 * Rx)
  P <- 1336 / (L - ELP - 0.05) -
    (1.336 / (
      (1.336 / (K + R)) - ((ELP + 0.05) / 1000)))
  attr(P, 'parameters') <- args
  return(P)
}