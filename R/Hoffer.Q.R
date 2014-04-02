#' Hoffer-Q Formula for Effective Lens Position
#' 
#' Calculate IOL effective lens position for emmetropia given axial length, 
#' corenal curvature, and lens A-constant or pACD constant.
#' 
#' Note: If the pACD constant, \code{pACD} for the lens is not provided, it is calculated from
#' the lens A-constant, \code{A}, using the Holladay approximation.
#' 
#' @param L length of the eye in millimeters (mm)
#' @param K average corneal curvature of the eye in diopters (D)
#' @param pACD IOL personalized ACD constant (mm)
#' @param A IOL A constant (D) used to determine pACD
#' @return ELP in mm
#' @seealso \code{\link{ELP}}
#' @family ELP
Hoffer.Q.ELP <- function(L, K=337.5/R, R, pACD, A) {
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
ELP.functions$Hoffer.Q <- Hoffer.Q.ELP

#' Hoffer-Q Formula for Emmetropic IOL Power
#' 
#' Calculate IOL power for emmetropia given axial length, 
#' corenal curvature, and effective lens position.
#' 
#' @param L length of the eye in millimeters (mm)
#' @param K average corneal curvature of the eye in diopters (D)
#' @param ELP IOL effective lens position (mm)
#' @param Rx resulting refractive error (D), defaults to 0 D
#' @param V resulting refractive error vertex distance (mm), defaults to 13 mm
#' @return Power of emmetropic IOL (D)
#' @seealso \code{\link{Power}}
#' @family Power
Hoffer.Q.Power <- function(L, K=337.5/R, R, ELP, Rx = 0, V = 13) {
  args <- list(L = L, K = K, ELP = ELP, Rx = Rx, V = V)
  R <- Rx / (1 - 0.012 * Rx)
  P <- 1336 / (L - ELP - 0.05) -
    (1.336 / (
      (1.336 / (K + R)) - ((ELP + 0.05) / 1000)))
  attr(P, 'parameters') <- args
  return(P)
}
Power.functions$Hoffer.Q <- Hoffer.Q.Power