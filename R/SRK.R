#' SRK Formula for IOL Power
#' 
#' Calculate IOL power for emmetropia given axial length, 
#' corenal curvature, and IOL A-constant.
#' 
#' @param L axial length of the eye in millimeters (mm)
#' @param K corneal power (D)
#' @param A IOL A-constant (D)
#' @param ELP IOL effective lens position (mm) used to calculate equivalent A-constant
#' @return Power of IOL (D)
#' @seealso \code{\link{Power}}
#' @family Power
SRK.Power <- function(L, K, A, ELP) {
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
Power.functions$SRK <- SRK.Power