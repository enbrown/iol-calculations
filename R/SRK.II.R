#' SRK-II Formula for IOL Power
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
SRK.II.Power <- function(L, K, A, ELP) {
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
  if (L < 20) A2 <- A + 3
  if (L >= 20 & L < 21) A2 <- A + 2
  if (L >= 21 & L < 22) A2 <- A + 1
  if (L >= 24.5) A2 <- A - 0.5
  P <- A2 - 2.5 * L - 0.9 * K
  attr(P, 'parameters') <- args
  return(P)
}
Power.functions$SRK.II <- SRK.II.Power