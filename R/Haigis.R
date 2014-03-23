#' Haigis Formula for Effective Lens Position
#' 
#' Calculate IOL effective lens position for emmetropia given axial length (L), 
#' anterior chamber depth (ACD), and a lens constant (a0, pACD, or A must be 
#' supplied.) If pACD is given, it is converted to an approximate a0 constant. 
#' If A is given, it is first convert to pACD and then to an approximate a0
#' constant.
#' 
#' Note: Currently this is just the provided pACD constant.
#' 
#' @param L ultrasound axial length (mm)
#' @param ACD ultrasound anterior chamber depth (mm)
#' @param a0 Haigis a0 lens constant (mm)
#' @param a1 Haigis a1 lens constant (unitless, defaults to 0.4)
#' @param a2 Haigis a2 lens constant (unitless, defaults to 0.1)
#' @param pACD personalized ACD (mm) lens constant
#' @param A SRK A lens constant (D)
#' @return ELP in mm
#' @seealso \code{\link{ELP}}
#' @family ELP
#' @references 
#' \url{http://www.augenklinik.uni-wuerzburg.de/uslab/ioltxt/haie.htm}
Haigis.ELP <-function(L, ACD = 3.37, a0, a1 = 0.4, a2 = 0.1, pACD, A) {
  # Check arguments
  if (missing(a0) & missing(pACD) & missing(A)) {
    stop("One of a0, pACD, or A must be provided to Haigis ELP function.")
  }
  stopifnot(is.numeric(L) & length(L) == 1 & is.finite(L))
  stopifnot(is.numeric(ACD) & length(ACD) == 1 & is.finite(ACD))
  if (missing(ACD)) {
    warning("Missing ACD for Haigis ELP function, using value of ", ACD, " mm.")
  }
  # Get arguments
  args <- list(L = L, ACD = ACD)
  if (missing(a0)) {
    if (missing(pACD) || ! is.finite(pACD)) {
      if (missing(A)) stop("Missing A constant for Haigis ELP function.")
      if (! is.numeric(A)) stop("Non-numeric argument A provided for Haigis ELP function.")
      if (length(A) != 1) stop("Only one numeric A argument is allowed for the Haigis ELP function.")
      if (A < 0) warning("Implausable negative A constant provided to Haigis ELP function.")
      if (! is.finite(A)) stop("Argument A must be finite for Haigis ELP function.")
      pACD <- Constants$IOL$A_to_ACD_a0 + A * Constants$IOL$A_to_ACD_a1
      args$A <- A
      warning("Using Haigis approximation for pACD from A constant: ", pACD, "mm")
    } else {
      # Check pACD value
      if (! is.numeric(pACD)) stop("Non-numeric argument pACD provided for Haigis ELP function.")
      if (length(pACD) > 1) stop("Only one numeric pACD argument is allowed for the Haigis ELP function.")
      if (pACD < 0) warning("Implausable negative pACD provided to Haigis ELP function.")
      if (! is.finite(pACD)) stop("Argument pACD must be finite for Haigis ELP function.")
      args$pACD <- pACD
    }
    a0 <- pACD - a1 * 3.37 - a2 * 23.39
    warning("Using Haigis approximation for A0 from pACD constant: ", a0, "mm")
  } else {
    if (! is.numeric(a0)) stop("Non-numeric argument A provided for Haigis ELP function.")
    if (length(a0) != 1) stop("Only one numeric A argument is allowed for the Haigis ELP function.")
    if (! is.finite(a0)) stop("Argument A must be finite for Haigis ELP function.")
    args$a0 <- a0
  }
  
  # Convert to optic ELP
  ELP <- a0 + a1 * ACD + a2 * L
  result <- ELP
  attr(result, 'parameters') <- args
  return(result)
}
ELP.functions$Haigis <- Haigis.ELP

#' Haigis Formula for Emmetropic IOL Power
#' 
#' Calculate IOL power for emmetropia given axial length, 
#' corneal curvature, and effective lens position.
#' 
#' @param L length of the eye in millimeters (mm)
#' @param R average corneal radius in millimeters (mm)
#' @param ELP IOL effective lens position (mm)
#' @param Rx resulting refractive error (D), defaults to 0 D
#' @param V resulting refractive error vertex distance (mm), defaults to 12 mm
#' @return Power of emmetropic IOL (D)
#' @seealso \code{\link{Power}}
#' @family Power
Haigis.Power <- function(L, R, ELP, Rx = 0, V = 12) {
  args <- list(L = L, R = R, ELP = ELP, Rx = Rx, V = V)
  nC <- 1.3315 # Corneal index of refraction
  DC <- (nC - 1.000) / (R / 1000)
  z <- DC + Rx / (1.000 - Rx * (V / 1000))
  n <- 1.336
  P <- n / (L / 1000 - ELP / 1000) - n / (n / z - ELP / 1000)
  attr(P, 'parameters') <- args
  return(P)
}
Power.functions$Haigis <- Haigis.Power