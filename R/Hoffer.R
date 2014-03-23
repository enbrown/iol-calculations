#' Hoffer Formula for Effective Lens Position
#' 
#' Calculate IOL effective lens position for emmetropia given axial length 
#' and lens A-constant or pACD constant.
#' 
#' Note: If the pACD constant, \code{ACD}, for the lens is not provided, it is calculated from
#' the lens A-constant, \code{A}, using the Holladay approximation.
#' 
#' @param L length of the eye in millimeters (mm)
#' @param ACD IOL personalized ACD constant (mm)
#' @param A IOL A constant (D) used to determine ACD
#' @return ELP in mm
#' @seealso \code{\link{ELP}}
#' @family ELP
#' @references \url{https://encrypted.google.com/books?id=NhWJsGFK6qgC&pg=PA8}
Hoffer.ELP <- function(L, ACD, A) {
  args <- list(L = L)
  if (missing(ACD) || ! is.finite(ACD)) {
    ACD <- ELP.functions$Holladay(A)
    args$A <- A
    warning("Using Holladay approximation for Hoffer's ACD from A constant: ", ACD, "mm")
  } else {
    args$ACD <- ACD
  }
  ELP <- 0.292 * L - 2.93 + (ACD - 3.94)
  result <- ELP
  attr(result, 'parameters') <- args
  return(result)
}
ELP.functions$Hoffer <- Hoffer.ELP

#' Hoffer Formula for Emmetropic IOL Power
#' 
#' Calculate IOL power for emmetropia given axial length, 
#' corenal curvature, and effective lens position.
#' 
#' @param L length of the eye in millimeters (mm)
#' @param K average corneal curvature of the eye in diopters (D)
#' @param ELP IOL effective lens position (mm)
#' @return Power of emmetropic IOL (D)
#' @seealso \code{\link{Power}}
#' @family Power
#' @references \url{https://encrypted.google.com/books?id=NhWJsGFK6qgC&pg=PA8}
Hoffer.Power <- function(L, K, ELP) {
  args <- list(L = L, K = K, ELP = ELP)
  P <- 1336 / (L - ELP - 0.05) - 
    1336 / (1336 / K - ELP - 0.05)
  attr(P, 'parameters') <- args
  return(P)
  #https://encrypted.google.com/books?id=NhWJsGFK6qgC&pg=PA8&lpg=PA8&dq=colenbrander+formula&source=bl&ots=j__sC0XHHg&sig=DEaK9Qg0LOvedzq4itcfe7pBZ4Q&hl=en&sa=X&ei=scdlUuC1JcHP2wW90IDAAg&ved=0CD4Q6AEwAg#v=onepage&q=colenbrander%20formula&f=false
  # equivalent formula
  return(1336 / (L - ELP - 0.05) - 
           1.336 / (1.336 / K - (ELP + 0.05) / 1000))
}
Power.functions$Hoffer <- Hoffer.Power