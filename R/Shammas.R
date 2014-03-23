#' Shammas Formula for IOL Power
#' 
#' Calculate IOL power for emmetropia given axial length, 
#' corenal curvature, and effective lens position.
#' 
#' Note: The axial length is assumed to be from immersion ultrasound. For 
#' contact ultrasound, add 0.24 mm.
#' 
#' @param L axial length of the eye in millimeters (mm) from immersion ultrasound
#' @param K corneal power (D)
#' @param ELP IOL effective lens position (mm)
#' @return Power of IOL (D)
#' @seealso \code{\link{Power}}
#' @family Power
#' @references \url{https://encrypted.google.com/books?id=NhWJsGFK6qgC&pg=PA8}
Shammas.Power <- function(L, K, ELP) {
  args <- list(L = L, K = K, ELP = ELP)
  #https://encrypted.google.com/books?id=NhWJsGFK6qgC&pg=PA8&lpg=PA8&dq=colenbrander+formula&source=bl&ots=j__sC0XHHg&sig=DEaK9Qg0LOvedzq4itcfe7pBZ4Q&hl=en&sa=X&ei=scdlUuC1JcHP2wW90IDAAg&ved=0CD4Q6AEwAg#v=onepage&q=colenbrander%20formula&f=false
  # Assumes axial length is from immersion ultrasound (add 0.24mm for contact)
  P <- 1336 / (L - 0.1 * (L - 23) - ELP - 0.05) -
    1 / (1.0125 / K - (ELP + 0.05) / 1336)
  attr(P, 'parameters') <- args
  return(P)
}
Power.functions$Shammas <- Shammas.Power