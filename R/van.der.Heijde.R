#' van der Heijde Formula for IOL Power
#' 
#' Calculate IOL power for emmetropia given axial length, 
#' corenal curvature, and effective lens position
#' 
#' @param L axial length of the eye in millimeters (mm)
#' @param K corneal power (D)
#' @param ELP IOL effective lens position (mm)
#' @return Power of IOL (D)
#' @seealso \code{\link{Power}}
#' @family Power
van.der.Heijde.Power <- function(L, K, ELP) {
  #https://encrypted.google.com/books?id=NhWJsGFK6qgC&pg=PA8&lpg=PA8&dq=colenbrander+formula&source=bl&ots=j__sC0XHHg&sig=DEaK9Qg0LOvedzq4itcfe7pBZ4Q&hl=en&sa=X&ei=scdlUuC1JcHP2wW90IDAAg&ved=0CD4Q6AEwAg#v=onepage&q=colenbrander%20formula&f=false
  P <- 1336 / (L - ELP) -
    1 / (1 / K - ELP / 1336)
  attr(P, 'parameters') <- list(L = L, K = K, ELP = ELP)
  return(P)
}
Power.functions$van.der.Heijde <- van.der.Heijde.Power