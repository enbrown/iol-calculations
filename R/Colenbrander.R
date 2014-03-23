#' Colenbrander Formula for IOL Power
#' 
#' Calculate IOL power for emmetropia given axial length, corneal power, and
#' effective lens position.
#' 
#' @param L axial length in millimeters (mm)
#' @param K corneal curvature in diopters (D)
#' @param ELP effective lens position in millimeters (mm)
#' @return IOL power for emmetropia
#' @references \url{https://encrypted.google.com/books?id=NhWJsGFK6qgC&pg=PA8}
#' @seealso \code{\link{Power}}
#' @family Power
Colenbrander.Power <- function(L, K, ELP) {
  #https://encrypted.google.com/books?id=NhWJsGFK6qgC&pg=PA8&lpg=PA8&dq=colenbrander+formula&source=bl&ots=j__sC0XHHg&sig=DEaK9Qg0LOvedzq4itcfe7pBZ4Q&hl=en&sa=X&ei=scdlUuC1JcHP2wW90IDAAg&ved=0CD4Q6AEwAg#v=onepage&q=colenbrander%20formula&f=false
  P <- (1336 / (L - ELP - 0.05) -
          1336 / (1336 / K - ELP - 0.05))
  attr(P, 'parameters') <- list(L = L, K = K, ELP = ELP)
  return(P)
}
Power.functions$Colenbrander <- Colenbrander.Power