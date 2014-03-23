#' Holladay Formula for Effective Lens Position
#' 
#' Calculate IOL effective lens position for emmetropia given lens A-constant.
#' 
#' @param A IOL A-constant (D)
#' @return ELP in mm
#' @seealso \code{\link{ELP}}
#' @family ELP
#' @references \url{http://books.google.com/books?id=SHjgbQ9auqYC&pg=PT93}
Holladay.ELP <- function(A) {
  # http://books.google.com/books?id=SHjgbQ9auqYC&pg=PT93&dq=iol+calculation+1336&hl=en&sa=X&ei=kXsXUubKNeiW2gX1woDgDQ&ved=0CEMQ6AEwAg#v=onepage&q=iol%20calculation%201336&f=false
  ELP <- Constants$IOL$A_to_pACD_a0 + Constants$IOL$A_to_pACD_a1 * A
  result <- ELP
  attr(result, 'parameters') <- list(A = A)
  return(result)
}
ELP.functions$Holladay <- Holladay.ELP

#' Holladay Formula for IOL Power
#' 
#' Calculate IOL power for emmetropia given axial length, 
#' corenal curvature, and effective lens position.
#' 
#' @param L length of the eye in millimeters (mm)
#' @param K corneal power (D)
#' @param ELP IOL effective lens position (mm)
#' @param Rx desired resulting refractive error (D), defaults to 0 D
#' @param V vertex distance of Rx (mm), defaults to 13 mm
#' @return Power of IOL (D)
#' @seealso \code{\link{Power}}
#' @family Power
#' @references \url{http://books.google.com/books?id=SHjgbQ9auqYC&pg=PT93}
Holladay.Power <- function(L, K, ELP, Rx = 0, V = 13) {
  args <- list(L = L, K = K, ELP = ELP, Rx = Rx, V = V)
  P <- 1336 / (L - ELP) -
    1336 / (
      1336 / ((1000 / (1000 / Rx - V)) + K) 
      - ELP)
  attr(P, 'parameters') <- args
  return(P)
}
Power.functions$Holladay <- Holladay.Power