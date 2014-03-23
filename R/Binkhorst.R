#' Binkhorst Formula for IOL Power
#' 
#' Calculate IOL power for emmetropia given axial length, corneal radius of 
#' curvature, and effective lens position. If corneal power (in diopters) is 
#' provided instead of the radius of curvature, then the radius of curvature 
#' will be computed from the corneal power and corneal index of refraction. A 
#' standard corneal index of refraction will be used if one isn't provided for 
#' this conversion.
#'
#' @param L axial length in millimeters (mm)
#' @param R corneal radius of curvature in millimeters (mm)
#' @param K corneal curvature in diopters (D)
#' @param cornea_n corneal index of refraction
#' @param ELP effective lens position in millimeters (mm)
#' @return IOL power for emmetropia
#' @references Binkhorst RD. The optical design of intraocular lens implants. 
#'   Ophthalmic Surg. Fall 1975; 6(3): 17--31. PMID:
#'   \href{http://www.ncbi.nlm.nih.gov/pubmed/1187085}{1187085}.
#'   
#'   \url{https://encrypted.google.com/books?id=NhWJsGFK6qgC&pg=PA8}
#' @seealso \code{\link{Power}}, \code{\link{Binkhorst.adjusted.Power}}
#' @family Power
Binkhorst.Power <- function(L, R, K, cornea_n, ELP) {
  if (missing(L) || missing(ELP)) {
    stop("Axial length (L) and effective lens position (ELP) required for Binkhorst equation.")
  }
  args <- list(L = L, ELP = ELP)
  if (missing(R) || ! is.finite(R)) {
    if (missing(K) || ! is.numeric(K) || ! is.finite(K) || K <= 0) {
      stop("Either K or R required for Binkhorst equation.")
    }
    args$K <- K
    if (missing(cornea_n) || is.finite(cornea_n)) {
      cornea_n <- 4/3
      warning("Using corneal index of refraction to convert K to R for Binkhorst equation: ", cornea_n)
    }
    args$cornea_n <- cornea_n
    R <- 1000 * (cornea_n - 1) / K
    warning("Convert K to R for Binkhorst equation: ", R, "mm")
  } else {
    args$R <- R
  }
  #https://encrypted.google.com/books?id=NhWJsGFK6qgC&pg=PA8&lpg=PA8&dq=colenbrander+formula&source=bl&ots=j__sC0XHHg&sig=DEaK9Qg0LOvedzq4itcfe7pBZ4Q&hl=en&sa=X&ei=scdlUuC1JcHP2wW90IDAAg&ved=0CD4Q6AEwAg#v=onepage&q=colenbrander%20formula&f=false
  P <- 1336 * (4 * R - L) / 
    ((L - ELP) * (4 * R - ELP))
  attr(P, 'parameters') <- args
  return(P)
}
Power.functions$Binkhorst <- Binkhorst.Power

#' Adjusted Binkhorst Formula for IOL Power
#' 
#' Calculate IOL power for emmetropia given axial length, corneal radius of 
#' curvature, and effective lens position. If corneal power (in diopters) is 
#' provided instead of the radius of curvature, then the radius of curvature 
#' will be computed from the corneal power and corneal index of refraction. A 
#' standard corneal index of refraction will be used if one isn't provided for 
#' this conversion.
#' 
#' This is an adjustment to the Binkhorst formula to correct for axial length.
#' 
#' @param L axial length in millimeters (mm)
#' @param R corneal radius of curvature in millimeters (mm)
#' @param K corneal curvature in diopters (D)
#' @param cornea_n corneal index of refraction
#' @param ELP effective lens position in millimeters (mm)
#' @return IOL power for emmetropia
#' @references Binkhorst RD. Intraocular Lens Power Calculation Manual: A Guide
#'   to The Author's TICC-40 Programs. 3rd Ed. New York: R. D. Binkhorst, 1984.
#'   
#'   \url{https://encrypted.google.com/books?id=NhWJsGFK6qgC&pg=PA11}
#' @seealso \code{\link{Power}}, \code{\link{Binkhorst.Power}}
#' @family Power
Binkhorst.adjusted.Power <- function(L, R, K, cornea_n, ELP) {
  if (missing(L) || missing(ELP)) {
    stop("Axial length (L) and effective lens position (ELP) required for Binkhorst equation.")
  }
  args <- list(L = L, ELP = ELP)
  if (missing(R) || ! is.finite(R)) {
    if (missing(K) || ! is.numeric(K) || ! is.finite(K) || K <= 0) {
      stop("Either K or R required for Binkhorst equation.")
    }
    args$K <- K
    if (missing(cornea_n) || ! is.finite(cornea_n)) {
      cornea_n <- 4/3
      warning("Using corneal index of refraction to convert K to R for Binkhorst's adjusted equation: ", cornea_n)
    }
    args$cornea_n <- cornea_n
    R <- 1000 * (cornea_n - 1) / K
    warning("Convert K to R for Binkhorst's adjusted equation: ", R, "mm")
  } else {
    args$R <- R
  }
  # https://encrypted.google.com/books?id=NhWJsGFK6qgC&pg=PA11
  #https://encrypted.google.com/books?id=NhWJsGFK6qgC&pg=PA8&lpg=PA8&dq=colenbrander+formula&source=bl&ots=j__sC0XHHg&sig=DEaK9Qg0LOvedzq4itcfe7pBZ4Q&hl=en&sa=X&ei=scdlUuC1JcHP2wW90IDAAg&ved=0CD4Q6AEwAg#v=onepage&q=colenbrander%20formula&f=false
  aELP <- ELP + 0.17 * (L - 23.45)
  P <- 1336 * (4 * R - L) / 
    ((L - aELP) * (4 * R - aELP))
  attr(P, 'parameters') <- args
  return(P)
}
Power.functions$Binkhorst.adjusted <- Binkhorst.adjusted.Power