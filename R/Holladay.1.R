#' Holladay 1 Formula for Effective Lens Position
#' 
#' Calculate IOL effective lens position for emmetropia given axial length, corneal radius of curvature, 
#' and lens constant.
#' 
#' Note: If the Holladay Surgeon-factor constant for the lens is not provided, it is calculated from
#' the lens A-constant or pACD-constant. Similarily, if the corneal radius of curvature is not provided,
#' it is calculated from the corneal curvature (in diopters) and corneal index of refraction.
#' 
#' @param L length of the eye in millimeters (mm)
#' @param R corneal radius of curvature (mm)
#' @param S IOL surgeon-factor constant (mm)
#' @param K corneal power (D) used to determine corneal radius of curvature
#' @param cornea_n corneal index of refraction used to determine corneal radius of curvature from K
#' @param A IOL A constant (D) used to determine surgeon-factor
#' @param pACD IOL personalized ACD constant (mm) used to determine surgeon-factor
#' @return ELP in mm
#' @seealso \code{\link{ELP}}
#' @family ELP
#' @references \url{https://encrypted.google.com/books?id=NhWJsGFK6qgC&pg=PA8}
Holladay.1.ELP <- function(L, R, S, K, cornea_n, A, pACD) {
  args <- list(L = L)
  #https://encrypted.google.com/books?id=NhWJsGFK6qgC&pg=PA8&lpg=PA8&dq=colenbrander+formula&source=bl&ots=j__sC0XHHg&sig=DEaK9Qg0LOvedzq4itcfe7pBZ4Q&hl=en&sa=X&ei=scdlUuC1JcHP2wW90IDAAg&ved=0CD4Q6AEwAg#v=onepage&q=colenbrander%20formula&f=false
  if (missing(R) || ! is.finite(R)) {
    if (missing(cornea_n) || !is.finite(cornea_n)) {
      cornea_n <- Constants$Biometry$corneal_index
      warning("Using standard corneal index of refraction to convert K to R for Holladay 1 equation: ", cornea_n)
    }
    args$cornea_n <- cornea_n
    args$K <- K
    R <- 1000 * (cornea_n - 1) / K
    warning("Convert K to R for Holladay 1 equation: ", R, "mm")
  } else {
    args$R <- R
  }
  if (missing(S) || ! is.finite(S)) {
    if (! missing(A) && is.finite(A)) {
      S <- Constants$IOL$A_to_S_a0 + Constants$IOL$A_to_S_a1 * A
      args$A <- A
      warning("Using Holladay approximation for S from A constant for Holladay 1 equation: ", S, "mm")
    } else if (! missing(pACD) && is.finite(pACD)) {
      S <- Constants$IOL$pACD_to_S_a0 + Constants$IOL$pACD_to_S_a1 * pACD
      args$pACD <- pACD
      warning("Using Holladay approximation for S from pACD constant for Holladay 1 equation: ", S, "mm")
    } else {
      stop("No S (surgeon factor) provided for Holladay 1 equation")
    }
  } else {
    args$S <- S
  }
  AG <- L * 12.5 * 1 / 23.45
  temp <- R * R - AG * AG / 4
  if (temp < 0) {
    warning("Calculation of anatomic anterior chamber depth poorly defined for this combination of corneal curvature and axial length in Holladay 1 formula")
    temp <- 0
    result <- NA
    attr(result, 'parameters') <- args
    return(result)
  }
  aACD <- 0.56 + R  - sqrt(temp)
  ELP <- aACD + S
  result <- ELP
  attr(result, 'parameters') <- args
  return(result)
}
ELP.functions$Holladay.1 <- Holladay.1.ELP

#' Holladay-1 Formula for IOL Power
#' 
#' Calculate IOL power for emmetropia given axial length, 
#' corenal curvature, and effective lens position.
#' 
#' Note: if the corneal radius, \code{R}, is not provided, it is determined 
#' from the corneal power, \code{K}, and the corneal index of refraction, 
#' \code{cornea_n}.
#' 
#' @param L length of the eye in millimeters (mm)
#' @param R corneal radius of curvature (mm)
#' @param ELP IOL effective lens position (mm)
#' @param K corneal power (D) used to determine corneal radius of curvature
#' @param cornea_n corneal index of refraction used to determine corneal radius of curvature from K, defaults to 1.3375
#' @param aqueous_n aqueous index of refraction, defaults to 1.3375
#' @param RT retinal thickness (mm), defaults to 0.2 mm
#' @param Rx desired resulting refractive error (D), defaults to 0 D
#' @param V vertex distance of Rx (mm), defaults to 13 mm
#' @return Power of IOL (D)
#' @seealso \code{\link{Power}}
#' @family Power
#' @references \url{https://encrypted.google.com/books?id=NhWJsGFK6qgC&pg=PA8}
Holladay.1.Power <- function(L, R, ELP,
                                       K, cornea_n = 1.3375,
                                       aqueous_n =1.336, RT = 0.2, Rx = 0, V = 13) {
  args <- list(L = L)
  #https://encrypted.google.com/books?id=NhWJsGFK6qgC&pg=PA8&lpg=PA8&dq=colenbrander+formula&source=bl&ots=j__sC0XHHg&sig=DEaK9Qg0LOvedzq4itcfe7pBZ4Q&hl=en&sa=X&ei=scdlUuC1JcHP2wW90IDAAg&ved=0CD4Q6AEwAg#v=onepage&q=colenbrander%20formula&f=false
  if (missing(R) || ! is.finite(R)) {
    if (missing(cornea_n) || !is.finite(cornea_n)) {
      cornea_n <- Constants$Biometry$corneal_index
      warning("Using standard corneal index of refraction to convert K to R for Holladay 1 equation: ", cornea_n)
    }
    args$cornea_n <- cornea_n
    args$K <- K
    R <- 1000 * (cornea_n - 1) / K
    warning("Convert K to R for Holladay 1 equation: ", R, "mm")
  } else {
    args$R <- R
  }
  Alm <- L + RT
  nc <- 4/3
  naRnc1Alm <- aqueous_n * R - (nc - 1) * Alm
  naRnc1ELP <- aqueous_n * R - (nc - 1) * ELP
  P <- (1000 * aqueous_n * (naRnc1Alm - 0.001 * Rx * (V * naRnc1Alm + Alm * R))) /
    ((Alm - ELP) * (naRnc1ELP - 0.001 * Rx * (V * naRnc1ELP + ELP * R)))
  result <- P
  attr(result, 'parameters') <- args
  return(result)
}
Power.functions$Holladay.1 <- Holladay.1.Power