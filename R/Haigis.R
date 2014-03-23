#' Haigis Formula for Effective Lens Position
#' 
#' Calculate IOL effective lens position for emmetropia given personalized
#' lens-constant anterior chamber depth (pACD). 
#' 
#' Note: Currently this is just the provided pACD constant.
#' 
#' @param pACD personalized ACD (mm) lens constant
#' @return ELP in mm
#' @seealso \code{\link{ELP}}
#' @family ELP
Haigis.ELP <- function(pACD) {
  # Check arguments
  if (missing(pACD)) {
    stop("Argument pACD missing for Haigis ELP function.")
  }
  if (! is.numeric(pACD)) {
    stop("Non-numeric argument pACD provided for Haigis ELP function.")
  }
  if (pACD < 0) {
    warning("Implausable negative pACD provided to Haigis ELP function.")
  }
  result <- pACD
  attr(result, 'parameters') <- list(pACD = pACD)
  return(result)
}
ELP.functions$Haigis <- Haigis.ELP