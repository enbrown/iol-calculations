#' Calculate Effective Lens Position (ELP) from Biometry and IOL Data
#' 
#' Using specified ocular biometry data including a measure of the eye's optical
#' length and corneal curvature, this function calculates one or more estimates 
#' of an intraocular lens's effective lens position (ELP). The ELP is the 
#' position in millimeters of the IOL's principle plane from the cornea's 
#' principle plane. The ELP used to be referred to as the anterior chamber depth
#' (ACD); however, ELP is a more accurate description.
#' 
#' @param L length of the eye in millimeters (mm)
#' @param K average corneal curvature of the eye in diopters (D)
#' @param R average corneal radius in millimeters (mm)
#' @param cornea_n effective corneal index of refraction
#' @param ACD ultrasound anterior chamber depth (mm)
#' @param A IOL A constant (D)
#' @param pACD IOL pACD constant (mm)
#' @param S IOL surgeon factor constant
#' @param a0 Haigis formula a0 lens constant (mm)
#' @param a1 Haigis formula a1 lens constant
#' @param a2 Haigis formula a2 lens constant
#' @param which string vector specifying which ELP formulas to use
#' @return Named numeric vector of effective lens position (in mm) for each ELP 
#'   formula requested.
#' @export
#' @seealso \code{\link{Power}}
#' @family ELP
#' @note If the some of the IOL constants A, pACD, or S are not provided, it may
#'   be derived from those given. A warning is generally produced when this 
#'   conversion is performed.
#'   
#'   The returned numeric vector is augmented with a \code{'parameters'} list 
#'   attribute describing which biometry and IOL parameters were used to 
#'   calculate each value. For example, if the Hoffer Q ELP is calculated given 
#'   an axial length, corneal power (via the \code{K} parameter), and an 
#'   A-constant (via the \code{A} parameter), then the \code{'parameters'} list 
#'   attribute will have a list named \code{Hoffer.Q} with three values: 
#'   \code{L}, \code{K}, and \code{A}.
#' @author Eric N. Brown \email{eric.n.brown@@gmail.com}
#' @examples
#' # Get the effective lens position of a normal eye with for the
#' # Alcon SA60AT lens using the Hoffer Q formula. This will compute
#' # the required pACD IOL constant from the provided A constant.
#' (elp <- ELP(L = 24, K = 44, A = 118.4, which = 'Hoffer.Q'))
#'  
#' # Check which parameters were used to calculate the ELP
#' attr(elp, 'parameters')$Hoffer.Q
#' 
#' # Get the ELP of a normal eye with modern formulas (Hoffer Q, SRK/T,
#' # and Holladay 1). Five warnings will be output for the conversion of
#' # the A constant to ACD for the SRK/T and Hoffer Q formulas, using a
#' # standard corneal index of refraction (since one wasn't provided) to
#' # convert corneal power to radius of curvature, and for approximating
#' # the Holladay 1 surgeon factor from the provided A constant.
#' (elp <- ELP(L = 24, K = 44, A = 118.4, which = 'modern'))
#' 
#' # Get the ELP of a normal eye with the Holladay 1 formula. Although
#' # both the IOL A constant and surgeon factor are provided, since the
#' # formula requires the surgeon factor, the A constant will be ignored
#' (elp <- ELP(L = 24, K = 44, A = 118.4, S = 1.45, which = 'Holladay.1'))
#' attr(elp, 'parameters')$Holladay.1
ELP <- function(L, 
                K, R = NA, cornea_n = NA,
                ACD = NA,
                A = NA, pACD = NA, S = NA, a0 = NA, a1, a2,
                which = 'modern') {
  cl <- match.call()
  
  # Determine which equations to use
  if ('all' %in% which) {
    which <- names(Power.functions)
  } else if ('modern' %in% which) {
    which <- c(which, 'SRK.T', 'Haigis', 'Hoffer.Q', 'Holladay.1')
  }
  which <- unique(which)
  which <- which[! which %in% c('all', 'modern')]
  
  # Get the ELP using each equation
  result <- list()
  for (i in which) {
    if (is.null(ELP.functions[[i]])) {
      warning("Unknown ELP method requested: ", i, ".")
      next
    }
    args <- names(cl) %in% names(formals(ELP.functions[[i]]))
    args <- as.list(cl)[args]
    result[[i]] <- do.call(ELP.functions[[i]], args)
#     if (i == 'Holladay') {
#       result[[i]] <- ELP.functions[[i]](A = A)
#     } else if (i == 'Haigis') {
#       args <- names(cl) %in% names(formals(ELP.functions[[i]]))
#       args <- as.list(cl)[args]
#       result[[i]] <- do.call(ELP.functions[[i]], args)
#     } else if (i == 'Hoffer') {
#       result[[i]] <- ELP.functions[[i]](L = L, ACD = pACD, A = A)
#     } else if (i == 'Hoffer.Q') {
#       result[[i]] <- ELP.functions[[i]](L = L, K = K, A = A, pACD = pACD)
#     } else if (i == 'Holladay.1') {
#       result[[i]] <- ELP.functions[[i]](L = L, 
#                                         R = R, cornea_n = cornea_n, K = K,
#                                         S = S, A = A, pACD = pACD)
#     } else if (i == 'SRK.T') {
#       result[[i]] <- ELP.functions[[i]](L = L, K = K, A = A, ACD = pACD)
#     } else if (i == 'all' || i == 'modern') {
#       # do nothing
#     } else {
#       warning("Unknown ELP method requested.")
#     }
  }
  
  # Remember the names of the ELP functions
  functions <- names(result)
  function.arguments <- vector(mode = 'character')
  ELP <- vector(mode = 'numeric')
  for (i in functions) {
    ELP[[i]] <- result[[i]]
    attr(ELP,'parameters')[[i]] <- attr(result[[i]],'parameters')
    function.arguments[[i]] <- paste(names(attr(result[[i]],'parameters')),
                                     collapse=', ')
  }
  names(function.arguments) <- NULL
  attr(ELP, 'call') <- cl
  attr(ELP, 'function') <- functions
  attr(ELP, 'function.arguments') <- function.arguments
  class(ELP) <- 'ELP'
  return(ELP)
}

#' @export
print.ELP <- function(x, ...) {
  # Remove all attributes other than names
  n <- names(x)
  attributes(x) <- NULL
  names(x) <- n
  # Print the object
  print(unclass(x))
}