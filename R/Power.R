#' Calculate IOL Power from Biometry Data and ELP
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
#' @param A IOL A constant
#' @param ELP effective lens position in millimeters (mm)
#' @param Rx desired refractive outcome in diopters (D), defaults to emmetropia
#' @param V vertex of refractive outcome in millimeters (mm), defaults to 13 mm
#' @param which string vector specifying which IOL power formulas to use
#' @return Named numeric vector of optimal IOL powers (in diopters, D) for each 
#'   formula requested
#' @export
#' @seealso \code{\link{ELP}}
#' @family Power
#' @author Eric N. Brown \email{eric.n.brown@@gmail.com}
Power <- function(L, K, A, ELP, Rx = 0, V = 13, which = 'all') {
  result <- list()
  if ('all' %in% which) {
    which <- names(Power.functions)
    which <- which[which != 'all']
  }
  if ('modern' %in% which) {
    which <- c(which, 'SRK.T', 'Holladay.1', 'Hoffer.Q')
    which <- which[which != 'modern']
  }
  which <- unique(which)
  for (i in which) {
    if (i %in% c('SRK', 'SRK.II')) {
      result[[i]] <- Power.functions[[i]](L = L, K = K, A = A)
    } else if (i %in% c('Colenbrander', 'Fyodorov', 'van.der.Heijde', 'Binkhorst', 
                        'Hoffer', 'Shammas', 'Binkhorst.adjusted',
                        'SRK.T')) {
      result[[i]] <- Power.functions[[i]](L = L, K = K, ELP = ELP)
    } else if (i %in% c('Holladay', 'Holladay.1', 'Hoffer.Q')) {
      result[[i]] <- Power.functions[[i]](L = L, K = K, ELP = ELP, Rx = Rx, V = V)
    } else {
      stop("Unknown IOL Power formula: '", i, "'")
    }
  }
  
  functions <- names(result)
  function.arguments <- vector(mode = 'character')
  P <- vector(mode = 'numeric')
  for (i in functions) {
    P[[i]] <- result[[i]]
    attr(P,'parameters')[[i]] <- attr(result[[i]],'parameters')
    function.arguments[[i]] <- paste(names(attr(result[[i]],'parameters')),
                                     collapse=', ')
  }
  names(function.arguments) <- NULL
  attr(P, 'function') <- functions
  attr(P, 'function.arguments') <- function.arguments
  class(P) <- 'Power'
  return(P)
}

#' @export
print.Power <- function(x, ...) {
  # Remove all attributes other than names
  n <- names(x)
  attributes(x) <- NULL
  names(x) <- n
  # Print the object
  print(unclass(x))
}