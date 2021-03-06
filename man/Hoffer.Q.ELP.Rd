% Generated by roxygen2 (4.0.0): do not edit by hand
\name{Hoffer.Q.ELP}
\alias{Hoffer.Q.ELP}
\title{Hoffer-Q Formula for Effective Lens Position}
\usage{
Hoffer.Q.ELP(L, K = 337.5/R, R, pACD, A)
}
\arguments{
  \item{L}{length of the eye in millimeters (mm)}

  \item{K}{average corneal curvature of the eye in diopters
  (D)}

  \item{pACD}{IOL personalized ACD constant (mm)}

  \item{A}{IOL A constant (D) used to determine pACD}
}
\value{
ELP in mm
}
\description{
Calculate IOL effective lens position for emmetropia given
axial length, corenal curvature, and lens A-constant or
pACD constant.
}
\details{
Note: If the pACD constant, \code{pACD} for the lens is not
provided, it is calculated from the lens A-constant,
\code{A}, using the Holladay approximation.
}
\seealso{
\code{\link{ELP}}

Other ELP: \code{\link{ELP}}; \code{\link{Haigis.ELP}};
\code{\link{Hoffer.ELP}}; \code{\link{Holladay.1.ELP}};
\code{\link{Holladay.ELP}}; \code{\link{SRK.T.ELP}}
}

