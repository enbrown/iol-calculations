% Generated by roxygen2 (4.0.0): do not edit by hand
\name{SRK.T.ELP}
\alias{SRK.T.ELP}
\title{SRK/T Formula for IOL Effective Lens Position}
\usage{
SRK.T.ELP(L, K = 337.5/R, R = 337.5/K, ACD, A)
}
\arguments{
  \item{L}{axial length of the eye in millimeters (mm)}

  \item{K}{corneal power (D)}

  \item{ACD}{IOL anterior chamber depth constant (mm)}

  \item{A}{IOL A-constant (D) used to calculate equivalent
  ACD}
}
\value{
Effective lens position of IOL (mm)
}
\description{
Calculate IOL effective lens position for emmetropia given
axial length, corenal curvature, and IOL A-constant.
}
\details{
Note: A warning is provided if the combination of corneal
curvature and axial length produces an unexpected corneal
height.
}
\seealso{
\code{\link{ELP}}

Other ELP: \code{\link{ELP}}; \code{\link{Haigis.ELP}};
\code{\link{Hoffer.ELP}}; \code{\link{Hoffer.Q.ELP}};
\code{\link{Holladay.1.ELP}}; \code{\link{Holladay.ELP}}
}

