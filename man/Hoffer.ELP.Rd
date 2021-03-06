% Generated by roxygen2 (4.0.0): do not edit by hand
\name{Hoffer.ELP}
\alias{Hoffer.ELP}
\title{Hoffer Formula for Effective Lens Position}
\usage{
Hoffer.ELP(L, ACD, A)
}
\arguments{
  \item{L}{length of the eye in millimeters (mm)}

  \item{ACD}{IOL personalized ACD constant (mm)}

  \item{A}{IOL A constant (D) used to determine ACD}
}
\value{
ELP in mm
}
\description{
Calculate IOL effective lens position for emmetropia given
axial length and lens A-constant or pACD constant.
}
\details{
Note: If the pACD constant, \code{ACD}, for the lens is not
provided, it is calculated from the lens A-constant,
\code{A}, using the Holladay approximation.
}
\references{
\url{https://encrypted.google.com/books?id=NhWJsGFK6qgC&pg=PA8}
}
\seealso{
\code{\link{ELP}}

Other ELP: \code{\link{ELP}}; \code{\link{Haigis.ELP}};
\code{\link{Hoffer.Q.ELP}}; \code{\link{Holladay.1.ELP}};
\code{\link{Holladay.ELP}}; \code{\link{SRK.T.ELP}}
}

