% Generated by roxygen2 (4.0.0): do not edit by hand
\name{Binkhorst.adjusted.Power}
\alias{Binkhorst.adjusted.Power}
\title{Adjusted Binkhorst Formula for IOL Power}
\usage{
Binkhorst.adjusted.Power(L, R, K, cornea_n, ELP)
}
\arguments{
  \item{L}{axial length in millimeters (mm)}

  \item{R}{corneal radius of curvature in millimeters (mm)}

  \item{K}{corneal curvature in diopters (D)}

  \item{cornea_n}{corneal index of refraction}

  \item{ELP}{effective lens position in millimeters (mm)}
}
\value{
IOL power for emmetropia
}
\description{
Calculate IOL power for emmetropia given axial length,
corneal radius of curvature, and effective lens position.
If corneal power (in diopters) is provided instead of the
radius of curvature, then the radius of curvature will be
computed from the corneal power and corneal index of
refraction. A standard corneal index of refraction will be
used if one isn't provided for this conversion.
}
\details{
This is an adjustment to the Binkhorst formula to correct
for axial length.
}
\references{
Binkhorst RD. Intraocular Lens Power Calculation Manual: A
Guide to The Author's TICC-40 Programs. 3rd Ed. New York:
R. D. Binkhorst, 1984.

\url{https://encrypted.google.com/books?id=NhWJsGFK6qgC&pg=PA11}
}
\seealso{
\code{\link{Power}}, \code{\link{Binkhorst.Power}}

Other Power: \code{\link{Binkhorst.Power}};
\code{\link{Colenbrander.Power}};
\code{\link{Fyodorov.Power}}; \code{\link{Haigis.Power}};
\code{\link{Hoffer.Power}}; \code{\link{Hoffer.Q.Power}};
\code{\link{Holladay.1.Power}};
\code{\link{Holladay.Power}}; \code{\link{Power}};
\code{\link{SRK.II.Power}}; \code{\link{SRK.Power}};
\code{\link{SRK.T.Power}}; \code{\link{Shammas.Power}};
\code{\link{van.der.Heijde.Power}}
}

