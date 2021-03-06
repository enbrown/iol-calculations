% Generated by roxygen2 (4.0.0): do not edit by hand
\name{Haigis.Power}
\alias{Haigis.Power}
\title{Haigis Formula for Emmetropic IOL Power}
\usage{
Haigis.Power(L, R, ELP, Rx = 0, V = 12)
}
\arguments{
  \item{L}{length of the eye in millimeters (mm)}

  \item{R}{average corneal radius in millimeters (mm)}

  \item{ELP}{IOL effective lens position (mm)}

  \item{Rx}{resulting refractive error (D), defaults to 0
  D}

  \item{V}{resulting refractive error vertex distance (mm),
  defaults to 12 mm}
}
\value{
Power of emmetropic IOL (D)
}
\description{
Calculate IOL power for emmetropia given axial length,
corneal curvature, and effective lens position.
}
\seealso{
\code{\link{Power}}

Other Power: \code{\link{Binkhorst.Power}};
\code{\link{Binkhorst.adjusted.Power}};
\code{\link{Colenbrander.Power}};
\code{\link{Fyodorov.Power}}; \code{\link{Hoffer.Power}};
\code{\link{Hoffer.Q.Power}};
\code{\link{Holladay.1.Power}};
\code{\link{Holladay.Power}}; \code{\link{Power}};
\code{\link{SRK.II.Power}}; \code{\link{SRK.Power}};
\code{\link{SRK.T.Power}}; \code{\link{Shammas.Power}};
\code{\link{van.der.Heijde.Power}}
}

