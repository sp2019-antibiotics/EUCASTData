\name{MIC}
\alias{MIC}
\docType{data}
\title{
  MIC Distributions of Microorganisms Without and With Resistance Mechanisms.
}
\description{
  The data give MIC distributions for individual organisms and
  antimicrobial agents in tables and histograms. The distributions are
  based on collated data from an increasing total of more than 20000 MIC
  distributions from worldwide sources. Unless otherwise specifically
  stated, the data are representative of results obtained with a variety
  of MIC methods. Different methods do not give exactly the same results
  but the results rarely vary by more than one doubling dilution
  step. In this way the aggregated MIC distributions encompass the
  variation between different investigators and between different
  methods.
}
\usage{data("MIC")}
\format{
  A data frame with 3309 observations on the following 24 variables.
  \describe{
    \item{\code{Antimicrobial}}{a factor}
    \item{\code{Bacterium}}{a factor}
    \item{\code{M0.002}}{a numeric vector}
    \item{\code{M0.004}}{a numeric vector}
    \item{\code{M0.008}}{a numeric vector}
    \item{\code{M0.016}}{a numeric vector}
    \item{\code{M0.032}}{a numeric vector}
    \item{\code{M0.064}}{a numeric vector}
    \item{\code{M0.125}}{a numeric vector}
    \item{\code{M0.25}}{a numeric vector}
    \item{\code{M0.5}}{a numeric vector}
    \item{\code{M1}}{a numeric vector}
    \item{\code{M2}}{a numeric vector}
    \item{\code{M4}}{a numeric vector}
    \item{\code{M8}}{a numeric vector}
    \item{\code{M16}}{a numeric vector}
    \item{\code{M32}}{a numeric vector}
    \item{\code{M64}}{a numeric vector}
    \item{\code{M128}}{a numeric vector}
    \item{\code{M256}}{a numeric vector}
    \item{\code{M512}}{a numeric vector}
    \item{\code{ECOFF}}{a numeric vector}
    \item{\code{Distributions}}{a numeric vector}
    \item{\code{Observations}}{a numeric vector}
  }
}
\source{
  EUCAST database for antimicrobial wild type distributions of microorganisms.
  \url{https://mic.eucast.org/Eucast2/}
}
\examples{
data("MIC")
}
\keyword{datasets}
