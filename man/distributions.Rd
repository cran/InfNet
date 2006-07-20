\name{distributions}

\alias{rzerotpoisson}
\alias{newrexp}
\alias{rlog}
\alias{rpoly.log}
\alias{rpower.law}

\title{ Simulates random numbers from Zero-truncated Poisson distribution, exponential, Logarithmic distribution (poly-logarithmic when alpha=1), poly logarithmic distribution and power law}
\description{
The considered pdf for the Zero-truncated Poisson distributionis is $P(N=k)=(lambda^k*exp^{-lambda})/(k!*(1-e^{-lambda})$.
The power law pdf is: $k^{-param}$
}
\usage{
rzerotpoisson(n, p)
newrexp(n, p)
rlog(n, param)
rpoly.log(n, param)
rpower.law(n, param)
}

\arguments{
\item{n}{Sample size}
\item{p}{Parameter}
\item{param}{Parameter distribution}
}

\keyword{distribution }
\keyword{datagen}

