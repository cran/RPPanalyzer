\name{curvePredictSigmoid}
\alias{curvePredictSigmoid}

\title{
Sigmoidal curve prediction.
}

\description{
3-parameter sigmoidal curve.
}

\usage{
curvePredictSigmoid(x, params)
}

\arguments{
  \item{x}{
Input value(s).
}
  \item{params}{
Parameter vector containing three parameters alpha, beta and gamma.
}
}

\details{
The model is defined as \code{alpha + beta*(2^(x*gamma))/(1+2^(x*gamma)))}.
}

\value{
The prediction f(x) of the input value(s).
}
\examples{
\dontrun{
x <- seq(-5, 5, by=0.1)
y <- curvePredictSigmoid(x, c(alpha=2, beta=1, gamma=1.5))
plot(x, y)
}
}
