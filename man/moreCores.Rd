% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/tricks.R
\name{moreCores}
\alias{moreCores}
\title{Take a time-heavy chunk of code that is parallelizable and speed it up.}
\usage{
moreCores(code, all.but = 1)
}
\arguments{
\item{code}{The code you want to execute}

\item{all.but}{How many cores do you want to exclude from this process? Defaults to 0.}
}
\description{
Take a time-heavy chunk of code that is parallelizable and speed it up.
}
\examples{
y <- numeric(10000000)
moreCores(for (i in 1:10000000) {y[i] <- i*(i+1)*(i-1)*(i+2)})
y
}
\keyword{parallel}

