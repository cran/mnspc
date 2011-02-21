\name{sample}
\docType{data}
\alias{sample}
\title{Example of acceptable in-control data.}
\description{
In order to use the \code{catData} function, the in-control data
should be in this form. The rows of the data frame are each of the
data vectors, with the columns being the vector components.
}
\usage{data(sample)}
\format{
  A example data frame with 20 in-control observations.
   \describe{
    \item{weight}{Weight of each object.}
    \item{defects}{Number of imperfections in the object.}
    \item{purity}{Purity of the object.}
  }
}
\examples{
library(mnspc)
data(sample)
}
\keyword{datasets}

