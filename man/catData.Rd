\name{catData}
\alias{catData}
\title{Transform components of data vectors into binary data.}
\description{
  This function first takes a collection of data vectors and determines the median of 
  each component. Each component of every vector is transformed to '0' if it is below
  the component median, or '1' if it is above the component median. A contingency table of 
  relative frequencies is returned. Log-linear modeling can be applied to the returned contingency table.
}
\usage{
catData(indat, table=TRUE)
}
\arguments{
  \item{indat}{The data to be discretized. See details.}
  \item{table}{If \code{TRUE}, a contingency table of the transformed binary vector frequencies is returned. If \code{FALSE}, the transformed binary vectors are returned.}
}
\value{
  A contingency table (or data frame).
}
\details{
The rows of \code{indat} must be the stacked data vectors with the columns being each vector component. For
an example of acceptable input data, load and view the dataset \code{sample} which is included in
the package. If any vectors contain missing components, they will be ignored and a warning message will
be printed.
}
\examples{
#Dataset 'sample' is included in the package.
data(sample)
catData(sample)
}
\references{
Qiu, P. (2008) Distribution-free multivariate process control based on log-linear modeling. \emph{IIE
Transactions}, 40, 664-677.
}
\keyword{distribution}
