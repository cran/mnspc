\name{hSearch}
\alias{hSearch}
\title{Searching for \eqn{h} when \eqn{k} is given.}
\description{
  Given a value of \eqn{k}, this procedure searches for the decision interval \eqn{h} to achieve
  a pre-specified in-control average run length (ARL).
}
\usage{
hSearch(f0, arl0, k, Lh, Uh, eh1=0.5, eh2=0.001, istart=0, iter=10000, print.hHistory=TRUE)
}
\arguments{
  \item{f0}{The estimated in-control distribution of the categorized response of the process.}
  \item{arl0}{The desired in-control ARL.}
  \item{k}{The allowance constant.}
  \item{Lh}{Lower bound of the search interval. See details.}
  \item{Uh}{Upper bound of the search interval. See details.}
  \item{eh1}{Stopping threshold 1. See details.}
  \item{eh2}{Stopping threshold 2. See details.}
  \item{istart}{Steady state start value.}
  \item{iter}{Number of replications used to compute ARLs throughout the procedure.}
  \item{print.hHistory}{If \code{TRUE}, the search history will be printed (recommended.)}
}
\details{
If there is no prior information about the search interval boundaries \code{Lh} and \code{Uh}, they
can be omitted. The procedure will first select appropriate decision interval bounds and then search for \eqn{h}. Choosing a very large \code{Uh} may slow down the algorithm substantially and choosing a \code{Uh} that is too small may result in the true \eqn{h} being excluded from the search interval. 

\code{istart=50} is a good approximation to the steady state start and will not
substantially slow down the procedure. Setting \code{istart} to more than 100 is generally not recommended. 

The stopping rules \code{eh1} and \code{eh2} are explained on pgs. 668-669 of Qiu (2008). \code{iter} should
be chosen based on desired precision and speed, although speed is generally
not an issue here.

This functions calls \code{getARL}.
}

\value{
  A list containing the following:
  \item{h}{The searched value of \eqn{h}}
  \item{arl0.actual}{The actual in-control ARL.}
}
\examples{
ic.dist<-rep(0.125,8)

hSearch(f0=ic.dist, arl0=200, k=0.003)

hSearch(f0=ic.dist, arl0=200, k=0.967, eh1=0.2, Lh=8, Uh=12, istart=100, iter=5000)
}
\references{
Qiu, P. (2008) Distribution-free multivariate process control based on log-linear modeling. \emph{IIE
Transactions}, 40, 664-677.
}

\keyword{decision}
