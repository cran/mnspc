\name{kSearch}
\alias{kSearch}
\title{Searching for the optimal value of \eqn{k} for a target shift.}
\description{
  This algorithm searches for the reference value \eqn{k}
  which results in the shortest out-of-control average run length (ARL) for a given out-of-control 
  distribution and in-control average run length. 
}
\usage{
kSearch(f0, g, arl0, Lk=0, Uk, ek=0.01, istart=0, iter=10000, print.kHistory=TRUE)
}
\arguments{
  \item{f0}{The estimated in-control distribution of the categorized process response.}
  \item{g}{The out-of-control distribution of the categorized process response..}
  \item{arl0}{The in-control ARL.}
  \item{Lk}{Lower bound of the search interval.}
  \item{Uk}{Upper bound of the search interval. See details.}
  \item{ek}{Stopping threshold. See details.}
  \item{istart}{Steady state start value.}
  \item{iter}{Number of run lengths used to compute the ARLs throughout the procedure.}
  \item{print.kHistory}{If \code{TRUE}, the search history will be printed (recommended.)}
}
\details{
For a given \code{f0}, \code{g}, and \code{arl0}, this function will provide all of the necessary
estimates to set up the CUSUM procedure in Qiu (2008). If there is no prior knowledge about \code{Uk}, 
the argument can be omitted. The procedure will determine \code{Uk} before searching for \eqn{k}. 

The stopping threshold \code{ek} is explained on pg. 668 in Qiu (2008). 

\code{istart=50} is a good approximation to the steady state start and will not
substantially slow down the procedure. Setting \code{istart} to more than 100 is generally not recommended. 

Due to the discreteness of the charting statistic, there will not always exist an \eqn{h} such that a target
in-control ARL \code{arl0} can be achieved for a given \code{f0}, \eqn{k}, and \code{istart}. If the actual in-control
ARL is significantly different from the desired in-control ARL, a warning message will be printed. 
If this message is printed, the problem can be alleviated by changing \code{arl0} (based on the 
search history) or by changing \code{istart}.

The number of iterations should be chosen based on desired precision and speed. As a warning, speed may be an issue here. For some distributions, this routine may take up to several hours to complete.

This function calls \code{getARL} and \code{hSearch}.
}

\value{
  A list containing the following:
  \item{k}{The searched value of \eqn{k}}
  \item{h}{The corresponding \eqn{h} to achieve desired in-control ARL.}
  \item{arl0.actual}{The actual in-control ARL.}
  \item{arl1}{The out-of-control ARL.}
}
\examples{
\dontrun{
ic.dist1<-rep(.125,8)
oc.dist1<-c(.93, rep(.01, 7))
kSearch(f0=ic.dist1, g=oc.dist1, Lk=0, Uk=.5, ek=.01, arl0=200)
}
}
\references{
Qiu, P. (2008) Distribution-free multivariate process control based on log-linear modeling. \emph{IIE
Transactions}, 40, 664-677.
}
\keyword{allowance}
