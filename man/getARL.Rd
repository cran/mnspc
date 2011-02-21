\name{getARL}
\alias{getARL}
\title{Computes ARL given \eqn{h} and \eqn{k}.}
\description{
  Computes the average run length (ARL) of a process when \eqn{h} and \eqn{k} are both known. 
}
\usage{
getARL(f0, g, h, k, istart=0, iter=10000, print.results=TRUE)
}
\arguments{
  \item{f0}{The in-control distribution of the categorized response of the process.}
  \item{g}{The actual distribution of the categorized response of the process. If \code{f0} = \code{g}, the
  in-control ARL will be computed. If \code{f0} \eqn{!=} \code{g}, then the 
  out-of-control ARL will be computed for out-of-control distribution \code{g}.}
  \item{h}{The control limit.} 
  \item{k}{The allowance constant.} 
  \item{istart}{Steady-state start value.} 
  \item{iter}{The number of run lengths averaged to get the ARL.} 
  \item{print.results}{If \code{TRUE}, the results of the procedure will be printed.}
}
\value{
  A list containing the following:
  \item{ARL}{The computed ARL.}
  \item{SE}{The standard error of the ARL.}
  \item{RLs}{A vector containing all \code{iter} computed run lengths.}
}
\examples{
ic.dist<-rep(0.125,8)
oc.dist<-c(0.2072, 0.0429, 0.2070, 0.0429, 0.2071, 0.0428, 0.2072, 0.0429) 

# Computes in-control ARL when the in-control distribution 
# of the process is ic.dist.
getARL(ic.dist, ic.dist, h=9.1268, k=0.004, iter=1000)

# Computes ARL when the in-control distribution is ic.dist, 
# the out-of-control distribution is oc.dist, and the 
# process starts out-of-control.
getARL(ic.dist, oc.dist, h=9.1268, k=0.004, iter=2500)

# Computes out-of-control ARL when the distribution 
# of the process changes from ic.dist to oc.dist at the 
# 50th time point; creates histogram of run lengths.
a<-getARL(ic.dist, oc.dist, h=9.6364, k=0.121, istart=50)
a$ARL
hist(a$RLs)
}
\references{
Qiu, P. (2008) Distribution-free multivariate process control based on log-linear modeling. \emph{IIE
Transactions}, 40, 664-677.
}
\keyword{ARL}
