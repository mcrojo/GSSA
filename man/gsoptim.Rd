\name{gsoptim}
\alias{gsoptim}
\title{
Golden Section Search Algorithm.
}
\description{
Implements the "Golden Section Search Algorithm" to find the minimum of a function in a given interval.
}
\usage{
gsoptim(g,int,e=1e-6)
}

\arguments{
  \item{g}{Function to be minimized.}
  \item{int}{Initial interval where the minimum of the function will be found.}
  \item{e}{Tolerance of the algorithm. Default set as \eqn{10^{-6}}.}
}

\details{
\code{gsoptim} finds the minimum of a function \eqn{g(x)} in a given interval but does not ensure that is the global minimum.\cr
\code{show()}, \code{summary()} and \code{plot()} methods are available.
}

\value{
\item{min}{Minimum of the function.}
\item{int.lower}{Lower bound of the interval where the minimum was found.}
\item{int.upper}{Upper bound of the interval where the minimum was found.}
\item{it}{Iterations until convergency.}
\item{g}{Original function where the minimum was found.}
}

\author{
Maria Constanza Rojo Alfaro.
}

\examples{
library(GSSA)
f<- function(x) x^2/2-2*x+3
test<-gsoptim(f,c(-10,10),1e-12)
slotNames(test)
summary(test)
plot(test)
}
