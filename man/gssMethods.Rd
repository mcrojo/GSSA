\docType{methods}
\name{gss-method}
\alias{plot,gss-method}
\alias{show,gss-method}
\alias{summary,gss-method}
\alias{[,gss-method}
\alias{[[,gss-method}
\alias{[<-,gss-method}
\alias{[[<-,gss-method}
\title{"gss" class methods}
\usage{
\S4method{plot}{gss}(x, y, ...)
\S4method{show}{gss}(object)
\S4method{summary}{gss}(object)
}
\arguments{
  \item{x}{gss object}
  \item{y}{Other parameter}
  \item{...}{Other parameters}
  \item{object}{gss object}
}

\description{

The package defines the following methods for the "gss" class:
\describe{
  \item{\code{plot}}{Plot the function and its minimum.}
  \item{\code{show}}{Print out the minimum value of the function corresponding to the "min" slot of the "gss" class.}
  \item{\code{summary}}{Main results from the algorithm.}
  \item{\code{[}}{ Extract part of the gss object.}
  \item{\code{[[}}{ Extract part of the gss object.}
\item{\code{[<-}}{Set value to the extract part of the gs osbject.}
\item{\code{[[<-}}{Set value to the extract part of the gss object.}
}

}

\author{
Maria Constanza Rojo Alfaro.
}
