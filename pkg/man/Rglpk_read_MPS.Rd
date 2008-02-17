\name{Rglpk_read_MPS}
\alias{Rglpk_read_MPS}
\alias{print.MPS_data}
\title{Interface to GLPK's MPS reader}
\description{
  High level R interface to the MPS reader of the GNU Linear Programming
  Kit (GLPK).
}
\usage{
## MPS file reader
Rglpk_read_MPS(file, type = "Standard", verbose = FALSE)

## print method
\method{print}{MPS_data}(x, \ldots)
}
\arguments{
  \item{file}{a character specifying the relative or absolute path to
    the MPS file.}
  \item{type}{a character specifying the MPS file type. Currently
    only the \code{"Standard"} file format is supported.}
  \item{verbose}{a logical for turning on/off additional solver output:
    Default: \code{FALSE}.}
  \item{x}{an object of class \code{"MPS_data"}.}
  \item{\ldots}{further arguments passed on to the print method.}
}
\details{\code{Rglpk_read_MPS()} takes the path to an MPS file as an
  argument and calls GLPK's MPS reader. The description of the linear or
  mixed integer linear program is return as an object of class
  \code{"MPS_data"}.
}
\value{
  \code{Rglpk_read_MPS()} returns the specification of a mixed integer
  linear program in \code{file} as an object of class
  \code{"MPS_data"}.
}
%%\references{}
\author{Stefan Theussl}
%%\seealso{}
\examples{
## not run:
## dat <- Rglpk_read_MPS(some_file.MPS)
## dat
}
\keyword{IO}
\keyword{utilities}
