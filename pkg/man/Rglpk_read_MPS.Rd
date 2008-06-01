\name{Rglpk_read_file}
\alias{Rglpk_read_file}
\alias{print.MP_data_from_file}
\title{Interface to GLPK's file reader}
\description{
  High level R interface to the MPS and CPLEX reader of the GNU Linear
  Programming Kit (GLPK).
}
\usage{
## MPS file reader
Rglpk_read_file(file, type = c("MPS_fixed", "MPS_free", "CPLEX_LP"),
verbose = FALSE) 

## print method
\method{print}{MP_data_from_file}(x, \ldots)
}
\arguments{
  \item{file}{a character specifying the relative or absolute path to
    the model file.}
  \item{type}{a character specifying the file type. This can be either
    \code{"MPS_fixed"}, \code{"MPS_free"} and \code{"CPLEX_LP"}.}
  \item{verbose}{a logical for turning on/off additional solver output:
    Default: \code{FALSE}.}
  \item{x}{an object of class \code{"MP_data_from_file"}.}
  \item{\ldots}{further arguments passed on to the print method.}
}
\details{\code{Rglpk_read_file()} takes the path to file as an
  argument and calls GLPK's file reader. The description of the linear or
  mixed integer linear program is return as an object of class
  \code{"MP_data_from_file"}.
}
\value{
  \code{Rglpk_read_file()} returns the specification of a mixed integer
  linear program defined in \code{file} as an object of class
  \code{"MP_data_from_file"}.
}
%%\references{}
\author{Stefan Theussl}
%%\seealso{}
\examples{
## not run:
## dat <- Rglpk_read_file(some_file.MPS)
## dat
}
\keyword{IO}
\keyword{utilities}
