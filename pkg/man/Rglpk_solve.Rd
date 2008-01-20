\name{Rglpk_solve_LP}
\alias{Rglpk_solve_LP}

\title{Linear and Mixed Integer Programming Solver Using GLPK}
\description{
  High level R interface to the GNU Linear Programming Kit for solving
  linear as well as mixed integer linear programming problems (MILP).
}
\usage{
Rglpk_solve_LP(obj, mat, dir, rhs, int, max = FALSE,
               bounds = NULL, verbose = FALSE)
}
\arguments{
  \item{obj}{a vector with the objective coefficients}
  \item{mat}{a vector or a matrix of the constraint coefficients}
  \item{dir}{a character vector with the directions of the constraints
        (\code{"<","<=", ...})} 
  \item{rhs}{the right hand side of the linear problem}
  \item{int}{a vector with indices representing which objective
    coefficients should be of type integer}
  \item{max}{the direction of the optimization. \code{TRUE} means that the
    objective is to maximize the linear problem, \code{FALSE} (default) means
    to minimize it}
  \item{bounds}{a list containing an element \sQuote{upper} as well as an
  element \sQuote{lower} containing the indices and corresponding bounds of the
  objective variable. The default for each variable is a bound between 0
  and \code{Inf} (\code{NULL} or not listed in \sQuote{bounds}).}
  \item{verbose}{Turn on/off additional solver output: \code{TRUE} or
    \code{FALSE} (default)}
}
\details{
  The GNU Linear Programming Kit is open source. The current version can
  be found at
  \url{http://www.gnu.org/software/glpk/glpk.html}. This package aims to
  provide a high level function to the low level C interface of the
  solver. There also exists an R interface done by Lopaka Lee which
  ports all low level C Interface routines of the GLPK API to R (R
  package \pkg{glpk}). 
}
\value{
  A list containing the optimal solution,  the corresponding
  values of the objectives and the status information of the solution (0
  -> optimal solution, 1 -> not feasible, undefined or unbounded solution).
}
\references{
  \item{GNU Linear Programming Kit}{
  \url{http://www.gnu.org/software/glpk/glpk.html}}
  \item{GLPK Interface to R}{
  \url{http://cran.wu-wien.ac.at/src/contrib/Descriptions/glpk.html}}
}
\author{Kurt Hornik and Stefan Theussl}
%\note{}
\seealso{
  \item{\code{\link[lpSolve]{lp}}}{in package \pkg{lpSolve}}
  \item{\code{\link[Rsymphony]{Rsymphony_solve_LP}}}{in package \pkg{Rsymphony}}
}
\examples{
## simple linear program (Ex. 17 of Hanke/Nettekoven 2003: "Linear 
## Programming")

obj <- c(2,4,3)
mat <- matrix(c(3,2,1,4,1,3,2,2,2),nrow=3)
dir <- c("<=","<=","<=")
rhs <- c(60,40,80)
max <- TRUE

Rglpk_solve_LP(obj, mat, dir, rhs, max=max)

## simple mixed integer program (Ex. 54 of Hanke/Nettekoven 2003: 
## "Linear Programming")

obj <- c(3,1,3)
mat <- matrix(c(-1,0,1,2,4,-3,1,-3,2),nrow=3)
dir <- c("<=","<=","<=")
rhs <- c(4,2,3)
int <- c(1,3)
max <- TRUE

Rglpk_solve_LP(obj, mat, dir, rhs, int, max)
}
\keyword{optimize}

