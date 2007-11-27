\name{glp_solve}
\alias{glp_solve}

\title{Linear and Mixed Integer Programming Solver Using GLPK}
\description{
  High level R interface to the GNU Linear Programming Kit for solving
  mixed integer problems (MIP) as well as normal linear programming
  problems.
}
\usage{
glp_solve(obj, mat, dir, rhs, int,
max = FALSE, verbose = FALSE)
}
\arguments{
  \item{obj}{a vector with the objective coefficients}
  \item{mat}{a vector or a matrix of the constraint coefficients}
  \item{dir}{a character vector with the directions of the constraints
        ("<","<=", ...)} 
  \item{rhs}{the right hand side of the linear problem}
  \item{int}{a vector with logical values, "TRUE" means that the
    corresponding objective variable should be of type integer}
  \item{max}{the direction of the optimization. TRUE means that the
    objective is to maximize the linear problem, FALSE (default) means
    to minimize it}
  \item{verbose}{Turn on/off solver more output: TRUE or FALSE (default)}
}
\details{
  The GNU Linear Programming Kit is Open Source. The current version can
  be found at
  http://www.gnu.org/software/glpk/glpk.html. This package aims to
  provide a high level function to the low level C interface of the
  solver. There also exists an R interface done by Lopaka Lee which
  ports all low level C Interface routines of the GLPK API to R (R
  package "glpk"). 
}
\value{
  A list containing the optimal solution as well as the corresponding
  values of the objectives.
}
\references{
  GNU Linear Programming Kit.
  \url{http://www.gnu.org/software/glpk/glpk.html}
  GLPK Interface to R.
  \url{http://cran.wu-wien.ac.at/src/contrib/Descriptions/glpk.html}
}
\author{Stefan Theussl}
%\note{}
\seealso{
  \code{\link[lpSolve]{lp} for another linear programming solver}
}
\examples{
## simple linear program (Ex. 17 of Hanke/Nettekoven 2003: "Linear 
## Programming")

obj <- c(2,4,4)
mat <- matrix(c(3,2,1,4,1,3,2,2,2),nrow=3)
dir <- c("<=","<=","<=")
rhs <- c(60,40,80)
int <- c(FALSE,FALSE,FALSE)
max <- TRUE

glp_solve(obj, mat, dir, rhs, int, max)

## simple mixed integer program (Ex. 54 of Hanke/Nettekoven 2003: 
## "Linear Programming")

obj <- c(3,1,3)
mat <- matrix(c(-1,0,1,2,4,-3,1,-3,2),nrow=3)
dir <- c("<=","<=","<=")
rhs <- c(4,2,3)
int <- c(TRUE,FALSE,TRUE)
max <- TRUE

glp_solve(obj, mat, dir, rhs, int, max)
}
\keyword{optimize}

