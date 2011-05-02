\name{create.SL.function}
\alias{create.SL.gam}
\alias{create.SL.glmnet}
\alias{create.SL.knn}
\alias{create.SL.nnet}

\title{
Create additional SL wrappers
}
\description{
These functions make it easy to create additional SL wrappers with different values for the function specific tuning parameters.
}
\usage{
create.SL.gam(deg.gam = c(3, 4))
create.SL.glmnet(alpha = c(.25, .50, .75))
create.SL.knn(k = c(20, 30, 40, 50))
create.SL.nnet(size = c(3, 4))
}

\arguments{
  \item{deg.gam}{
Degree parameter for \code{gam}
}
  \item{alpha}{
Mixing parameter for \code{glmnet}
  }
  \item{k}{
Number of nearest neighbors in \code{knn}
  }
  \item{size}{
Size parameter for \code{nnet}
  }
}

\value{
generates functions in the global environment
}

\author{ Eric C Polley \email{eric.polley@nih.gov} }

\examples{
create.SL.gam()
create.SL.glmnet()
create.SL.knn()
create.SL.nnet()
}

\keyword{utilities}

