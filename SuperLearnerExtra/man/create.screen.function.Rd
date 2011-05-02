\name{create.screen.function}
\alias{create.screen.corP}
\alias{create.screen.corRank}

\title{
Create additional screen wrappers
}
\description{
These functions make it easy to create additional screen wrappers with different values for the function specific tuning parameters.
}
\usage{
create.screen.corP(minPvalue = c(0.05, 0.025, 0.01))
create.screen.corRank(rank = c(20, 30 , 40))
}

\arguments{
  \item{minPvalue}{
p-value parameter for \code{screen.corP}
}
  \item{rank}{
rank cut-off for \code{screen.corRank}
  }
}

\value{
generates functions in the global environment
}

\author{ Eric C Polley \email{eric.polley@nih.gov} }

\examples{
create.screen.corP()
create.screen.corRank()
}

\keyword{utilities}

