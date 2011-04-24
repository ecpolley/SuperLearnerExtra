\name{write.template}
\alias{write.method.template}
\alias{write.screen.template}
\alias{write.SL.template}

\title{
Create a new file with the SuperLearner template wrapper text
}
\description{
Create a new file with the SuperLearner template wrapper text
}
\usage{
write.method.template(file = "", ...)
write.screen.template(file = '', ...)
write.SL.template(file = '', ...)
}
\arguments{
  \item{file}{
A connection, passed to \code{\link{cat}}
}
  \item{\dots}{
Additional arguments passed to \code{\link{cat}}
}
}

\value{
None
}


\examples{
write.SL.template(file = '')
}
\keyword{utilities}
