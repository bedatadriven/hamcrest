% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hamcrest.R
\name{not}
\alias{not}
\title{Negates the result of a matcher function}
\usage{
not(matcher)
}
\arguments{
\item{matcher}{one of the matcher functions, see Details of
\code{\link{assertThat}}.}
}
\description{
Negates the result of a matcher function
}
\examples{
\dontrun{
assertThat(1, not(identicalTo(2)))
}
}
