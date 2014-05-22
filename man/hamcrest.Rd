\name{hamcrest}
\alias{hamcrest}
\alias{assertThat}
\alias{assertTrue}
\alias{assertFalse}
\alias{closeTo}
\alias{equalTo}
\alias{identicalTo}
\alias{instanceOf}
\alias{isTrue}
\alias{isFalse}
\title{
    Unit tests using the Hamcrest framework
}
\description{
    The Hamcrest framework for writing unit tests is based on an assertion function
    and matcher objects. A matcher object implements a single rule that is used by
    the assertion function to perform the test. This package provides a small set
    of matcher objects in the form of functions. The functionality provided by this
    set of matcher objects is identical to those implemented in Renjin, the R
    interpreter for the Java Virtual Machine. The goal of this package is therefore
    to be able to run Renjin's unit tests written in R through the GNU R
    interpreter.
}
\usage{
    assertThat(actual, matcher)
    assertTrue(value)
    assertFalse(value)
    closeTo(expected, delta)
    equalTo(expected)
    identicalTo(expected)
    instanceOf(expected)
    isTrue()
    isFalse()
}
\arguments{
    \item{actual}{object to be matched}
    \item{matcher}{one of the matcher functions, see \emph{Details}}
    \item{value}{a vector}
    \item{expected}{object passed to the matcher function. For \code{closeTo()}
this argument must be a numeric vector.}
    \item{delta}{a numeric vector of length one that defines the maximum allowed difference}
}
\details{
Use the \code{assertThat()} function to write your unit test: the first
argument is the result you want to test and the second argument is the matcher
that embodies the rule against which you want to test the result.

\itemize{
    \item \code{assertThat(actual, equalTo(expected))} checks that \code{actual} is
    equal to (using the \code{==} comparison operator) \code{expected}. The
    \code{equalTo()} matcher function can therefore also be used to compare strings.
    \item \code{assertThat(actual, closeTo(expected, delta))} checks that
    \code{actual} is \emph{close to} \code{expected} with a maximum allowed
    difference of \code{delta}. The \code{closeTo()} matcher function can only be
    used for numeric arguments.
    \item \code{assertThat(actual, identicalTo(expected))} checks if \code{actual}
    is \emph{identical} to (using the \code{\link{identical}} function)
    \code{expected}.  \item \code{assertThat(actual, isTrue())} and its shorthand
    \code{assertTrue(value)} check that \code{actual} or \code{value} is identical
    to \code{TRUE} (i.e. the logical vector or length 1). This is the same as
    \code{assertThat(actual, identicalTo(TRUE))}. Use \code{assertThat(actual,
    isFalse())} or \code{assertFalse(value)} to check if \code{actual} or
    \code{value} are \code{FALSE}.
    \item \code{assertThat(actual, instanceOf(expected))} checks if \code{actual}
    has class (using the \code{\link{inherits}} function) \code{expected}.
}
}

% vim: tw=80 ft=rhelp

