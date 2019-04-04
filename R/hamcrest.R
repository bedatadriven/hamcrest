
# --------------------------------------
# ASSERTION FUNCTION
# --------------------------------------

#' Make a test assertion
#'
#' Use the \code{assertThat()} function to write your unit test: the first
#' argument is the result you want to test and the second argument is the
#' matcher that embodies the rule against which you want to test the result.
#'
#' @param actual object to be matched
#' @param matcher one of the matcher functions, see Details
#'
#' @details
#' \itemize{
#' \item \code{assertThat(actual, equalTo(expected))} checks that \code{actual}
#' is equal to (using the \code{==} comparison operator) \code{expected}. The
#' \code{equalTo()} matcher function can therefore also be used to compare
#' strings.
#' \item \code{assertThat(actual, closeTo(expected, delta))} checks that
#' \code{actual} is \emph{close to} \code{expected} with a maximum allowed
#' difference of \code{delta}. The \code{closeTo()} matcher function can only be
#' used for numeric arguments.
#' \item \code{assertThat(actual, identicalTo(expected))} checks if
#' \code{actual} is \emph{identical} to (using the \code{\link{identical}}
#' function) \code{expected}.
#' \item \code{assertThat(actual, isTrue())} and its shorthand
#' \code{assertTrue(value)} check that \code{actual} or \code{value} is
#' identical to \code{TRUE} (i.e. the logical vector or length 1). This is the
#' same as \code{assertThat(actual, identicalTo(TRUE))}. Use
#' \code{assertThat(actual, isFalse())} or \code{assertFalse(value)} to check if
#' \code{actual} or \code{value} are \code{FALSE}.
#' \item \code{assertThat(actual, instanceOf(expected))} checks if \code{actual}
#' has class (using the \code{\link{inherits}} function) \code{expected}.
#' }
#'
#' @export
assertThat <- function(actual, matcher) {

	call <- match.call()

	if(!matcher(actual)) {
		stop(sprintf("\nassertThat(%s, %s) failed\nGot: %s",
				deparse0(call$actual), deparse0(call$matcher), deparse0(actual)))
	}
}

#' assertTrue
#'
#' @param value a vector
#'
#' @export
assertTrue <- function(value) {

	call <- match.call()

	if(!identical(value, TRUE)) {
		stop(sprintf("\nassertTrue(%s) failed\nGot: %s",
				deparse0(call$value), deparse0(value)))
	}
}

#' assertFalse
#'
#' @param value a vector
#'
#' @export
assertFalse <- function(value) {

	call <- match.call()

	if(!identical(value, FALSE)) {
		stop(sprintf("\nassertFalse(%s) failed\nGot: %s",
				deparse0(call$value), deparse0(value)))
	}
}

# --------------------------------------
# MATCHER FUNCTIONS
# --------------------------------------

#' NOTE: this function isn't used in Renjin test (currently).
#' @noRd
compareReal <- function(actual, expected, tol) {
  rel.diff <- abs(expected - actual) / abs(expected)
  finite <- is.finite(rel.diff) & expected != 0
  finiteValuesCloseEnough <- all(rel.diff[finite] < tol)
  nonFiniteValuesIdentical <- identical(expected[!finite], actual[!finite])

  return( finiteValuesCloseEnough && nonFiniteValuesIdentical )
}

#' closeTo
#'
#' @param expected object passed to the matcher function. For \code{closeTo()}
#'   this argument must be a numeric vector.
#' @param delta a numeric vector of length one that defines the maximum allowed
#'   difference
#'
#' @export
closeTo <- function(expected, delta) {
    stopifnot(is.numeric(expected) & is.numeric(delta) & length(delta) == 1L)
	function(actual) {
		length(expected) == length(actual) &&
				all(abs(expected-actual)<delta)
	}
}

#' identicalTo
#'
#' @param expected object passed to the matcher function. For \code{closeTo()}
#'   this argument must be a numeric vector.
#' @param tol numeric tolerance.
#'
#' @export
identicalTo <- function(expected, tol = NULL) {
	tolMissing <- missing(tol)
	function(actual) {
	    identical.rec(actual, expected, tol)
	}
}

#' deparsesTo
#'
#' @param expected object passed to the matcher function. For \code{closeTo()}
#'   this argument must be a numeric vector.
#'
#' @export
deparsesTo <- function(expected) {
    function(actual) {
        identical(paste(deparse(actual), collapse=""), expected)
    }
}

#' equalTo
#'
#' @param expected object passed to the matcher function. For \code{closeTo()}
#'   this argument must be a numeric vector.
#'
#' @export
equalTo <- function(expected) {
	function(actual) {
		length(actual) == length(expected) &&
				all(actual == expected)
	}
}

#' instanceOf
#'
#' @param expected object passed to the matcher function. For \code{closeTo()}
#'   this argument must be a numeric vector.
#'
#' @export
instanceOf <- function(expected) {
    function(actual) {
        inherits(actual, expected)
    }
}

#' isTrue
#'
#' @export
isTrue <- function() {
    function(actual) {
        identical(TRUE, actual)
    }
}

#' isFalse
#'
#' @export
isFalse <- function() {
    function(actual) {
        identical(FALSE, actual)
    }
}

#' throwsError
#'
#' @export
throwsError <- function() {
	function(actual) {
		result <- tryCatch( force(actual), error = function(e) e )
		return(inherits(result, "error"))
	}
}

#' emitsWarning
#'
#' @export
emitsWarning <- function() {
	function(actual) {
		result <- tryCatch( force(actual), warning = function(e) e )
		return(inherits(result, "warning"))
	}
}

#' not
#'
#' @param matcher one of the matcher functions, see Details of
#'   \code{\link{assertThat}}.
#'
#' @export
not <- function(matcher) {
	function(actual) {
		return(!matcher(actual))
	}
}
