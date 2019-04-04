# --------------------------------------
# ASSERTION FUNCTION
# --------------------------------------

#' @export
assertThat <- function(actual, matcher) {

	call <- match.call()

	if(!matcher(actual)) {
		stop(sprintf("\nassertThat(%s, %s) failed\nGot: %s",
				deparse0(call$actual), deparse0(call$matcher), deparse0(actual)))
	}
}

#' @export
assertTrue <- function(value) {

	call <- match.call()

	if(!identical(value, TRUE)) {
		stop(sprintf("\nassertTrue(%s) failed\nGot: %s",
				deparse0(call$value), deparse0(value)))
	}
}

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

#' @export
closeTo <- function(expected, delta) {
    stopifnot(is.numeric(expected) & is.numeric(delta) & length(delta) == 1L)
	function(actual) {
		length(expected) == length(actual) &&
				all(abs(expected-actual)<delta)
	}
}

#' @export
identicalTo <- function(expected, tol = NULL) {
	tolMissing <- missing(tol)
	function(actual) {
	    identical.rec(actual, expected, tol)
	}
}

#' @export
deparsesTo <- function(expected) {
    function(actual) {
        identical(paste(deparse(actual), collapse=""), expected)
    }
}

#' @export
equalTo <- function(expected) {
	function(actual) {
		length(actual) == length(expected) &&
				all(actual == expected)
	}
}

#' @export
instanceOf <- function(expected) {
    function(actual) {
        inherits(actual, expected)
    }
}

#' @export
isTrue <- function() {
    function(actual) {
        identical(TRUE, actual)
    }
}

#' @export
isFalse <- function() {
    function(actual) {
        identical(FALSE, actual)
    }
}

#' @export
throwsError <- function() {
	function(actual) {
		result <- tryCatch( force(actual), error = function(e) e )
		return(inherits(result, "error"))
	}
}

#' @export
emitsWarning <- function() {
	function(actual) {
		result <- tryCatch( force(actual), warning = function(e) e )
		return(inherits(result, "warning"))
	}
}

#' @export
not <- function(matcher) {
	function(actual) {
		return(!matcher(actual))
	}
}
