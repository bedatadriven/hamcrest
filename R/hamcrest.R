
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
  
  matches <- tryCatch( matcher(actual), error = function(e) {
    stop(sprintf("\nassertThat(%s, %s) failed\nError: %s",
                 deparse0(call$actual), deparse0(call$matcher), deparse0(e$message)))
  })
  
  if(!matches) {
    stop(sprintf("\nassertThat(%s, %s) failed\nGot: %s", 
                 deparse0(call$actual), deparse0(call$matcher), deparse0(actual)))
  }
}

#' assertTrue
#'
#' @param value a vector
#' @examples \dontrun{
#' assertTrue(is.numeric(2019))
#' }
#' @seealso isTrue
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
#' @examples \dontrun{
#' assertFalse(is.character(1L))
#' }
#' @seealso isFalse
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

#' closeTo
#'
#' @param expected a numeric vector. 
#' @param delta a numeric vector of length one that defines the maximum allowed
#'   difference
#' @examples \dontrun{
#' assertThat(-0.50557992900139, closeTo(-0.50557, delta = 1e4))
#' }
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
#' @param expected object passed to the matcher function.
#' @param tol numeric tolerance.
#' @examples \dontrun{
#' assertThat(floor(-1.5), identicalTo(-2))
#' }
#' @export
identicalTo <- function(expected, tol = NULL) {
	tolMissing <- missing(tol)
	function(actual) {
	    identical.rec(actual, expected, tol)
	}
}

#' deparsesTo
#'
#' @param expected object passed to the matcher function.
#' @examples \dontrun{
#' assertThat(unlist(quote(sin(3.14)), recursive = FALSE), deparsesTo("sin(3.14)"))
#' }
#' @export
deparsesTo <- function(expected) {
    function(actual) {
        identical(paste(deparse(actual), collapse=""), expected)
    }
}

#' equalTo
#'
#' @param expected object passed to the matcher function.
#' @examples \dontrun{
#' assertThat(qnorm(0, 0, 1, TRUE, FALSE), equalTo(-Inf))
#' }
#' @export
equalTo <- function(expected) {
  function(actual) {
    if (is.list(actual))
      equal.rec(actual, expected)
    else
      length(actual) == length(expected) && all(actual == expected)
  }
}

#' instanceOf
#'
#' @param expected object passed to the matcher function.
#' @examples \dontrun{
#' df <- data.frame(x = c(1, 1, 1, 2, 2, 3, 3),
#' y = c(1, 2, 3, 4, 5, 6, 7))
#' res <- by(df$y, df$x, sum)
#' assertThat(res, instanceOf("by"))
#' 
#' class(df) <- "result"
#' assertThat(df, instanceOf("result"))
#' }
#' @export
instanceOf <- function(expected) {
    function(actual) {
        inherits(actual, expected)
    }
}

#' isTrue
#'
#' @examples \dontrun{
#' assertThat(is.integer(1L), isTrue())
#' }
#' @seealso assertTrue
#' @export
isTrue <- function() {
    function(actual) {
        identical(TRUE, actual)
    }
}

#' isFalse
#'
#' @examples \dontrun{
#' assertThat(is.character(seq(10)), isFalse())
#' }
#' @seealso assertFalse
#' @export
isFalse <- function() {
    function(actual) {
        identical(FALSE, actual)
    }
}

#' throwsError
#' 
#' @examples \dontrun{
#' assertThat(log("a"), throwsError())
#' }
#' @export
throwsError <- function() {
	function(actual) {
		result <- tryCatch( force(actual), error = function(e) e )
		return(inherits(result, "error"))
	}
}

#' emitsWarning
#'
#' @examples \dontrun{
#' assertThat(any(range(2.0,3.0)), emitsWarning())
#' }
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
#' @examples \dontrun{
#' assertThat(1, not(identicalTo(2)))
#' }
#' @export
not <- function(matcher) {
	function(actual) {
		return(!matcher(actual))
	}
}
