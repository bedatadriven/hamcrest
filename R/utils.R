
deparse0 <- function(expr) {
  paste(deparse(expr), collapse = "")
}

identical.attributes <- function(actual, expected, tol = NULL) {
  # Should have the same set of names,
  # though not necessarily in the same order
  if(length(setdiff(names(expected), names(actual))) > 0) {
    return(FALSE)
  }

  # Otherwise verify that the values are identical
  for(a in names(expected)) {
    if(!identical.rec(actual[[a]], expected[[a]], tol)) {
      return(FALSE)
    }
  }
  return(TRUE)
}

identical.rec <- function(actual, expected, tol = NULL) {
  if (length(actual) != length(expected))
    return(FALSE)
  if (typeof(actual) != typeof(expected))
    return(FALSE)
  if (!identical.attributes(attributes(actual), attributes(expected), tol)) {
    return(FALSE)
  }
  if (is.list(actual)) {
    for (i in seq_along(actual)) {
      isSame <- identical.rec(actual[[i]], expected[[i]], tol)
      if (!isSame){
        return(FALSE)
      }
    }
    return(TRUE)
  } else if (!is.null(tol) && is.double(actual)) {
    compareReal(unclass(actual), unclass(expected), tol)
  } else if (!is.null(tol) && is.complex(actual)) {
    compareReal(unclass(Re(actual)), unclass(Re(expected)), tol) &&
      compareReal(unclass(Im(actual)), unclass(Im(expected)), tol)
  } else {
    return(identical(actual, expected))
  }
}
