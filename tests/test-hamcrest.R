
library(hamcrest)

testHamcrest("assertTrue", {
  assertTrue(is.numeric(2019))
  assertTrue(anyNA(c(1, NA, 3)))
  assertTrue(exists("airmiles", envir = getNamespace("datasets")))
  assertTrue(".GlobalEnv" %in% search())
})

testHamcrest("assertFalse", {
  assertFalse(is.character(1L))
  assertFalse(identical(-Inf,  Inf))
  assertFalse(anyNA(list(a = c(1, NA, 3), b = "a")))
  assertFalse(anyNA(as.POSIXlt(Sys.time())))
})

testHamcrest("assertThat closeTo", {
  assertThat(-0.50557992900139, closeTo(-0.50557, delta = 1e4))
  assertThat(dnorm(0, 0, 1), closeTo(0.3989423, delta = 1e4))
})

testHamcrest("assertThat identicalTo", {
  assertThat(floor(-1.5), identicalTo(-2))
  assertThat(length(NULL), identicalTo(0L))
  L <- list(0, 1, 2)
  assertThat(deparse(L), identicalTo("list(0, 1, 2)"))
})

testHamcrest("assertThat deparsesTo", {
  assertThat(unlist(quote(sin(3.14)), recursive = FALSE), deparsesTo("sin(3.14)"))
})

testHamcrest("assertThat equalTo", {
  assertThat(qnorm(0, 0, 1,  TRUE,  FALSE), equalTo(-Inf))
})

testHamcrest("assertThat instanceOf", {
  df <- data.frame(x = c(1, 1, 1, 2, 2, 3, 3),
                   y = c(1, 2, 3, 4, 5, 6, 7))
  res <- by(df$y, df$x, sum)
  assertThat(res, instanceOf("by"))
})

testHamcrest("assertThat isTrue", {
  assertThat(is.integer(1L), isTrue())
})

testHamcrest("assertThat isFalse", {
  assertThat(is.character(seq(10)), isFalse())
})

testHamcrest("assertThat throwsError", {
  assertThat(log("a"), throwsError())
  assertThat(exp(NULL), throwsError())
})

testHamcrest("assertThat emitsWarning", {
  assertThat(any(range(2.0, 3.0)), emitsWarning())
})

testHamcrest("assertThat not", {
  assertThat(1, not(identicalTo(2)))
})

testHamcrest("custom matchers", {

  isDate <- function() {
    function(actual) {
      inherits(actual, "Date")
    }
  }

  assertThat(Sys.Date(), isDate())
  assertThat(assertThat(c("A", "B", "C"), isDate()), throwsError())

  isEvenInteger <- function() {
    function(actual) {
      actual %% 2L == 0L
    }
  }

  assertThat(4, isEvenInteger())
  assertThat(assertThat(5, isEvenInteger()), throwsError())
  assertThat(assertThat("a", isEvenInteger()), throwsError())

})


