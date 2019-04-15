
### Basic unit tests written by using hamcrest for hamcrest ###

library(hamcrest)

# assertTrue --------------------------------------------------------------
test.assertTrue <- function() {
  assertTrue(is.numeric(2019))
  assertTrue(anyNA(c(1, NA, 3)))
  assertTrue(exists("airmiles", envir = getNamespace("datasets")))
  assertTrue(".GlobalEnv" %in% search())
}

# assertFalse -------------------------------------------------------------
test.assertFalse <- function() {
  assertFalse(is.character(1L))
  assertFalse(identical(-Inf,  Inf))
  assertFalse(anyNA(list(a = c(1, NA, 3), b = "a")))
  assertFalse(anyNA(as.POSIXlt(Sys.time())))
}

# closeTo -----------------------------------------------------------------
test.assertCloseTo <- function() {
  assertThat(-0.50557992900139, closeTo(-0.50557, delta = 1e4))
  assertThat(dnorm(0, 0, 1), closeTo(0.3989423, delta = 1e4))
}

# identicalTo -------------------------------------------------------------
test.assertIdenticalTo <- function() {
  assertThat(floor(-1.5), identicalTo(-2))
  assertThat(length(NULL), identicalTo(0))
  L <- list(0, 1, 2)
  assertThat(deparse(L), identicalTo("list(0, 1, 2)"))
}

# deparsesTo --------------------------------------------------------------
test.assertDeparsesTo <- function() {
  assertThat(unlist(quote(sin(3.14)), recursive = FALSE), deparsesTo("sin(3.14)"))
}

# equalTo -----------------------------------------------------------------
test.assertEqualTo <- function() {
  assertThat(qnorm(0, 0, 1,  TRUE,  FALSE), equalTo(-Inf))
}

# instanceOf --------------------------------------------------------------
test.assertInstanceOf <- function() {
  df <- data.frame(x = c(1, 1, 1, 2, 2, 3, 3),
                   y = c(1, 2, 3, 4, 5, 6, 7))
  res <- by(df$y, df$x, sum)
  assertThat(res, instanceOf("by"))
}

# isTrue ------------------------------------------------------------------
test.assertIsTrue <- function() {
  assertThat(is.integer(1L), isTrue())
}

# isFalse -----------------------------------------------------------------
test.assertIsFalse <- function() {
  assertThat(is.character(seq(10)), isFalse())
}

# throwsError -------------------------------------------------------------
test.assertThrowsError <- function() {
  assertThat(log("a"), throwsError())
  assertThat(exp(NULL), throwsError())
}

# emitsWarning ------------------------------------------------------------
test.assertEmitsWarning <- function() {
  assertThat(any(range(2.0,3.0)), emitsWarning())
}

# not ---------------------------------------------------------------------
test.assertNotMatch <- function() {
  assertThat(1, not(identicalTo(2)))
}

### RUN ASSERTION TEST FUNCTIONS ###
test.assertTrue()
test.assertFalse()
test.assertCloseTo()
test.assertIdenticalTo()
test.assertDeparsesTo()
test.assertEqualTo()
test.assertInstanceOf()
test.assertIsTrue()
test.assertIsFalse()
test.assertThrowsError()
test.assertEmitsWarning()
test.assertNotMatch()

