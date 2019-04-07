
library(hamcrest)

### ------------------------------------------------------------ ###
### Basic unit tests written by using hamcrest for hamcrest ---- ###
### ------------------------------------------------------------ ###

### assertTrue
assertTrue(is.numeric(2019))

### assertFalse
assertFalse(is.character(1L))

### closeTo
assertThat(-0.50557992900139, closeTo(-0.50557, delta = 1e4))

### identicalTo
assertThat(floor(-1.5), identicalTo(-2))

### deparsesTo
assertThat(unlist(quote(sin(3.14)), recursive = FALSE), deparsesTo("sin(3.14)"))

### equalTo
assertThat(qnorm(0, 0, 1,  TRUE,  FALSE), equalTo(-Inf))

### instanceOf
df <- data.frame(x = c(1, 1, 1, 2, 2, 3, 3),
                 y = c(1, 2, 3, 4, 5, 6, 7))
res <- by(df$y, df$x, sum)
assertThat(res, instanceOf("by"))

### isTrue
assertThat(is.integer(1L), isTrue())

### isFalse
assertThat(is.character(seq(10)), isFalse())

### throwsError
assertThat(log("a"), throwsError())

### emitsWarning
assertThat(any(range(2.0,3.0)), emitsWarning())

### not
assertThat(1, not(identicalTo(2)))