# hamcrest

[![CRAN status badge](https://www.r-pkg.org/badges/version/hamcrest)](https://cran.r-project.org/package=hamcrest) [![Dependencies](https://tinyverse.netlify.com/badge/hamcrest)](https://cran.r-project.org/package=hamcrest)

GNU R package implementing the Hamcrest framework for writing unit tests.

## Installation

<!--You can install the released version of *hamcrest* from [CRAN](https://cran.r-project.org/package=hamcrest) with:

```r
install.packages("hamcrest")
```
-->
You can install the development version:
```r
# install.packages("devtools")
devtools::install_github("bedatadriven/hamcrest")
```

From source:
```r
git clone https://github.com/bedatadriven/hamcrest.git
R CMD INSTALL hamcrest
```

## Usage

Read the
[**vignette**](https://cran.r-project.org/web/packages/hamcrest/vignettes/writing-unit-tests-with-hamcrest.html)
for more information. For more information about how **Renjin** uses *hamcrest*
to write unit tests, please see
[**this**](http://docs.renjin.org/en/latest/writing-renjin-extensions.html#using-the-hamcrest-package-to-write-unit-tests)
documentation post.

## See also

*hamcrest* package is extensively used in **Renjin**, a JVM-based interpreter
for the R language. See www.renjin.org for more details.

## License

The source code in this package is released under the Apache License version
2.0. See the LICENSE file for the full license.
