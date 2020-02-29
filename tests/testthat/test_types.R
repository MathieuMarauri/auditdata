test_that(
  "Numeric, categorical and date type checks are correct",
  {
    x <- rnorm(1000)
    expect_equal(which_type(x), "numeric")
    
    x <- c(rep(10, 3), rep(4.7, 7))
    expect_equal(which_type(x), "categorical")
    
    x <- factor(rep(letters, 3))
    expect_equal(which_type(x), "categorical")
    
    x <- sample(c(TRUE, FALSE), replace = TRUE, size = 100)
    expect_equal(which_type(x), "categorical")
    
    x <- letters
    expect_equal(which_type(x), "categorical")
    
    x <- seq(as.Date("1910/1/1"), as.Date("1999/1/1"), "years")
    expect_equal(which_type(x), "date")
    
    x <- seq(from = Sys.time(), length.out = 100, by = "mins")
    expect_equal(which_type(x), "date")
  }
)