library("data.table") # dataset manipulation

test_that(
  "Frequency table is built with ordering of levels possible", 
  {
    x <- rep(letters, times = 3:28)
    res <- freq_table(x)
    expected_res <- data.table(
      value = rev(letters),
      freq = 28:3,
      percent = round(28:3 / 403 * 100, digits = 0)
    )
    expect_equal(res, expected_res)
    
    res <- freq_table(x, max_length = 10)
    expected_res <- data.table(
      value = rev(letters)[1:10],
      freq = (28:3)[1:10],
      percent = round(28:3 / 403 * 100, digits = 0)[1:10]
    )
    expect_equal(res, expected_res)
    
    res <- freq_table(x, cum = TRUE)
    expected_res <- data.table(
      value = rev(letters),
      freq = 28:3,
      percent = round(28:3 / 403 * 100, digits = 0),
      cum_freq = cumsum(28:3),
      cum_percent = cumsum(round(28:3 / 403 * 100, digits = 0))
    )
    expect_equal(res, expected_res)
  }
)