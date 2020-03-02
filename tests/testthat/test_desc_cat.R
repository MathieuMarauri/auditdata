context("describe")

test_that(
  "Describe categorical vector with or without plot", 
  {
    x <- c(rep(letters, times = 3:28), rep(NA, 5))
    
    # default arguments, nno plot
    res <- audit_vector_categorical(x, plot = FALSE)
    expected_res_summary <- data.frame(
      Indicator = c("Length", "Number of NAs", "Number of unique values", "Number of values with duplicates"),
      Value = c(408L, 5L, 26L, 26L),
      stringsAsFactors = FALSE
    )
    expect_equal(res$summary_stat, expected_res_summary)
    
    expected_res_freq <- data.table(
      value = factor(rev(letters)[1:15]),
      freq = (28:3)[1:15],
      percent = round(28:3 / 403 * 100, digits = 0)[1:15],
      cum_freq = cumsum(28:3)[1:15],
      cum_percent = cumsum(round(28:3 / 403 * 100, digits = 0))[1:15]
    )
    names(expected_res_freq) <- c("Level", "Freq", "%", "Cumulative freq", "Cumulative %")
    expect_equal(res$freq_table, expected_res_freq)
    
    vdiffr::expect_doppelganger(
      title = "basic cat",
      fig = res$graph
    )
    
    # specific arguments, no plot, long names
    x <- c(rep(paste(letters, letters, letters, letters, letters, sep = "."), times = 3:28), rep(NA, 5))
    res <- audit_vector_categorical(x, max_length = Inf, nchar = 3, plot = FALSE)
    expected_res_summary <- data.frame(
      Indicator = c("Length", "Number of NAs", "Number of unique values", "Number of values with duplicates"),
      Value = c(408L, 5L, 26L, 26L),
      stringsAsFactors = FALSE
    )
    expect_equal(res$summary_stat, expected_res_summary)
    
    expected_res_freq <- data.table(
      value = factor(paste0(
        substr(rev(paste(letters, letters, letters, letters, letters, sep = ".")), 0, 3),
        ifelse(nchar(rev(paste(letters, letters, letters, letters, letters, sep = "."))) > 3, "...", "")
      )),
      freq = 28:3,
      percent = round(28:3 / 403 * 100, digits = 0),
      cum_freq = cumsum(28:3),
      cum_percent = cumsum(round(28:3 / 403 * 100, digits = 0))
    )
    names(expected_res_freq) <- c("Level", "Freq", "%", "Cumulative freq", "Cumulative %")
    expect_equal(res$freq_table, expected_res_freq)
    
    vdiffr::expect_doppelganger(
      title = "long names cat",
      fig = res$graph
    )
  }
)



