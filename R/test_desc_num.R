context("describe")

test_that(
  "Describe numerical vector", 
  {
    set.seed(123456)
    x <- c(rnorm(1000, mean = 10, sd = 3), rnorm(1000, mean = 20, sd = 5), rep(NA, 100))
    
    # default arguments
    res <- audit_vector_numeric(x, plot = FALSE)
    expected_res_summary <- data.frame(
      Indicator = c("Length", "Number of NAs", "Min", "Mean", "Max", "Range", "Inter-quartile range", "Standard deviation"),
      Value = c(2100, 100, -9.718934, 17.582508, 40.325843, 50.044776, 6.998134, 5.779367),
      stringsAsFactors = FALSE
    )
    expect_equal(res$summary_stat, expected_res_summary)
    
    expected_res_extreme_values <- data.frame(
      value = c("-9.71893375536238, -1.94953930158385, -1.1606790570488, -0.722125379963856, 0.952276124138905",
                "40.3258427370646, 38.942395108156, 38.4101402360336, 38.1943433014993, 37.7649242081302")
    )
    rownames(expected_res_extreme_values) <- c("Lowest values", "Highest values")
    expect_equal(res$extreme_values, expected_res_extreme_values)
    
    expected_res_percentiles <- matrix(
      c("Min", "-1.29752558322947", "0.05", "6.10139596709213", 
        "0.1", "7.48694554707612", "0.25", "9.99822527167663", "Median", 
        "13.6130149226302", "0.75", "19.904298843555", "0.9", "24.3727277975508", 
        "0.95", "26.2736963096208", "Max", "37.9768812507468"),
      nrow = 2, byrow = FALSE
    )
    rownames(expected_res_percentiles) <- c("Level", "Value")
    expect_equal(res$percentiles, expected_res_percentiles)
    
    vdiffr::expect_doppelganger(
      title = "basic num",
      fig = res$graph
    )
  }
)



