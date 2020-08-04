testthat::test_that("copy_data.zoo mantains the correct file names", {

  df <- data.frame(date = seq(as.Date("2020-04-01"), by = "day", length.out = 10),
                        value = c(1:10))

  copy_data(df)

  test_df <- paste_df()
  test_df$date <- as.Date(test_df$date)

  testthat::expect_equal(test_df, df)
})
