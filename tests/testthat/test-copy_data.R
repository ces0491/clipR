testthat::test_that("copy_data works as expected", {

  # test df
  test_df <- data.frame(date = seq(as.Date("2020-04-01"), by = "day", length.out = 10),
                        value = c(1:10))

  copy_data(test_df)
  clip_data_df <- read.delim("clipboard")
  clip_data_df$date <- as.Date(clip_data_df$date)

  # test zoo/xts
  mx <- matrix(test_df$value, nrow = 5, ncol = 2)
  colnames(mx) <- LETTERS[1:2]
  test_zoo <- zoo::zoo(x = mx, order.by = test_df$date[1:5])

  copy_data(test_zoo)
  clip_data_zoo <- read.delim("clipboard")
  clip_data_zoo <- zoo::zoo(clip_data_zoo[-1], order.by = as.Date(clip_data_zoo$date))

  # test list
  test_list <- as.list(test_df)

  copy_data(test_list)
  clip_data_list <- read.delim("clipboard")
  clip_data_list <- list(date = as.Date(clip_data_list$date), value = clip_data_list$value)

  # test vector
  test_vec <- test_df$value

  copy_data(test_vec)
  clip_data_vec <- read.delim("clipboard")
  clip_data_vec <- clip_data_vec[,1]


  testthat::expect_equal(test_df, clip_data_df)
  testthat::expect_equal(test_zoo, clip_data_zoo)
  testthat::expect_equal(test_list, clip_data_list)
  testthat::expect_equal(test_vec, clip_data_vec)

})
