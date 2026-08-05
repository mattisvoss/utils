test_that("matches on the key, not on row position", {
  df <- data.frame(
    item = c("cattle", "milk", "cattle", "milk"),
    year_concerned = c(2022L, 2022L, 2021L, 2021L),   # deliberately unsorted
    price = c(110, 45, 100, 40)
  )
  out <- join_lag(df, cols = "price", by = "item")

  expect_equal(out$price_lag1, c(100, 40, NA, NA))
  expect_equal(out$year_concerned, df$year_concerned)  # row order preserved
})

test_that("a gap returns NA rather than the wrong period", {
  df <- data.frame(
    item = "cattle",
    year_concerned = c(2021L, 2022L, 2024L),
    price = c(100, 110, 130)
  )
  expect_warning(
    out <- join_lag(df, cols = "price", by = "item"),
    class = "join_lag_gap"
  )
  # 2024 must NOT inherit 2022's price
  expect_true(is.na(out$price_lag1[out$year_concerned == 2024L]))
})

test_that("series edges are silent; only interior holes are reported", {
  df <- data.frame(
    item = "cattle",
    year_concerned = c(2021L, 2022L, 2023L),
    price = c(100, 110, 120)
  )
  expect_silent(out <- join_lag(df, cols = "price", by = "item"))
  expect_true(is.na(out$price_lag1[1]))  # start of series, legitimately NA
})

test_that("on_gap = 'error' hard-fails for production runs", {
  df <- data.frame(item = "cattle",
                   year_concerned = c(2021L, 2024L),
                   price = c(100, 130))
  expect_error(
    join_lag(df, cols = "price", by = "item", on_gap = "error"),
    class = "join_lag_gap"
  )
})

test_that("duplicate keys are always an error", {
  df <- data.frame(item = c("cattle", "cattle", "cattle"),
                   year_concerned = c(2021L, 2022L, 2022L),
                   price = c(100, 110, 111))
  expect_error(join_lag(df, cols = "price", by = "item"),
               class = "join_lag_duplicate_keys")
})

test_that("NA periods never match each other", {
  df <- data.frame(item = c("cattle", "cattle"),
                   year_concerned = c(NA_integer_, 2022L),
                   price = c(100, 110))
  expect_warning(out <- join_lag(df, cols = "price", by = "item"),
                 class = "join_lag_na_time")
  expect_true(all(is.na(out$price_lag1)))
})

test_that("k and suffix behave", {
  df <- data.frame(item = "cattle",
                   year_concerned = 2020:2023,
                   price = c(100, 110, 120, 130))

  expect_equal(join_lag(df, "price", "item", k = 2)$price_lag2,
               c(NA, NA, 100, 110))
  expect_equal(join_lag(df, "price", "item", k = -1)$price_lead1,
               c(110, 120, 130, NA))
  expect_true("price_prev" %in%
                names(join_lag(df, "price", "item", suffix = "_prev")))
})

test_that("ungrouped series and multi-column keys both work", {
  df <- data.frame(year_concerned = 2020:2022, price = c(1, 2, 3))
  expect_equal(join_lag(df, "price", by = character(0))$price_lag1,
               c(NA, 1, 2))

  df2 <- expand.grid(item = c("a", "b"), region = c("N", "S"),
                     year_concerned = 2021:2022,
                     stringsAsFactors = FALSE)
  df2$price <- seq_len(nrow(df2))
  out <- join_lag(df2, "price", by = c("item", "region"))
  expect_equal(sum(is.na(out$price_lag1)), 4L)  # one per item-region series
})

test_that("bad input is caught early", {
  df <- data.frame(item = "a", year_concerned = 2021L, price = 1)
  expect_error(join_lag(df, "price", "item", k = 0), class = "join_lag_bad_input")
  expect_error(join_lag(df, "nope", "item"), class = "join_lag_bad_input")
  expect_error(join_lag(df, "price", "item", k = 1.5), class = "join_lag_bad_input")
  expect_error(join_lag(df, "item", "item"), class = "join_lag_bad_input")

  df$price_lag1 <- 0
  expect_error(join_lag(df, "price", "item"), class = "join_lag_bad_input")
})
