test_that("every year in a series gets the same base price", {
  df <- data.frame(
    item = c("cattle", "cattle", "cattle", "milk", "milk", "milk"),
    year_concerned = rep(2020:2022, times = 2),
    price = c(100, 110, 130, 40, 45, 44),
    stringsAsFactors = FALSE
  )
  out <- join_at(df, cols = "price", by = "item", at = 2020)

  expect_equal(out$price_base2020, c(100, 100, 100, 40, 40, 40))
})

test_that("row order is irrelevant and preserved", {
  df <- data.frame(
    item = c("milk", "cattle", "cattle", "milk"),
    year_concerned = c(2021L, 2021L, 2020L, 2020L),
    price = c(45, 110, 100, 40),
    stringsAsFactors = FALSE
  )
  out <- join_at(df, cols = "price", by = "item", at = 2020)

  expect_equal(out$price_base2020, c(40, 100, 100, 40))
  expect_equal(out$year_concerned, df$year_concerned)
})

test_that("the base year rows match themselves", {
  df <- data.frame(
    item = c("cattle", "cattle"),
    year_concerned = c(2020L, 2021L),
    price = c(100, 110),
    stringsAsFactors = FALSE
  )
  out <- join_at(df, cols = "price", by = "item", at = 2020)

  base_rows <- out$year_concerned == 2020L
  expect_equal(out$price_base2020[base_rows], out$price[base_rows])
})

test_that("a product absent from the base year warns and returns NA", {
  # Sugar beet ceased in 2005; against a 2020 base it has no base price.
  df <- data.frame(
    item = c("cattle", "cattle", "sugar_beet"),
    year_concerned = c(2020L, 2021L, 2004L),
    price = c(100, 110, 30),
    stringsAsFactors = FALSE
  )
  expect_warning(
    out <- join_at(df, cols = "price", by = "item", at = 2020),
    class = "join_at_missing_base"
  )
  expect_true(is.na(out$price_base2020[out$item == "sugar_beet"]))
  expect_false(anyNA(out$price_base2020[out$item == "cattle"]))
})

test_that("on_missing controls severity", {
  df <- data.frame(
    item = c("cattle", "sugar_beet"),
    year_concerned = c(2020L, 2004L),
    price = c(100, 30),
    stringsAsFactors = FALSE
  )
  expect_error(join_at(df, "price", "item", at = 2020, on_missing = "error"),
               class = "join_at_missing_base")
  expect_silent(join_at(df, "price", "item", at = 2020, on_missing = "ignore"))
})

test_that("a base year with no rows at all is an error, not a silent NA column", {
  df <- data.frame(item = "cattle", year_concerned = 2021L, price = 110,
                   stringsAsFactors = FALSE)
  expect_error(join_at(df, "price", "item", at = 1999),
               class = "join_at_missing_base")
})

test_that("duplicate keys are an error", {
  df <- data.frame(
    item = c("cattle", "cattle"),
    year_concerned = c(2020L, 2020L),
    price = c(100, 101),
    stringsAsFactors = FALSE
  )
  expect_error(join_at(df, "price", "item", at = 2020),
               class = "join_at_duplicate_keys")
})

test_that("many-to-one holds: one base row feeds many current rows", {
  df <- data.frame(
    item = "cattle",
    year_concerned = 2015:2024,
    price = seq(100, 145, by = 5),
    stringsAsFactors = FALSE
  )
  out <- join_at(df, "price", "item", at = 2020)

  expect_equal(nrow(out), nrow(df))          # no fan-out
  expect_equal(length(unique(out$price_base2020)), 1L)
})

test_that("ungrouped series, multi-column keys, and suffix all work", {
  df <- data.frame(year_concerned = 2019:2021, price = c(10, 11, 12))
  expect_equal(join_at(df, "price", by = character(0), at = 2020)$price_base2020,
               c(11, 11, 11))

  df2 <- expand.grid(item = c("a", "b"), region = c("N", "S"),
                     year_concerned = 2020:2021, stringsAsFactors = FALSE)
  df2$price <- seq_len(nrow(df2))
  out <- join_at(df2, "price", by = c("item", "region"), at = 2020)
  expect_equal(nrow(out), nrow(df2))
  expect_false(anyNA(out$price_base2020))

  expect_true("price_k" %in%
                names(join_at(df, "price", character(0), at = 2020,
                              suffix = "_k")))
})

test_that("bad input is caught early", {
  df <- data.frame(item = "a", year_concerned = 2020L, price = 1,
                   stringsAsFactors = FALSE)
  expect_error(join_at(df, "nope", "item", at = 2020), class = "join_at_bad_input")
  expect_error(join_at(df, "price", "item", at = 2020.5), class = "join_at_bad_input")
  expect_error(join_at(df, "item", "item", at = 2020), class = "join_at_bad_input")

  df$price_base2020 <- 0
  expect_error(join_at(df, "price", "item", at = 2020), class = "join_at_bad_input")
})
