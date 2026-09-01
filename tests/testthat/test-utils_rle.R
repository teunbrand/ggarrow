test_that("rle creation works", {

  x <- new_rle(c(1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L))
  expect_equal(field(x, "length"), c(3L, 3L, 3L))
  expect_equal(field(x, "group"), c(1L, 2L, 1L))

  x <- new_rle(lengths = c(3L, 4L, 5L))
  expect_equal(field(x, "length"), c(3L, 4L, 5L))
  expect_equal(field(x, "group"), c(1L, 2L, 3L))

  x <- new_rle(alt = 5L)
  expect_equal(field(x, "length"), 5L)
  expect_equal(field(x, "group"), 1L)
})

test_that("various rle utilities work", {

  x <- new_rle(rep.int(1L:5L, 5L:1L))

  # rle length
  expect_equal(rle_length(x), 15L)

  # rle decoding
  expect_equal(rle_inv(x), rep.int(1L:5L, 5L:1L))

  # rle starts
  expect_equal(rle_start(x), c(1L, 6L, 10L, 13L, 15L))

  # rle ends
  expect_equal(rle_end(x), c(5L, 9L, 12L, 14L, 15L))

  # rle validation
  y <- x
  field(y, "group")  <- c(1L, NA, 3L, 4L, NA)
  field(y, "length") <- c(5L, 4L, 3L, 0L, 0L)
  expect_equal(rle_valid(y), c(TRUE, FALSE, TRUE, FALSE, FALSE))


  # Create index with rle
  expect_equal(
    rle_idx(x),
    list(1L:5L, 6L:9L, 10L:12L, 13L:14L, 15L)
  )

  # Rebasing rle
  y <- x
  field(y, "group") <- LETTERS[1L:5L]
  expect_equal(rle_rebase(y), x)

  # Splitting with rle
  y <- LETTERS[1L:15L]
  expect_equal(
    split_rle(y, x),
    list(
      LETTERS[1L:5L], LETTERS[6L:9L], LETTERS[10L:12L], LETTERS[13L:14L], LETTERS[15L]
    )
  )
})


test_that("rle subsetting works", {

  x <- new_rle(lengths = rep(3L, 3L))

  test <- rle_subset(x, c(2L, 4L, 6L, 7L, 8L, 9L))
  expect_equal(field(test, "length"), 1L:3L)

  test <- rle_subset(x, c(4L, 6L))
  expect_equal(field(test, "length"), c(0L, 2L, 0L))

  # Test 0-length fields are handled appropriately
  x <- new_rle(lengths = c(0L, 3L, 0L, 3L))
  test <- rle_subset(x, c(2L, 4L, 5L))
  expect_equal(field(test, "length"), c(0L, 1L, 0L, 2L))

  expect_error(
    rle_subset(x, 8L),
    class = "vctrs_error_subscript_oob"
  )
})
