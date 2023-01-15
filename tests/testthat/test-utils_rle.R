test_that("rle creation works", {

  x <- new_rle(c(1, 1, 1, 2, 2, 2, 1, 1, 1))
  expect_equal(field(x, "length"), c(3, 3, 3))
  expect_equal(field(x, "group"), c(1, 2, 1))

  x <- new_rle(lengths = c(3, 4, 5))
  expect_equal(field(x, "length"), c(3, 4, 5))
  expect_equal(field(x, "group"), c(1, 2, 3))

  x <- new_rle(alt = 5)
  expect_equal(field(x, "length"), 5)
  expect_equal(field(x, "group"), 1)
})

test_that("various rle utilities work", {

  x <- new_rle(rep.int(1:5, 5:1))

  # rle length
  expect_equal(rle_length(x), 15)

  # rle decoding
  expect_equal(rle_inv(x), rep.int(1:5, 5:1))

  # rle starts
  expect_equal(rle_start(x), c(1, 6, 10, 13, 15))

  # rle ends
  expect_equal(rle_end(x), c(5, 9, 12, 14, 15))

  # rle validation
  y <- x
  field(y, "group")  <- c(1, NA, 3, 4, NA)
  field(y, "length") <- c(5, 4,  3, 0, 0)
  expect_equal(rle_valid(y), c(TRUE, FALSE, TRUE, FALSE, FALSE))


  # Create index with rle
  expect_equal(
    rle_idx(x),
    list(1:5, 6:9, 10:12, 13:14, 15)
  )

  # Rebasing rle
  y <- x
  field(y, "group") <- LETTERS[1:5]
  expect_equal(rle_rebase(y), x)

  # Splitting with rle
  y <- LETTERS[1:15]
  expect_equal(
    split_rle(y, x),
    list(
      LETTERS[1:5], LETTERS[6:9], LETTERS[10:12], LETTERS[13:14], LETTERS[15]
    )
  )
})


test_that("rle subsetting works", {

  x <- new_rle(lengths = rep(3, 3))

  test <- rle_subset(x, c(2, 4, 6, 7, 8, 9))
  expect_equal(field(test, "length"), 1:3)

  test <- rle_subset(x, c(4, 6))
  expect_equal(field(test, 'length'), c(0, 2, 0))

  # Test 0-length fields are handled appropriately
  x <- new_rle(lengths = c(0, 3, 0, 3))
  test <- rle_subset(x, c(2, 4, 5))
  expect_equal(field(test, 'length'), c(0, 1, 0, 2))

  expect_error(
    rle_subset(x, 8),
    class = "vctrs_error_subscript_oob"
  )
})
