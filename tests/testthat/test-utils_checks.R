test_that("validate_length works as intended", {

  ans <- validate_length(NULL, head = 5, fins = NULL, mid = 1, default = 10)
  expect_equal(ans, list(head = 5, fins = 10, mid = 1))

  ans <- validate_length(NULL, head = unit(5, "mm"), fins = NULL,
                         mid = unit(1, "inch"), default = unit(5, "pt"))
  expect_equal(
    ans,
    list(head = unit(5, "mm"), fins = unit(5, "pt"), mid = unit(1, "in"))
  )

  expect_error(
    validate_length(default = NULL),
    "must be a number"
  )
})

