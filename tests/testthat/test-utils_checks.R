test_that("validate_length works as intended", {
  ans <- validate_length(
    NULL,
    head = 5.0,
    fins = NULL,
    mid = 1.0,
    default = 10.0
  )
  expect_identical(ans, list(head = 5.0, fins = 10.0, mid = 1.0))

  ans <- validate_length(
    NULL,
    head = unit(5.0, "mm"),
    fins = NULL,
    mid = unit(1.0, "inch"),
    default = unit(5.0, "pt")
  )
  expect_identical(
    ans,
    list(head = unit(5.0, "mm"), fins = unit(5.0, "pt"), mid = unit(1.0, "in"))
  )

  expect_error(
    validate_length(default = NULL),
    "must be a number"
  )
})
