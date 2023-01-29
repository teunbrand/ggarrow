test_that("annotate_arrow works", {
  # Specifically, need to ensure that given 1/3 length arguments is sufficient
  p <- ggplot() +
    annotate_arrow(
      x = c(0, 1), y = c(0, 1),
      arrow_fins = arrow_fins_line(),
      length_head = unit(5, "mm")
    )

  vdiffr::expect_doppelganger("annotate_arrow", p)
})
