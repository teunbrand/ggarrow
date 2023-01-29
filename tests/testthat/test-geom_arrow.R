test_that("geom_arrow looks alright", {
  p <- ggplot(whirlpool(), aes(x, y, colour = group, linewidth = arc)) +
    geom_arrow(length_head = unit(10, "mm"))

  vdiffr::expect_doppelganger("geom_arrow whirlpool", p)
})
