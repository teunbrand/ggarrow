test_that("geom_arrow looks alright", {
  p <- ggplot(whirlpool(), aes(x, y, colour = group, linewidth = arc)) +
    geom_arrow(length_head = unit(10, "mm"))

  vdiffr::expect_doppelganger("geom_arrow whirlpool", p)
})

test_that("geom_arrow can mix linetypes", {

  # Linewidth is allowed to vary if the linetype is solid
  p <- ggplot(whirlpool(), aes(x, y, colour = group)) +
    geom_arrow(
      aes(linewidth = ifelse(group == "1", arc, 0.5), linetype = group)
    )

  vdiffr::expect_doppelganger("geom_arrow linetypes", p)

  # It is not allowed to vary among non-solid linetypes
  p <- ggplot(whirlpool(), aes(x, y, colour = group)) +
    geom_arrow(
      aes(linewidth = arc, linetype = group)
    )
  expect_error(ggplotGrob(p), "with varying widths")


})
