
test_that("geom_arrow_curve draws curves correctly", {

  t <- seq(0, 2 * pi, length.out = 4)[-1]

  df <- data.frame(
    x = cos(t), y = sin(t),
    xend = cos(t + 1.8), yend = sin(t + 1.8)
  )

  p <- ggplot(df, aes(x, y, xend = xend, yend = yend)) +
    coord_equal(xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1))

  vdiffr::expect_doppelganger(
    "geom_arrow_curve with curves",
    p + geom_arrow_curve(linewidth_fins = 0, linewidth_head = 3,
                         curvature = 0.66)
  )

  vdiffr::expect_doppelganger(
    "geom_arrow_curve without curves",
    p + geom_arrow_curve(linewidth_fins = 0, linewidth_head = 3, curvature = 0)
  )
})
