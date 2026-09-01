test_that("geom_arrow_segments looks alright", {

  df <- data.frame(
    x = c(0.1, 0.2, -0.1, 0.5, -0.3, 11.0),
    y = c(0.2, -0.1, 0.5, 0.0, -0.2, 0.4),
    xend = c(10.0, 12.0, 8.0, 14.0, 7.0, 0.0),
    yend = c(12.0, 9.0, 10.0, 13.0, 8.0, 12.0)
  )

  p <- ggplot(df, aes(x, y, xend = xend, yend = yend)) +
    geom_arrow_segment(
      linewidth_head = 5.0, linewidth_fins = 1.0,
      arrow_mid = arrow_head_wings(),
      colour = alpha("dodgerblue", 0.3), stroke_colour = "black"
    )

  vdiffr::expect_doppelganger("geom_arrow_segment quiver", p)

})

test_that("the distort argument works", {

  mapping <- aes(x = x, y = y, xend = xend, yend = yend)

  p <- ggplot() +
    geom_arrow_segment(
      data = data.frame(x = 0.0, xend = 1.0, y = 0.0, yend = 1.2),
      aes(!!!mapping, colour = "sinewave"), distort = "sinewave"
    ) +
    geom_arrow_segment(
      data = data.frame(x = 1.0, xend = 2.0, y = 1.2, yend = 0.0),
      aes(!!!mapping, colour = "sawtooth"), distort = "sawtooth"
    ) +
    geom_arrow_segment(
      data = data.frame(x = 0.0, xend = 2.0, y = 0.0, yend = 0.0),
      aes(!!!mapping, colour = "squarewave"), distort = "squarewave"
    ) +
    theme(
      legend.key.size = unit(1.0, "cm")
    )

  vdiffr::expect_doppelganger("geom_arrow_segment distortion", p)
})
