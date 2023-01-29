test_that("geom_arrow_segments looks alright", {

  df <- data.frame(
    x = c(0.1, 0.2, -0.1, 0.5, -0.3, 11),
    y = c(0.2, -0.1, 0.5, 0, -0.2, 0.4),
    xend = c(10, 12, 8, 14, 7, 0),
    yend = c(12, 9, 10, 13, 8, 12)
  )

  p <- ggplot(df, aes(x, y, xend = xend, yend = yend)) +
    geom_arrow_segment(
      linewidth_head = 5, linewidth_fins = 1,
      arrow_mid = arrow_head_wings(),
      colour = alpha("dodgerblue", 0.3), stroke_colour = "black"
    )

  vdiffr::expect_doppelganger("geom_arrow_segment quiver", p)

})
