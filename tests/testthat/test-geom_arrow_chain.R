test_that("geom_arrow_chain looks alright", {
  t <- seq(0.0, 2.0 * pi, length.out = 15L)
  l <- rep(c(1.0, 0.4), length.out = 15L)

  df <- data.frame(
    x = cos(t) * l,
    y = sin(t) * l,
    size = c(t[length(t)], t[-1L])
  )

  p <- ggplot(df, aes(x, y, size = size)) +
    geom_point(colour = 2L) +
    geom_arrow_chain(aes(linewidth = size), length_head = unit(10.0, "mm"))

  vdiffr::expect_doppelganger("geom_arrow_chain start", p)
})

test_that("the `sep` argument works as intended", {
  df <- data.frame(
    x = c(0.0, 2.0, 1.0, 2.0, 0.0, 1.0, 2.0),
    y = c(0.0, 0.0, 1.2, 0.0, 0.0, 1.2, 0.0)
  )

  p <- ggplot(df, aes(x, y)) +
    geom_arrow_chain(sep = 2.0, size = 12.0)

  vdiffr::expect_doppelganger("geom_arrow_chain sep", p)
})
