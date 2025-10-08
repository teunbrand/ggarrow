test_that("geom_arrow_chain looks alright", {

  t <- seq(0, 2 * pi, length.out = 15)
  l <- rep(c(1, 0.4), length.out = 15)

  df <- data.frame(
    x = cos(t) * l,
    y = sin(t) * l,
    size = c(t[length(t)], t[-1])
  )

  p <- ggplot(df, aes(x, y, size = size)) +
    geom_point(colour = 2) +
    geom_arrow_chain(aes(linewidth = size), length_head = unit(10, "mm"))

  vdiffr::expect_doppelganger("geom_arrow_chain start", p)
})

test_that("the `sep` argument works as intended", {

  df <- data.frame(
    x = c(0, 2, 1, 2, 0, 1, 2),
    y = c(0, 0, 1.2, 0, 0, 1.2, 0)
  )

  p <- ggplot(df, aes(x, y)) +
    geom_arrow_chain(sep = 2, size = 12)

  vdiffr::expect_doppelganger("geom_arrow_chain sep", p)
})
