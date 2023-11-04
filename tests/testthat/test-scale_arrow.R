df <- data.frame(
  x = c(0, 1, 0, 1, 0, 1),
  y = c(1, 1, 2, 2, 3, 3),
  group = c("A", "A", "B", "B", "C", "C")
)

test_that("arrow_head scales work", {
  p <- ggplot(df, aes(x, y, colour = group)) +
    geom_arrow(
      aes(arrow_head = group),
      length_head = unit(1, "cm"),
      linewidth = 3
    ) +
    scale_arrow_head_discrete(
      values = list("fins_feather", NULL, arrow_head_wings())
    )

  vdiffr::expect_doppelganger("arrow_head scale", p)
})

test_that("arrow_fins scales work", {

  p <- ggplot(df, aes(x, y, colour = group)) +
    geom_arrow(
      aes(arrow_fins = group),
      length_fins = unit(1, "cm"),
      linewidth = 3, arrow_head = NULL
    ) +
    scale_arrow_fins_discrete(
      values = list("head_wings", NULL, arrow_fins_feather())
    )

  vdiffr::expect_doppelganger("arrow_fins scale", p)
})

test_that("arrow_mid scales work", {

  p <- ggplot(df, aes(x, y, colour = group)) +
    geom_arrow(
      aes(arrow_mid = group),
      length_mid = unit(1, "cm"),
      linewidth = 3, arrow_head = NULL
    ) +
    scale_arrow_mid_discrete(
      values = list("head_wings", NULL, arrow_head_line())
    )

  vdiffr::expect_doppelganger("arrow_mid scale", p)
})

