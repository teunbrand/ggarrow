test_that("continuous arrow scales throw correct errors", {

  f <- "text"
  expect_error(
    scale_arrow_head_continuous(generator = f),
    "must be a"
  )
  f <- function() "text"
  expect_error(
    scale_arrow_fins_continuous(generator = f),
    "must have arguments"
  )
  f <- function(a, b) a + b
  expect_error(
    scale_arrow_mid_continuous(generator = f, map_arg = "x"),
    "must be an argument"
  )
  expect_warning(
    scale_arrow_head_continuous(
      generator = f, map_arg = "a", other_args = list(foo = "bar")
    ),
    "has unknown arguments"
  )
  expect_error(
    scale_arrow_head_continuous(range = c(-Inf, NA)),
    "must be a finite numeric vector"
  )
})

test_that("arrow_pal works as intended", {
  # Can find functions in ggarrow
  expect_equal(
    arrow_pal("head_wings"),
    list(arrow_head_wings())
  )

  # Cannot find non existing functions
  expect_error(
    arrow_pal("foobar"),
    "Cannot find function"
  )

  on.exit(env_unbind(global_env(), "arrow_foobar"), add = TRUE)

  # Can find functions in global environment
  env_bind(global_env(), arrow_foobar = function() matrix(1:4, ncol = 2))
  expect_equal(
    arrow_pal("foobar"),
    list(matrix(1:4, ncol = 2))
  )

  env_bind(global_env(), arrow_foobar = function() array(1:24, 2:4))
  expect_error(
    arrow_pal("foobar"),
    "not a matrix"
  )

  env_bind(global_env(), arrow_foobar = function() matrix(1:2, ncol = 1))
  expect_error(
    arrow_pal("foobar"),
    "does not have dimension"
  )

  env_bind(global_env(), arrow_foobar = function() matrix(LETTERS[1:4], ncol = 2))
  expect_error(
    arrow_pal("foobar"),
    "does not have the type"
  )
})

# Visual tests ------------------------------------------------------------

df <- data.frame(
  x = c(0, 1, 0, 1, 0, 1),
  y = c(1, 1, 2, 2, 3, 3),
  group = c("A", "A", "B", "B", "C", "C")
)

test_that("discrete arrow_head scales work", {
  p <- ggplot(df, aes(x, y, colour = group)) +
    geom_arrow(
      aes(arrow_head = group),
      length_head = unit(1, "cm"),
      linewidth = 3
    ) +
    scale_arrow_head_discrete(
      values = list("fins_feather", NULL, arrow_head_wings())
    )

  vdiffr::expect_doppelganger("discrete arrow_head scale", p)
})

test_that("discrete arrow_fins scales work", {

  p <- ggplot(df, aes(x, y, colour = group)) +
    geom_arrow(
      aes(arrow_fins = group),
      length_fins = unit(1, "cm"),
      linewidth = 3, arrow_head = NULL
    ) +
    scale_arrow_fins_discrete(
      values = list("head_wings", NULL, arrow_fins_feather())
    )

  vdiffr::expect_doppelganger("discrete arrow_fins scale", p)
})

test_that("discrete arrow_mid scales work", {

  p <- ggplot(df, aes(x, y, colour = group)) +
    geom_arrow(
      aes(arrow_mid = group),
      length_mid = unit(1, "cm"),
      linewidth = 3, arrow_head = NULL
    ) +
    scale_arrow_mid_discrete(
      values = list("head_wings", NULL, arrow_head_line())
    )

  vdiffr::expect_doppelganger("discrete arrow_mid scale", p)
})

test_that("continuous arrow_head scales work", {
  p <- ggplot(df, aes(x, y, colour = group)) +
    geom_arrow(
      aes(arrow_head = y),
      length_head = unit(1, "cm"),
      linewidth = 3
    ) +
    scale_arrow_head_continuous(
      generator = arrow_head_line, map_arg = "angle",
      range = c(30, 60)
    )

  vdiffr::expect_doppelganger("continuous arrow_head scale", p)
})

test_that("continuous arrow_fins scales work", {

  p <- ggplot(df, aes(x, y, colour = group)) +
    geom_arrow(
      aes(arrow_fins = y),
      length_fins = unit(1, "cm"),
      linewidth = 3, arrow_head = NULL
    ) +
    scale_arrow_fins_continuous(
      generator = arrow_fins_line, map_arg = "angle",
      range = c(30, 60)
    )

  vdiffr::expect_doppelganger("continuous arrow_fins scale", p)
})

test_that("continuous arrow_mid scales work", {

  p <- ggplot(df, aes(x, y, colour = group)) +
    geom_arrow(
      aes(arrow_mid = y),
      length_mid = unit(1, "cm"),
      linewidth = 3, arrow_head = NULL
    ) +
    scale_arrow_mid_continuous(
      generator = arrow_fins_feather, map_arg = "height",
      range = c(0.3, 0.9)
    )

  vdiffr::expect_doppelganger("continuous arrow_mid scale", p)
})
