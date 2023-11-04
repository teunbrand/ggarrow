
test_that("arrow keys work", {
  arrow  <- geom_arrow()
  dummy  <- arrow$geom$use_defaults(NULL)
  params <- arrow$geom_params
  key    <- draw_key_arrow(dummy, params, 10)

  vdiffr::expect_doppelganger(
    "arrow key", key
  )
})

