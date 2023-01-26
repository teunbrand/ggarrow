arrow <- grob_arrow(
  x = unit(c(0.2, 0.4, 0.6, 0.8, 0.2, 0.4, 0.6, 0.8), "npc"),
  y = unit(c(0.2, 0.8, 0.2, 0.8, 0.8, 0.2, 0.8, 0.2), "npc"),
  id.lengths = c(4, 4),
  arrow_head = NULL,
  arrow_fins = NULL,
  gp = gpar(fill = c("dodgerblue", "tomato"), col = "black"),
  shaft_width = unit(3, "mm")
)

test_that("grob_arrow can draw without arrowheads or -fins", {

  my_arrow <- arrow

  vdiffr::expect_doppelganger(
    "no head, no fins",
    function() {
      grid.newpage()
      grid.draw(my_arrow)
    }
  )
})

test_that("arrow_heads can be placed at the end of a line", {

  my_arrow <- arrow
  my_arrow$arrow_head  <- arrow_head_wings()
  my_arrow$head_length <- unit(c(5, 10), "mm")

  vdiffr::expect_doppelganger(
    "head only",
    function() {
      grid.newpage()
      grid.draw(my_arrow)
    }
  )
})

test_that("arrow_fins can be placed at the beginning of a line", {

  my_arrow <- arrow
  my_arrow$arrow_fins  <- arrow_fins_feather()
  my_arrow$fins_length <- unit(c(10, 20), "mm")

  vdiffr::expect_doppelganger(
    "fins only",
    function() {
      grid.newpage()
      grid.draw(my_arrow)
    }
  )
})

test_that("arrow heads and fins can both be placed", {

  my_arrow <- arrow
  my_arrow$arrow_fins  <- arrow_fins_feather()
  my_arrow$fins_length <- unit(c(10, 20), "mm")
  my_arrow$arrow_head  <- arrow_head_wings()
  my_arrow$head_length <- unit(c(5, 10), "mm")

  vdiffr::expect_doppelganger(
    "head and fins",
    function() {
      grid.newpage()
      grid.draw(my_arrow)
    }
  )
})

test_that("arrows can have variable widths", {

  my_arrow <- arrow
  my_arrow$arrow_fins  <- arrow_fins_feather()
  my_arrow$fins_length <- unit(c(10, 20), "mm")
  my_arrow$arrow_head  <- arrow_head_wings()
  my_arrow$head_length <- unit(c(5, 10), "mm")
  my_arrow$shaft_width <- unit(
    c(2, 4, 6, 8, 8, 6, 4, 2), "mm"
  )

  vdiffr::expect_doppelganger(
    "variable width",
    function() {
      grid.newpage()
      grid.draw(my_arrow)
    }
  )
})

test_that("inner arrows can be drawn at positions", {

  my_arrow <- arrow
  my_arrow$arrow_inner <- arrow_head_wings()
  my_arrow$inner_length <- unit(10, "mm")
  my_arrow$inner_just <- c(0.25, 0.5, 0.75)

  vdiffr::expect_doppelganger(
    "inner position",
    function() {
      grid.newpage()
      grid.draw(my_arrow)
    }
  )
})

test_that("inner arrows can be drawn at distance", {

  my_arrow <- arrow
  my_arrow$arrow_inner <- arrow_head_wings()
  my_arrow$inner_length <- unit(10, "mm")
  my_arrow$inner_just <- unit(2, "cm")

  vdiffr::expect_doppelganger(
    "inner distance",
    function() {
      grid.newpage()
      grid.draw(my_arrow)
    }
  )
})
