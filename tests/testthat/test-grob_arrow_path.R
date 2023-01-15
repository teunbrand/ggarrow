
test_that("grob_arrow errors with fewer than two points per group", {
  expect_error(
    grob_arrow(
      x = 1:3,
      y = 1:3,
      id.lengths = c(2, 1)
    ),
    "cannot draw arrows"
  )
})

arrow <- grob_arrow(
  x = unit(c(0.33, 0.33, 0.66, 0.66), "npc") + unit(c(0, 10, 0, 10), "mm"),
  y = unit(c(0.5, 0.5, 0.5, 0.5), "npc") + unit(c(0, 10, 0, 10), "mm"),
  id.lengths = c(2, 2),
  gp = gpar(fill = c("dodgerblue", "tomato"))
)

test_that("positive control: arrows can be drawn", {

  my_arrow <- arrow

  vdiffr::expect_doppelganger(
    "standard arrows",
    function() {
      grid.newpage()
      grid.draw(my_arrow)
    }
  )
})

test_that("negative control: resected arrows are discarded", {

  my_arrow <- arrow
  my_arrow$resect$fins <- my_arrow$resect$head <- unit(c(15, 15), "mm")

  vdiffr::expect_doppelganger(
    "discarded arrows",
    function() {
      grid.newpage()
      grid.draw(my_arrow)
    }
  )
})

test_that("discarded arrows preserve graphical parameters", {

  my_arrow <- arrow
  my_arrow$resect$fins <- my_arrow$resect$head <- unit(c(15, 0), "mm")
  my_arrow$shaft_width <- unit(c(0.5, 0.5, 2, 2), "mm")

  vdiffr::expect_doppelganger(
    "fat tomato arrow",
    function() {
      grid.newpage()
      grid.draw(my_arrow)
    }
  )

  my_arrow$resect$fins <- my_arrow$resect$head <- unit(c(0, 15), "mm")

  vdiffr::expect_doppelganger(
    "thin dodgerblue arrow",
    function() {
      grid.newpage()
      grid.draw(my_arrow)
    }
  )

})
