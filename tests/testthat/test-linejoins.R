
arrow <- grob_arrow(
  x = unit(c(0.2, 0.6, 0.7, 0.8), "npc"),
  y = unit(c(0.2, 0.8, 0.2, 0.8), "npc"),
  gp = gpar(fill = "dodgerblue", col = NA),
  shaft_width = unit(3, "mm")
)

line <- polylineGrob(
  x = c(0.2, 0.6, 0.7, 0.8),
  y = c(0.8, 0.2, 0.8, 0.2),
  gp = gpar(col = "tomato", lwd = 3 * .stroke, lineend = "butt")
)

test_that("linejoin_round works", {

  my_arrow <- arrow
  my_line  <- line

  my_arrow$gp$linejoin <- my_line$gp$linejoin <- "round"

  vdiffr::expect_doppelganger(
    "linejoin round",
    function() {
      grid.newpage()
      grid.draw(my_arrow)
      grid.draw(my_line)
    }
  )
})

test_that("linejoin_mitre works", {
  my_arrow <- arrow
  my_line  <- line

  my_arrow$gp$linejoin  <- my_line$gp$linejoin <- "mitre"
  my_arrow$gp$linemitre <- my_line$gp$linemitre <- 3

  vdiffr::expect_doppelganger(
    "linejoin mitre",
    function() {
      grid.newpage()
      grid.draw(my_arrow)
      grid.draw(my_line)
    }
  )
})

test_that("linejoin_bevel works", {
  my_arrow <- arrow
  my_line  <- line

  my_arrow$gp$linejoin  <- my_line$gp$linejoin <- "bevel"

  vdiffr::expect_doppelganger(
    "linejoin bevel",
    function() {
      grid.newpage()
      grid.draw(my_arrow)
      grid.draw(my_line)
    }
  )
})
