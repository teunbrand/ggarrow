# Heads -------------------------------------------------------------------

test_that("arrow_head_wings displays as intended", {

  arrow <- grob_arrow(
    x = unit(c(0.2, 0.8), "npc"),
    y = unit(c(0, 0), "npc"),
    length_head = unit(10, "mm"), shaft_width = unit(2, "mm"),
    gp = gpar(fill = NA)
  )
  y <- 1/6

  offset <- c(20, 30, 90, 45, 135)
  inset  <- c(30, 60, 60, 90, 30)

  arrows <- lapply(seq_along(offset), function(i) {
    my_arrow <- arrow
    my_arrow$y <- unit(c(y, y) * i, "npc")
    my_arrow$arrow_head <- arrow_head_wings(
      offset = offset[i], inset = inset[i]
    )
    my_arrow
  })

  labs <- textGrob(
    y = unit(1:5 * y + 0.5 * y, "npc"),
    label = paste0("offset = ", offset, ", inset = ", inset)
  )

  lines <- segmentsGrob(
    x0 = unit(0.8, "npc") - unit(c(0, 10), "mm"),
    x1 = unit(0.8, "npc") - unit(c(0, 10), "mm"),
    gp = gpar(col = 2:3)
  )

  vdiffr::expect_doppelganger(
    "arrow_head_wings",
    function() {
      grid.newpage()
      grid.draw(lines)
      lapply(arrows, grid.draw)
      grid.draw(labs)
    }
  )
})

test_that("arrow_head_line displays as intended", {

  arrow <- grob_arrow(
    x = unit(c(0.2, 0.8), "npc"),
    y = unit(c(0, 0), "npc"),
    length_head = unit(10, "mm"), shaft_width = unit(2, "mm"),
    gp = gpar(fill = NA)
  )
  y <- 1/6

  angle <- c(30, 30, 90, 135, 30)
  linewidth <- unit(c(2, 1, 2, 2, 2), "mm")
  length <- unit(c(10, 10, 10, 10, 15), "mm")
  lineend <- c("butt", "butt", "round", "square", "parallel")

  arrows <- lapply(seq_along(angle), function(i) {
    my_arrow <- arrow
    my_arrow$y <- unit(c(y, y) * i, "npc")
    my_arrow$arrow_head <- arrow_head_line(angle = angle[i], lineend = lineend[i])
    my_arrow$length_head <- length[i]
    my_arrow$shaft_width <- linewidth[i]
    my_arrow
  })

  labs <- textGrob(
    y = unit(1:5 * y + 0.5 * y, "npc"),
    label = paste0("angle = ", angle, ", width = ", linewidth, ", length = ",
                   length,", lineend = ", lineend)
  )

  lines <- segmentsGrob(
    x0 = unit(0.8, "npc"),
    x1 = unit(0.8, "npc"),
    gp = gpar(col = 2)
  )

  vdiffr::expect_doppelganger(
    "arrow_head_line",
    function() {
      grid.newpage()
      grid.draw(lines)
      lapply(arrows, grid.draw)
      grid.draw(labs)
    }
  )
})

test_that("arrow_head_minimal displays as intended", {

  arrow <- grob_arrow(
    x = unit(c(0.2, 0.8), "npc"),
    y = unit(c(0, 0), "npc"),
    length_head = unit(10, "mm"), shaft_width = unit(2, "mm"),
    gp = gpar(fill = NA)
  )
  y <- 1/6

  angle <- c(45, 30, 90, 135, 5)
  width <- c(4, 2, 4, 4, 4)

  arrows <- lapply(seq_along(angle), function(i) {
    my_arrow <- arrow
    my_arrow$y <- unit(c(y, y) * i, "npc")
    my_arrow$arrow_head <- arrow_head_minimal(angle = angle[i])
    my_arrow$shaft_width <- unit(width[i], "mm")
    my_arrow
  })

  labs <- textGrob(
    y = unit(1:5 * y + 0.5 * y, "npc"),
    label = paste0("angle = ", angle, ", width = ", width)
  )

  lines <- segmentsGrob(
    x0 = unit(0.8, "npc"),
    x1 = unit(0.8, "npc"),
    gp = gpar(col = 2)
  )

  vdiffr::expect_doppelganger(
    "arrow_head_minimal",
    function() {
      grid.newpage()
      grid.draw(lines)
      lapply(arrows, grid.draw)
      grid.draw(labs)
    }
  )
})

# Fins --------------------------------------------------------------------

test_that("arrow_fins_feather displays as intended", {

  arrow <- grob_arrow(
    x = unit(c(0.2, 0.8), "npc"),
    y = unit(c(0, 0), "npc"),
    arrow_head = NULL,
    length_fins = unit(10, "mm"),
    shaft_width = unit(2, "mm"),
    gp = gpar(fill = NA)
  )
  y <- 1/6

  indent  <- c(0.3, 0, 0.3, 0.3, -0.3)
  outdent <- c(0.3, 0.3, 0, 0.3, -0.3)
  height  <- c(0.5, 0.5, 0.5, 1, 0.5)

  arrows <- lapply(seq_along(indent), function(i) {
    my_arrow <- arrow
    my_arrow$y <- unit(c(y, y) * i, "npc")
    my_arrow$arrow_fins <- arrow_fins_feather(
      indent = indent[i], outdent = outdent[i], height = height[i]
    )
    my_arrow$name <- paste0(my_arrow$name, i)
    my_arrow
  })

  labs <- textGrob(
    y = unit(1:5 * y + 0.5 * y, "npc"),
    label = paste0("indent = ", indent, ", outdent = ", outdent, ", height = ",
                   height)
  )

  lines <- segmentsGrob(
    x0 = unit(0.2, "npc") + unit(c(0, 10), "mm"),
    x1 = unit(0.2, "npc") + unit(c(0, 10), "mm"),
    gp = gpar(col = 2:3)
  )

  vdiffr::expect_doppelganger(
    "arrow_fins_feather",
    function() {
      grid.newpage()
      grid.draw(lines)
      lapply(arrows, grid.draw)
      grid.draw(labs)
    }
  )
})

test_that("arrow_fins_line displays as intended", {

  arrow <- grob_arrow(
    x = unit(c(0.2, 0.8), "npc"),
    y = unit(c(0, 0), "npc"),
    arrow_head = NULL,
    length_fins = unit(10, "mm"),
    shaft_width = unit(2, "mm"),
    gp = gpar(fill = NA)
  )
  y <- 1/6

  angle  <- c(30, 30, 90, 135, 30)
  width  <- c(2, 1, 2, 2, 2)
  length <- c(10, 10, 10, 10, 15)
  lineend <- c("butt", "butt", "round", "square", "parallel")

  arrows <- lapply(seq_along(angle), function(i) {
    my_arrow <- arrow
    my_arrow$y <- unit(c(y, y) * i, "npc")
    my_arrow$arrow_fins <- arrow_fins_line(angle = angle[i], lineend = lineend[i])
    my_arrow$shaft_width <- unit(width[i], "mm")
    my_arrow$length_fins <- unit(length[i], "mm")
    my_arrow$name <- paste0(my_arrow$name, i)
    my_arrow
  })

  labs <- textGrob(
    y = unit(1:5 * y + 0.5 * y, "npc"),
    label = paste0("angle = ", angle, ", width = ", width, ", length = ",
                   length, ', lineend = ', lineend)
  )

  lines <- segmentsGrob(
    x0 = unit(0.2, "npc"),
    x1 = unit(0.2, "npc"),
    gp = gpar(col = 2)
  )

  vdiffr::expect_doppelganger(
    "arrow_fins_line",
    function() {
      grid.newpage()
      grid.draw(lines)
      lapply(arrows, grid.draw)
      grid.draw(labs)
    }
  )
})


