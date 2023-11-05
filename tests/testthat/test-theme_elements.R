test_that("arrow theme elements looks like they should", {
  p <- ggplot() +
    xlim(0, 1) + ylim(0, 1) +
    theme(
      axis.line = element_line(colour = "red"),
      # Proper arrow with variable width for x-axis line
      axis.line.x = element_arrow(
        arrow_head = "head_wings", linewidth_head = 2, linewidth_fins = 0,
        colour = "black"
      ),
      # Just a variable width line for the y-axis line
      axis.line.y = element_arrow(
        linewidth_head = 0, linewidth_fins = 5, lineend = "round",
        stroke_colour = "black", stroke_width = 1
      ),
      # Arrows for the y-axis ticks
      axis.ticks.y = element_arrow(arrow_fins = arrow_head_line(angle = 45)),
      # Variable width lines for the x-axis ticks
      axis.ticks.x = element_arrow(linewidth_head = 3, linewidth_fins = 0),
      axis.ticks.length = unit(0.5, 'cm'),
      # Arrows for major panel grid
      panel.grid.major = element_arrow(
        arrow_head = "head_wings", arrow_fins = "fins_feather", length = 10
      ),
      # Shortened lines for the minor panel grid
      panel.grid.minor = element_arrow(resect = 20)
    )
  vdiffr::expect_doppelganger("theme lines as arrows", p)
})


