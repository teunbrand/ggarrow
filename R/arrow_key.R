#' Legend key glyph for arrows
#'
#' Like any [legend key glyphs][ggplot2::draw_key], this key can be used to
#' display arrows in a legend.
#'
#' @inheritParams ggplot2::draw_key_rect
#'
#' @return An `<arrow_path>` grob
#' @export
#' @examples
#' ggplot(mpg, aes(displ, colour = factor(cyl))) +
#'   geom_density(key_glyph = draw_key_arrow)
draw_key_arrow <- function(data, params, size) {

  width <- unit(data$linewidth * .pt / .stroke, "mm")
  length_head <- params$length_head %||% 4
  length_fins <- params$length_fins %||% 4
  if (!is.unit(length_head)) {
    length_head <- length_head * width
  }
  if (!is.unit(length_fins)) {
    length_fins <- length_fins * width
  }

  grob <- grob_arrow(
    x = unit(c(0.1, 0.9), "npc"),
    y = unit(c(0.1, 0.9), "npc"),
    arrow_head  = data$arrow_head %||% params$arrow$head,
    arrow_fins  = data$arrow_fins %||% params$arrow$fins,
    length_head = length_head,
    length_fins = length_fins,
    shaft_width = width,
    force_arrow = TRUE,
    gp = gpar(
      col  = data$stroke_colour    %||% NA,
      fill = alpha(data$colour     %||% "black", data$alpha %||% NA),
      lwd  = data$stroke_width     %||% 0.5 * .pt,
      lty  = data$linetype         %||% 1,
      linejoin  = params$linejoin  %||% "round",
      linemitre = params$linemitre %||% 10
    )
  )

  # Explanation of magic numbers for future me:
  # Arrow is placed diagonal so width/height of arrow is length/sqrt(2)
  # We accommodate both the arrow head and arrow fins, even if absent
  # 1.25 is for spanning 0.9 - 0.1 = 0.8 npcs: 1 / 0.8 = 1.25 multiplier
  # 10 is for mm --> cm

  size <- (length_head + length_fins) * 1.25 / (sqrt(2) * 10)
  attr(grob, "width") <- attr(grob, "height") <- size

  grob
}
