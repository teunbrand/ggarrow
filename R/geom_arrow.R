# Constructors ------------------------------------------------------------

#' Arrows
#'
#' This arrow geom can be used to draw lines adorned with arrow heads or fins.
#' It is useful as an annotation layer to point to or away from other things
#' on the plot. An arrow typically consists of three parts: the arrowhead, the
#' shaft and fins. This geom places arrow heads at the end of a line and fins
#' at the beginning of a line.
#'
#' @inheritParams ggplot2::geom_path
#' @param arrow_head,arrow_fins,arrow_mid A function call to one of the
#'   [arrow ornament][arrow_ornaments] functions that can determine the shape
#'   of the arrow head, fins or middle (interior) arrows.
#' @param length_head,length_fins,length_mid Determines the length of the arrow
#'   ornaments. Can be one of the following:
#'   \describe{
#'     \item{A `numeric(1)` (default)}{to set the ornament size relative to
#'     the linewidth of the shaft.}
#'     \item{A `<`[`unit`][grid::unit]`>`}{to control the size in an absolute
#'     manner. Behaviour of relative units such as `"npc"` or `"null"` is
#'     undefined.}
#'   }
#' @param resect,resect_head,resect_fins A `numeric(1)` denoting millimetres or
#'   `<`[`unit`][grid::unit]`>` to shorten the arrow. `resect_head` shortens
#'   the arrow from the arrow head side, whereas `resect_fins` shortens the
#'   arrow from the fins side. Both inherit from `resect`.
#' @param justify A `numeric(1)` between \[0-1\] to control where the
#'   arrows should be drawn relative to the path's endpoints. A value of `0`
#'   sets the arrow's tips at the path's end, whereas a value of `1` sets the
#'   arrow's base at the path's end.
#' @param mid_place Sets the location of middle (interior) arrows, when
#'   applicable. Can be one of the following:
#'   \describe{
#'     \item{A `numeric` vector}{with values between \[0-1\] to set middle
#'     arrows at relative positions along the arc-length of a path.}
#'     \item{A `<`[`unit`][grid::unit]`>`}{to fill a path with arrows with
#'     the provided unit as distance between one arrow to the next.}
#'   }
#' @param force_arrow A `logical(1)` which, if `TRUE` an arrow will be drawn
#'   even when the length of the arrow is shorter than the arrow heads and fins.
#'   If `FALSE`, will drop such arrows.
#'
#' @eval ggplot2:::rd_aesthetics("geom", "arrow")
#' @return A `<Layer>` ggproto object that can be added to a plot.
#' @export
#' @family arrow geoms
#'
#' @examples
#' # Setting up a plot
#' p <- ggplot(whirlpool(), aes(x, y, colour = group)) +
#'   coord_equal()
#'
#' # A standard arrow
#' p + geom_arrow()
#'
#' # Arrows can have varying linewidths
#' p + geom_arrow(aes(linewidth = arc))
#'
#' # You can use `length_head` to decouple arrow-head size from linewidth
#' p + geom_arrow(aes(linewidth = arc), length_head = unit(10, "mm"))
#'
#' # The arrow head shape can be controlled with the `arrow_head` argument
#' p + geom_arrow(arrow_head = arrow_head_line(), length_head = unit(10, "mm"))
#'
#' # This works similarly for the arrow fins
#' p + geom_arrow(arrow_fins = arrow_fins_feather(), length_fins = unit(7, "mm"))
geom_arrow <- function(
  mapping   = NULL,
  data      = NULL,
  stat      = "identity",
  position  = "identity",
  ...,
  arrow_head  = arrow_head_wings(),
  arrow_fins  = NULL,
  arrow_mid   = NULL,
  length      = 4,
  length_head = NULL,
  length_fins = NULL,
  length_mid  = NULL,
  justify     = 0,
  force_arrow = FALSE,
  mid_place   = 0.5,
  resect      = 0,
  resect_head = NULL,
  resect_fins = NULL,
  lineend     = "butt",
  linejoin    = "round",
  linemitre   = 10,
  na.rm       = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  length <- validate_length(
    length, length_head, length_fins, length_mid
  )
  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomArrow,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      arrow  = list(head = arrow_head,  fins = arrow_fins,  mid  = arrow_mid),
      length      = length,
      justify     = justify,
      force_arrow = force_arrow,
      mid_place   = mid_place,
      resect_head = resect_head %||% resect,
      resect_fins = resect_fins %||% resect,
      lineend     = lineend,
      linejoin    = linejoin,
      linemitre   = linemitre,
      na.rm       = na.rm,
      ...
    )
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname ggarrow_extensions
#' @format NULL
#' @usage NULL
GeomArrow <- ggproto(
  "GeomArrow", GeomPath,

  default_aes = aes(
    colour    = "black",
    linewidth = 1,
    alpha     = NA,
    arrow_head = NULL,
    arrow_fins = NULL,
    arrow_mid  = NULL,
    stroke_colour = NA,
    stroke_width  = 0.25
  ),

  draw_panel = function(
    self, data, panel_params, coord,
    lineend     = "butt",
    linejoin    = "round",
    linemitre   = 10,
    na.rm       = FALSE,
    arrow       = list(head = arrow_head_wings(), fins = NULL, mid = NULL),
    length      = list(head = 4, fins = 4, mid = 4),
    justify     = 0,
    force_arrow = FALSE,
    mid_place   = 0.5,
    resect_head = 0,
    resect_fins = 0
  ) {

    if (!anyDuplicated(data$group)) {
      cli::cli_inform(c(paste0(
        "{.fn {snake_class(self)}}: Each group consists of only one ",
        "observation"
      ), i = "Do you need to adjust the {.field group} aesthetic?"))
    }

    data <- data[order(data$group), , drop = FALSE]
    data <- coord_munch(coord, data, panel_params)

    rows <- ave(seq_len(nrow(data)), data$group, FUN = base::length)
    data <- data[rows >= 2, , drop = FALSE]
    if (nrow(data) < 2) {
      return(zeroGrob())
    }

    # Attribute check
    attrs <- c("alpha", "colour", "stroke_colour", "stroke_width")
    attrs <- intersect(attrs, names(data))
    constant <- vapply(split(data, data$group), function(df) {
      nrow(unique0(df[, attrs])) == 1
    }, logical(1))

    if (!all(constant)) {
      cli::cli_abort(paste0(
        "{.fn {snake_class(self)}} can't have varying {.field colour}, ",
        "{.field alpha}, {.field stroke_colour}, and/or {.field stroke_width} ",
        "along the line."
      ))
    }

    n <- nrow(data)
    group_diff <- data$group[-1] != data$group[-n]
    start <- c(TRUE, group_diff)
    end   <- c(group_diff, TRUE)

    width <- unit(data$linewidth * .pt / .stroke, "mm")
    if (!is.unit(length$head)) {
      length$head <- (length$head %||% 4) * width[end]
    }
    if (!is.unit(length$fins)) {
      length$fins <- (length$fins %||% 4) * width[start]
    }

    id <- match(data$group, unique(data$group))
    grob_arrow(
      x  = unit(data$x, "native"),
      y  = unit(data$y, "native"),
      id = id,
      arrow_head  = data$arrow_head[start] %||% arrow$head,
      arrow_fins  = data$arrow_fins[end]   %||% arrow$fins,
      arrow_mid   = data$arrow_mid[start]  %||% arrow$mid,
      length_head = length$head,
      length_fins = length$fins,
      length_mid  = length$mid %||% 4,
      justify     = justify,
      force_arrow = force_arrow,
      mid_place   = mid_place,
      shaft_width = width,
      resect_head = as_unit(resect_head, "mm"),
      resect_fins = as_unit(resect_fins, "mm"),
      gp = gpar(
        col  = data$stroke_colour[start],
        fill = alpha(data$colour, data$alpha)[start],
        lwd  = data$stroke_width[start] * .pt,
        linejoin  = linejoin,
        linemitre = linemitre,
        lineend   = lineend
      )
    )
  },

  draw_key = draw_key_arrow
)

