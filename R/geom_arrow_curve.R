#' Curves with arrows
#'
#' This arrow geom can be used to draw curves from one point to oneanother with
#' arrow heads or fins.
#'
#' @inheritParams ggplot2::geom_curve
#' @inheritParams geom_arrow
#'
#' @return A `<Layer>` ggproto object that can be added to a plot.
#' @export
#' @eval ggplot2:::rd_aesthetics("geom", "arrow")
#' @family arrow geoms
#'
#' @examples
#' curve_data <- data.frame(
#'   x1 = c(2.62, 1.835),
#'   x2 = c(3.57, 5.250),
#'   y1 = c(21.0, 33.9),
#'   y2 = c(15.0, 10.4),
#'   group = c("A", "B")
#' )
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   geom_arrow_curve(
#'     aes(x = x1, y = y1, xend = x2, yend = y2,
#'         colour = group, arrow_head = group),
#'     data = curve_data,
#'     curvature = -0.2, length_head = 10
#'   )
geom_arrow_curve <- function(
  mapping   = NULL,
  data      = NULL,
  stat      = "identity",
  position  = "identity",
  ...,
  # Curve params
  curvature = 0.5,
  angle     = 90,
  ncp       = 5,
  # Arrow params
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
  resect_head <- resect_head %||% resect
  resect_fins <- resect_fins %||% resect
  check_number_decimal(resect_head, min = 0, allow_infinite = FALSE)
  check_number_decimal(resect_fins, min = 0, allow_infinite = FALSE)
  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomArrowCurve,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(

      curvature = curvature,
      angle     = angle,
      ncp       = ncp,

      arrow  = list(head = arrow_head, fins = arrow_fins, mid  = arrow_mid),
      length      = length,
      justify     = justify,
      force_arrow = force_arrow,
      mid_place   = mid_place,
      resect      = list(head = resect_head, fins = resect_fins),
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
GeomArrowCurve <- ggproto(
  "GeomArrowCurve", GeomArrow,

  required_aes = c("x", "y", "xend|yend"),

  default_aes = aes(
    colour    = "black",
    linewidth = 1,
    linewidth_head = NULL,
    linewidth_fins = NULL,
    arrow_head     = NULL,
    arrow_fins     = NULL,
    arrow_mid      = NULL,
    alpha          = NA,
    stroke_colour  = NA,
    stroke_width   = 0.25
  ),

  draw_panel = function(
    data, panel_params, coord,
    linejoin = "round", linemitre = 10, lineend = "butt",
    na.rm       = FALSE,
    arrow       = list(head = arrow_head_wings(), fins = NULL, mid = NULL),
    length      = list(head = 4, fins = 4, mid = 4),
    justify     = 0,
    force_arrow = FALSE,
    mid_place   = 0.5,
    resect      = list(head = 0, fins = 0),
    curvature   = 0.5,
    angle       = 90,
    ncp         = 5
  ) {
    data <- warn_discrete_resect(data, resect)
    data$yend <- data$yend %||% data$y
    data$xend <- data$xend %||% data$x
    data$linewidth_head <- data$linewidth_head %||% data$linewidth
    data$linewidth_fins <- data$linewidth_fins %||% data$linewidth
    data$linewidth <- NULL

    if (!coord$is_linear()) {
      cli::cli_warn(
        "{.fn geom_arrow_curve} is not implemented for non-linear coordinates."
      )
    }

    data <- coord$transform(data, panel_params)

    head_width <- unit(data$linewidth_head * .pt / .stroke, "mm")
    if (!is.unit(length$head)) {
      length$head <- (length$head %||% 4) * head_width
    }

    fins_width <- unit(data$linewidth_fins * .pt / .stroke, "mm")
    if (!is.unit(length$fins)) {
      length$fins <- (length$fins %||% 4) * fins_width
    }

    grob_arrow_curve(
      unit(data$x, "native"),    unit(data$y, "native"),
      unit(data$xend, "native"), unit(data$yend, "native"),
      # Curve parameters
      curvature = curvature, angle = angle, ncp = ncp,
      square = FALSE, squareShape = 1, inflect = FALSE, open = TRUE,
      # Arrow parameters
      arrow_head  = data$arrow_head %||% arrow$head,
      arrow_fins  = data$arrow_fins %||% arrow$fins,
      arrow_mid   = data$arrow_mid  %||% arrow$mid,
      length_head = length$head,
      length_fins = length$fins,
      length_mid  = length$mid %||% 4,
      justify     = justify,
      force_arrow = force_arrow,
      mid_place   = mid_place,
      width_head  = head_width,
      width_fins  = fins_width,
      resect_head = as_unit(data$resect_head %||% resect$head, "mm"),
      resect_fins = as_unit(data$resect_fins %||% resect$fins, "mm"),
      gp = gpar(
        col  = data$stroke_colour,
        fill = alpha(data$colour, data$alpha),
        lwd  = data$stroke_width * .pt,
        linejoin  = linejoin,
        linemitre = linemitre,
        lineend   = lineend
      )
    )
  }
)
