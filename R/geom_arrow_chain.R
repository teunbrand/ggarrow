# Constructor -------------------------------------------------------------

#' Arrow chains
#'
#' An arrow chains connects a set of coordinates with a sequence of arrows. The
#' `geom_arrow_chain()` function can be useful to connect observations in a
#' directed manner.
#'
#' @inheritParams geom_arrow
#'
#' @eval ggplot2:::rd_aesthetics("geom", "arrow_segment", extra_note = paste0(
#'  "The `linewidth_fins` and `linewidth_head` inherit from `linewidth`. ",
#' " They can be used to seperately control the start- and end-width."
#' ))
#' @inherit geom_arrow return
#' @export
#' @family arrow geoms
#'
#' @examples
#' # Setup dummy data
#' t <- seq(0, 2 * pi, length.out = 11)
#' l <- rep(c(1, 0.4), length.out = 11)
#'
#' df <- data.frame(
#'   x = cos(t) * l,
#'   y = sin(t) * l,
#'   size = l + 0.4
#' )
#'
#' p <- ggplot(df, aes(x, y, size = size)) +
#'   geom_point(colour = 2) +
#'   coord_equal()
#'
#' # An arrow chains adapts to the `size` aesthetic to go nicely with points
#' p + geom_arrow_chain()
#'
#' # Without arrowhead, it is similar to a `type = 'b'` base R plot
#' p + geom_arrow_chain(arrow_head = NULL)
#'
#' # To widen the gap, one can increase the `resect` parameter
#' p + geom_arrow_chain(resect = 5)
#'
#' # To ignore the points, set `resect` and `size` to 0
#' p + geom_arrow_chain(size = 0, resect = 0)
#'
#' # Linewidths will be interpolated across arrows
#' p + geom_arrow_chain(aes(linewidth = seq_along(x)))
#'
#' # Alternatively, we can set them seperately for starts and ends
#' p + geom_arrow_chain(linewidth_fins = 0, linewidth_head = 3)
geom_arrow_chain <- function(
  mapping   = NULL,
  data      = NULL,
  stat      = "identity",
  position  = "identity",
  ...,
  arrow_head  = arrow_head_wings(),
  arrow_fins  = NULL,
  arrow_mid   = NULL,
  length_head = 4,
  length_fins = 4,
  length_mid  = 4,
  justify     = 0,
  force_arrow = FALSE,
  mid_place   = 0.5,
  resect      = 1,
  resect_head = NULL,
  resect_fins = NULL,
  lineend     = "butt",
  linejoin    = "round",
  linemitre   = 10,
  na.rm       = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomArrowChain,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      arrow  = list(head = arrow_head,  fins = arrow_fins,  mid  = arrow_mid),
      length = list(head = length_head, fins = length_fins, mid  = length_mid),
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
GeomArrowChain <- ggproto(
  "GeomArrowChain", GeomArrow,

  default_aes = aes(
    colour    = "black",
    linewidth = 1,
    linewidth_head = NULL,
    linewidth_fins = NULL,
    alpha     = NA,
    stroke_colour = NA,
    stroke_width  = 0.25,
    size      = 1.5
  ),
  rename_size = FALSE,

  draw_panel = function(
    self, data, panel_params, coord,
    linejoin = "round", linemitre = 10, lineend = "butt",
    na.rm       = FALSE,
    arrow       = list(head = arrow_head_wings(), fins = NULL, mid = NULL),
    length      = list(head = 4, fins = 4, mid = 4),
    justify     = 0,
    force_arrow = force_arrow,
    mid_place   = 0.5,
    resect_head = 0,
    resect_fins = 0
  ) {
    data$linewidth_head <- data$linewidth_head %||% data$linewidth
    data$linewidth_fins <- data$linewidth_fins %||% data$linewidth
    data$linewidth <- NULL

    if (!anyDuplicated(data$group)) {
      cli::cli_inform(c(paste0(
        "{.fn {snake_class(self)}}: Each group consists of only one ",
        "observation"
      ), i = "Do you need to adjust the {.field group} aesthetic?"))
    }

    data <- data[order(data$group), , drop = FALSE]

    rows <- ave(seq_len(nrow(data)), data$group, FUN = base::length)
    data <- data[rows >= 2, , drop = FALSE]
    if (nrow(data) < 2) {
      return(zeroGrob())
    }

    n    <- nrow(data)
    data <- vec_interleave(data[-n, , drop = FALSE], data[-1, , drop = FALSE])
    data$group <- rep(seq_len(nrow(data) / 2), each = 2)
    data <- coord_munch(coord, data, panel_params)

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

    data$linewidth <- ifelse(start, data$linewidth_fins, data$linewidth_head)

    width <- unit(data$linewidth * .pt / .stroke, "mm")
    if (!is.unit(length$head)) {
      length$head <- length$head * width[end]
    }
    if (!is.unit(length$fins)) {
      length$fins <- length$fins * width[start]
    }

    size <- data$size

    id <- match(data$group, unique(data$group))
    grob_arrow(
      x  = unit(data$x, "native"),
      y  = unit(data$y, "native"),
      id = id,
      arrow_head  = arrow$head,
      arrow_fins  = arrow$fins,
      arrow_mid   = arrow$mid,
      length_head = length$head,
      length_fins = length$fins,
      length_mid  = length$mid,
      justify     = justify,
      force_arrow = force_arrow,
      mid_place   = mid_place,
      shaft_width = width,
      resect_head = as_unit(resect_head, "mm") + unit(size[end],   "pt"),
      resect_fins = as_unit(resect_fins, "mm") + unit(size[start], "pt"),
      gp = gpar(
        col  = data$stroke_colour[start],
        fill = alpha(data$colour, data$alpha)[start],
        lwd  = data$stroke_width[start] * .pt,
        linejoin  = linejoin,
        linemitre = linemitre
      )
    )
  }
)
