# Constructor -------------------------------------------------------------

#' Arrow segments
#'
#' `geom_arrow_segment()` draws a straight arrow between points (x, y) and
#' (xend, yend). In contrast to `geom_segment()`, the `xend` and `yend`
#' aesthetics default to `x` and `y` respectively, so only one of `xend` and
#' `yend` is required.
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
#' set.seed(42)
#' df <- data.frame(
#'   x = LETTERS[1:6],
#'   y = 6:1 + rnorm(6)
#' )
#'
#' # We can omit either `xend` or `yend` for this segment geom
#' p <- ggplot(df, aes(x, y = 0, yend = y, colour = x))
#' p + geom_arrow_segment()
#'
#' # We can set the linewidth globally
#' p + geom_arrow_segment(aes(linewidth = y))
#'
#' # Or seperately for the head and fins
#' p + geom_arrow_segment(aes(linewidth_head = y, linewidth_fins = 0))
#'
#' # We can also place arrows in the middle
#' p + geom_arrow_segment(
#'   arrow_mid = arrow_head_line(), mid_place = c(0.33, 0.66),
#'   arrow_head = NULL # Turn off head
#' )
geom_arrow_segment <- function(
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
    geom        = GeomArrowSegment,
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
GeomArrowSegment <- ggproto(
  "GeomArrowSegment", GeomSegment,

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
    self, data, panel_params, coord,
    linejoin = "round", linemitre = 10, lineend = "butt",
    na.rm       = FALSE,
    arrow       = list(head = arrow_head_wings(), fins = NULL, mid = NULL),
    length      = list(head = 4, fins = 4, mid = 4),
    justify     = 0,
    force_arrow = FALSE,
    mid_place   = 0.5,
    resect_head = 0,
    resect_fins = 0
  ) {
    data$yend <- data$yend %||% data$y
    data$xend <- data$xend %||% data$x
    data$linewidth_head <- data$linewidth_head %||% data$linewidth
    data$linewidth_fins <- data$linewidth_fins %||% data$linewidth
    data$linewidth <- NULL

    data <- remove_missing(
      data, na.rm = na.rm,
      c("x", "y", "xend", "yend", "linewidth_head", "linewidth_fins", "shape"),
      name = "geom_arrow_segment"
    )

    if (empty(data)) {
      return(zeroGrob())
    }

    data$group <- seq_len(nrow(data))
    starts     <- rename(
      subset(data, select = c(-xend, -yend, -linewidth_head)),
      c(linewidth_fins = "linewidth")
    )
    ends       <- rename(
      subset(data, select = c(-x, -y, -linewidth_fins)),
      c(xend = "x", yend = "y", linewidth_head = "linewidth")
    )
    pieces <- vec_rbind(starts, ends)
    pieces <- pieces[order(pieces$group), ]

    GeomArrow$draw_panel(
      pieces, panel_params, coord,
      linejoin = linejoin, linemitre = linemitre, na.rm = na.rm,
      arrow = arrow, length = length, justify = justify,
      force_arrow = force_arrow, mid_place = mid_place,
      resect_head = resect_head, resect_fins = resect_fins
    )
  },

  draw_key = draw_key_arrow
)
