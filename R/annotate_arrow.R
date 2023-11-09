#' Arrow annotation layer
#'
#' This function mirrors [`annotate()`][ggplot2::annotate] with the following
#' changes. First, the `geom` argument is pre-populated with `"arrow"`. Second,
#' several parameters from ggarrow are special-cased, because no warning needs
#' to be issued when they don't have length 1.
#'
#' @inheritParams ggplot2::annotate
#' @param x,y,xmin,ymin,xmax,ymax,xend,yend Positioning aesthetics. At least
#'   one of these must be specified.
#'
#' @inherit geom_arrow return
#' @return A `<Layer>` ggproto object that can be added to a plot.
#' @export
#' @family arrow geoms
#'
#' @examples
#' # Annotate an arrow
#' ggplot() +
#'   annotate_arrow(
#'     x = c(0, 1), y = c(0, 1),
#'     arrow_head = arrow_head_line(),
#'     arrow_fins = arrow_fins_line(),
#'     length_head = unit(5, "mm"),
#'     length_fins = unit(5, "mm")
#'   )
#'
#' # Still works with other geoms as well
#' ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() +
#'   annotate_arrow("text", x = 4, y = 25, label = "Some text")
annotate_arrow <- function(
  geom = "arrow",
  x = NULL,
  y = NULL,
  xmin = NULL,
  xmax = NULL,
  ymin = NULL,
  ymax = NULL,
  xend = NULL,
  yend = NULL,
  ...,
  na.rm = FALSE
) {
  if (is.character(geom) && geom %in% c("abline", "hline", "vline")) {
    cli::cli_warn(c(
      "{.arg geom} must not be {.val {geom}}.",
      i = "Please use {.fnn {paste0('geom_', geom)}} directly instead."
    ))
  }
  position <- list(x = x, xmin = xmin, xmax = xmax, xend = xend,
                   y = y, ymin = ymin, ymax = ymax, yend = yend)
  position <- position[lengths(position) > 0]

  aesthetics <- c(position, list2(...))
  lengths    <- lengths(aesthetics)
  arrow_nms <- c("arrow_head", "arrow_fins", "arrow_mid")
  length_nms <- c("length_head", "length_fins", "length_mid")
  n <- vec_unique(lengths)
  if (length(n) > 1) {
    # Don't count length/arrow args
    lengths[names(aesthetics) %in% c(arrow_nms, length_nms)] <- 1L
    n <- vec_unique(lengths)
    n <- setdiff(n, 1L)
  }
  if (length(n) > 1L) {
    bad <- lengths != 1L
    details <- paste0(names(aesthetics)[bad], " (", lengths[bad], ")")
    cli::cli_abort("Unequal parameter lengths: {details}")
  }
  args <- list2(...)

  # Special case arrow/length arguments
  if (any(arrow_nms %in% names(args))) {
    arrow <- list(head = args$arrow_head,
                  fins = args$arrow_fins,
                  mid = args$arrow_mid)
    args  <- args[setdiff(names(args), arrow_nms)]
    arrow <- arrow[lengths(arrow) > 0]
    if (length(arrow) > 0) {
      args$arrow <- arrow
    }
  }
  if (any(length_nms %in% names(args))) {
    length <- list(head = args$length_head,
                   fins = args$length_fins,
                   mid  = args$length_mid)
    args   <- args[setdiff(names(args), length_nms)]
    length <- length[lengths(length) > 0]
    if (length(length) > 0) {
      args$length <- length
    }
  }

  data <- data_frame0(!!!position, .size = n)
  layer(
    geom = geom,
    params = list2(na.rm = na.rm, !!!args),
    stat = StatIdentity,
    position = PositionIdentity,
    data = data,
    mapping = aes_all(names(data)),
    inherit.aes = FALSE, show.legend = FALSE
  )
}
