
#' Arrow theme element
#'
#' Using the [theme][ggplot2::theme] system, draws arrows in places where
#' [`element_line()`][ggplot2::element_line] are valid theme elements. Note
#' that the default use of `element_arrow()` does *not* actually draw an arrow
#' unless one of the `arrow_` arguments is set.
#'
#' @param colour The colour of the arrow.
#' @param linewidth,linewidth_head,linewidth_fins The width of the arrow shaft
#'   in millimetres. `linewidth` is the default width, whereas `linewidth_head`
#'   and `linewidth_fins` can set non-uniform width at the end and start of the
#'   line respectively.
#' @param stroke_colour The colour of the arrow outline.
#' @param stroke_width The width of the arrow outlien.
#' @param arrow_head,arrow_fins,arrow_mid Arrow
#'   [ornament shapes][arrow_ornaments] for the arrow head, arrow fins and
#'   middle arrows respectively. Can be one of the following:
#'     * `NULL` for not drawing the ornament.
#'     * A `<character>` of length 1 naming an ornament constructor without the
#'     `"arrow_"`-prefix, like `"head_wings"` or `"fins_feather"`.
#'     * A 2-column matrix, such as those built by the
#'     [ornament constructors][arrow_ornaments].
#' @param length,length_head,length_fins,length_mid Determines the size of
#'   the arrow ornaments. `length` sets the default length, whereas
#'   `length_head`, `length_fins` and `length_mid` set the lengths of the
#'   arrow head, arrow fins or middle arrows respectively. Can be one of the
#'   following:
#'   * A `<numeric>` to set the ornament size relative to the `linewidth{_\*}`
#'   settings.
#'   * A [`<unit>`][grid::unit] to control the ornament size in an absolute
#'   manner. Behaviour of relative units such as `"npc"` or `"null"` is
#'   undefined.
#' @param resect,resect_head,resect_fins A `numeric(1)` denoting millimetres
#'   or [`<unit>`][grid::unit] to set an offset from the start and end points
#'   of the line such that the arrow is shortened. `resect` sets the default
#'   offset, whereas `resect_head` and `resect_fins` sets these offsets for the
#'   end- and start-point respectively.
#' @param justify A `numeric(1)` between \[0-1\] to control where the arrow
#'   ornaments should be drawn relative to the (resected) path's endpoints.
#'   A value of `0` (default) sets the ornament's tips at the path's endpoint,
#'   whereas a value of `1` sets the ornament's base at the path's endpoint.
#' @param force_arrow A `logical(1)` which if `TRUE`, will draw arrow ornaments
#'   even when the path's length is shorter than the arrow heads and fins.
#'   If `FALSE`, such ornaments will be dropped.
#' @param mid_place Sets the location of middle (interior) ornaments when
#'   `arrow_mid` has been provided. Can be one of the following:
#'   * A `<numeric>` vector with values between \[0-1\] to set middle ornaments
#'     at relative positions along the arc-length of the (resected) path.
#'   * A [`<unit>`][grid::unit] to fill a path with ornaments with th provided
#'     unit as spacing between one ornament to the next.
#' @param lineend A `character(1)` setting the style of the line ends without
#'   ornaments. Can be `"round"`, `"butt"` or `"square"`.
#' @param linejoin A `character(1)` setting the style of path corners. Can be
#'   `"round"`, `"mitre"` or `"bevel"`.
#' @param linemitre A `numeric(1)` greater than 1 setting the path's mitre
#'   limits.
#' @param inherit.blank A `logical(1)` indicating if this element should
#'   inherit the existence of an `<element_blank>` among its parents. If `TRUE`,
#'   the existence of a blank element among its parents will cause this element
#'   to be blank as well. If `FALSE`, any blank parent element will be ignored
#'   when calculating final element state.
#'
#' @return An `<element_arrow>` object that can replace `<element_line>` objects
#'   in [`theme()`][ggplot2::theme].
#' @export
#'
#' @examples
#' # Setting a bunch of arrows all over the theme
#' ggplot(whirlpool(5), aes(x, y, group = group)) +
#'   geom_path() +
#'   theme(
#'     # Proper arrow with variable width for x-axis line
#'     axis.line.x = element_arrow(
#'       arrow_head = "head_wings", linewidth_head = 2, linewidth_fins = 0
#'     ),
#'     # Just a variable width line for the y-axis line
#'     axis.line.y = element_arrow(linewidth_head = 0, linewidth_fins = 5,
#'                                 lineend = "round"),
#'     # Arrows for the y-axis ticks
#'     axis.ticks.y = element_arrow(arrow_fins = arrow_head_line(angle = 45)),
#'     # Variable width lines for the x-axis ticks
#'     axis.ticks.x = element_arrow(linewidth_head = 3, linewidth_fins = 0),
#'     axis.ticks.length = unit(0.5, 'cm'),
#'     # Arrows for major panel grid
#'     panel.grid.major = element_arrow(
#'       arrow_head = "head_wings", arrow_fins = "fins_feather", length = 10
#'     ),
#'     # Shortened lines for the minor panel grid
#'     panel.grid.minor = element_arrow(resect = 20)
#'   )

element_arrow <- function(
  colour         = NULL,
  linewidth      = NULL,
  linewidth_head = NULL,
  linewidth_fins = NULL,
  stroke_colour  = NULL,
  stroke_width   = NULL,
  arrow_head     = NULL,
  arrow_fins     = NULL,
  arrow_mid      = NULL,
  length         = NULL,
  length_head    = NULL,
  length_fins    = NULL,
  length_mid     = NULL,
  resect         = NULL,
  resect_head    = NULL,
  resect_fins    = NULL,
  justify        = NULL,
  force_arrow    = NULL,
  mid_place      = NULL,
  lineend        = NULL,
  linejoin       = NULL,
  linemitre      = NULL,
  inherit.blank  = FALSE
) {

  structure(list(
    colour         = colour,
    linewidth      = linewidth,
    linewidth_head = linewidth_head,
    linewidth_fins = linewidth_fins,
    stroke_colour  = stroke_colour,
    stroke_width   = stroke_width,
    arrow_head     = arrow_head,
    arrow_fins     = arrow_fins,
    arrow_mid      = arrow_mid,
    length         = length,
    length_head    = length_head,
    length_fins    = length_fins,
    length_mid     = length_mid,
    resect         = resect,
    resect_head    = resect_head,
    resect_fins    = resect_fins,
    justify        = justify,
    force_arrow    = force_arrow,
    mid_place      = mid_place,
    lineend        = lineend,
    linejoin       = linejoin,
    linemitre      = linemitre,
    inherit.blank  = inherit.blank
  ), class = c("element_arrow", "element_line", "element"))
}

#' @export
#' @method element_grob element_arrow
element_grob.element_arrow <- function(
  element, x = 0:1, y = 0:1,
  colour      = NULL, stroke_colour  = NULL, stroke_width   = NULL,
  linewidth   = NULL, linewidth_head = NULL, linewidth_fins = NULL,
  arrow_head  = NULL, arrow_fins     = NULL, arrow_mid      = NULL,
  length_head = NULL, length_fins    = NULL, length_mid     = NULL,
  resect_head = NULL, resect_fins    = NULL, resect         = NULL,
  justify     = NULL, force_arrow    = NULL, mid_place      = NULL,
  lineend     = NULL, linejoin       = NULL, linemitre      = NULL,
  length = NULL, default.units = "npc", id = NULL, id.lengths = NULL,
  ...
) {
  keep <- rep(TRUE, length(x))
  if (!is.unit(x)) {
    keep <- is.finite(x) & keep
  }
  if (!is.unit(y)) {
    keep <- is.finite(y) & keep
  }

  # Fill in width defaults
  width      <- linewidth      %||% element$linewidth      %||% 1
  width_head <- linewidth_head %||% element$linewidth_head %||% width
  width_fins <- linewidth_fins %||% element$linewidth_fins %||% width

  # Resolve id, dropping empty groups
  id <- rle_subset(validate_id(id, id.lengths, length(x)), keep)
  id <- id[field(id, "length") > 0]
  id.lengths <- field(id, "length")

  # Interpolate width from start to end
  width_prop <- seq_len(rle_length(id)) - rep(rle_start(id), id.lengths)
  width_prop <- width_prop / rep(id.lengths - 1, id.lengths)
  width_head <- rep(rep(width_head, length.out = length(id)), id.lengths)
  width_fins <- rep(rep(width_fins, length.out = length(id)), id.lengths)
  width      <- width_prop * width_head + (1 - width_prop) * width_fins

  # Fill in resect defaults
  resect      <- resect      %||% element$resect      %||% 0
  resect_head <- resect_head %||% element$resect_head %||% resect
  resect_fins <- resect_fins %||% element$resect_fins %||% resect

  # Fill in length defaults
  length      <- length      %||% element$length      %||% 4
  length_head <- length_head %||% element$length_head %||% length
  length_fins <- length_fins %||% element$length_fins %||% length
  length_mid  <- length_mid  %||% element$length_mid  %||% length
  if (!is.unit(length_head)) {
    length_head <- length_head * width_head[rle_end(id)]
  }
  if (!is.unit(length_fins)) {
    length_fins <- length_fins * width_fins[rle_start(id)]
  }

  grob_arrow(
    x = as_unit(x[keep], default.units),
    y = as_unit(y[keep], default.units),
    id = id,
    arrow_head  = arrow_head  %||% element$arrow_head,
    arrow_fins  = arrow_fins  %||% element$arrow_fins,
    arrow_mid   = arrow_mid   %||% element$arrow_mid,
    length_head = length_head,
    length_fins = length_fins,
    length_mid  = length_mid  %||% element$length_mid  %||% 4,
    justify     = justify     %||% element$justify     %||% 0,
    force_arrow = force_arrow %||% element$force_arrow,
    mid_place   = mid_place   %||% element$mid_place,
    shaft_width = unit(width * .pt / .stroke, "mm"),
    resect_fins = as_unit(resect_fins, "mm"),
    resect_head = as_unit(resect_head, "mm"),
    gp = gpar(
      col  = stroke_colour  %||% element$stroke_colour %||% NA,
      fill = colour         %||% element$colour        %||% "black",
      lwd  = stroke_width   %||% element$stroke_width  %||% 0.25,
      linejoin = linejoin   %||% element$linejoin      %||% "round",
      lineend  = lineend    %||% element$lineend       %||% "butt",
      linemitre = linemitre %||% element$linemitre     %||% 10
    )
  )
}
