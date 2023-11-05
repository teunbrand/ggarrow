# Constructor -------------------------------------------------------------

#' Arrow grob
#'
#' Creates a graphical object that draws arrows. An arrow typically consists of
#' three parts: the arrowhead, the shaft and fins. Relative to how an arrow
#' is drawn from coordinates, these three parts describe the end, middle and
#' beginning of an arrow line.
#'
#' @inheritParams grid::polylineGrob
#' @param arrow_head,arrow_fins,arrow_mid A `<matrix[n, 2]>`, such as those
#'   returned by [arrow ornament][arrow_ornaments] functions, giving arrow
#'   shapes. The matrix can (should) have the `notch_angle` attribute that will
#'   be used to fuse the shaft to the arrow ornaments. If `NULL`, no ornament
#'   will be drawn.
#' @param length_head,length_fins,length_mid A [`<unit>`][grid::unit] object
#'   controlling the size of the arrow ornaments.
#' @param shaft_width A [`<unit>`][grid::unit] object controlling the width
#'   of the arrow's shaft.
#' @param resect,resect_fins,resect_head A [`<unit>`][grid::unit] object that
#'   can be used to create an offset between the endings of the coordinates
#'   and where the arrow will be displayed visually. `resect_fins` and
#'   `resect_head` control this offset at the start and end of the arrow
#'   respectively and both default to `resect`.
#' @param force_arrow A `logical(1)` which, if `TRUE` an arrow will be drawn
#'   even when the length of the arrow is shorter than the arrow heads and fins.
#'   If `FALSE`, will drop such arrows.
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
#'
#' @return A `<arrow_path>` [graphical object][grid::grob].
#' @export
#'
#' @examples
#' requireNamespace("grid")
#'
#' # Creating an arrow
#' arrow <- grob_arrow(
#'   x = unit(c(0.2, 0.5, 0.8, 0.2, 0.5, 0.8), "npc"),
#'   y = unit(c(0.2, 0.8, 0.2, 0.8, 0.2, 0.8), "npc"),
#'   id.lengths  = c(3, 3),
#'   arrow_head  = arrow_head_wings(),
#'   arrow_fins  = arrow_fins_feather(),
#'   length_fins = 8,
#'   shaft_width = 1,
#'   gp = grid::gpar(fill = c("dodgerblue", "tomato"), col = "black")
#' )
#'
#' # Drawing the arrow
#' grid::grid.newpage(); grid::grid.draw(arrow)
grob_arrow <- function(
  x = unit(c(0, 1), "npc"),
  y = unit(c(0, 1), "npc"),
  id            = NULL,
  id.lengths    = NULL,
  arrow_head    = arrow_head_wings(),
  arrow_fins    = NULL,
  arrow_mid     = NULL,
  length_head   = unit(5, "mm"),
  length_fins   = NULL,
  length_mid    = NULL,
  justify       = 0,
  shaft_width   = unit(1, "mm"),
  mid_place     = 0.5,
  resect        = unit(0, "mm"),
  resect_fins   = NULL,
  resect_head   = NULL,
  force_arrow   = FALSE,
  default.units = "mm",
  name = NULL,
  gp   = gpar(),
  vp   = NULL
) {
  force(arrow_head)

  id <- validate_id(id, id.lengths, length(x))
  resect <- validate_resect(resect, resect_head, resect_fins, default.units, id)
  n <- length(id)
  arrow_head <- validate_ornament(arrow_head, n)
  arrow_fins <- validate_ornament(arrow_fins, n)
  arrow_mid  <- validate_ornament(arrow_mid,  n)

  grid::gTree(
    x = as_unit(x, default.units),
    y = as_unit(y, default.units),
    id_rle      = id,
    arrow_head  = arrow_head,
    arrow_fins  = arrow_fins,
    arrow_mid   = arrow_mid,
    shaft_width = as_unit(shaft_width, default.units),
    length_head = as_unit(length_head, default.units),
    length_fins = as_unit(length_fins %||% length_head, default.units),
    length_mid  = length_mid %||% as_unit(length_head,  default.units),
    justify     = pmax(pmin(justify, 1), 0),
    mid_place   = mid_place,
    resect      = resect,
    force_arrow = isTRUE(force_arrow),
    name = name,
    gp   = gp,
    vp   = vp,
    cl   = "arrow_path"
  )
}

# Draw method -------------------------------------------------------------

#' @export
makeContent.arrow_path <- function(x) {

  # Extract parameters
  id     <- x$id_rle
  xmm    <- as_mm(x$x, "x")
  ymm    <- as_mm(x$y, "y")
  width  <- along_rle(as_mm(x$shaft_width), id)

  # Set arrow ornaments
  resect <- lapply(x$resect, as_mm)
  head <- resolve_ornament(
    x$arrow_head, x$length_head, id, width, resect$head, type = "head"
  )
  fins <- resolve_ornament(
    x$arrow_fins, x$length_fins, id, width, resect$fins, type = "fins"
  )

  # Trim line to make place for arrow pieces
  resect$fins <- resect$fins + fins$resect * (1 - x$justify)
  resect$head <- resect$head + head$resect * (1 - x$justify)
  line <- resect_line(xmm, ymm, id, resect$head, resect$fins, width)

  # Extrude and notch path
  shaft <- shape_shaft(
    line$x, line$y, line$id, line$width,
    head$angle, fins$angle, line$angle,
    x$gp
  )

  # Place arrow pieces
  head <- place_arrow(
    head$ornament, line$x, line$y, line$id,
    head$scale, line$angle$last,
    type = "head", force = x$force_arrow %||% FALSE
  )
  fins <- place_arrow(
    fins$ornament, line$x, line$y, line$id,
    fins$scale, line$angle$first,
    type = "fins", force = x$force_arrow %||% FALSE
  )
  inner <- resolve_inner(
    x$arrow_mid, x$length_mid,
    line$x, line$y, line$id, width,
    placement = x$mid_place
  )

  # Finish arrow
  arrow <- combine_arrow(head, fins, shaft, inner)

  if (length(arrow$x) == 0) {
    ans <- gList(zeroGrob())
  } else {
    ans <- gList(pathGrob(
      x = unit(arrow$x, "mm"),
      y = unit(arrow$y, "mm"),
      id.lengths = arrow$id,
      pathId.lengths = arrow$path_id,
      rule = "evenodd",
      gp = drop_gp(x$gp, line$id)
    ))
  }

  setChildren(x, ans)
}
