# Constructor -------------------------------------------------------------

#' Arrow grob
#'
#' Creates a graphical object that draws arrows. An arrow typically consists of
#' three parts: the arrowhead, the shaft and fins. Relative to how an arrow
#' is drawn from coordinates, these three parts describe the end, middle and
#' beginning of an arrow line.
#'
#' @inheritParams grid::polylineGrob
#' @param arrow_head,arrow_fins A `<matrix[n, 2]>`, such as those returned by
#'   [arrow ornament][arrow_ornaments] functions, giving arrow shapes.
#'   The matrix can (should) have the `notch_angle` attribute that will be used
#'   to fuse the shaft to the arrow ornaments. If `NULL`, no ornament will be
#'   drawn.
#' @param head_length,fins_length A `<`[unit][grid::unit]`>` object controlling
#'   the size of the arrow ornaments.
#' @param shaft_width A `<`[unit][grid::unit]`>` object controlling the width
#'   of the arrow's shaft.
#' @param resect,resect_fins,resect_head A `<`[unit][grid::unit]`>` object that
#'   can be used to create an offset between the endings of the coordinates
#'   and where the arrow will be displayed visually. `resect_fins` and
#'   `resect_head` control this offset at the start and end of the arrow
#'   respectively and both default to `resect`.
#' @param force_arrow A `logical(1)` which, if `TRUE` an arrow will be drawn
#'   even when the length of the arrow is shorter than the arrow heads and fins.
#'   If `FALSE`, will drop such arrows.
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
#'   fins_length = 8,
#'   shaft_width = 1,
#'   gp = gpar(fill = c("dodgerblue", "tomato"), col = "black")
#' )
#'
#' # Drawing the arrow
#' grid.newpage(); grid.draw(arrow)
grob_arrow <- function(
  x = unit(c(0, 1), "npc"),
  y = unit(c(0, 1), "npc"),
  id            = NULL,
  id.lengths    = NULL,
  arrow_head    = arrow_head_wings(),
  arrow_fins    = NULL,
  arrow_inner   = NULL,
  head_length   = unit(5, "mm"),
  fins_length   = NULL,
  inner_length  = NULL,
  shaft_width   = unit(1, "mm"),
  inner_just    = 0.5,
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

  grid::gTree(
    x = as_unit(x, default.units),
    y = as_unit(y, default.units),
    id_rle      = id,
    arrow_head  = arrow_head,
    arrow_fins  = arrow_fins,
    arrow_inner = arrow_inner,
    shaft_width = as_unit(shaft_width, default.units),
    head_length = as_unit(head_length, default.units),
    fins_length = as_unit(fins_length %||% head_length, default.units),
    inner_length = inner_length %||% as_unit(head_length, default.units),
    inner_just   = inner_just,
    resect      = resect,
    force_arrow = force_arrow,
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
  head <- resolve_ornament(x$arrow_head, x$head_length, id, width, "head")
  fins <- resolve_ornament(x$arrow_fins, x$fins_length, id, width, "fins")

  # Trim line to make place for arrow pieces
  resect <- lapply(x$resect, as_mm)
  resect$fins <- resect$fins + fins$resect
  resect$head <- resect$head + head$resect
  line <- resect_line(xmm, ymm, id, resect$head, resect$fins)

  # Inner arrows
  inner <- resolve_inner(
    x$arrow_inner, x$inner_length,
    line$x, line$y, line$id, width,
    placement = x$inner_just
  )

  # Extrude and notch path
  shaft <- shape_shaft(
    line$x, line$y, line$id, width,
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
