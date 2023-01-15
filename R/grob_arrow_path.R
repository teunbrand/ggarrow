# Constructor -------------------------------------------------------------

#' Arrow grob
#'
#' Creates a graphical object that draws arrows. An arrow typically consists of
#' three parts: the arrowhead, the shaft and fins. Relative to how an arrow
#' is drawn from coordinates, these three parts describe the end, middle and
#' beginning of an arrow line.
#'
#' @inheritParams grid::polylineGrob
#' @param arrow_head,arrow_fins A `<matrix[n, 2]>` giving arrow shapes.
#'   The matrix can have the `notch_angle` attribute that will be used to fuse
#'   the shaft to the arrow ornaments. If `NULL`, no ornament will be drawn.
#' @param head_length,fins_length A `<`[unit][grid::unit]`>` object controlling
#'   the size of the arrow ornaments.
#' @param shaft_width A `<`[unit][grid::unit]`>` object controlling the width
#'   of the arrow's shaft.
#' @param resect,resect_fins,resect_head A `<`[unit][grid::unit]`>` object that
#'   can be used to create an offset between the endings of the coordinates
#'   and where the arrow will be displayed visually. `resect_fins` and
#'   `resect_head` control this offset at the start and end of the arrow
#'   respectively and default to `resect`.
#' @param force_arrow A `logical(1)` which, if `TRUE`, will draw an arrow even
#'   when the entire shaft is resected. If `FALSE`, will not draw an arrow if
#'   a shaft has disappeared.
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
  head_length   = unit(5, "mm"),
  fins_length   = unit(5, "mm"),
  shaft_width   = unit(1, "mm"),
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
    shaft_width = as_unit(shaft_width, default.units),
    head_length = as_unit(head_length, default.units),
    fins_length = as_unit(fins_length, default.units),
    resect      = resect,
    force_arrow = force_arrow,
    name = name,
    gp   = gp,
    vp   = vp,
    cl   = "arrow_path"
  )
}

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
  line <- resect_line(xmm, ymm, id, resect$head, resect$fins)
  line <- resect_line(line$x, line$y, line$id, head$length, fins$length)

  # Extrude and notch path
  shaft <- shape_shaft(
    line$x, line$y, line$id, width,
    head$angle, fins$angle, line$angle,
    x$gp
  )

  # Place arrow pieces
  head <- place_arrow(
    head$ornament, line$x, line$y, line$id, head$length, line$angle$last,
    type = "head", force = x$force_arrow %||% FALSE
  )
  fins <- place_arrow(
    fins$ornament, line$x, line$y, line$id, fins$length, line$angle$first,
    type = "fins", force = x$force_arrow %||% FALSE
  )

  # Finish arrow
  arrow <- combine_arrow(head, fins, shaft)

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
