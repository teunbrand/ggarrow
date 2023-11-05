# Constructor -------------------------------------------------------------

#' Arrow curve grob.
#'
#' Creates a graphical object that draws curved arrows.
#'
#' @inheritParams grid::curveGrob
#' @inheritDotParams grob_arrow -shaft_width
#' @param width_fins,width_head A [`<unit>`][grid::unit] object controlling the
#'   width of the arrow's shaft at the `(x1, y1)` and `(x2, y2)` location
#'   respectively.
#'
#' @return A `<curve_arrow>` [graphical object][grid::grob].
#' @export
#'
#' @examples
#' requireNamespace("grid")
#'
#' # Creating the curved arrow
#' grob <- grob_arrow_curve(
#'   x1 = unit(0.25, "npc"), y1 = unit(0.25, "npc"),
#'   x2 = unit(0.75, "npc"), y2 = unit(0.75, "npc"),
#'   angle = 90, curvature = 0.5, ncp = 5,
#'   arrow_head = arrow_head_line()
#' )
#'
#' # Drawing the arrow
#' grid::grid.newpage(); grid::grid.draw(grob)
grob_arrow_curve <- function(
  x1, y1, x2, y2,
  default.units = "mm",
  curvature     = 1,
  angle         = 90,
  ncp           = 1,
  shape         = 0.5,
  square        = TRUE,
  squareShape   = 1,
  inflect       = FALSE,
  open          = TRUE,
  name          = NULL,
  gp            = gpar(),
  vp            = NULL,
  ...,
  width_head    = unit(1, "mm"),
  width_fins    = unit(1, "mm")
) {

  params <- list(..., width_head = width_head, width_fins = width_fins, gp = gp)

  curve_grob <- curveGrob(
    x1, y1, x2, y2,
    default.units = default.units,
    curvature     = curvature,
    angle         = angle,
    ncp           = ncp,
    shape         = shape,
    square        = square,
    squareShape   = squareShape,
    inflect       = inflect,
    open          = open
  )

  gTree(
    curve  = curve_grob,
    params = params,
    name   = name,
    vp     = vp,
    cl     = "curve_arrow"
  )
}

# Draw method -------------------------------------------------------------

#' @export
makeContent.curve_arrow <- function(x) {
  # Extract relevant bits
  params <- x$params
  curve  <- x$curve
  curve  <- makeContent(curve)$children[[1]]

  # Get points from curve
  if (inherits(curve, "xspline")) {
    pts <- xsplinePoints(curve)
    pts <- if (all(c("x", "y") %in% names(pts))) list(pts) else pts
    xx  <- do.call(unit.c, lapply(pts, `[[`, i = "x"))
    yy  <- do.call(unit.c, lapply(pts, `[[`, i = "y"))
    id  <- vapply(pts, function(x) length(x$x), integer(1))
    id  <- rep(seq_along(id), id)
  } else if (inherits(curve, "segments")) {
    xx <- unit.c(curve$x0, curve$x1)
    yy <- unit.c(curve$y0, curve$y1)
    i  <- seq_along(curve$x0)
    id <- rep(i, each = 2)
    i  <- `dim<-`(rbind(i, i + max(i), deparse.level = 0), NULL)
    xx <- xx[i]
    yy <- yy[i]
  }

  rleid <- new_rle(id)
  leng  <- field(rleid, "length")
  start <- rle_start(rleid)
  end   <- rle_end(rleid)

  # Interpolate width along arc-length
  arc <- arc_length(as_mm(xx), as_mm(yy), start = start, length = leng)
  norm <- arc / arc[rep.int(end, leng)]
  width_head <- rep(as_mm(params$width_head %||% 1), leng)
  width_fins <- rep(as_mm(params$width_fins %||% 1), leng)
  width <- unit(width_fins * (1 - norm) + width_head * norm, "mm")

  # Remove old width parametrisation
  params$width_head <- NULL
  params$width_fins <- NULL

  # Build and evaluate arrow grob
  grob <- inject(grob_arrow(
    x = xx, y = yy, id = id, shaft_width = width,
    !!!params
  ))
  grob <- makeContent(grob)

  setChildren(x, gList(grob$children[[1]]))
}
