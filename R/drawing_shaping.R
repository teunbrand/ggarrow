shape_shaft <- function(
  x, y, id, width,
  last_angle = NULL, first_angle = NULL, angle_line,
  gp = gpar()
) {

  valid <- rle_valid(id)

  if (any(!valid)) {
    if (all(!valid)) {
      return(NULL)
    }
    keep <- rep(valid, field(id, "length"))
    retry <- shape_shaft(
      x = x[keep], y = y[keep], id = id[valid], width = width[keep],
      last_angle = last_angle, first_angle = first_angle,
      angle_line = list(
        first = angle_line$first[valid],
        last  = angle_line$last[valid]
      )
    )
    ans <- vector("list", length(id))
    ans[valid] <- retry
    return(ans)
  }

  offset <- extrude_line(x, y, id, width, gp = gp)

  offset <- notch_shaft(
    offset, x, y, id, width,
    angle_line$last, last_angle,
    type = "head"
  )

  offset <- notch_shaft(
    offset, x, y, id, width,
    angle_line$first, first_angle,
    type = "fins"
  )

  close_offset(
    offset, x, y, angle_line,
    is.na(first_angle), is.na(last_angle),
    id, gp
  )
}

notch_shaft <- function(
  offset, x, y, id, width,
  angle_line, angle_notch,
  type = "head", buffer = 0.01
) {
  if (all(is.na(angle_notch)) || is.null(angle_notch)) {
    return(offset)
  }
  i <- !is.na(angle_notch)
  angle <- angle_line[i] + pi
  angle_notch <- angle_notch[i]

  if (type == "head") {
    index_fun <- rle_end
    positive  <- angle - angle_notch - buffer
    negative  <- angle + angle_notch + buffer
  } else {
    index_fun <- rle_start
    positive  <- angle + angle_notch + buffer
    negative  <- angle - angle_notch - buffer
  }

  index <- index_fun(id)[i]
  left  <- index_fun(offset$id_left)[i]
  right <- index_fun(offset$id_right)[i]

  leng  <- 0.5 * width[index] / sin(angle_notch)

  offset$x_left[left]   <- x[index] + cos(negative) * leng
  offset$x_right[right] <- x[index] + cos(positive) * leng

  offset$y_left[left]   <- y[index] + sin(negative) * leng
  offset$y_right[right] <- y[index] + sin(positive) * leng

  offset
}

combine_arrow <- function(head, fins, shaft, inner = NULL) {

  arrow <- polygon_union(head,  shaft)
  arrow <- polygon_union(fins,  arrow)
  arrow <- polygon_union(inner, arrow)

  if (is_named(arrow[[1]])) {
    x <- lapply(arrow, `[[`, "x")
    y <- lapply(arrow, `[[`, "y")
    lens <- lengths(x)
  } else {
    # Recursively extract x/y
    x <- lapply(arrow, lapply, `[[`, "x")
    y <- lapply(arrow, lapply, `[[`, "y")
    lens <- lapply(x, lengths)
  }

  list(
    x  = unlist(x, recursive = TRUE, use.names = FALSE),
    y  = unlist(y, recursive = TRUE, use.names = FALSE),
    id = unlist0(lens),
    path_id = vapply(lens, sum, 1L)
  )
}

# Helpers -----------------------------------------------------------------

close_offset <- function(offset, x, y, angle_line, empty_start, empty_end,
                         id, gp) {

  lineend <- gp$lineend %||% "butt"

  end   <- rle_end(id)
  start <- rle_start(id)

  xend <- x[end]
  yend <- y[end]

  xstart <- x[start]
  ystart <- y[start]

  if (any(empty_start) && lineend == "round") {
    cx <- xstart[empty_start]
    cy <- ystart[empty_start]
    angle <- outer(
      norm_angle(angle_line$first[empty_start]),
      seq(-.halfpi, .halfpi, length.out = 30),
      FUN = `+`
    )
    left  <- rle_start(offset$id_left)[empty_start]
    right <- rle_start(offset$id_right)[empty_start]
    width <- dist_length(
      offset$x_left[left] - offset$x_right[right],
      offset$y_left[left] - offset$y_right[right]
    )
    idrep  <- rep(seq_along(id)[empty_start], 30)
    xstart <- as.list(xstart)
    ystart <- as.list(ystart)
    xstart[empty_start] <- split(cx + cos(angle) * width / 2, idrep)
    ystart[empty_start] <- split(cy + sin(angle) * width / 2, idrep)
  }

  if (any(empty_end) && lineend == "round") {
    cx <- xend[empty_end]
    cy <- yend[empty_end]
    angle <- outer(
      norm_angle(angle_line$last[empty_end]),
      seq(-.halfpi, .halfpi, length.out = 30),
      FUN = `+`
    )
    left  <- rle_end(offset$id_left)[empty_end]
    right <- rle_end(offset$id_right)[empty_end]
    width <- dist_length(
      offset$x_left[left] - offset$x_right[right],
      offset$y_left[left] - offset$y_right[right]
    )
    idrep <- rep(seq_along(id)[empty_end], 30)
    xend <- as.list(xend)
    yend <- as.list(yend)
    xend[empty_end] <- split(cx + cos(angle) * width / 2, idrep)
    yend[empty_end] <- split(cy + sin(angle) * width / 2, idrep)
  }

  Map(
    function(xend, yend, xstart, ystart, L, R) {
      list(
        x = c(offset$x_left[L], xend, offset$x_right[R], xstart),
        y = c(offset$y_left[L], yend, offset$y_right[R], ystart)
      )
    },
    xend = xend, yend = yend, xstart = xstart, ystart = ystart,
    L = rle_idx(offset$id_left),
    R = rle_idx(offset$id_right, rev = TRUE)
  )
}

polygon_union <- function(A, B) {
  if (is.null(A)) {
    return(B)
  }
  if (is.null(B)) {
    return(A)
  }
  list <- Map(inner_polygon_union, A = A, B = B)
  named <- vapply(list, is_named, logical(1))
  if (all(named) || all(!named)) {
    return(list)
  }
  list[named] <- lapply(list[named], list)
  list
}

inner_polygon_union <- function(A, B) {
  if (is.null(A)) {
    return(B)
  }
  if (is.null(B)) {
    return(A)
  }
  polyclip::polyclip(A, B, op = "union", fillB = "nonzero", fillA = "nonzero")
}


#' Debug tool for shaft notching
#'
#' This is a low-level tool to debug the notching of arrow ornaments onto
#' arrow shafts.
#'
#' @param ornament An [arrow ornament][arrow_ornaments].
#' @param width A `numeric(1)` value for the line's width.
#'
#' @return A `list` with `x` and `y` elements.
#' @export
#' @keywords internal
#'
#' @examples
#' NULL
debug_notching <- function(ornament, width = 0.2) {
  line <- resect_line(
    x = c(-2, 0), y = c(0, 0), new_rle(lengths = 2),
    end = attr(ornament, "resect")
  )
  shape_shaft(
    x = line$x, y = line$y, id = line$id,
    width = rep(width, 2), angle_line = line$angle,
    last_angle = attr(ornament, "notch_angle")
  )[[1]]
}
