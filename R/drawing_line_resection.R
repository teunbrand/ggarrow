# Line resection ----------------------------------------------------------

#' Resecting lines
#'
#' Trims pieces off the line endings to make room for arrowheads or because
#' so instructed.
#'
#' @param x,y A `numeric` vector of the same length for polyline coordinates.
#' @param id A `<vctrs_group_rle>` object giving the grouping of `x` and `y`.
#'   The run-lengths should add up to `length(x)`.
#' @param end,begin A positive `numeric` distance to cut off the beginning or
#'   end of the line. Should have the same length as `id` has runs.
#'
#' @return
#' A `<list>` with the following components:
#'   * `x`: The new x-coordinates
#'   * `y`: The new y-coordinates
#'   * `id`: Updated grouping. May contain `NA` groups for lines that have
#'           been resected in their entirety.
#'   * `angle` A list with `first` and `last` elements, giving the angles
#'             in radians of the resected lines. Every element is parallel to
#'             `id`.
#' @noRd
#' @examples
#' new <- resect_line(
#'   x = c(0, 1, 0, 1),
#'   y = c(0, 1, 1, 0),
#'   id = vec_group_rle(c(1, 1, 2, 2)),
#'   end = c(0.3, 0.1),
#'   begin = c(0.5, 0.2)
#' )
#' grid.polyline(new$x, new$y, rle_inv(new$id))
resect_line <- function(x, y, id, end, begin) {

  # Initialise line and angles
  line      <- list(x = x, y = y, id = id)
  end_angle <- begin_angle <- NULL

  # Don't resect negative numbers
  end   <- pmax(end, 0)
  begin <- pmax(begin, 0)

  line <- resect_end(line$x, line$y, line$id, end)
  end_angle <- line$angle

  line <- resect_start(line$x, line$y, line$id, begin)
  begin_angle <- line$angle

  line$angle <- list(first = begin_angle, last = end_angle)
  line
}

# The actual workhorse function for line resection:
resect_end <- function(x, y, id, resect) {

  # Calculate default angle
  id_end <- rle_end(id)
  angle  <- atan2(y[id_end] - y[id_end - 1], x[id_end] - x[id_end - 1])

  # Early exit when no resection needs to be performed
  if (!any(resect > 0)) {
    ans <- list(x = x, y = y, id = id, angle = angle)
    return(ans)
  }

  resect   <- abs(resect)
  id_start <- rle_start(id)
  id_leng  <- field(id, "length")

  # Find runs that are within resection distance
  dist   <- dist_to_end(x, y, id_end, id_leng)
  resect <- rep.int(resect, id_leng)
  cut    <- dist < resect
  runs   <- vec_group_rle(data_frame0(x = cut, y = rle_inv(id)))
  final  <- rle_end(runs)
  cut    <- cut[final]

  if (all(cut)) {
    # Early exit if all values are within range
    id <- new_rle(lengths = field(id, "length"))
    field(id, "group") <- rep(NA, length(id))
    ans <- list(x = x, y = y, id = id, angle = angle)
    return(ans)
  }

  first <- rle_start(runs)[cut]
  final <- final[cut]

  if (any(id_start %in% first)) {
    # This happens when an entire line is within distance
    # Mark line as defunct and exclude its starting point from calculations
    delete <- which(id_start %in% first)
    field(id, "group")[delete] <- rep(NA, length(delete))
    keep  <- !(first %in% id_start)
    first <- first[keep]
    final <- final[keep]
    cut   <- cut[final]
  } else {
    keep <- rep(TRUE, length(first))
  }

  if (length(first) > 0) {

    # Interpolate points
    resect <- resect[first]
    dist   <- (resect - dist[first - 1]) / (dist[first] - dist[first - 1])
    newx   <- x[first - 1] * (1 - dist) + x[first] * dist
    newy   <- y[first - 1] * (1 - dist) + y[first] * dist
    angle[which(keep)] <- atan2(y[final] - newy, x[final] - newx)

    # Mark out-of-bound points as NA
    oob <- which(first != final)
    if (length(oob)) {
      # first  <- intersect(first, oob)
      oob    <- unlist0(Map(`:`, intersect(first, oob), intersect(final, oob)))
      x[oob] <- NA
      y[oob] <- NA
    }

    # Replace with intersecting point
    x[first] <- newx
    y[first] <- newy

    # Throw out NA (out-of-bounds point)
    oob <- is.na(x)
    x   <- x[!oob]
    y   <- y[!oob]
    id  <- rle_subset(id, !oob)
  }

  list(
    x  = x,
    y  = y,
    id = id,
    angle = angle
  )
}

# Resecting the start is the same as resecting the end in reverse
resect_start <- function(x, y, id, resect) {
  if (!any(resect > 0)) {
    start <- rle_start(id)
    ans <- list(
      x = x, y = y, id = id,
      angle = atan2(y[start] - y[start + 1], x[start] - x[start + 1])
    )
    return(ans)
  }
  line   <- resect_end(rev(x), rev(y), rev(id), rev(resect))
  line[] <- lapply(line, rev)
  line
}

# Helpers -----------------------------------------------------------------

dist_to_end <- function(x, y, end, length) {
  dist_length(x - rep.int(x[end], length), y - rep.int(y[end], length))
}
