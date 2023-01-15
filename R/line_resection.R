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

  if (!any(resect > 0)) {
    end <- rle_end(id)
    ans <- list(
      x = x, y = y, id = id,
      angle = atan2(y[end] - y[end - 1], x[end] - x[end - 1])
    )
    return(ans)
  }

  resect <- abs(resect)
  start  <- rle_start(id)
  end    <- rle_end(id)
  leng   <- field(id, "length")

  # Distance to end point
  cx <- rep.int(x[end], leng)
  cy <- rep.int(y[end], leng)
  dist <- sqrt((x - cx)^2 + (y - cy)^2)
  angle <- atan2(y[end] - y[end - 1], x[end] - x[end - 1])

  # Find runs that are within 'resect' of end
  long_resect <- rep.int(resect, field(id, "length"))
  rle <- rle(dist < long_resect)

  if (all(rle$values)) {
    # Early exit if all values are within range
    id <- new_rle(lengths = field(id, "length"))
    field(id, "group") <- rep(NA, length(id))
    ans <- list(x = x, y = y, id = id, angle = angle)
    return(ans)
  }

  ends    <- cumsum(rle$lengths)
  starts  <- (ends - rle$lengths + 1)[rle$values]

  if (any(start %in% starts)) {
    # This happens when an entire line is within distance
    # Mark line as defunct and exclude its starting point from calculations
    delete <- which(start %in% starts)
    field(id, "group")[delete] <- rep(NA, length(delete))
    starts <- setdiff(starts, start)
  }

  if (length(starts) > 0) {

    # Interpolate points
    resect <- long_resect[starts]
    d      <- (resect - dist[starts - 1]) / (dist[starts] - dist[starts - 1])
    newx   <- x[starts - 1] * (1 - d) + x[starts] * d
    newy   <- y[starts - 1] * (1 - d) + y[starts] * d
    angle  <- atan2(y[end] - newy, x[end] - newx)

    # Delete out-of-bound points
    oob <- which(starts != end)
    if (length(oob)) {
      starts <- intersect(starts, oob)
      end <- intersect(end, oob)
      oob <- unlist0(Map(`:`, starts, end))
      x[oob] <- NA
      y[oob] <- NA
    }

    # Replace with intersecting point
    x[starts] <- newx
    y[starts] <- newy

    # Throw out NA (out-of-bounds point)
    is_na <- is.na(x)
    x  <- x[!is_na]
    y  <- y[!is_na]
    id <- rle_subset(id, !is_na)
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
