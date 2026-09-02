
.rad2deg <- 180.0 / pi
.deg2rad <- pi / 180.0
.halfpi  <- pi / 2.0
.twopi   <- 2.0 * pi

xy_angle <- function(x, y, norm = FALSE) {
  atan2(diff(y), diff(x)) + .halfpi * as.numeric(norm)
}
rotation_matrix <- function(angle) {
  cos   <- cos(angle)
  sin   <- sin(angle)
  value <- vec_interleave(cos, sin, -sin, cos)
  array(value, dim = c(2L, 2L, length(angle)))
}

# Takes xy as a list of xy-coordinates and vectorised angle / scale
rotate_scale <- function(xy, angle = 0.0) {
  if (!is.list(xy)) {
    xy <- list(xy)
  }
  if (length(xy) == 1L && length(angle) > 1L) {
    # Recycle xy to match angle
    xy <- rep(xy, length(angle))
  }

  rot   <- rotation_matrix(angle)
  dim   <- dim(rot)

  Map(tcrossprod, x = xy, y = lapply(seq_len(dim[3L]), function(i) rot[, , i]))
}

scale_translate <- function(xy_list, x_offset, y_offset, scale) {
  Map(
    function(xy, x, y, s) {
      xy * s + c(rep(x, nrow(xy)), rep(y, nrow(xy)))
    },
    xy = xy_list, x = x_offset, y = y_offset, s = scale
  )
}

dist_length <- function(x, y) {
  sqrt(x ^ 2.0 + y ^ 2.0)
}

arc_length <- function(x, y, start = 1L, length = NULL) {
  dist   <- c(0.0, dist_length(diff(x), diff(y)))
  dist[start] <- 0.0
  dist <- cumsum(dist)
  if (!is.null(length)) {
    dist - dist[rep.int(start, length)]
  } else {
    dist
  }
}

norm_angle <- function(angle) angle %% .twopi
