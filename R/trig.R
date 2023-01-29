
.rad2deg <- 180 / pi
.deg2rad <- pi / 180
.halfpi  <- pi / 2
.twopi   <- 2 * pi

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
rotate_scale <- function(xy, angle = 0) {
  if (!is.list(xy)) {
    xy <- list(xy)
  }
  if (length(xy) == 1 && length(angle) > 1) {
    # Recycle xy to match angle
    xy <- rep(xy, length(angle))
  }

  rot   <- rotation_matrix(angle)
  dim   <- dim(rot)

  Map(tcrossprod, x = xy, y = lapply(seq_len(dim[3]), function(i) rot[, , i]))
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
  sqrt(x ^ 2 + y ^ 2)
}

arc_length <- function(x, y, start, length) {
  dist   <- c(0, dist_length(diff(x), diff(y)))
  dist[start] <- 0
  dist <- cumsum(dist)
  dist - dist[rep.int(start, length)]
}

norm_angle <- function(angle) angle %% .twopi
