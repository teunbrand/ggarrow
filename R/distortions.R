# Distortion functions ----------------------------------------------------

#' Line distortion functions
#'
#' These are functions that build a pattern that can be used as the `distort`
#' argument in arrow geoms. They produce one 'oscillation' that will be repeated
#' to distort a line.
#'
#' @param length A positive `numeric(1)` setting the length along the path in
#'   millimetres that one repetition of the distortion occupies. Actual length
#'   may end up slightly wider to cover a remainder length.
#' @param width A `numeric(1)` setting the width (amplitude) of the distortion
#'   in millimetres. Negative values can be used to flip the distortion.
#' @param n An `integer(1)` setting the number of vertices to use for the path.
#'
#' @returns A `<matrix[n, 2]>` with `x` and `y` columns describing a path.
#'   It may have a `size` attribute.
#' @name distortion_functions
#'
#' @details
#' It is possible to write your own `distort_*()` function. The assumption
#' is that an oscillation starts at the `(0, 0)` coordinate and ends at the
#' `(length, 0)` coordinate.
#'
#' The exact length of an oscillation will be fitted based on the arc-length of
#' the path it will be placed on. If the arc-length will not accommodate an
#' exact integer number of oscillations, the oscillations will be stretched to
#' cover the whole arc-length.
#'
#' @examples
#' plot(distort_sinewave(), type = 'l')
NULL

#' @export
#' @describeIn distortion_functions
#' Follows a sine waveform.
distort_sinewave <- function(length = 4.0, width = 2.0, n = 30L) {
  check_number_whole(n, min = 1.0)
  length <- abs(length)
  x <- seq(0.0, 2.0 * pi, length.out = n + 1L)[-(n + 1L)]
  y <- sin(x) * width * 0.5
  x <- x * length / (2.0 * pi)
  out <- cbind(x = x, y = y)
  attr(out, "size") <- length
  out
}

#' @export
#' @describeIn distortion_functions
#' Follows a triangular waveform.
distort_sawtooth <- function(length = 4.0, width = 2.0) {
  length <- abs(length)
  x <- c(0.0, 0.25, 0.75, 1.0) * length
  y <- c(0.0, 1.0, -1.0, 0.0) * width * 0.5
  out <- cbind(x = x, y = y)
  attr(out, "size") <- length
  out
}

#' @export
#' @describeIn distortion_functions
#' Follows a square waveform.
distort_squarewave <- function(length = 4.0, width = 2.0) {
  length <- abs(length)
  x <- c(0.0, 0.0, 0.5, 0.5, 1.0, 1.0) * length
  y <- c(0.0, 0.5, 0.5, -0.5, -0.5, 0.0) * width
  out <- cbind(x = x, y = y)
  attr(out, "size") <- length
  out
}

# Helpers -----------------------------------------------------------------

validate_distortion <- function(
  distortion,
  arg = caller_arg(distortion),
  call = caller_env()
) {
  if (is.null(distortion)) {
    return(distortion)
  }

  # Resolve character distortions
  if (is.character(distortion) && !is.matrix(distortion)) {
    pattern <- paste0("distort_", distortion)
    fun <- get0(pattern, envir = global_env(), mode = "function") %||%
      get0(pattern, envir = asNamespace("ggarrow"), mode = "function")
    if (!is.function(fun)) {
      cli::cli_abort(
        "Cannot find function {.fun {pattern}} to draw distortions.",
        call = call
      )
    }
    distortion <- fun()
  }

  # Check matrix distortions
  if (!is.matrix(distortion)) {
    cli::cli_abort(
      "{.arg {arg}} must be a {.cls matrix}, \\
      not {.obj_type_friendly {distortion}}.",
      call = call
    )
  }
  if (!typeof(distortion) %in% c("integer", "double")) {
    cli::cli_abort(
      "{.arg {arg}} must have a numeric type, \\
      not {.cls {typeof(distortion)}} type.",
      call = call
    )
  }
  dim <- dim(distortion)
  if (prod(dim) == 0.0) {
    cli::cli_abort(
      "{.arg {arg}} cannot be empty.",
      call = call
    )
  }
  if (dim[2L] != 2L) {
    cli::cli_abort(
      "{.arg {arg}} must have exactly 2 columns, not {dim[2]}.",
      call = call
    )
  }
  len <- attr(distortion, "size") %||% diff(range(distortion[, 1L]))
  if (zero_range(c(0.0, len))) {
    cli::cli_abort(
      "The length of the {.arg {arg}} argument is too close to 0.",
      call = call
    )
  }
  distortion
}

project_distortion <- function(line, distort) {
  if (empty(distort) ||
      zero_range(range(distort[, 2L])) ||
      zero_range(range(distort[, 1L]))) {
    return(dedup_line(line))
  }

  size_distort <- attr(distort, "size") %||% diff(range(distort[, 1L]))
  n_vertex <- nrow(distort)

  x <- line$x
  y <- line$y
  w <- line$width

  # Reduce 2D x/y to 1D metric along arc-length
  arc_length <- arc_length(x, y)
  arc_starts <- arc_length[rle_start(line$id)]
  arc_ranges <- arc_length[rle_end(line$id)] - arc_starts

  # Figure out how many times we can fit the distortion along the arc-length
  n_distort <- pmax(arc_ranges %/% size_distort, 1L)
  # Adjust size so we can fit a whole number of oscillations
  size_fit <- arc_ranges / n_distort

  # compute distortion vertices projected on arc-length
  vertices <-
    # Lay down oscillations for all paths scaled lengthwise to 0-1
    rep(distort[, 1L] / size_distort, sum(n_distort)) *
    # Rescale these to the fitted size
    rep(rep.int(size_fit, n_distort), each = n_vertex) +
    # Increment oscillations within each path
    rep(
      (sequence(n_distort) - 1L) * rep(size_fit, n_distort),
      each = n_vertex
    ) +
    # Add path offsets to oscillations
    rep(arc_starts, n_distort * n_vertex)

  # Interpolate x/y/width at distortion vertices
  # NOTE: `all.inside` does *NOT* apply within each path, but maybe it should?
  before <- findInterval(vertices, arc_length, all.inside = TRUE)
  delta <- interpol_dist(vertices, before, arc_length)
  after <- before + 1L
  x <- x[before] * (1.0 - delta) + x[after] * delta
  y <- y[before] * (1.0 - delta) + y[after] * delta
  w <- w[before] * (1.0 - delta) + w[after] * delta

  # Radiate out the y-dimension of the distortion
  dy <- rep(distort[, 2L], sum(n_distort))
  angle <- xy_angle(line$x, line$y, norm = TRUE)[before]
  line$x <- x + cos(angle) * dy
  line$y <- y + sin(angle) * dy
  line$id <- new_rle(lengths = n_distort * n_vertex)
  line$width <- w
  line
}
