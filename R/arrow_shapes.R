# Ornament functions ------------------------------------------------------

#' Arrow ornament functions
#'
#' There are two types of arrow ornament functions: functions for arrow heads,
#' and functions for arrow fins. The heads and fins can be used interchangeably,
#' but the name makes it clearer what is suitable.
#'
#' @param offset,angle A `numeric(1)` giving an angle in degrees for the angle
#'   between the line and tip.
#' @param inset A `numeric(1)` giving an angle in degrees for the angle inside
#'   the tip of the arrowhead.
#' @param asp A `numeric(1)` ratio between the length of the feathers and the
#'   height of the feathers.
#' @param indent,outdent A `numeric(1)` giving the fraction of the feather
#'   feather length to offset the notch and the end respectively.
#'
#' @details
#' The convention for these functions is that the arrow shaft is fused to the
#' ornament at the (0,0) point and the ornaments ends at the (1,0) point.
#'
#' @return A `<matrix[n, 2]>` with `x` and `y` columns describing a polygon.
#'   It has a `notch_angle` attribute that is used fusing the fins/head to the
#'   shaft of the arrow. They can be given to an arrow plotting function.
#' @name arrow_ornaments
#'
#' @examples
#' # Plotting winged head
#' plot(c(-0.5, 1), c(-0.6, 0.6), type = "n")
#' polygon(arrow_head_wings(), col = "gray")
#'
#' # Plotting feather fins
#' plot(c(0, 1), c(-0.25, 0.25), type = "n")
#' polygon(arrow_fins_feather(), col = "gray")
NULL

#' @export
#' @describeIn arrow_ornaments
#' Places two triangles at either side of the line. Let ABC be a triangle,
#' where A is at the end of the line, B is on the line and C is the arrow
#' wingtip. Then `offset` is the angle at corner A and `inset` is the angle at
#' corner C.
arrow_head_wings <- function(
  offset = 20,
  inset  = 30
) {
  offset <- offset * .deg2rad
  inset  <- inset  * .deg2rad

  wing_angle   <- c(-1, 1) * offset
  inner_angle  <- wing_angle + pi + c(-1, 1) * inset
  inner_length <- sin(offset) / sin(pi - inset - offset)

  wings <- list(
    x = 1 + cos(wing_angle),
    y = 0 + sin(wing_angle)
  )

  inner <- list(
    x = wings$x + cos(inner_angle) * inner_length,
    y = wings$y + sin(inner_angle) * inner_length
  )

  ans <- cbind(
    x = c(1, wings$x[1], inner$x[1], wings$x[2]) - inner$x[1],
    y = c(0, wings$y[1], inner$y[1], wings$y[2])
  )
  mult <- 1 / ans[1]
  attr(ans, "front_angle") <- offset
  attr(ans, "notch_angle") <- offset + inset
  ans * mult
}

#' @export
#' @describeIn arrow_ornaments
#' Places trapezoids at either side of the line. Let ABCD be a quadrilateral
#' shape, where A is at the end of the line, B is on the line, and CD is
#' parallel to AB, but offset from the line. Then, `indent` is the distance
#' along the line between A and D and `outdent` is the distance along the line
#' between B and C.
arrow_fins_feather <- function(
  indent  = 0.3,
  outdent = indent,
  height  = 0.5
) {
  dent <- c(
    pmin(indent, c(0, 0, 0)),
    -pmin(outdent, c(0, 0, 0))
  )[c(1:2, 4:6, 3)]

  x <- c(1 - indent, 1, 0 + outdent, 0, 0 + outdent, 1) + dent
  y <- c(0, 1, 1, 0, -1, -1) * height / 2

  resect <- x[4]
  ans <- cbind(x = x - resect, y = y)
  attr(ans, "front_angle") <- atan2(height, x[3] - x[4])
  angle <- xy_angle(x[c(3, 4, 5)], y[c(3, 4, 5)])
  angle <- norm_angle(diff(angle) + pi) + pi
  attr(ans, "notch_angle") <- angle / 2 + .halfpi
  attr(ans, "resect") <- 1 - resect
  attr(ans, "length") <- 1
  ans
}

#' @export
#' @describeIn arrow_ornaments
#' A line as an arrow head.
arrow_head_line <- function(angle = 30, lineend = "butt") {
  angle <- angle * .deg2rad
  lineend  <- arg_match0(lineend, c("butt", "round", "parallel", "square"))

  function(length, width) {

    if (lineend == "square") {
      length <- length + 0.5 * width
    }

    x <- 1 - cos(angle) * length
    y <- 0 - sin(angle) * length

    norm <- angle + .halfpi
    if (lineend == "parallel") {
      x <- c(x, x + width / sin(angle))
      y <- c(y, y)
    } else if (lineend == "round") {
      cx <- x + cos(norm) * width / 2
      cy <- y + sin(norm) * width / 2
      norm <- seq(norm + pi, norm, length.out = 30)
      x <- c(x, cx + cos(norm) * width / 2)
      y <- c(y, cy + sin(norm) * width / 2)
    } else {
      x <- c(x, x + cos(norm) * width)
      y <- c(y, y + sin(norm) * width)
    }

    n <- length(x)
    next_len <- y[n] / sin(angle)

    x <- c(x, x[n] + cos(angle - pi) * next_len)
    y <- c(y, y[n] + sin(angle - pi) * next_len)

    x <- c(1, x, x[rev(seq_len(n))])
    y <- c(0, y, -y[rev(seq_len(n))])

    ans <- cbind(x = x, y = y)
    attr(ans, "notch_angle") <- angle
    attr(ans, "resect") <- (1 - x[n + 2])
    ans[, "x"] <- ans[, "x"] - x[n + 2]

    ans
  }
}

#' @export
#' @describeIn arrow_ornaments
#' A line as an arrow fin.
arrow_fins_line <- function(angle = 30, lineend = "butt") {
  angle <- pi + angle * .deg2rad
  lineend  <- arg_match0(lineend, c("butt", "round", "parallel", "square"))

  function(length, width) {

    if (lineend == "square") {
      length <- length + 0.5 * width
    }

    x <- 1 - cos(angle) * length
    y <- 0 - sin(angle) * length

    norm <- angle + .halfpi

    if (lineend == "parallel") {
      x <- c(x, x + width / sin(angle))
      y <- c(y, y)
    } else if (lineend == "round") {
      cx <- x + cos(norm) * width / 2
      cy <- y + sin(norm) * width / 2
      norm <- seq(norm + pi, norm, length.out = 30)
      x <- c(x, cx + cos(norm) * width / 2)
      y <- c(y, cy + sin(norm) * width / 2)
    } else {
      x <- c(x, x + cos(norm) * width)
      y <- c(y, y + sin(norm) * width)
    }

    n <- length(x)
    next_len <- y[n] / sin(angle)

    x <- c(x, x[n] + cos(angle - pi) * next_len)
    y <- c(y, y[n] + sin(angle - pi) * next_len)

    x <- c(1, x, x[rev(seq_len(n))])
    y <- c(0, y, -y[rev(seq_len(n))])

    ans <- cbind(x = x, y = y)
    attr(ans, "notch_angle") <- -angle
    attr(ans, "resect") <- max(x) - 1
    ans[, "x"] <- ans[, "x"] - 1
    attr(ans, "length") <- 1

    ans
  }
}

#' @export
#' @describeIn arrow_ornaments
#' This is a 'fake' arrow head who in practice doesn't draw anything, but
#' sets the `notch_angle` attribute such that the arrow shaft is whittled into
#' a triangular point.
arrow_head_minimal <- function(angle = 45) {
  angle <- angle * .deg2rad
  ans <- cbind(x = c(0, 0), y = c(1, -1))
  attr(ans, "notch_angle") <- angle
  ans
}

#' @export
#' @describeIn arrow_ornaments
#' This is a 'fake' arrow head who in practise doesn't draw anything, but
#' sets the `notch_angle` attribute such that a triangle is taken out of the
#' arrow shaft.
arrow_fins_minimal <- function(angle = 45) {
  angle <- (angle * .deg2rad) + .halfpi
  ans <- cbind(x = c(0, 0), y = c(1, -1))
  attr(ans, "notch_angle") <- angle
  ans
}
