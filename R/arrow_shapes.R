# Ornament functions ------------------------------------------------------



arrow_head_wings <- function(
  offset = 20 * .deg2rad,
  inset  = 30 * .deg2rad
) {

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

arrow_fins_feather <- function(
  inset  = 0.3,
  outset = inset,
  asp    = 0.5
) {

  x <- c(1 - inset, 1, 0 + outset, 0, 0 + outset, 1)
  y <- c(0, 1, 1, 0, -1, -1) * asp / 2

  ans <- cbind(x = x, y = y)
  attr(ans, "front_angle") <- atan2(asp, x[3] - x[4])
  attr(ans, "notch_angle") <- atan2(asp / 2, x[1] - x[2])
  ans
}

arrow_head_minimal <- function(angle = 45 * .deg2rad) {
  ans <- cbind(x = c(0, 0), y = c(1, -1))
  attr(ans, "notch_angle") <- angle
  ans
}

arrow_fins_minimal <- function(angle = 45 * .deg2rad) {
  ans <- cbind(x = c(0, 0), y = c(1, -1))
  attr(ans, "notch_angle") <- angle + .halfpi
  ans
}

# Resolve ornaments -------------------------------------------------------

resolve_ornament <- function(ornament, length, id, width, type = "head") {

  if (is.null(ornament)) {
    ans <- list(ornament = NULL, length = rep(0, length(id)), angle = NULL)
    return(ans)
  }
  if (type == "head") {
    i <- rle_end(id)
  } else {
    i <- rle_start(id)
  }
  length <- pmax(as_mm(length), width[i] / diff(range(ornament[, "y"])))
  length <- max(ornament[, "x"]) * length
  angle  <- attr(ornament, "notch_angle") %||% .halfpi
  list(ornament = ornament, length = length, angle = angle)
}





