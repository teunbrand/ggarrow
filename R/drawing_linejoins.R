# Line extrusion ----------------------------------------------------------

extrude_line <- function(x, y, id, width, gp = gpar()) {

  width <- width / 2
  width <- rep_len(width, sum(field(id, "length")))

  switch(
    gp$linejoin %||% "mitre",
    "round" = linejoin_round(x, y, id, width),
    "bevel" = linejoin_bevel(x, y, id, width),
    linejoin_mitre(x, y, id, width, mitre = gp$linemitre %||% 10)
  )
}

# Round -------------------------------------------------------------------

linejoin_round <- function(x, y, id, lwd, min_arc = 0.1) {

  lwd <- rep_len(lwd, length(x))

  start <- rle_start(id)
  end   <- rle_end(id)
  se    <- c(start, end)

  theta <- xy_angle(x, y, norm = TRUE)

  before <- c(NA, theta)
  after  <- c(theta, NA)

  before[start] <- before[start + 1L]
  after[end]    <- after[end - 1L]

  bisect <- (before + after) / 2
  bislen <- lwd / cos(bisect - after)

  ## Join clockwise -------------------------------------------------------
  # Note: this isn't actually clockwise, but seems reasonable to differentiate
  # between directions

  # Calculate angles
  delta <- before - after
  delta <- norm_angle(delta + pi) - pi

  # Initialise segments
  n_seg <- pmax(min_arc, delta) %/% min_arc
  n_seg[se] <- 1
  idx <- rep.int(seq_along(n_seg), n_seg)

  # Interpolate angle
  angle <- unlist0(Map(seq, delta, 0, length.out = n_seg)) + after[idx]
  singles <- n_seg == 1
  angle[singles[idx]] <- bisect[singles]

  # Set lengths
  leng <- lwd[idx]
  leng[singles[idx]] <- bislen[singles]

  # New coordinates
  x_cw  <- x[idx] + cos(angle) * leng
  y_cw  <- y[idx] + sin(angle) * leng
  id_cw <- new_rle(x = rep(rle_inv(id), n_seg))

  ## Join anti-clockwise --------------------------------------------------

  # Calculate angles
  delta <- -delta

  # Initialise segments
  n_seg <- pmax(min_arc, delta) %/% min_arc
  n_seg[se] <- 1
  idx <- rep.int(seq_along(n_seg), n_seg)

  # Interpolate angle
  angle <- unlist0(Map(seq, 0, delta, length.out = n_seg)) + before[idx]
  singles <- n_seg == 1
  angle[singles[idx]] <- bisect[singles]

  # Set lengths
  leng <- lwd[idx]
  leng[singles[idx]] <- bislen[singles]

  # New coordinates
  x_acw <- x[idx] - cos(angle) * leng
  y_acw <- y[idx] - sin(angle) * leng
  id_acw <- new_rle(x = rep(rle_inv(id), n_seg))

  list(
    x_left   = x_acw,
    y_left   = y_acw,
    id_left  = id_acw,
    x_right  = x_cw,
    y_right  = y_cw,
    id_right = id_cw
  )
}

# Mitre -------------------------------------------------------------------

linejoin_mitre <- function(x, y, id, lwd, mitre = 1) {

  start <- rle_start(id)
  end   <- rle_end(id)
  se    <- c(start, end)

  theta <- xy_angle(x, y, norm = TRUE)

  before <- c(NA, theta)
  after  <- c(theta, NA)

  before[start] <- before[start + 1L]
  after[end]    <- after[end - 1L]

  bisect <- (before + after) / 2
  bislen <- lwd / cos(bisect - after)
  delta  <- before - after

  should_bevel <- (bislen / lwd) > mitre
  should_bevel[lwd <= 0] <- FALSE
  should_bevel[se] <- FALSE

  ## Join clockwise -------------------------------------------------------
  # Note: this isn't actually clockwise, but seems reasonable to differentiate
  # between directions

  do_bevel <- should_bevel & delta > 0

  n_seg <- do_bevel + 1L
  idx   <- rep.int(seq_along(n_seg), n_seg)

  bevel <- rbind(before, after)
  angle <- bisect[idx]
  angle[do_bevel[idx]] <- as.vector(bevel[, do_bevel])

  leng <- lwd[idx]
  leng[!do_bevel[idx]] <- bislen[!do_bevel]

  x_cw  <- x[idx] + cos(angle) * leng
  y_cw  <- y[idx] + sin(angle) * leng
  id_cw <- new_rle(x = rep(rle_inv(id), n_seg))

  ## Join anti-clockwise --------------------------------------------------

  do_bevel <- should_bevel & delta < 0

  n_seg <- do_bevel + 1L
  idx   <- rep.int(seq_along(n_seg), n_seg)

  bevel <- rbind(before, after)
  angle <- bisect[idx]
  angle[do_bevel[idx]] <- as.vector(bevel[, do_bevel])

  leng <- lwd[idx]
  leng[!do_bevel[idx]] <- bislen[!do_bevel]

  x_acw  <- x[idx] - cos(angle) * leng
  y_acw  <- y[idx] - sin(angle) * leng
  id_acw <- new_rle(x = rep(rle_inv(id), n_seg))

  list(
    x_left   = x_acw,
    y_left   = y_acw,
    id_left  = id_acw,
    x_right  = x_cw,
    y_right  = y_cw,
    id_right = id_cw
  )
}

# Bevel -------------------------------------------------------------------

linejoin_bevel <- function(x, y, id, lwd) {
  linejoin_mitre(x, y, id, lwd, mitre = -Inf)
}
