# Called at the geom level
# Finds groups of arrows that have the same path and assigns some offset value.
separate_offsets <- function(x, y, group, sep = NULL) {
  n_groups <- length(unique(group))
  out <- rep(0L, n_groups)
  if (is.null(sep) || all(as.numeric(sep) == 0.0)) {
    return(out)
  }

  x <- split(x, group)
  y <- split(y, group)

  # We also want to match groups that have perfectly inverse paths
  groups <- data_frame0(
    x = c(x, lapply(x, rev)),
    y = c(y, lapply(y, rev)),
  )
  id <- vec_group_id(groups)

  # Collapse the inverse matches
  matches <- pmin(
    id[seq_len(n_groups)],
    id[(n_groups + 1L):length(id)]
  )
  # If we have no matching groups, there is nothing to offset
  if (!vec_duplicate_any(matches)) {
    return(out)
  }

  # Inverse match get opposite sign
  signed <- ifelse(matches == id[seq_len(n_groups)], 1.0, -1.0) * sep
  if (is.unit(signed)) {
    out <- unit(rep(0.0, length.out = length(out)), "mm")
  }

  # Apply separator distance
  dups <- unique(matches[duplicated(matches)])
  for (match in dups) {
    i <- which(matches == match)
    out[i] <- (seq_along(i) - (length(i) + 1.0) / 2.0) * signed[i]
  }
  out
}

offset_paths <- function(x, y, id, offset) {
  if (is.null(offset) || all(offset == 0L)) {
    return(list(x = x, y = y))
  }
  idx <- rle_idx(id)

  for (i in seq_along(offset)[offset != 0L]) {
    these <- idx[[i]]
    offsets <- simplified_offset(x[these], y[these], d = offset[i])
    x[these] <- offsets$x
    y[these] <- offsets$y
  }
  list(x = x, y = y)
}

simplified_offset <- function(x, y, d) {
  theta <- xy_angle(x, y, norm = TRUE)

  before <- c(theta[1L], theta)
  after  <- c(theta, theta[length(theta)])

  bisect <- (before + after) / 2.0

  offset <- d / cos(bisect - after)

  list(
    x = offset * cos(bisect) + x,
    y = offset * sin(bisect) + y
  )
}
