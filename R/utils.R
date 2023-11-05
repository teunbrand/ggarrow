
unlist0  <- function(x) unlist(x, FALSE, FALSE)
vapply0  <- function(..., USE.NAMES = FALSE) vapply(..., USE.NAMES = USE.NAMES)
lengths0 <- function(x) lengths(x, use.names = FALSE)

match_id_group = function(x, id, nm = caller_arg(x)) {
  n <- length(id)
  m <- length(x)
  if (m == n) {
    return(x)
  }
  vec_recycle(x, n, x_arg = nm)
}

print_head <- function(x, n = 5) {
  out <- as.character(head(x))
  if (length(x) > 5) {
    out <- c(out, "...")
  }
  return(out)
}

list_setdiff <- function(x, y) {
  x[setdiff(names(x), names(y))]
}

drop_gp <- function(gp, id) {
  valid <- rle_valid(id)
  if (all(valid)) {
    return(gp)
  }
  valid <- which(valid)

  gp <- unclass(gp)
  constant <- is_constant(gp)
  gp[!constant] <- lapply(gp[!constant], `[`, i = valid)
  do.call(gpar, gp)
}

interpol_dist <- function(x, index, ref) {
  rindex <- ref[index]
  (x - rindex) / (ref[index + 1] - rindex)
}

linear_interpol <- function(x, index, d) {
  x[index] * (1 - d) + x[index + 1] * d
}

polygonise <- function(xy_list) {
  lapply(xy_list, function(mtx) {
    list(x = mtx[, 1], y = mtx[, 2])
  })
}

# Coercions ---------------------------------------------------------------

as_unit <- function(x, unit) {
  if (is.unit(x)) {
    return(x)
  }
  unit(x, unit)
}

as_mm <- function(x, axis = "x", valueOnly = TRUE) {
  if (!is.unit(x)) {
    return(x)
  }
  switch(
    axis,
    x = convertX(x, "mm", valueOnly),
    y = convertY(x, "mm", valueOnly),
    stop()
  )
}

as_cli <- function(x) cli::cli_fmt(cli::cli_text(x))
