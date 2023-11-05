
as_unit <- function(x, unit) {
  if (is.unit(x)) {
    return(x)
  }
  unit(x, unit)
}

unlist0  <- function(x) unlist(x, FALSE, FALSE)
vapply0  <- function(..., USE.NAMES = FALSE) vapply(..., USE.NAMES = USE.NAMES)
lengths0 <- function(x) lengths(x, use.names = FALSE)

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

match_id_group = function(x, id, nm = caller_arg(x)) {
  n <- length(id)
  m <- length(x)
  if (m == n) {
    return(x)
  }
  vec_recycle(x, n, x_arg = nm)
}

validate_resect <- function(resect, head = NULL, fins = NULL, unit, id) {

  resect_fins <- as_unit(fins %||% resect, unit)
  resect_head <- as_unit(head %||% resect, unit)
  list(
    fins = match_id_group(resect_fins, id),
    head = match_id_group(resect_head, id)
  )
}

validate_id <- function(id = NULL, id.lengths = NULL, alt = NULL,
                        fun_nm = "grob_arrow") {

  if (!inherits(id, "vctrs_group_rle")) {
    id <- new_rle(id, id.lengths, alt)
  }

  if (any(field(id, "length") < 2)) {
    n <- sum(field(id, "length") < 2)
    cli::cli_abort(c(
      "{.fn {fun_nm}} cannot draw arrows with fewer than two points per group.",
      i = "There {?is/are} {n} group{?s} with fewer than two points."
    ))
  }

  if (anyDuplicated(field(id, "group"))) {
    is_dup     <- which(duplicated(field(id, "group")))
    dup_groups <- field(id, "group")[is_dup]
    cli::cli_warn(c(
      paste0(
        "{.fn {fun_nm}} needs sorted groups. The following group{?s} ",
        "{?has/have} duplicate runs: {.and {print_head(dup_groups)}}."
      ),
      i = "These runs are now considered unique."
    ))
    field(id, "group") <- seq_along(id)
  }

  id
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

is_constant <- function(x) {
  if (is.list(x)) {
    ans <- vapply0(x, is_constant, logical(1))
    return(ans)
  }
  if (length(x) == 1L) {
    return(TRUE)
  }
  if (vec_unique_count(x) == 1L) {
    return(TRUE)
  }
  FALSE
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
