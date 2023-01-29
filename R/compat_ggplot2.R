
data_frame0 <- function(...) data_frame(..., .name_repair = "minimal")

unique0 <- function(x, ...) if (is.null(x)) x else vec_unique(x, ...)

snake_class <- function(x) {
  snakeize(class(x)[1])
}

snakeize <- function(x) {
  x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x <- gsub(".", "_", x, fixed = TRUE)
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  chartr(upper_ascii, lower_ascii, x)
}

upper_ascii <- paste0(LETTERS, collapse = "")
lower_ascii <- paste0(letters, collapse = "")

remove_missing <- function(df, na.rm = FALSE, vars = names(df), name = "",
                           finite = FALSE) {
  if (!is_bool(na.rm)) {
    cli::cli_abort("{.arg na.rm} must be a logical scalar.")
  }

  missing <- detect_missing(df, vars, finite)

  if (any(missing)) {
    df <- df[!missing, ]
    if (!na.rm) {
      if (name != "") {
        name <- paste0(" ({.fn ", name, "})")
      }
      msg <- paste0(
        "Removed {sum(missing)} rows containing",
        if (finite) "non-finite" else "missing",
        "values", name, "."
      )
      cli::cli_warn(msg)
    }
  }
  df
}

detect_missing <- function(df, vars, finite = FALSE) {
  vars <- intersect(vars, names(df))
  !cases(df[, vars, drop = FALSE], if (finite) is_finite else is_complete)
}

cases <- function(x, fun) {
  ok <- vapply(x, fun, logical(nrow(x)))
  if (is.vector(ok)) {
    all(ok)
  } else {
    rowSums(as.matrix(ok)) == ncol(x)
  }
}

is_finite <- function(x) {
  if (typeof(x) == "list") {
    !vapply(x, is.null, logical(1))
  } else {
    is.finite(x)
  }
}

is_complete <- function(x) {
  if (typeof(x) == "list") {
    !vapply(x, is.null, logical(1))
  } else {
    !is.na(x)
  }
}

empty <- function(df) {
  is.null(df) || prod(dim(df)) == 0 || inherits(df, "waiver")
}

rename <- function(x, replace) {
  current  <- names(x)
  previous <- names(replace)
  missing  <- setdiff(previous, current)
  if (length(missing) > 0) {
    replace  <- replace[!previous %in% missing]
    previous <- names(replace)
  }
  names(x)[match(previous, current)] <- as.vector(replace)
  x
}
