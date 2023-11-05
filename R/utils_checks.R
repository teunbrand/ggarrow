# Properties --------------------------------------------------------------

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

# Checks ------------------------------------------------------------------

check_ornament_length <- function(x, arg_nm = caller_arg(x),
                                  call = caller_env()) {
  check_number_decimal(
    x,
    min = 0,
    allow_infinite = FALSE,
    allow_na       = FALSE,
    allow_null     = FALSE,
    arg = arg_nm, call = call
  )
}

check_ornament_matrix <- function(
    x, arg_nm = caller_arg(x), call = caller_env()
) {
  if (!is.matrix(x)) {
    return(x)
  }
  if (ncol(x) != 2) {
    cli::cli_abort("{.arg {arg_nm}} must have 2 columns.", call = call)
  }
  if (!typeof(x) %in% c("integer", "double")) {
    cli::cli_abort("{.arg {arg_nm}} must be {.cls numeric}", call = call)
  }
}

# Validators --------------------------------------------------------------

validate_length <- function(base = NULL, head = NULL, fins = NULL, mid = NULL,
                            default = 4, call = caller_env()) {
  base <- base %||% default
  check_ornament_length(base, arg_nm = "length",      call = call)
  head <- head %||% base
  check_ornament_length(head, arg_nm = "length_head", call = call)
  fins <- fins %||% base
  check_ornament_length(fins, arg_nm = "length_fins", call = call)
  mid  <- mid  %||% base
  check_ornament_length(mid,  arg_nm = "length_mid",  call = call)
  list(
    head = head,
    fins = fins,
    mid  = mid
  )
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

validate_ornament_character <- function(x) {
  if (!is.character(x)) {
    return(x)
  }
  if (length(unique(x)) == 1) {
    x <- x[1]
  }
  arrow_pal(x)
}

validate_ornament_list <- function(
    x, arg = caller_arg(x), call = caller_env()
) {
  if (!is.list(x)) {
    return(x)
  }
  x <- lapply(x, validate_ornament, n = 1, arg = arg, call = call)
  if (length(x) == 1L) {
    x <- .subset2(x, 1L)
  }
  x
}

validate_ornament <- function(ornament, n,
                              arg = caller_arg(ornament),
                              call = caller_env()) {
  if (is.null(ornament) || is.function(ornament)) {
    return(ornament)
  }

  # Check character/list
  ornament <- validate_ornament_character(ornament)
  ornament <- validate_ornament_list(ornament, arg = arg, call = call)

  if (is.null(ornament) || is.function(ornament)) {
    return(ornament)
  }

  # Check matrices
  check_ornament_matrix(ornament, arg_nm = arg, call = call)
  if (is.matrix(ornament)) {
    if (is.null(colnames(ornament))) {
      colnames(ornament) <- c("x", "y")
    }
    return(ornament)
  }
  # Check lengths
  if (length(ornament) %in% c(1, n)) {
    return(ornament)
  }
  if (n == 1) {
    cli::cli_abort("{.arg {arg}} must have length 1.", call = call)
  }
  cli::cli_abort("{.arg {arg}} must have length 1 or {n}", call = call)
}

validate_matrix_list <- function(
  list, dim = c(NA, 2), typeof = c("integer", "double"),
  fun_allowed = TRUE,
  x_arg = caller_arg(x), call = caller_env()
) {
  if (is.matrix(list)) {
    list <- list(list)
  }
  if (!is.list(list)) {
    cli::cli_abort("{.arg {x_arg}} must be a {.cls list}.", call = call)
  }

  not_matrix <- integer()
  not_dim    <- integer()
  not_typeof <- integer()

  for (i in seq_along(list)) {
    x <- .subset2(list, i)
    if (is.function(x)) {
      if (fun_allowed) {
        next
      }
      not_matrix <- c(not_matrix, i)
      next
    }
    if (!is.matrix(x)) {
      not_matrix <- c(not_matrix, i)
      next
    }
    if (!typeof(x) %in% typeof) {
      not_typeof <- c(not_typeof, i)
    }
    dim_match <- dim(x) == dim
    dim_match[is.na(dim_match)] <- TRUE
    if (!all(dim_match)) {
      not_dim <- c(not_dim, i)
    }
  }

  if (sum(length(not_matrix), length(not_dim), length(not_typeof)) == 0) {
    return(list)
  }
  msg <- c("{.arg {x_arg}} is not a list of valid matrices.")
  n_not_matrix <- length(not_matrix)
  if (n_not_matrix > 0) {
    msg <- c(msg, i = paste0(
      "{cli::qty(n_not_matrix)}Element{?s} {.and {not_matrix}}",
      "{cli::qty(n_not_matrix)} {?is/are} not {?a / }matri{?x/ces}."
    ))
  }
  n_not_dim <- length(not_dim)
  if (n_not_dim > 0) {
    dim <- as.character(dim)
    dim[is.na(dim)] <- c("n", "m")[is.na(dim)]
    dim <- paste0("[", dim[1], ", ", dim[2], "]")
    msg <- c(msg, i = paste0(
      "{cli::qty(n_not_dim)}Element{?s} {.and {not_dim}} ",
      "{cli::qty(n_not_dim)}{?does/do} not have dimension {.field {dim}}."
    ))
  }
  n_not_typeof <- length(not_typeof)
  if (n_not_typeof > 0) {
    msg <- c(msg, i = paste0(
      "{cli::qty(n_not_typeof)}Element{?s} {.and {not_typeof}} ",
      "{cli::qty(n_not_typeof)}{?does/do} not have the type {.cls {typeof}}."
    ))
  }
  cli::cli_abort(msg, call = call)
}
