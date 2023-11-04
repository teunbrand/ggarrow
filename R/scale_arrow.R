#' Discrete arrow scales
#'
#' These scales can map discrete input to various sorts of arrow shapes.
#' The arrow head, arrow fins and middle arrows have separate scales.
#'
#' @param values One of the following:
#'   * A `<character>` vector of arrow function names, without the
#'   `arrow_`-prefix, such as `"head_wings"` or `"fins_line"`.
#'   * An unnested `<list>`, possibly mixed `<list>`, containing any of the
#'   following elements:
#'       * A single `<character>` as described above.
#'       * A `<function>` that when called without any arguments produces
#'       a 2-column `<matrix>` that can be used as an arrow.
#'       * A 2-column `<matrix>` giving a polygon to use as an arrow.
#'   * `NULL`, which defaults to a built-in palette with a maximum of 3 arrows.
#' @param aesthetics The names of the aesthetics that this scale works with
#' @inheritDotParams ggplot2::discrete_scale -aesthetics
#'
#' @return A `<Scale>` that can be added to a plot.
#' @name discrete_arrow_scales
#' @export
#'
#' @examples
#' # A standard arrow plot
#' p <- ggplot(whirlpool(5), aes(x, y, colour = group)) +
#'   geom_arrow(length_head = 10, length_fins = 10, arrow_head = NULL)
#'
#' # A character vector naming arrow shapes as arrow head scale
#' p + aes(arrow_head = group) +
#'   scale_arrow_head_discrete(values = c(
#'       "head_wings", "head_line", "head_minimal", "fins_line", "fins_feather"
#'   ))
#'
#' # A mixed list with arrows as arrow fins scale
#' p + aes(arrow_fins = group) +
#'   scale_arrow_fins_discrete(values = list(
#'     "head_wings",              # Using a character
#'     arrow_head_wings(20, 100), # Using an arrow function
#'     NULL,                      # No arrow
#'     matrix(c(1, 0, 0, 0, 0.5, -0.5), ncol = 2), # A matrix
#'     "fins_feather"
#'   ))
scale_arrow_head_discrete <- function(
  values = NULL, aesthetics = "arrow_head", ...
) {
  values <- values %||% c("head_wings", "head_line", "head_minimal")
  new_manual_scale(
    aesthetic = aesthetics, values = values,
    ...
  )
}

#' @export
#' @rdname discrete_arrow_scales
scale_arrow_fins_discrete <- function(
  values = NULL, aesthetics = "arrow_fins", ...
) {
  values <- values %||% c("fins_feather", "fins_line", "fins_minimal")
  new_manual_scale(
    aesthetic = aesthetics, values = values,
    ...
  )
}

#' @export
#' @rdname discrete_arrow_scales
scale_arrow_mid_discrete <- function(
  values = NULL, aesthetics = "arrow_mid", ...
) {
  values <- values %||% c("head_wings", "head_line", "fins_feather", "fins_line")
  new_manual_scale(
    aesthetic = aesthetics, values = values,
    ...
  )
}

new_manual_scale <- function(aesthetic, values = NULL, breaks = waiver(), ...,
                             limits = NULL, call = caller_call()) {

  call <- call %||% current_call()
  if (is_missing(values)) {
    values <- NULL
  } else {
    force(values)
  }

  if (is.null(limits) && !is.null(names(values))) {
    force(aesthetic)
    limits <- function(x) {
      x <- intersect(x, c(names(values), NA)) %||% character()
      if (length(x) < 1) {
        cli::cli_warn(paste0(
          "No shared levels found between {.code names(values)} of the manual ",
          "scale and the data's {.field {aesthetic}} values."
        ))
      }
      x
    }
  }

  if (is.vector(values) && is.null(names(values))
      && !inherits(breaks, "waiver") && !is.null(breaks) &&
      !is.function(breaks)) {
    if (length(breaks) <= length(values)) {
      names(values) <- breaks
    } else {
      names(values) <- breaks[1:length(values)]
    }
  }

  pal <- function(n) {
    if (n > length(values)) {
      cli::cli_abort(paste0(
        "Insufficient values in manual scale. ",
        "{n} needed but only {length(values)} provided."
      ))
    }
    values
  }

  args <- list2(aesthetics = aesthetic, palette = pal, breaks = breaks,
                limits = limits, ...)
  if ("call" %in% fn_fmls_names(discrete_scale)) {
    args <- c(args, list(call = call))
  } else {
    args <- c(args, list(scale_name = "arrow_scale"))
  }
  inject(discrete_scale(!!!args))
}

arrow_pal <- function(x) {

  if (is.character(x)) {
    x <- as.list(x)
  }
  is_char <- vapply(x, is.character, logical(1))
  x[is_char] <- lapply(x[is_char], function(name) {

    pattern <- paste0("arrow_", name)
    fun <- NULL
    if (exists(pattern, envir = global_env(), mode = "function")) {
      fun <- get(pattern, envir = global_env(), mode = "function")
    }
    if (is.null(fun)) {
      nsenv <- asNamespace("ggarrow")
      if (exists(pattern, envir = nsenv, mode = "function")) {
        fun <- get(pattern, envir = nsenv, mode = "function")
      }
    }
    if (!is.function(fun)) {
      cli::cli_abort("Cannot find function {.fun {pattern}} to draw arrows.")
    }
    fun()
  })

  validate_matrix_list(x, dim = c(NA, NA), typeof = c("integer", "double"))
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
