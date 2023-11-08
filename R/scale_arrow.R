# Discrete scales ---------------------------------------------------------

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
  manual_scale(
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
  manual_scale(
    aesthetic = aesthetics, values = values,
    ...
  )
}

#' @export
#' @rdname discrete_arrow_scales
scale_arrow_mid_discrete <- function(
  values = NULL, aesthetics = "arrow_mid", ...
) {
  values <- values %||% c(
    "head_wings", "head_line", "fins_feather", "fins_line"
  )
  manual_scale(
    aesthetic = aesthetics, values = values,
    ...
  )
}

# Continuous scales -------------------------------------------------------

#' Continuous arrow scales
#'
#' These scales can map continuous input to an argument of an arrow generator.
#' The arrow head, arrow fins and middle arrows have separate scales and
#' by default use different generators.
#'
#' @inheritParams ggplot2::continuous_scale
#' @param generator A `<function>` that can create an arrow ornament, such
#'   as [ornamantation](arrow_ornament) functions.
#' @param map_arg An argument of the `generator` function to map input to.
#' @param other_args Additional, fixed, arguments to pass to the `generator`.
#' @param range The range that `generator`'s `map_arg` may take
#'
#' @return A `<Scale>` that can be added to a plot.
#' @name continuous_arrow_scales
#' @export
#'
#' @examples
#' base <- ggplot(whirlpool(5), aes(x, y, colour = group)) +
#'   coord_fixed()
#'
#' p <- base +
#'   geom_arrow(
#'     aes(arrow_head = as.integer(group)),
#'     length_head = 10
#'   )
#'
#' # A typical scale
#' p + scale_arrow_head_continuous()
#'
#' # Change other arguments passed to the generator
#' p + scale_arrow_head_continuous(other_args = list(inset = 90))
#'
#' # Using another argument of the generator
#' p + scale_arrow_head_continuous(name = "inset",  map_arg = "inset")
#'
#' # Using a different generator
#' p + scale_arrow_head_continuous(
#'   generator = arrow_head_line,
#'   map_arg = "angle",
#'   range = c(20, 80)
#' )
#'
#' # The same goes for other arrow aesthetics, but the `generator()` might
#' # differ.
#' base +
#'   geom_arrow(
#'     aes(arrow_fins = as.integer(group), arrow_mid = as.integer(group)),
#'     length_fins = 10, arrow_head = NULL
#'   ) +
#'   scale_arrow_fins_continuous(map_arg = "height", range = c(0.1, 1)) +
#'   scale_arrow_mid_continuous(map_arg = "inset")
scale_arrow_head_continuous <- function(
  name   = waiver(),
  breaks = waiver(),
  labels = waiver(),
  limits = NULL,
  generator  = arrow_head_wings,
  map_arg    = "offset",
  other_args = list(),
  range = c(10, 80),
  trans = "identity",
  guide = "legend"
) {
  continuous_scale(
    "arrow_head", "arrowhead",
    palette = generator_pal(
      generator, map_arg, other_args, range, caller_arg(generator)
    ),
    name = name, breaks = breaks, labels = labels, limits = limits,
    trans = trans, guide = guide
  )
}

#' @export
#' @rdname continuous_arrow_scales
scale_arrow_fins_continuous <- function(
  name = waiver(),
  breaks = waiver(),
  labels = waiver(),
  limits = NULL,
  generator  = arrow_fins_feather,
  map_arg    = "indent",
  other_args = list(),
  range = c(0, 1),
  trans = "identity",
  guide = "legend"
) {
  continuous_scale(
    "arrow_fins", "arrowfins",
    palette = generator_pal(
      generator, map_arg, other_args, range, caller_arg(generator)
    ),
    name = name, breaks = breaks, labels = labels, limits = limits,
    trans = trans, guide = guide
  )
}

#' @export
#' @rdname continuous_arrow_scales
scale_arrow_mid_continuous <- function(
  name = waiver(),
  breaks = waiver(),
  labels = waiver(),
  limits = NULL,
  generator  = arrow_head_wings,
  map_arg    = "offset",
  other_args = list(),
  range = c(10, 80),
  trans = "identity",
  guide = "legend"
) {
  continuous_scale(
    "arrow_mid", "arrowhead",
    palette = generator_pal(
      generator, map_arg, other_args, range, caller_arg(generator)
    ),
    name = name, breaks = breaks, labels = labels, limits = limits,
    trans = trans, guide = guide
  )
}

# Resection scales --------------------------------------------------------

#' Scale for resection
#'
#' Arrow geoms have a `resect` aesthetic that controls how much an arrow should
#' be shortened. These scales can help to rescale the output range of resection.
#'
#' @inheritDotParams ggplot2::continuous_scale -super -palette
#' @param range A numeric vector of length 2 indicating the minimum and maximum
#'   size of the resection after transformation in millimetres. `range` is
#'   mutually exclusive with the `values` argument in discrete scales.
#' @param values (Discrete scale only) A numeric vector to map data values to.
#'   The values will be matched in order with the limits of the scale, or with
#'   `breaks` if provided. If this is a named vector, the values will be
#'   matched based on the names instead. Data values that don't match will be
#'   given `na.value`. `values` is mutually exclusive with the `range`
#' @inheritParams ggplot2::continuous_scale
#'
#' @details
#' Conceptually, these scales depart slightly from ggplot2 conventions. The
#' `scale_resect_continuous()` function returns an identity scale
#' when `range = NULL` (default) and a typical continuous scale when the
#' `range` argument is set.
#' The `scale_resect_discrete()` acts as a manual scale when `values` is set
#' and as an ordinal scale when `range` is set.
#'
#' @return A `<Scale>` that can be added to a plot.
#' @export
#' @name scale_resect
#'
#' @examples
#' # A plot with points indicating path ends
#' p <- ggplot(whirlpool(5), aes(x, y, colour = group)) +
#'   geom_point(data = ~ subset(.x, arc == ave(arc, group, FUN = max)))
#'
#' # Resect scale as an identity scale
#' p + geom_arrow(aes(resect_head = as.integer(group))) +
#'   scale_resect_continuous()
#'
#' # Resect scale as typical continuous scale
#' p + geom_arrow(aes(resect_head = as.integer(group))) +
#'   scale_resect_continuous(range = c(0, 10))
#'
#' # Resect scale as manual scale
#' p + geom_arrow(aes(resect_head = group)) +
#'   scale_resect_discrete(values = c(10, 5, 0, 5, 10))
#'
#' # Resect scale as ordinal scale
#' p + geom_arrow(aes(resect_head = group)) +
#'   scale_resect_discrete(range = c(0, 10))
scale_resect_continuous <- function(
  ...,
  range = NULL,
  aesthetics = c("resect_head", "resect_fins"),
  guide = "none"
) {
  if (is.null(range)) {
    pal   <- identity_pal()
    super <- ScaleContinuousIdentity
  } else {
    check_number_decimal(range[1], min = 0, allow_infinite = FALSE)
    check_number_decimal(range[2], min = 0, allow_infinite = FALSE)
    pal   <- rescale_pal(range)
    super <- ScaleContinuous
  }
  continuous_scale(
    aesthetics = aesthetics, "resect_scale", palette = pal, guide = guide,
    ..., super = super
  )
}

#' @export
#' @rdname scale_resect
scale_resect_discrete <- function(
  ...,
  values = NULL,
  aesthetics = c("resect_head", "resect_fins"),
  range = NULL,
  guide = "none"
) {
  if (!xor(is.null(values), is.null(range))) {
    msg <- "Only one of the {.arg range} and {.arg values} must be set."
    if (is.null(values) && is.null(range)) {
      msg <- c(msg, i = "Currently, {.emph neither} argument is set.")
    } else {
      msg <- c(msg, i = "Currently, {.emph both} arguments are set.")
    }
    cli::cli_abort(msg)
  }

  if (!is.null(range)) {
    check_number_decimal(range[1], min = 0, allow_infinite = FALSE)
    check_number_decimal(range[2], min = 0, allow_infinite = FALSE)
    force(range)
    discrete_scale(
      aesthetics = aesthetics, "resect_scale",
      palette = function(n) {
        seq(range[1], range[2], length.out = n)
      },
      guide = guide,
      ...
    )
  } else {
    manual_scale(aesthetic = aesthetics, values = values, guide = guide)
  }
}

# Helpers -----------------------------------------------------------------

generator_pal <- function(
  generator, map_arg, other_args,
  range = c(0, 1), gen_name,
  call = caller_env()
) {
  check_function(generator)
  arg_names <- fn_fmls_names(generator)
  if (length(arg_names) == 0) {
    cli::cli_abort("{.fn {gen_name}} must have arguments.", call = call)
  }
  if (!map_arg %in% arg_names) {
    cli::cli_abort(
      "{.arg {map_arg}} must be an argument to {.fn {gen_name}}.",
      call = call
    )
  }
  if (!"..." %in% arg_names) {
    extra <- setdiff(names(other_args), arg_names)
    other_args <- other_args[intersect(names(other_args), arg_names)]
    if (length(extra) > 0) {
      cli::cli_warn(c(
        "{.arg other_args} has unknown arguments of {.fn {gen_name}}.",
        i = "{.and {.field {extra}}} {?has/have} been dropped."
      ))
    }
  }
  if (!is.numeric(range) || length(range) != 2 || !all(is.finite(range))) {
    cli::cli_abort(
      "{.arg range} must be a finite numeric vector of length 2.",
      call = call
    )
  }
  force(other_args)

  function(x) {
    if (length(x) == 0) {
      return(NULL)
    }
    x <- rescale(x, to = range, from = c(0, 1))
    lapply(x, function(input) {
      exec(
        generator,
        !!map_arg := input,
        !!!other_args
      )
    })
  }
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

  validate_matrix_list(x, dim = c(NA, 2), typeof = c("integer", "double"))
}
