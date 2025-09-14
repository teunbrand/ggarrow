
property_nullable <- function(class = S7::class_any, ...) {
  S7::new_property(
    class = S7::new_union(NULL, class),
    ...
  )
}

property_colour <- function(allow_null = TRUE, pattern = FALSE, default = NULL) {
  class <- S7::new_union(
    S7::class_character,
    S7::class_logical
  )
  if (isTRUE(pattern)) {
    class <- S7::new_union(class, S7::new_S3_class("GridPattern"))
  }
  if (isTRUE(allow_null)) {
    class <- S7::new_union(class, NULL)
  }
  S7::new_property(
    class = class,
    default = default
  )
}

property_choice <- function(options, allow_null = FALSE, default = NULL) {
  force(options)
  class <- S7::class_character
  if (isTRUE(allow_null)) {
    class <- S7::new_union(class, NULL)
  }
  validator <- function(value) {
    if (allow_null && is.null(value)) {
      return(character())
    }
    if (!is_character(value)) {
      return(as_cli("must be a string, not {.obj_type_friendly {value}}."))
    }
    if (all(value %in% options)) {
      return(character())
    }
    as_cli("must be one of {.or {.val {options}}}.")
  }
  S7::new_property(
    class = class,
    validator = validator,
    default = default
  )
}

property_arrow <- function(allow_null = TRUE) {
  class <- S7::new_union(
    S7::class_character,
    S7::class_numeric,
    S7::class_function
  )
  if (isTRUE(allow_null)) {
    class <- S7::new_union(NULL, class)
  }

  validator <- function(value) {
    if (is.null(value) || is.function(value)) {
      return(character())
    }
    if (is.character(value) && length(value) != 1) {
      return(as_cli("must be scalar when {.cls character}."))
    }
    if (!is.numeric(value)) {
      return(character())
    }
    if (!is.matrix(value)) {
      return(as_cli("must be a {.cls matrix} when {.cls numeric}."))
    }
    if (ncol(value) != 2) {
      return(as_cli("must have 2 columns when a {.cls matrix}."))
    }
    if (nrow(value) < 1) {
      return(as_cli("must have at least 1 row when a {.cls matrix}"))
    }
    return(character())
  }
  S7::new_property(
    class = class,
    validator = validator
  )
}

class_unit <- S7::new_union(
  S7::new_S3_class("unit"),
  S7::new_S3_class("simpleUnit")
)

property_length <- function(allow_null = TRUE, ...) {
  class <- S7::new_union(class_unit, S7::class_numeric)
  if (isTRUE(allow_null)) {
    class <- S7::new_union(NULL, class)
  }
  S7::new_property(
    class = class,
    ...
  )
}

check_restriction <- function(value, n = NA, min = 0, max = 1L) {
  if (!is.na(n) && !length(value) %in% n) {
    return(paste0("must have length ", n, "."))
  }
  if (any(value < min)) {
    return(paste0("must be at least ", min, "."))
  }
  if (any(value > max)) {
    return(paste0("must be at most ", max, "."))
  }
  return(character())
}

property_boolean <- function(allow_null = FALSE, default = TRUE) {
  class <- S7::class_logical
  if (isTRUE(allow_null)) {
    class <- S7::new_union(class, NULL)
  }
  validator <- function(value) {
    if ((allow_null) && is.null(value) || is_bool(value)) {
      return(character())
    }
    "must be a boolean."
  }
  S7::new_property(
    class = class,
    validator = validator,
    default = default
  )
}
