# Continuous arrow scales

These scales can map continuous input to an argument of an arrow
generator. The arrow head, arrow fins and middle arrows have separate
scales and by default use different generators.

## Usage

``` r
scale_arrow_head_continuous(
  name = waiver(),
  breaks = waiver(),
  labels = waiver(),
  limits = NULL,
  generator = arrow_head_wings,
  map_arg = "offset",
  other_args = list(),
  range = c(10, 80),
  transform = "identity",
  guide = "legend"
)

scale_arrow_fins_continuous(
  name = waiver(),
  breaks = waiver(),
  labels = waiver(),
  limits = NULL,
  generator = arrow_fins_feather,
  map_arg = "indent",
  other_args = list(),
  range = c(0, 1),
  transform = "identity",
  guide = "legend"
)

scale_arrow_mid_continuous(
  name = waiver(),
  breaks = waiver(),
  labels = waiver(),
  limits = NULL,
  generator = arrow_head_wings,
  map_arg = "offset",
  other_args = list(),
  range = c(10, 80),
  transform = "identity",
  guide = "legend"
)
```

## Arguments

- name:

  The name of the scale. Used as the axis or legend title. If
  [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html), the
  default, the name of the scale is taken from the first mapping used
  for that aesthetic. If `NULL`, the legend title will be omitted.

- breaks:

  One of:

  - `NULL` for no breaks

  - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
    for the default breaks computed by the [transformation
    object](https://scales.r-lib.org/reference/new_transform.html)

  - A numeric vector of positions

  - A function that takes the limits as input and returns breaks as
    output (e.g., a function returned by
    [`scales::extended_breaks()`](https://scales.r-lib.org/reference/breaks_extended.html)).
    Note that for position scales, limits are provided after scale
    expansion. Also accepts rlang
    [lambda](https://rlang.r-lib.org/reference/as_function.html)
    function notation.

- labels:

  One of the options below. Please note that when `labels` is a vector,
  it is highly recommended to also set the `breaks` argument as a vector
  to protect against unintended mismatches.

  - `NULL` for no labels

  - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
    for the default labels computed by the transformation object

  - A character vector giving labels (must be same length as `breaks`)

  - An expression vector (must be the same length as breaks). See
    ?plotmath for details.

  - A function that takes the breaks as input and returns labels as
    output. Also accepts rlang
    [lambda](https://rlang.r-lib.org/reference/as_function.html)
    function notation.

- limits:

  One of:

  - `NULL` to use the default scale range

  - A numeric vector of length two providing limits of the scale. Use
    `NA` to refer to the existing minimum or maximum

  - A function that accepts the existing (automatic) limits and returns
    new limits. Also accepts rlang
    [lambda](https://rlang.r-lib.org/reference/as_function.html)
    function notation. Note that setting limits on positional scales
    will **remove** data outside of the limits. If the purpose is to
    zoom, use the limit argument in the coordinate system (see
    [`coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.html)).

- generator:

  A `<function>` that can create an arrow ornament, such as
  [ornamentation](https://teunbrand.github.io/ggarrow/reference/arrow_ornaments.md)
  functions.

- map_arg:

  An argument of the `generator` function to map input to.

- other_args:

  Additional, fixed, arguments to pass to the `generator`.

- range:

  The range that `generator`'s `map_arg` may take

- transform:

  For continuous scales, the name of a transformation object or the
  object itself. Built-in transformations include "asn", "atanh",
  "boxcox", "date", "exp", "hms", "identity", "log", "log10", "log1p",
  "log2", "logit", "modulus", "probability", "probit", "pseudo_log",
  "reciprocal", "reverse", "sqrt" and "time".

  A transformation object bundles together a transform, its inverse, and
  methods for generating breaks and labels. Transformation objects are
  defined in the scales package, and are called `transform_<name>`. If
  transformations require arguments, you can call them from the scales
  package, e.g.
  [`scales::transform_boxcox(p = 2)`](https://scales.r-lib.org/reference/transform_boxcox.html).
  You can create your own transformation with
  [`scales::new_transform()`](https://scales.r-lib.org/reference/new_transform.html).

- guide:

  A function used to create a guide or its name. See
  [`guides()`](https://ggplot2.tidyverse.org/reference/guides.html) for
  more information.

## Value

A `<Scale>` that can be added to a plot.

## Examples

``` r
base <- ggplot(whirlpool(5), aes(x, y, colour = group)) +
  coord_fixed()

p <- base +
  geom_arrow(
    aes(arrow_head = as.integer(group)),
    length_head = 10
  )

# A typical scale
p + scale_arrow_head_continuous()


# Change other arguments passed to the generator
p + scale_arrow_head_continuous(other_args = list(inset = 90))


# Using another argument of the generator
p + scale_arrow_head_continuous(name = "inset",  map_arg = "inset")


# Using a different generator
p + scale_arrow_head_continuous(
  generator = arrow_head_line,
  map_arg = "angle",
  range = c(20, 80)
)


# The same goes for other arrow aesthetics, but the `generator()` might
# differ.
base +
  geom_arrow(
    aes(arrow_fins = as.integer(group), arrow_mid = as.integer(group)),
    length_fins = 10, arrow_head = NULL
  ) +
  scale_arrow_fins_continuous(map_arg = "height", range = c(0.1, 1)) +
  scale_arrow_mid_continuous(map_arg = "inset")
```
