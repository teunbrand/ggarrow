# Scale for resection

Arrow geoms have a `resect` aesthetic that controls how much an arrow
should be shortened. These scales can help to rescale the output range
of resection.

## Usage

``` r
scale_resect_continuous(
  ...,
  range = NULL,
  aesthetics = c("resect_head", "resect_fins"),
  guide = "none"
)

scale_resect_discrete(
  ...,
  values = NULL,
  aesthetics = c("resect_head", "resect_fins"),
  range = NULL,
  guide = "none"
)
```

## Arguments

- ...:

  Arguments passed on to
  [`ggplot2::continuous_scale`](https://ggplot2.tidyverse.org/reference/continuous_scale.html),
  [`ggplot2::discrete_scale`](https://ggplot2.tidyverse.org/reference/discrete_scale.html)

  `name`

  :   The name of the scale. Used as the axis or legend title. If
      [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html),
      the default, the name of the scale is taken from the first mapping
      used for that aesthetic. If `NULL`, the legend title will be
      omitted.

  `breaks`

  :   One of:

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

  `minor_breaks`

  :   One of:

      - `NULL` for no minor breaks

      - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
        for the default breaks (none for discrete, one minor break
        between each major break for continuous)

      - A numeric vector of positions

      - A function that given the limits returns a vector of minor
        breaks. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation. When the function has two arguments, it will
        be given the limits and major break positions.

  `n.breaks`

  :   An integer guiding the number of major breaks. The algorithm may
      choose a slightly different number to ensure nice break labels.
      Will only have an effect if `breaks = waiver()`. Use `NULL` to use
      the default number of breaks given by the transformation.

  `labels`

  :   One of the options below. Please note that when `labels` is a
      vector, it is highly recommended to also set the `breaks` argument
      as a vector to protect against unintended mismatches.

      - `NULL` for no labels

      - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
        for the default labels computed by the transformation object

      - A character vector giving labels (must be same length as
        `breaks`)

      - An expression vector (must be the same length as breaks). See
        ?plotmath for details.

      - A function that takes the breaks as input and returns labels as
        output. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation.

  `limits`

  :   One of:

      - `NULL` to use the default scale range

      - A numeric vector of length two providing limits of the scale.
        Use `NA` to refer to the existing minimum or maximum

      - A function that accepts the existing (automatic) limits and
        returns new limits. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation. Note that setting limits on positional scales
        will **remove** data outside of the limits. If the purpose is to
        zoom, use the limit argument in the coordinate system (see
        [`coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.html)).

  `rescaler`

  :   A function used to scale the input values to the range \[0, 1\].
      This is always
      [`scales::rescale()`](https://scales.r-lib.org/reference/rescale.html),
      except for diverging and n colour gradients (i.e.,
      [`scale_colour_gradient2()`](https://ggplot2.tidyverse.org/reference/scale_gradient.html),
      [`scale_colour_gradientn()`](https://ggplot2.tidyverse.org/reference/scale_gradient.html)).
      The `rescaler` is ignored by position scales, which always use
      [`scales::rescale()`](https://scales.r-lib.org/reference/rescale.html).
      Also accepts rlang
      [lambda](https://rlang.r-lib.org/reference/as_function.html)
      function notation.

  `oob`

  :   One of:

      - Function that handles limits outside of the scale limits (out of
        bounds). Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation.

      - The default
        ([`scales::censor()`](https://scales.r-lib.org/reference/oob.html))
        replaces out of bounds values with `NA`.

      - [`scales::squish()`](https://scales.r-lib.org/reference/oob.html)
        for squishing out of bounds values into range.

      - [`scales::squish_infinite()`](https://scales.r-lib.org/reference/oob.html)
        for squishing infinite values into range.

  `expand`

  :   For position scales, a vector of range expansion constants used to
      add some padding around the data to ensure that they are placed
      some distance away from the axes. Use the convenience function
      [`expansion()`](https://ggplot2.tidyverse.org/reference/expansion.html)
      to generate the values for the `expand` argument. The defaults are
      to expand the scale by 5% on each side for continuous variables,
      and by 0.6 units on each side for discrete variables.

  `na.value`

  :   Missing values will be replaced with this value.

  `transform`

  :   For continuous scales, the name of a transformation object or the
      object itself. Built-in transformations include "asn", "atanh",
      "boxcox", "date", "exp", "hms", "identity", "log", "log10",
      "log1p", "log2", "logit", "modulus", "probability", "probit",
      "pseudo_log", "reciprocal", "reverse", "sqrt" and "time".

      A transformation object bundles together a transform, its inverse,
      and methods for generating breaks and labels. Transformation
      objects are defined in the scales package, and are called
      `transform_<name>`. If transformations require arguments, you can
      call them from the scales package, e.g.
      [`scales::transform_boxcox(p = 2)`](https://scales.r-lib.org/reference/transform_boxcox.html).
      You can create your own transformation with
      [`scales::new_transform()`](https://scales.r-lib.org/reference/new_transform.html).

  `trans`

  :   **\[deprecated\]** Deprecated in favour of `transform`.

  `position`

  :   For position scales, The position of the axis. `left` or `right`
      for y axes, `top` or `bottom` for x axes.

  `fallback.palette`

  :   Function to use when `palette = NULL` and the palette is not
      represented in the theme.

  `call`

  :   The `call` used to construct the scale for reporting messages.

  `na.translate`

  :   Unlike continuous scales, discrete scales can easily show missing
      values, and do so by default. If you want to remove missing values
      from a discrete scale, specify `na.translate = FALSE`.

  `drop`

  :   Should unused factor levels be omitted from the scale? The
      default, `TRUE`, uses the levels that appear in the data; `FALSE`
      includes the levels in the factor. Please note that to display
      every level in a legend, the layer should use
      `show.legend = TRUE`.

- range:

  A numeric vector of length 2 indicating the minimum and maximum size
  of the resection after transformation in millimetres. `range` is
  mutually exclusive with the `values` argument in discrete scales.

- aesthetics:

  The names of the aesthetics that this scale works with.

- guide:

  A function used to create a guide or its name. See
  [`guides()`](https://ggplot2.tidyverse.org/reference/guides.html) for
  more information.

- values:

  (Discrete scale only) A numeric vector to map data values to. The
  values will be matched in order with the limits of the scale, or with
  `breaks` if provided. If this is a named vector, the values will be
  matched based on the names instead. Data values that don't match will
  be given `na.value`. `values` is mutually exclusive with the `range`

## Value

A `<Scale>` that can be added to a plot.

## Details

Conceptually, these scales depart slightly from ggplot2 conventions. The
`scale_resect_continuous()` function returns an identity scale when
`range = NULL` (default) and a typical continuous scale when the `range`
argument is set. The `scale_resect_discrete()` acts as a manual scale
when `values` is set and as an ordinal scale when `range` is set.

## Examples

``` r
# A plot with points indicating path ends
p <- ggplot(whirlpool(5), aes(x, y, colour = group)) +
  geom_point(data = ~ subset(.x, arc == ave(arc, group, FUN = max)))

# Resect scale as an identity scale
p + geom_arrow(aes(resect_head = as.integer(group))) +
  scale_resect_continuous()


# Resect scale as typical continuous scale
p + geom_arrow(aes(resect_head = as.integer(group))) +
  scale_resect_continuous(range = c(0, 10))


# Resect scale as manual scale
p + geom_arrow(aes(resect_head = group)) +
  scale_resect_discrete(values = c(10, 5, 0, 5, 10))


# Resect scale as ordinal scale
p + geom_arrow(aes(resect_head = group)) +
  scale_resect_discrete(range = c(0, 10))
```
