# Curves with arrows

This arrow geom can be used to draw curves from one point to oneanother
with arrow heads or fins.

## Usage

``` r
geom_arrow_curve(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  curvature = 0.5,
  angle = 90,
  ncp = 5,
  arrow_head = arrow_head_wings(),
  arrow_fins = NULL,
  arrow_mid = NULL,
  length = 4,
  length_head = NULL,
  length_fins = NULL,
  length_mid = NULL,
  justify = 0,
  force_arrow = FALSE,
  mid_place = 0.5,
  resect = 0,
  resect_head = NULL,
  resect_fins = NULL,
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  sep = 0,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  for which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data. A `function` can be created from a `formula` (e.g.
  `~ head(.x, 10)`).

- stat:

  The statistical transformation to use on the data for this layer. When
  using a `geom_*()` function to construct a layer, the `stat` argument
  can be used to override the default coupling between geoms and stats.
  The `stat` argument accepts the following:

  - A `Stat` ggproto subclass, for example `StatCount`.

  - A string naming the stat. To give the stat as a string, strip the
    function name of the `stat_` prefix. For example, to use
    [`stat_count()`](https://ggplot2.tidyverse.org/reference/geom_bar.html),
    give the stat as `"count"`.

  - For more information and other ways to specify the stat, see the
    [layer
    stat](https://ggplot2.tidyverse.org/reference/layer_stats.html)
    documentation.

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html).
    This method allows for passing extra arguments to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html),
    give the position as `"jitter"`.

  - For more information and other ways to specify the position, see the
    [layer
    position](https://ggplot2.tidyverse.org/reference/layer_positions.html)
    documentation.

- ...:

  Other arguments passed on to
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html)'s
  `params` argument. These arguments broadly fall into one of 4
  categories below. Notably, further arguments to the `position`
  argument, or aesthetics that are required can *not* be passed through
  `...`. Unknown arguments that are not part of the 4 categories below
  are ignored.

  - Static aesthetics that are not mapped to a scale, but are at a fixed
    value and apply to the layer as a whole. For example,
    `colour = "red"` or `linewidth = 3`. The geom's documentation has an
    **Aesthetics** section that lists the available options. The
    'required' aesthetics cannot be passed on to the `params`. Please
    note that while passing unmapped aesthetics as vectors is
    technically possible, the order and required length is not
    guaranteed to be parallel to the input data.

  - When constructing a layer using a `stat_*()` function, the `...`
    argument can be used to pass on parameters to the `geom` part of the
    layer. An example of this is
    `stat_density(geom = "area", outline.type = "both")`. The geom's
    documentation lists which parameters it can accept.

  - Inversely, when constructing a layer using a `geom_*()` function,
    the `...` argument can be used to pass on parameters to the `stat`
    part of the layer. An example of this is
    `geom_area(stat = "density", adjust = 0.5)`. The stat's
    documentation lists which parameters it can accept.

  - The `key_glyph` argument of
    [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html) may
    also be passed on through `...`. This can be one of the functions
    described as [key
    glyphs](https://ggplot2.tidyverse.org/reference/draw_key.html), to
    change the display of the layer in the legend.

- curvature:

  A numeric value giving the amount of curvature. Negative values
  produce left-hand curves, positive values produce right-hand curves,
  and zero produces a straight line.

- angle:

  A numeric value between 0 and 180, giving an amount to skew the
  control points of the curve. Values less than 90 skew the curve
  towards the start point and values greater than 90 skew the curve
  towards the end point.

- ncp:

  The number of control points used to draw the curve. More control
  points creates a smoother curve.

- arrow_head, arrow_fins, arrow_mid:

  A function call to one of the [arrow
  ornament](https://teunbrand.github.io/ggarrow/reference/arrow_ornaments.md)
  functions that can determine the shape of the arrow head, fins or
  middle (interior) arrows.

- length, length_head, length_fins, length_mid:

  Determines the size of the arrow ornaments. `length` sets the default
  length, whereas `length_head`, `length_fins` and `length_mid` set the
  lengths of the arrow head, arrow fins or middle arrows respectively.
  Can be one of the following:

  - A `<numeric>` to set the ornament size relative to the
    `linewidth{_\*}` settings.

  - A [`<unit>`](https://rdrr.io/r/grid/unit.html) to control the
    ornament size in an absolute manner. Behaviour of relative units
    such as `"npc"` or `"null"` is undefined.

- justify:

  A `numeric(1)` between \[0-1\] to control where the arrows should be
  drawn relative to the path's endpoints. A value of `0` sets the
  arrow's tips at the path's end, whereas a value of `1` sets the
  arrow's base at the path's end.

- force_arrow:

  A `logical(1)` which, if `TRUE` an arrow will be drawn even when the
  length of the arrow is shorter than the arrow heads and fins. If
  `FALSE`, will drop such arrows.

- mid_place:

  Sets the location of middle (interior) arrows, when applicable. Can be
  one of the following:

  A `numeric` vector

  :   with values between \[0-1\] to set middle arrows at relative
      positions along the arc-length of a path.

  A `<`[`unit`](https://rdrr.io/r/grid/unit.html)`>`

  :   to fill a path with arrows with the provided unit as distance
      between one arrow to the next.

- resect, resect_head, resect_fins:

  A `numeric(1)` denoting millimetres or
  `<`[`unit`](https://rdrr.io/r/grid/unit.html)`>` to shorten the arrow.
  `resect_head` shortens the arrow from the arrow head side, whereas
  `resect_fins` shortens the arrow from the fins side. Both inherit from
  `resect`.

- lineend:

  Line end style (round, butt, square).

- linejoin:

  Line join style (round, mitre, bevel).

- linemitre:

  Line mitre limit (number greater than 1).

- sep:

  A `numeric(1)` setting offset spacing in millimetres between arrow
  paths that are identical or identical as inverses. The default, 0,
  will draw paths without offsets. Alternatively, a
  `<`[`unit`](https://rdrr.io/r/grid/unit.html)`>`.

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display. To include legend
  keys for all levels, even when no data exists, use `TRUE`. If `NA`,
  all levels are shown in legend, but unobserved levels are omitted.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`annotation_borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

## Value

A `<Layer>` ggproto object that can be added to a plot.

## Aesthetics

[`geom_arrow()`](https://teunbrand.github.io/ggarrow/reference/geom_arrow.md)
understands the following aesthetics. Required aesthetics are displayed
in bold and defaults are displayed for optional aesthetics:

|  |  |  |
|----|----|----|
| • | **[`x`](https://ggplot2.tidyverse.org/reference/aes_position.html)** |  |
| • | **[`y`](https://ggplot2.tidyverse.org/reference/aes_position.html)** |  |
| • | [`alpha`](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html) | → `NA` |
| • | `arrow_fins` | → `NULL` |
| • | `arrow_head` | → `NULL` |
| • | `arrow_mid` | → `NULL` |
| • | [`colour`](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html) | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) |
| • | [`group`](https://ggplot2.tidyverse.org/reference/aes_group_order.html) | → inferred |
| • | [`linetype`](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html) | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) |
| • | [`linewidth`](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html) | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) |
| • | `resect_fins` | → `NULL` |
| • | `resect_head` | → `NULL` |
| • | `stroke_colour` | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) |
| • | `stroke_width` | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) |

Learn more about setting these aesthetics in
[`vignette("ggplot2-specs")`](https://ggplot2.tidyverse.org/articles/ggplot2-specs.html).

## See also

Other arrow geoms:
[`annotate_arrow()`](https://teunbrand.github.io/ggarrow/reference/annotate_arrow.md),
[`geom_arrow()`](https://teunbrand.github.io/ggarrow/reference/geom_arrow.md),
[`geom_arrow_chain()`](https://teunbrand.github.io/ggarrow/reference/geom_arrow_chain.md),
[`geom_arrow_segment()`](https://teunbrand.github.io/ggarrow/reference/geom_arrow_segment.md)

## Examples

``` r
curve_data <- data.frame(
  x1 = c(2.62, 1.835),
  x2 = c(3.57, 5.250),
  y1 = c(21.0, 33.9),
  y2 = c(15.0, 10.4),
  group = c("A", "B")
)

ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  geom_arrow_curve(
    aes(x = x1, y = y1, xend = x2, yend = y2,
        colour = group, arrow_head = group),
    data = curve_data,
    curvature = -0.2, length_head = 10
  )
```
