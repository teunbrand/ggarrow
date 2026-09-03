# Changelog

## ggarrow 0.2.0

This is a minor release introducing a few new features.

- The `grob_arrow(offset)` argument can draw the arrows at some distance
  from the original path. Note that this isn’t exposed in layers
  ([\#8](https://github.com/teunbrand/ggarrow/issues/8)).
- `geom_arrow(sep)` uses the `grob_arrow(offset)` to display co-located
  arrows in a dodged fashion
  ([\#8](https://github.com/teunbrand/ggarrow/issues/8)).
- `grob_arrow(distort)` is an experimental way to draw patterned lines.
  It is exposed in layers via the `geom_arrow(distort)` parameter too
  ([\#6](https://github.com/teunbrand/ggarrow/issues/6)).
- New asymmetric arrow ornaments
  ([\#8](https://github.com/teunbrand/ggarrow/issues/8)):
  - [`arrow_head_halfline()`](https://teunbrand.github.io/ggarrow/reference/arrow_ornaments.md)
  - [`arrow_head_halfwing()`](https://teunbrand.github.io/ggarrow/reference/arrow_ornaments.md)
- Fixed bug where numeric `resect_fins` aesthetic in
  [`geom_arrow_segment()`](https://teunbrand.github.io/ggarrow/reference/geom_arrow_segment.md)
  might have flunked a type check
  ([\#15](https://github.com/teunbrand/ggarrow/issues/15)).

## ggarrow 0.1.1

CRAN release: 2025-09-14

This is a small release adapting to new features in ggplot2 4.0.0 and
fixing a bug.

- Geom default have been adapted to the dynamic theming in ggplot2
  4.0.0.
- [`element_arrow()`](https://teunbrand.github.io/ggarrow/reference/element_arrow.md)
  is implemented as an S7 class from ggplot2 4.0.0 onwards.
- Graphical properties are better preserved when arrows are forced
  ([\#9](https://github.com/teunbrand/ggarrow/issues/9)).

## ggarrow 0.1.0

CRAN release: 2024-06-05

Initial CRAN submission

### Geom layers

Several geom functions:

- [`geom_arrow()`](https://teunbrand.github.io/ggarrow/reference/geom_arrow.md)
  that works like
  [`geom_path()`](https://ggplot2.tidyverse.org/reference/geom_path.html).
- [`geom_arrow_segment()`](https://teunbrand.github.io/ggarrow/reference/geom_arrow_segment.md)
  that works like
  [`geom_segment()`](https://ggplot2.tidyverse.org/reference/geom_segment.html)
- [`geom_arrow_chain()`](https://teunbrand.github.io/ggarrow/reference/geom_arrow_chain.md)
  that works like a `type = "b"` plot from base R.
- [`geom_arrow_curve()`](https://teunbrand.github.io/ggarrow/reference/geom_arrow_curve.md)
  that works like
  [`geom_curve()`](https://ggplot2.tidyverse.org/reference/geom_segment.html).

### Arrow grob

The function
[`grob_arrow()`](https://teunbrand.github.io/ggarrow/reference/grob_arrow.md)
creates a grid grob that largely mimics the `polylineGrob()` grob, with
the following differences.

- Arrow ornaments can be placed at the end, start or middle of the line.
  - `arrow_{head/fins/mid}` controls the shape of the ornament.
  - `length_{head/fins/mid}` controls the size of the ornament.
  - `justify` controls where an ornament is placed relative to the line
    ends.
  - `mid_place` controls where middle (interior) arrows are placed.
  - `resect{_head/_fins}` can shorten the endpoints.
  - `force_arrow` determines if arrows are drawn on lines that are too
    short to accommodate ornaments.
- The linewidth of the line can vary within a line.
  - `shaft_width` controls the linewidth for every point in `x` and `y`
- A line can be shortened from their endpoints using the
  `resect{_head/_fins}` arguments.
- The line is treated as a polygon, so `fill` controls the colour of the
  line, whereas `colour` controls the colour of an outline.

### Arrow scales

The `arrow_{head/fins/mid}` have been promoted from parameters to
aesthetics. Correspondingly, there are now 3 discrete scales for these 3
aesthetics.

### Arrow shapes

Added the following shapes for ornaments:

- [`arrow_head_wings()`](https://teunbrand.github.io/ggarrow/reference/arrow_ornaments.md)
  that looks like a classic arrow.
- [`arrow_head_line()`](https://teunbrand.github.io/ggarrow/reference/arrow_ornaments.md)
  that are two line protruding from the tip of the line.
- [`arrow_head_minimal()`](https://teunbrand.github.io/ggarrow/reference/arrow_ornaments.md)
  that is just a stumpy line-end.
- [`arrow_fins_feather()`](https://teunbrand.github.io/ggarrow/reference/arrow_ornaments.md)
  two stacked parallelograms.
- [`arrow_fins_line()`](https://teunbrand.github.io/ggarrow/reference/arrow_ornaments.md)
  also two lines but in opposite direction.
- [`arrow_fins_minimal()`](https://teunbrand.github.io/ggarrow/reference/arrow_ornaments.md)
  that is just a line-end with a triangle taken out.
- [`arrow_cup()`](https://teunbrand.github.io/ggarrow/reference/arrow_ornaments.md)
  that forms an arc around a point.

### Other

An
[`annotate_arrow()`](https://teunbrand.github.io/ggarrow/reference/annotate_arrow.md)
layer to specifically do arrow annotations. An
[`element_arrow()`](https://teunbrand.github.io/ggarrow/reference/element_arrow.md)
to replace line elements in the
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) by
arrows.
