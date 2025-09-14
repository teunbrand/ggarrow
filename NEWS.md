# ggarrow (development version)

* Geom default have been adapted to the dynamic theming in ggplot2 4.0.0.
* `element_arrow()` is implemented as an S7 class from ggplot2 4.0.0 onwards.
* Graphical properties are better preserved when arrows are forced (#9).

# ggarrow 0.1.0

Initial CRAN submission

## Geom layers

Several geom functions:

* `geom_arrow()` that works like `geom_path()`.
* `geom_arrow_segment()` that works like `geom_segment()`
* `geom_arrow_chain()` that works like a `type = "b"` plot from base R.
* `geom_arrow_curve()` that works like `geom_curve()`.

## Arrow grob

The function `grob_arrow()` creates a grid grob that largely mimics the 
`polylineGrob()` grob, with the following differences.

* Arrow ornaments can be placed at the end, start or middle of the line.
    * `arrow_{head/fins/mid}` controls the shape of the ornament.
    * `length_{head/fins/mid}` controls the size of the ornament.
    * `justify` controls where an ornament is placed relative to the line ends.
    * `mid_place` controls where middle (interior) arrows are placed.
    * `resect{_head/_fins}` can shorten the endpoints.
    * `force_arrow` determines if arrows are drawn on lines that are too short
      to accommodate ornaments.
* The linewidth of the line can vary within a line.
    * `shaft_width` controls the linewidth for every point in `x` and `y`
* A line can be shortened from their endpoints using the `resect{_head/_fins}` 
  arguments.
* The line is treated as a polygon, so `fill` controls the colour of the line,
  whereas `colour` controls the colour of an outline.
  
## Arrow scales

The `arrow_{head/fins/mid}` have been promoted from parameters to aesthetics.
Correspondingly, there are now 3 discrete scales for these 3 aesthetics.

## Arrow shapes

Added the following shapes for ornaments:

* `arrow_head_wings()` that looks like a classic arrow.
* `arrow_head_line()` that are two line protruding from the tip of the line.
* `arrow_head_minimal()` that is just a stumpy line-end.
* `arrow_fins_feather()` two stacked parallelograms.
* `arrow_fins_line()` also two lines but in opposite direction.
* `arrow_fins_minimal()` that is just a line-end with a triangle taken out.
* `arrow_cup()` that forms an arc around a point.

## Other

An `annotate_arrow()` layer to specifically do arrow annotations.
An `element_arrow()` to replace line elements in the `theme()` by arrows.
