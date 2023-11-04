# ggarrow 0.0.0.9000

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

## Other

An `annotate_arrow()` layer to specifically do arrow annotations.
