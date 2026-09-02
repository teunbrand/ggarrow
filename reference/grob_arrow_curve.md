# Arrow curve grob.

Creates a graphical object that draws curved arrows.

## Usage

``` r
grob_arrow_curve(
  x1,
  y1,
  x2,
  y2,
  default.units = "mm",
  curvature = 1,
  angle = 90,
  ncp = 1L,
  shape = 0.5,
  square = TRUE,
  squareShape = 1L,
  inflect = FALSE,
  open = TRUE,
  name = NULL,
  gp = gpar(),
  vp = NULL,
  ...,
  width_head = unit(1, "mm"),
  width_fins = unit(1, "mm")
)
```

## Arguments

- x1:

  A numeric vector or unit object specifying the x-location of the start
  point.

- y1:

  A numeric vector or unit object specifying the y-location of the start
  point.

- x2:

  A numeric vector or unit object specifying the x-location of the end
  point.

- y2:

  A numeric vector or unit object specifying the y-location of the end
  point.

- default.units:

  A string indicating the default units to use if `x1`, `y1`, `x2` or
  `y2` are only given as numeric values.

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

- shape:

  A numeric vector of values between -1 and 1, which control the shape
  of the curve relative to its control points. See `grid.xspline` for
  more details.

- square:

  A logical value that controls whether control points for the curve are
  created city-block fashion or obliquely. When `ncp` is 1 and `angle`
  is 90, this is typically `TRUE`, otherwise this should probably be set
  to `FALSE` (see Examples below).

- squareShape:

  A `shape` value to control the behaviour of the curve relative to any
  additional control point that is inserted if `square` is `TRUE`.

- inflect:

  A logical value specifying whether the curve should be cut in half and
  inverted (see Examples below).

- open:

  A logical value indicating whether to close the curve (connect the
  start and end points).

- name:

  A character identifier.

- gp:

  An object of class `"gpar"`, typically the output from a call to the
  function [`gpar`](https://rdrr.io/r/grid/gpar.html). This is basically
  a list of graphical parameter settings.

- vp:

  A Grid viewport object (or NULL).

- ...:

  Arguments passed on to
  [`grob_arrow`](https://teunbrand.github.io/ggarrow/reference/grob_arrow.md)

  `arrow_head,arrow_fins,arrow_mid`

  :   A `<matrix[n, 2]>`, such as those returned by [arrow
      ornament](https://teunbrand.github.io/ggarrow/reference/arrow_ornaments.md)
      functions, giving arrow shapes. The matrix can (should) have the
      `notch_angle` attribute that will be used to fuse the shaft to the
      arrow ornaments. If `NULL`, no ornament will be drawn.

  `length_head,length_fins,length_mid`

  :   A [`<unit>`](https://rdrr.io/r/grid/unit.html) object controlling
      the size of the arrow ornaments.

  `resect,resect_fins,resect_head`

  :   A [`<unit>`](https://rdrr.io/r/grid/unit.html) object that can be
      used to create an offset between the endings of the coordinates
      and where the arrow will be displayed visually. `resect_fins` and
      `resect_head` control this offset at the start and end of the
      arrow respectively and both default to `resect`.

  `force_arrow`

  :   A `logical(1)` which, if `TRUE` an arrow will be drawn even when
      the length of the arrow is shorter than the arrow heads and fins.
      If `FALSE`, will drop such arrows.

  `justify`

  :   A `numeric(1)` between \[0-1\] to control where the arrows should
      be drawn relative to the path's endpoints. A value of `0` sets the
      arrow's tips at the path's end, whereas a value of `1` sets the
      arrow's base at the path's end.

  `offset`

  :   A [`numeric()`](https://rdrr.io/r/base/numeric.html) vector giving
      the offset in millimetres per group to displace paths.

  `distort`

  :   **\[experimental\]**

      A way of specifying a line distortion. A line distortion is not
      great at dealing with paths with pronounced angles. One of the
      following:

      - `NULL` to not distort any paths.

      - A `<matrix[n, 2]>` of x/y coordinates giving one 'oscillation'
        of a distortion in millimetres. The
        [distortion](https://teunbrand.github.io/ggarrow/reference/distortion_functions.md)
        functions create such matrices.

      - A string, naming one of the
        [distortion](https://teunbrand.github.io/ggarrow/reference/distortion_functions.md)
        functions without the `distort_` prefix.

  `mid_place`

  :   Sets the location of middle (interior) arrows, when applicable.
      Can be one of the following:

      A `numeric` vector

      :   with values between \[0-1\] to set middle arrows at relative
          positions along the arc-length of a path.

      A `<`[`unit`](https://rdrr.io/r/grid/unit.html)`>`

      :   to fill a path with arrows with the provided unit as distance
          between one arrow to the next.

  `x`

  :   A numeric vector or unit object specifying x-values.

  `y`

  :   A numeric vector or unit object specifying y-values.

  `id`

  :   A numeric vector used to separate locations in `x` and `y` into
      multiple lines. All locations with the same `id` belong to the
      same line.

  `id.lengths`

  :   A numeric vector used to separate locations in `x` and `y` into
      multiple lines. Specifies consecutive blocks of locations which
      make up separate lines.

- width_fins, width_head:

  A [`<unit>`](https://rdrr.io/r/grid/unit.html) object controlling the
  width of the arrow's shaft at the `(x1, y1)` and `(x2, y2)` location
  respectively.

## Value

A `<curve_arrow>` [graphical
object](https://rdrr.io/r/grid/grid.grob.html).

## Examples

``` r
requireNamespace("grid")

# Creating the curved arrow
grob <- grob_arrow_curve(
  x1 = unit(0.25, "npc"), y1 = unit(0.25, "npc"),
  x2 = unit(0.75, "npc"), y2 = unit(0.75, "npc"),
  angle = 90, curvature = 0.5, ncp = 5,
  arrow_head = arrow_head_line()
)

# Drawing the arrow
grid::grid.newpage(); grid::grid.draw(grob)
```
